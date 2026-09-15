! fx_html.f90 — transcript.html writer.  Port of racket/html.rkt (transcript->html), byte-for-byte:
! self-contained dark page, one <details open> per rollout, clickable links, attachments embedded
! from the attachments/ folder by relative path, SPEC 0.6 Build-mode tool cards.
!
! Racket semantics reproduced here:
!   esc        escapes & < > " ; a non-string value goes through py-str, JSON null renders ""
!   one-line   \r removed, \n -> space, cut to `limit` CODE POINTS (not bytes)
!   reply-html citation offsets are code-point offsets into the reply text
!   ~a         `format "~a"` of a jsexpr (turn index): ints as digits, strings raw, null "null"
!   pretty-json-string  md.rkt's display printer: Racket string->jsexpr acceptance rules (one
!              value, trailing text ignored, strict escapes/surrogates/numbers), keys sorted by
!              code point with the last duplicate winning, floats printed as Racket CS prints them
module fx_html
  use iso_fortran_env, only: int64, real64
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  use fx_util
  use fx_json
  use fx_grok, only: truthy, py_str
  implicit none
  private
  public :: transcript_html, pretty_json_string

  character(len=*), parameter :: NL = achar(10)
  character(len=*), parameter :: CR = achar(13)
  character(len=*), parameter :: DOT = ' '//achar(194)//achar(183)//' '        ! " · "
  character(len=*), parameter :: EMDASH = achar(226)//achar(128)//achar(148)    ! "—"
  character(len=*), parameter :: A_TAIL = '" target="_blank" rel="noopener noreferrer">'

  character(len=*), parameter :: CSS = &
    'body{background:#111418;color:#d7dce2;font:15px/1.5 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:0;padding:24px;}' // &
    'main{max-width:1100px;margin:0 auto;}' // &
    'h1{font-size:22px;color:#fff;margin:0 0 6px;}' // &
    '.meta{color:#8b95a1;font-size:13px;margin-bottom:24px;} .meta a{color:#7fb3ff;}' // &
    'a{color:#7fb3ff;text-decoration:none;} a:hover{text-decoration:underline;}' // &
    '.turn{border:1px solid #262c34;border-radius:10px;padding:16px 18px;margin:0 0 18px;background:#161a1f;}' // &
    '.turn.user{background:#1b2230;border-color:#2b3a52;}' // &
    '.turn-head{font-weight:600;color:#fff;margin-bottom:8px;} .turn-head .ts{color:#8b95a1;font-weight:400;font-size:12px;margin-left:8px;}' // &
    '.text{white-space:pre-wrap;word-wrap:break-word;}' // &
    '.attachments{margin:6px 0 12px;} .attachments figure{display:inline-block;margin:0 12px 8px 0;vertical-align:top;}' // &
    '.attachments img{max-width:420px;max-height:420px;border:1px solid #2a323c;border-radius:6px;display:block;}' // &
    '.attachments figcaption{font-size:12px;color:#8b95a1;margin-top:4px;}' // &
    'details{border:1px solid #2a323c;border-radius:8px;margin:8px 0;background:#12161b;}' // &
    'summary{cursor:pointer;padding:8px 12px;font-weight:600;color:#e6ebf0;} summary .role{color:#f0b35a;font-weight:400;font-size:12px;margin-left:6px;}' // &
    '.events{padding:4px 14px 10px 14px;} .ev{margin:6px 0;padding-left:10px;border-left:2px solid #2a323c;}' // &
    '.ev.summary{color:#aeb7c2;font-style:italic;} .ev.tool .label{color:#9ad0a0;font-weight:600;} .ev.tool .q{color:#e6ebf0;}' // &
    '.ev.chatroom .label{color:#f0b35a;font-weight:600;} .ev.chatroom .msg{white-space:pre-wrap;background:#0e1114;border-radius:6px;padding:8px 10px;margin-top:4px;}' // &
    '.ev.tool pre.code{white-space:pre-wrap;word-wrap:break-word;overflow-x:auto;background:#0e1114;border:1px solid #232a32;border-radius:6px;padding:8px 10px;margin:4px 0 0;font:12px/1.45 Consolas,Menlo,Monaco,monospace;color:#cdd6df;}' // &
    '.ev.tool .sub{color:#8b95a1;font-size:12px;margin-top:6px;}' // &
    '.results{margin:4px 0 0 0;padding-left:18px;font-size:13px;color:#aeb7c2;} .results li{margin:2px 0;}' // &
    '.results .prev{color:#7d8794;} .xpost{color:#aeb7c2;}' // &
    '.section-title{margin:16px 0 6px;font-size:13px;letter-spacing:.06em;text-transform:uppercase;color:#8b95a1;}' // &
    'sup.cite a{color:#f0b35a;font-size:11px;} ol.cites{font-size:13px;color:#aeb7c2;}' // &
    '.sources ul{padding-left:18px;font-size:13px;color:#aeb7c2;} .sources li{margin:2px 0;}' // &
    '.warn{color:#f0b35a;}'

contains

  ! ================================================================ public entry point

  ! transcript->html
  !   t                 handle of the transcript object (transcript.json as parsed by jparse)
  !   attachment_paths  handle of a JSON object fileId -> relative path ("attachments/0-<id>-image.png");
  !                     0 (or any non-object) = the Racket default (hash).  An entry whose value is not
  !                     a string counts as absent (Racket's #f entry = not downloaded).
  !   out_text          the complete page (UTF-8)
  !   generator         #:generator, default "grok-export-rkt"; grok-export.rkt passes "grok-export-rkt 0.2.0"
  subroutine transcript_html(t, attachment_paths, out_text, generator)
    integer, intent(in) :: t, attachment_paths
    character(len=:), allocatable, intent(out) :: out_text
    character(len=*), intent(in), optional :: generator
    type(strbuf) :: o
    character(len=:), allocatable :: gen, title, idx, rel, mime, u, tl
    integer :: c, tn, turn, p, atts, a, th, rl, ev, cits, ci, n, s, webs, xs, w, x, ipass, im
    logical :: found

    if (present(generator)) then
       gen = generator
    else
       gen = 'grok-export-rkt'
    end if
    c = hsh(jget(t, 'conversation'))
    tn = py_or2(jget(c, 'title'), jget(c, 'conversationId'))
    if (tn == 0) then
       title = ''
    else
       title = py_str(tn)
    end if
    call sb_add(o, '<!doctype html>'//NL//'<html lang="en">'//NL//'<head>'//NL//'<meta charset="utf-8">'//NL)
    call sb_add(o, '<meta name="viewport" content="width=device-width, initial-scale=1">'//NL)
    call sb_add(o, '<meta name="generator" content="'//esc(gen)//'">'//NL)
    call sb_add(o, '<title>'//esc(title)//'</title>'//NL//'<style>'//CSS//'</style>'//NL//'</head>'//NL// &
                   '<body>'//NL//'<main>'//NL)
    call sb_add(o, '<h1>'//esc(title)//'</h1>'//NL)
    call sb_add(o, '<div class="meta">Conversation '//escn(jget(c, 'conversationId')))
    call sb_add(o, DOT//'Source '//link_node(jget(c, 'sourceUrl')))
    call sb_add(o, DOT//'Created '//escn(jget(c, 'createTime')))
    call sb_add(o, DOT//'Modified '//escn(jget(c, 'modifyTime')))
    p = jget(c, 'isPublic')
    if (.not. jis_null(p)) call sb_add(o, DOT//'Public '//py_str(p))
    call sb_add(o, '</div>'//NL)

    ! offBranchTurns (edited messages, regenerated replies) follow the current branch under their own heading
    do ipass = 1, 2
    if (ipass == 1) then
       turn = jfirst(lst(jget(t, 'turns')))
    else
       turn = jfirst(lst(jget(t, 'offBranchTurns')))
       if (turn > 0) call sb_add(o, '<h2 class="other-versions">Other versions</h2>'//NL// &
          '<div class="meta">Edited messages and regenerated replies that are not on the current branch of the '// &
          'conversation (the branch the page shows).</div>'//NL)
    end if
    do while (turn > 0)
       if (ipass == 1) then
          idx = disp(jget(turn, 'index')); tl = 'turn '//idx
       else
          idx = 'o'//disp(jget(turn, 'index')); tl = 'other version '//disp(jget(turn, 'index'))
       end if
       if (node_is(jget(turn, 'sender'), 'human')) then
          call sb_add(o, '<section class="turn user" id="turn-'//idx//'">'//NL)
          call sb_add(o, '<div class="turn-head">User <span class="ts">'//tl//DOT//escn(jget(turn, 'createTime'))// &
                         '</span></div>'//NL)
          call sb_add(o, image_surfaces_html(turn))
          atts = lst(jget(turn, 'attachments'))
          if (atts > 0) then
             call sb_add(o, '<div class="attachments">'//NL)
             a = jfirst(atts)
             do while (a > 0)
                call att_lookup(attachment_paths, jget(a, 'fileId'), rel, found)
                mime = py_str(jget(a, 'mimeType'))
                call sb_add(o, '<figure>')
                if (found .and. starts_with(mime, 'image/')) then
                   call sb_add(o, '<a href="'//esc(rel)//'"><img src="'//esc(rel)//'" alt="'//escn(jget(a, 'fileName'))//'"></a>')
                else if (found) then
                   call sb_add(o, '<a href="'//esc(rel)//'">'//escn(jget(a, 'fileName'))//'</a>')
                else
                   call sb_add(o, link_node_text(jget(a, 'contentUrl'), py_str(jget(a, 'fileName'))))
                end if
                call sb_add(o, '<figcaption>'//escn(jget(a, 'fileName'))//' ('//esc(mime)//', '//escn(jget(a, 'sizeBytes'))// &
                               ' bytes)')
                if (.not. found) call sb_add(o, ' <span class="warn">not downloaded</span>')
                call sb_add(o, '</figcaption>')
                call sb_add(o, '</figure>'//NL)
                a = jnext(a)
             end do
             call sb_add(o, '</div>'//NL)
          end if
          call sb_add(o, '<div class="text">'//escn(jget(turn, 'text'))//'</div>'//NL//'</section>'//NL)
       else
          th = hsh(jget(turn, 'thinking'))
          call sb_add(o, '<section class="turn assistant" id="turn-'//idx//'">'//NL)
          call sb_add(o, '<div class="turn-head">Grok <span class="ts">'//tl//DOT//escn(jget(turn, 'createTime'))// &
                         DOT//'model '//escn(jget(turn, 'model'))//'</span></div>'//NL)
          call sb_add(o, image_surfaces_html(turn))
          call sb_add(o, '<div class="section-title">Thoughts ('//escn(jget(th, 'durationMs'))//' ms)</div>'//NL)
          rl = jfirst(lst(jget(th, 'rollouts')))
          do while (rl > 0)
             call sb_add(o, '<details open><summary>'//escn(jget(rl, 'id')))
             if (node_is(jget(rl, 'role'), 'Leader')) call sb_add(o, '<span class="role">Leader</span>')
             call sb_add(o, '<span class="role">'//itoa(jlen(lst(jget(rl, 'events'))))//' events</span>')
             call sb_add(o, '</summary>'//NL//'<div class="events">'//NL)
             ev = jfirst(lst(jget(rl, 'events')))
             do while (ev > 0)
                call sb_add(o, event_html(ev))
                ev = jnext(ev)
             end do
             call sb_add(o, '</div>'//NL//'</details>'//NL)
             rl = jnext(rl)
          end do
          call sb_add(o, '<div class="section-title">Reply</div>'//NL)
          cits = lst(jget(turn, 'citations'))
          call sb_add(o, '<div class="text">'//reply_html(py_str(jget(turn, 'text')), cits, idx)//'</div>'//NL)
          if (cits > 0) then
             call sb_add(o, '<ol class="cites">'//NL)
             ci = jfirst(cits); n = 0
             do while (ci > 0)
                n = n + 1
                p = jget(ci, 'url')
                call sb_add(o, '<li id="cite-'//idx//'-'//itoa(n)//'">')
                if (truthy(p)) then
                   call sb_add(o, link_node(p))
                else
                   call sb_add(o, 'unresolved')
                end if
                call sb_add(o, ' <span class="prev">(citationId '//escn(jget(ci, 'citationId'))//', card '// &
                               escn(jget(ci, 'cardId')))
                p = jget(ci, 'kind')
                if (truthy(p)) call sb_add(o, ', '//escn(p))
                call sb_add(o, ')</span></li>'//NL)
                ci = jnext(ci)
             end do
             call sb_add(o, '</ol>'//NL)
          end if
          if (jlen(lst(jget(turn, 'images'))) > 0) then
             call sb_add(o, '<div class="attachments">'//NL)
             im = jfirst(lst(jget(turn, 'images')))
             do while (im > 0)
                call sb_add(o, '<figure><a href="'//escn(jget(im, 'link'))//'"><img src="'//escn(jget(im, 'url'))//'" alt="'// &
                               escn(jget(im, 'title'))//'" loading="lazy"></a><figcaption>'//escn(jget(im, 'title'))//DOT// &
                               escn(jget(im, 'source'))//'</figcaption></figure>'//NL)
                im = jnext(im)
             end do
             call sb_add(o, '</div>'//NL)
          end if
          s = hsh(jget(turn, 'sources'))
          webs = lst(jget(s, 'webSearchResults'))
          xs = lst(jget(s, 'xposts'))
          call sb_add(o, '<div class="section-title">Sources ('//itoa(jlen(webs))//' web, '//itoa(jlen(xs))//' X posts, '// &
                         escn(jget(s, 'toolResultRows'))//' tool result rows)</div>'//NL)
          call sb_add(o, '<div class="sources"><ul>'//NL)
          w = jfirst(webs)
          do while (w > 0)
             call sb_add(o, '<li>'//link_node_text(jget(w, 'url'), one_line_n(py_or2(jget(w, 'title'), jget(w, 'url'))))// &
                            '</li>'//NL)
             w = jnext(w)
          end do
          x = jfirst(xs)
          do while (x > 0)
             u = 'https://x.com/'//py_str(jget(x, 'username'))//'/status/'//py_str(jget(x, 'postId'))
             call sb_add(o, '<li class="xpost">'//link_str_text(u, '@'//py_str(jget(x, 'username'))//' '// &
                            py_str(jget(x, 'postId')))//': '//esc(one_line_n(jget(x, 'text'), 300))//'</li>'//NL)
             x = jnext(x)
          end do
          call sb_add(o, '</ul></div>'//NL//'</section>'//NL)
       end if
       turn = jnext(turn)
    end do
    end do
    call sb_add(o, '</main>'//NL//'</body>'//NL//'</html>'//NL)
    out_text = sb_str(o)
  end subroutine transcript_html

  ! ================================================================ small helpers

  logical function streq(a, b)          ! exact equality (Fortran == pads with blanks)
    character(len=*), intent(in) :: a, b
    streq = .false.
    if (len(a) == len(b)) streq = (a == b)
  end function streq

  logical function node_is(p, s)        ! (equal? p "s")
    integer, intent(in) :: p
    character(len=*), intent(in) :: s
    node_is = .false.
    if (jis_str(p)) node_is = streq(jstr(p), s)
  end function node_is

  integer function lst(p)               ! or-empty-list: handle when a non-empty array, else 0
    integer, intent(in) :: p
    lst = 0
    if (jis_arr(p)) then
       if (jlen(p) > 0) lst = p
    end if
  end function lst

  integer function hsh(p)               ! or-empty-hash: handle when a non-empty object, else 0
    integer, intent(in) :: p
    hsh = 0
    if (jis_obj(p)) then
       if (jlen(p) > 0) hsh = p
    end if
  end function hsh

  integer function py_or2(a, b)         ! (py-or a b ""): first truthy handle, 0 standing for ""
    integer, intent(in) :: a, b
    py_or2 = 0
    if (truthy(a)) then
       py_or2 = a
    else if (truthy(b)) then
       py_or2 = b
    end if
  end function py_or2

  function esc(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: b
    integer :: i, run
    if (scan(s, '&<>"') == 0) then
       r = s; return
    end if
    run = 1
    do i = 1, len(s)
       select case (s(i:i))
       case ('&', '<', '>', '"')
          if (i > run) call sb_add(b, s(run:i-1))
          select case (s(i:i))
          case ('&'); call sb_add(b, '&amp;')
          case ('<'); call sb_add(b, '&lt;')
          case ('>'); call sb_add(b, '&gt;')
          case default; call sb_add(b, '&quot;')
          end select
          run = i + 1
       end select
    end do
    if (run <= len(s)) call sb_add(b, s(run:))
    r = sb_str(b)
  end function esc

  function escn(p) result(r)            ! (esc v) for a JSON value
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    if (jis_str(p)) then
       r = esc(jstr(p))
    else if (jis_null(p)) then
       r = ''
    else
       r = esc(py_str(p))
    end if
  end function escn

  logical function is_url(s)
    character(len=*), intent(in) :: s
    is_url = starts_with(s, 'http://') .or. starts_with(s, 'https://')
  end function is_url

  logical function nurl(p)
    integer, intent(in) :: p
    nurl = .false.
    if (jis_str(p)) nurl = is_url(jstr(p))
  end function nurl

  function anchor(eu, et) result(r)
    character(len=*), intent(in) :: eu, et
    character(len=:), allocatable :: r
    r = '<a href="'//eu//A_TAIL//et//'</a>'
  end function anchor

  function link_node(p) result(r)       ! (link u)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    if (nurl(p)) then
       r = esc(jstr(p)); r = anchor(r, r)
    else
       r = escn(p)
    end if
  end function link_node

  function link_node_text(p, text) result(r)   ! (link u text) with a string text
    integer, intent(in) :: p
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: r
    if (nurl(p)) then
       r = anchor(esc(jstr(p)), esc(text))
    else
       r = esc(text)
    end if
  end function link_node_text

  function link_str_text(u, text) result(r)
    character(len=*), intent(in) :: u, text
    character(len=:), allocatable :: r
    if (is_url(u)) then
       r = anchor(esc(u), esc(text))
    else
       r = esc(text)
    end if
  end function link_str_text

  function link_str(u) result(r)
    character(len=*), intent(in) :: u
    character(len=:), allocatable :: r
    if (is_url(u)) then
       r = esc(u); r = anchor(r, r)
    else
       r = esc(u)
    end if
  end function link_str

  ! ---------------------------------------------------------------- code points

  integer function ubyte(ch)
    character, intent(in) :: ch
    ubyte = iachar(ch)
    if (ubyte < 0) ubyte = ubyte + 256
  end function ubyte

  integer function cp_count(s)
    character(len=*), intent(in) :: s
    integer :: i, c
    cp_count = 0
    do i = 1, len(s)
       c = ubyte(s(i:i))
       if (c < 128 .or. c >= 192) cp_count = cp_count + 1
    end do
  end function cp_count

  ! byte position just after k more code points, starting at byte position b (next unread byte)
  integer function cp_skip(s, b, k) result(q)
    character(len=*), intent(in) :: s
    integer, intent(in) :: b, k
    integer :: j, c
    q = b
    do j = 1, k
       if (q > len(s)) exit
       q = q + 1
       do while (q <= len(s))
          c = ubyte(s(q:q))
          if (c < 128 .or. c >= 192) exit
          q = q + 1
       end do
    end do
  end function cp_skip

  function one_line_s(s0, limit) result(r)
    character(len=*), intent(in) :: s0
    integer, intent(in), optional :: limit
    character(len=:), allocatable :: r
    r = s0
    if (index(r, CR) > 0) r = replace_all(r, CR, '')
    if (index(r, NL) > 0) r = replace_all(r, NL, ' ')
    if (present(limit)) then
       if (cp_count(r) > limit) r = r(1:cp_skip(r, 1, limit) - 1)
    end if
  end function one_line_s

  function one_line_n(p, limit) result(r)
    integer, intent(in) :: p
    integer, intent(in), optional :: limit
    character(len=:), allocatable :: r
    if (jis_str(p)) then
       r = one_line_s(jstr(p), limit)
    else if (truthy(p)) then
       r = one_line_s(py_str(p), limit)
    else
       r = ''
    end if
  end function one_line_n

  ! ---------------------------------------------------------------- numbers / display

  ! number->string of a JSON number as Racket's read-json reads it
  function num_text(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    r = jraw_text(p)
    if (scan(r, '.eE') == 0) then
       if (verify(r, '-0') == 0) r = '0'
    else
       r = racket_json_float(r)
    end if
  end function num_text

  ! (format "~a" v) of a jsexpr
  recursive function disp(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    type(strbuf) :: b
    integer :: q
    select case (jtype(p))
    case (J_STR);  r = jstr(p)
    case (J_NUM);  r = num_text(p)
    case (J_TRUE); r = '#t'
    case (J_FALSE); r = '#f'
    case (J_ARR)
       call sb_add(b, '(')
       q = jfirst(p)
       do while (q > 0)
          if (q /= jfirst(p)) call sb_add(b, ' ')
          call sb_add(b, disp(q)); q = jnext(q)
       end do
       call sb_add(b, ')'); r = sb_str(b)
    case (J_OBJ)
       call sb_add(b, '#hasheq(')
       q = jfirst(p)
       do while (q > 0)
          if (q /= jfirst(p)) call sb_add(b, ' ')
          call sb_add(b, '('//jname(q)//' . '//disp(q)//')'); q = jnext(q)
       end do
       call sb_add(b, ')'); r = sb_str(b)
    case default;  r = 'null'
    end select
  end function disp

  ! A JSON float literal (grammar already validated) -> Racket CS number->string of the flonum
  ! read-json makes of it; +inf.0/-inf.0 come back as the JSON string literal write-pretty-json emits.
  function racket_json_float(t) result(r)
    character(len=*), intent(in) :: t
    character(len=:), allocatable :: r
    character(len=:), allocatable :: digs
    integer :: i, sgn, ex, nfrac, ios, lead
    logical :: has_exp, zero
    real(real64) :: x, rexp
    sgn = 1; i = 1
    if (t(1:1) == '-') then
       sgn = -1; i = 2
    end if
    digs = ''; nfrac = 0; has_exp = .false.; ex = 0
    do while (i <= len(t))
       if (t(i:i) == '.' .or. t(i:i) == 'e' .or. t(i:i) == 'E') exit
       digs = digs//t(i:i); i = i + 1
    end do
    if (i <= len(t)) then
       if (t(i:i) == '.') then
          i = i + 1
          do while (i <= len(t))
             if (t(i:i) == 'e' .or. t(i:i) == 'E') exit
             digs = digs//t(i:i); nfrac = nfrac + 1; i = i + 1
          end do
       end if
    end if
    if (i <= len(t)) then
       has_exp = .true.
       read(t(i+1:), *, iostat=ios) ex
       if (ios /= 0) ex = 0
    end if
    lead = verify(digs, '0')
    zero = (lead == 0)
    if (zero) then
       r = '0.0'; return
    end if
    if (has_exp) then
       ! safe-exponential->inexact: (log |n| 10) + exp outside [-400, 400] short-circuits
       read(digs(lead:min(len(digs), lead+15)), *, iostat=ios) rexp
       rexp = log10(rexp) + real(len(digs) - min(len(digs), lead+15), real64) + real(ex - nfrac, real64)
       if (rexp < -400.0_real64) then
          if (sgn > 0) then
             r = '0.0'
          else
             r = '-0.0'
          end if
          return
       else if (rexp > 400.0_real64) then
          if (sgn > 0) then
             r = '"+inf.0"'
          else
             r = '"-inf.0"'
          end if
          return
       end if
    end if
    read(t, *, iostat=ios) x
    if (ios /= 0 .or. .not. ieee_is_finite(x)) then
       if (sgn > 0) then
          r = '"+inf.0"'
       else
          r = '"-inf.0"'
       end if
       return
    end if
    if (x == 0.0_real64) then            ! underflow keeps the sign: -3e-330 -> -0.0
       if (sign(1.0_real64, x) < 0.0_real64) then
          r = '-0.0'
       else
          r = '0.0'
       end if
       return
    end if
    r = racket_flonum(x)
  end function racket_json_float

  ! number->string of a finite non-zero flonum, Racket CS: shortest round-trip digits;
  ! positional when -4 <= E <= max(13, ndigits+2) (E = scientific exponent), else d.ddde+NN / d.ddde-N
  function racket_flonum(x) result(r)
    real(real64), intent(in) :: x
    character(len=:), allocatable :: r
    character(len=64) :: buf
    character(len=16) :: fmt
    character(len=:), allocatable :: m, digs
    integer :: p, k, e, ios, nd
    real(real64) :: y
    digs = ''; e = 0
    do p = 1, 17
       ! RC: a digit tie rounds away from zero, as Racket's shortest printer does (…676.25 -> …676.3)
       write(fmt, '(A,I0,A)') '(RC,ES40.', p - 1, 'E5)'
       write(buf, fmt) abs(x)
       read(buf, *, iostat=ios) y
       if (ios == 0 .and. y == abs(x)) exit
    end do
    buf = adjustl(buf)
    k = index(buf, 'E')
    m = buf(1:k-1)
    read(buf(k+1:), *) e
    digs = ''
    do p = 1, len(m)
       if (m(p:p) >= '0' .and. m(p:p) <= '9') digs = digs//m(p:p)
    end do
    nd = len(digs)
    do while (nd > 1 .and. digs(nd:nd) == '0')
       nd = nd - 1
    end do
    digs = digs(1:nd)
    if (e >= -4 .and. e <= max(13, nd + 2)) then
       if (e >= 0) then
          if (nd <= e + 1) then
             r = digs//repeat('0', e + 1 - nd)//'.0'
          else
             r = digs(1:e+1)//'.'//digs(e+2:)
          end if
       else
          r = '0.'//repeat('0', -e - 1)//digs
       end if
    else
       r = digs(1:1)
       if (nd > 1) r = r//'.'//digs(2:)
       if (e >= 0) then
          r = r//'e+'//itoa(e)
       else
          r = r//'e'//itoa(e)
       end if
    end if
    if (x < 0.0_real64) r = '-'//r
  end function racket_flonum

  ! ---------------------------------------------------------------- pretty JSON (md.rkt)

  ! pretty-json-string: s parsed with string->jsexpr; an object/array is printed with sorted keys,
  ! anything else (or a parse failure) returns s unchanged.
  function pretty_json_string(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: i, start, root
    type(strbuf) :: b
    logical :: ok
    r = s
    i = 1
    call jv_ws(s, i)
    if (i > len(s)) return
    if (s(i:i) /= '{' .and. s(i:i) /= '[') return
    start = i
    ok = jv_value(s, i, 0)
    if (.not. ok) return
    root = jparse(s(start:i-1))
    if (root == 0) return
    call pj_write(b, root, 0)
    r = sb_str(b)
  end function pretty_json_string

  subroutine jv_ws(s, i)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: i
    do while (i <= len(s))
       select case (iachar(s(i:i)))
       case (32, 9, 10, 13)
          i = i + 1
       case default
          exit
       end select
    end do
  end subroutine jv_ws

  recursive logical function jv_value(s, i, depth) result(ok)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: i
    integer, intent(in) :: depth
    ok = .false.
    if (depth > 4000) return        ! fx_json's jparse nesting limit
    call jv_ws(s, i)
    if (i > len(s)) return
    select case (s(i:i))
    case ('t'); ok = jv_lit(s, i, 'true')
    case ('f'); ok = jv_lit(s, i, 'false')
    case ('n'); ok = jv_lit(s, i, 'null')
    case ('-', '0':'9'); ok = jv_num(s, i)
    case ('"'); i = i + 1; ok = jv_str(s, i)
    case ('['); i = i + 1; ok = jv_list(s, i, ']', .false., depth)
    case ('{'); i = i + 1; ok = jv_list(s, i, '}', .true., depth)
    end select
  end function jv_value

  recursive logical function jv_list(s, i, endc, isobj, depth) result(ok)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: i
    character, intent(in) :: endc
    logical, intent(in) :: isobj
    integer, intent(in) :: depth
    ok = .false.
    call jv_ws(s, i)
    if (i > len(s)) return
    if (s(i:i) == endc) then
       i = i + 1; ok = .true.; return
    end if
    do
       if (isobj) then
          call jv_ws(s, i)
          if (i > len(s)) return
          if (s(i:i) /= '"') return
          i = i + 1
          if (.not. jv_str(s, i)) return
          call jv_ws(s, i)
          if (i > len(s)) return
          if (s(i:i) /= ':') return
          i = i + 1
       end if
       if (.not. jv_value(s, i, depth + 1)) return
       call jv_ws(s, i)
       if (i > len(s)) return
       if (s(i:i) == endc) then
          i = i + 1; ok = .true.; return
       else if (s(i:i) == ',') then
          i = i + 1
       else
          return
       end if
    end do
  end function jv_list

  logical function jv_lit(s, i, word) result(ok)
    character(len=*), intent(in) :: s, word
    integer, intent(inout) :: i
    integer :: c
    ok = .false.
    if (i + len(word) - 1 > len(s)) return
    if (s(i:i+len(word)-1) /= word) return
    i = i + len(word)
    if (i <= len(s)) then
       c = iachar(s(i:i))
       if ((c >= 97 .and. c <= 122) .or. (c >= 65 .and. c <= 90) .or. (c >= 48 .and. c <= 57) .or. c == 95) return
    end if
    ok = .true.
  end function jv_lit

  logical function isdig(s, i)
    character(len=*), intent(in) :: s
    integer, intent(in) :: i
    isdig = .false.
    if (i <= len(s)) isdig = (s(i:i) >= '0' .and. s(i:i) <= '9')
  end function isdig

  logical function jv_num(s, i) result(ok)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: i
    ok = .false.
    if (s(i:i) == '-') i = i + 1
    if (.not. isdig(s, i)) return
    if (s(i:i) == '0') then
       i = i + 1
    else
       do while (isdig(s, i))
          i = i + 1
       end do
    end if
    if (i <= len(s)) then
       if (s(i:i) == '.') then
          i = i + 1
          if (.not. isdig(s, i)) return
          do while (isdig(s, i))
             i = i + 1
          end do
       end if
    end if
    if (i <= len(s)) then
       if (s(i:i) == 'e' .or. s(i:i) == 'E') then
          i = i + 1
          if (i <= len(s)) then
             if (s(i:i) == '+' .or. s(i:i) == '-') i = i + 1
          end if
          if (.not. isdig(s, i)) return
          do while (isdig(s, i))
             i = i + 1
          end do
       end if
    end if
    ok = .true.
  end function jv_num

  ! string body starting after the opening quote; i ends after the closing quote
  logical function jv_str(s, i) result(ok)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: i
    integer :: e, e2
    ok = .false.
    do
       if (i > len(s)) return
       select case (s(i:i))
       case ('"')
          i = i + 1; ok = .true.; return
       case ('\')
          i = i + 1
          if (i > len(s)) return
          select case (s(i:i))
          case ('b', 'n', 'r', 'f', 't', '\', '"', '/')
             i = i + 1
          case ('u')
             e = hex4(s, i + 1)
             if (e < 0) return
             i = i + 5
             if (e >= 55296 .and. e <= 56319) then
                if (i + 1 > len(s)) return
                if (s(i:i+1) /= '\u') return
                e2 = hex4(s, i + 2)
                if (e2 < 56320 .or. e2 > 57343) return
                i = i + 6
             else if (e >= 56320 .and. e <= 57343) then
                return
             end if
          case default
             return
          end select
       case default
          i = i + 1
       end select
    end do
  end function jv_str

  integer function hex4(s, p)            ! -1 when s(p:p+3) is not 4 hex digits
    character(len=*), intent(in) :: s
    integer, intent(in) :: p
    integer :: k, c
    hex4 = -1
    if (p + 3 > len(s)) return
    hex4 = 0
    do k = p, p + 3
       c = iachar(s(k:k))
       if (c >= 48 .and. c <= 57) then
          hex4 = hex4*16 + c - 48
       else if (c >= 65 .and. c <= 70) then
          hex4 = hex4*16 + c - 55
       else if (c >= 97 .and. c <= 102) then
          hex4 = hex4*16 + c - 87
       else
          hex4 = -1; return
       end if
    end do
  end function hex4

  logical function bytes_gt(a, b)        ! string>? by code point == unsigned UTF-8 bytes
    character(len=*), intent(in) :: a, b
    integer :: i, ca, cb
    do i = 1, min(len(a), len(b))
       ca = ubyte(a(i:i)); cb = ubyte(b(i:i))
       if (ca /= cb) then
          bytes_gt = (ca > cb); return
       end if
    end do
    bytes_gt = (len(a) > len(b))
  end function bytes_gt

  recursive subroutine pj_write(b, p, ind)
    type(strbuf), intent(inout) :: b
    integer, intent(in) :: p, ind
    type :: namebox
       character(len=:), allocatable :: s
    end type namebox
    type(namebox), allocatable :: nm(:)
    type(namebox) :: tn
    integer, allocatable :: kid(:)
    integer :: n, m, q, i, j, tk
    logical :: dup
    select case (jtype(p))
    case (J_OBJ)
       n = jlen(p)
       allocate(nm(n), kid(n))
       m = 0
       q = jfirst(p)
       do while (q > 0)
          m = m + 1; kid(m) = q; nm(m)%s = jname(q)
          q = jnext(q)
       end do
       ! drop every entry that a later one with the same key replaces (immutable hasheq: last wins)
       m = 0
       do i = 1, n
          dup = .false.
          do j = i + 1, n
             if (streq(nm(i)%s, nm(j)%s)) then
                dup = .true.; exit
             end if
          end do
          if (.not. dup) then
             m = m + 1; kid(m) = kid(i); nm(m)%s = nm(i)%s
          end if
       end do
       do i = 2, m
          tk = kid(i); tn%s = nm(i)%s; j = i - 1
          do while (j >= 1)
             if (.not. bytes_gt(nm(j)%s, tn%s)) exit
             kid(j+1) = kid(j); nm(j+1)%s = nm(j)%s; j = j - 1
          end do
          kid(j+1) = tk; nm(j+1)%s = tn%s
       end do
       if (m == 0) then
          call sb_add(b, '{}')
       else
          call sb_add(b, '{'//NL)
          do i = 1, m
             if (i > 1) call sb_add(b, ','//NL)
             call sb_add(b, repeat(' ', ind + 1))
             call sb_add(b, jquote(nm(i)%s))
             call sb_add(b, ': ')
             call pj_write(b, kid(i), ind + 1)
          end do
          call sb_add(b, NL//repeat(' ', ind)//'}')
       end if
    case (J_ARR)
       if (jlen(p) == 0) then
          call sb_add(b, '[]')
       else
          call sb_add(b, '['//NL)
          q = jfirst(p)
          do while (q > 0)
             if (q /= jfirst(p)) call sb_add(b, ','//NL)
             call sb_add(b, repeat(' ', ind + 1))
             call pj_write(b, q, ind + 1)
             q = jnext(q)
          end do
          call sb_add(b, NL//repeat(' ', ind)//']')
       end if
    case (J_STR)
       call sb_add(b, jquote(jstr(p)))
    case (J_TRUE)
       call sb_add(b, 'true')
    case (J_FALSE)
       call sb_add(b, 'false')
    case (J_NUM)
       call sb_add(b, num_text(p))
    case default
       call sb_add(b, 'null')
    end select
  end subroutine pj_write

  ! (pretty-json-string (jsexpr->string v)) for a non-string transcript value
  function pretty_json_value(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    type(strbuf) :: b
    select case (jtype(p))
    case (J_OBJ, J_ARR)
       call pj_write(b, p, 0)
       r = sb_str(b)
    case (J_TRUE); r = 'true'
    case (J_FALSE); r = 'false'
    case (J_NUM); r = num_text(p)
    case (J_STR); r = jquote(jstr(p))
    case default; r = 'null'
    end select
  end function pretty_json_value

  ! ---------------------------------------------------------------- blocks

  function pre_block(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    if (index(s, CR) > 0) then
       r = '<pre class="code">'//esc(replace_all(s, CR, ''))//'</pre>'//NL
    else
       r = '<pre class="code">'//esc(s)//'</pre>'//NL
    end if
  end function pre_block

  function sub_line(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    r = '<div class="sub">'//esc(s)//'</div>'//NL
  end function sub_line

  function count_suffix(nitems) result(r)
    integer, intent(in) :: nitems
    character(len=:), allocatable :: r
    if (nitems > 0) then
       r = ' <span class="prev">('//itoa(nitems)//' results)</span>'
    else
       r = ''
    end if
  end function count_suffix

  function tool_head(label, q, nitems) result(r)
    character(len=*), intent(in) :: label, q
    integer, intent(in) :: nitems
    character(len=:), allocatable :: r
    r = '<div class="ev tool"><span class="label">'//esc(label)//'</span> <span class="q">'//link_str(q)//'</span>'// &
        count_suffix(nitems)//NL
  end function tool_head

  ! arg-str: first key whose value is not null; strings as-is, anything else py-str; "" when none
  function arg_str(a, k1, k2) result(r)
    integer, intent(in) :: a
    character(len=*), intent(in) :: k1
    character(len=*), intent(in), optional :: k2
    character(len=:), allocatable :: r
    integer :: v
    r = ''
    v = jget(a, k1)
    if (jis_null(v) .and. present(k2)) v = jget(a, k2)
    if (jis_str(v)) then
       r = jstr(v)
    else if (.not. jis_null(v)) then
       r = py_str(v)
    end if
  end function arg_str

  function results_html(res) result(r)
    integer, intent(in) :: res
    character(len=:), allocatable :: r
    type(strbuf) :: b
    character(len=:), allocatable :: u
    integer :: items, kind, it, p
    items = lst(jget(res, 'items'))
    kind = jget(res, 'kind')
    if (items == 0) then
       r = ''; return
    end if
    call sb_add(b, '<ul class="results">'//NL)
    it = jfirst(items)
    do while (it > 0)
       if (node_is(kind, 'web')) then
          call sb_add(b, '<li>'//link_node_text(jget(it, 'url'), one_line_n(py_or2(jget(it, 'title'), jget(it, 'url')))))
          p = jget(it, 'preview')
          if (truthy(p)) call sb_add(b, ' <span class="prev">'//EMDASH//' '//esc(one_line_n(p, 300))//'</span>')
          call sb_add(b, '</li>'//NL)
       else if (node_is(kind, 'x')) then
          u = 'https://x.com/'//py_str(jget(it, 'username'))//'/status/'//py_str(jget(it, 'postId'))
          call sb_add(b, '<li class="xpost">'//link_str_text(u, '@'//py_str(jget(it, 'username'))//' ('// &
                         py_str(jget(it, 'postId'))//')')//': '//esc(one_line_n(jget(it, 'text'), 300))//'</li>'//NL)
       else
          call sb_add(b, '<li>'//esc(py_str(it))//'</li>'//NL)
       end if
       it = jnext(it)
    end do
    call sb_add(b, '</ul>'//NL)
    r = sb_str(b)
  end function results_html

  function build_tool_html(label, k, a, res) result(r)
    character(len=*), intent(in) :: label, k
    integer, intent(in) :: a, res
    character(len=:), allocatable :: r
    character(len=:), allocatable :: cmd, fp, olds, news, det, v, aj
    integer :: nitems, q, ndet
    nitems = jlen(lst(jget(res, 'items')))
    if (streq(k, 'bash')) then
       cmd = arg_str(a, 'command')
       r = tool_head(label, one_line_s(arg_str(a, 'description')), nitems)
       if (len(cmd) > 0) r = r//pre_block(cmd)
       ! command output (WebSocket history, see fx_grok grok_merge_ws_results)
       if (jequal_str(jget(res, 'kind'), 'code') .and. jis_str(jget(res, 'stdout'))) then
          if (jis_int(jget(res, 'exitCode'))) then
             r = r//sub_line('Output (exit code '//num_text(jget(res, 'exitCode'))//'):')//pre_block(jstr(jget(res, 'stdout')))
          else
             r = r//sub_line('Output:')//pre_block(jstr(jget(res, 'stdout')))
          end if
       end if
    else if (streq(k, 'editFile')) then
       fp = arg_str(a, 'filePath', 'file_path')
       olds = arg_str(a, 'oldString', 'old_string')
       news = arg_str(a, 'newString', 'new_string')
       r = tool_head(label, fp, nitems)
       if (len(olds) > 0) r = r//sub_line('Replaced:')//pre_block(olds)//sub_line('With:')
       if (len(news) > 0 .or. len(olds) > 0) r = r//pre_block(news)
    else if (streq(k, 'readFile')) then
       det = ''; ndet = 0
       v = arg_str(a, 'fileType', 'file_type')
       if (len(v) > 0) call join(det, ndet, v)
       q = jget(a, 'offset')
       if (jis_int(q)) call join(det, ndet, 'offset '//num_text(q))
       q = jget(a, 'limit')
       if (jis_int(q)) call join(det, ndet, 'limit '//num_text(q))
       r = tool_head(label, arg_str(a, 'filePath', 'file_path'), nitems)
       if (ndet > 0) r = r//sub_line(det)
    else if (streq(k, 'listDir')) then
       r = tool_head(label, arg_str(a, 'targetDirectory', 'target_directory'), nitems)
    else if (streq(k, 'imageSearch')) then
       r = tool_head(label, one_line_s(arg_str(a, 'imageDescription', 'image_description')), nitems)//results_html(res)
    else
       aj = arg_str(a, 'toolArgsJson', 'tool_args_json')
       r = tool_head(label, arg_str(a, 'toolName', 'tool_name'), nitems)
       if (len(aj) > 0) r = r//pre_block(pretty_json_string(aj))
       r = r//results_html(res)
    end if
    r = r//'</div>'//NL
  end function build_tool_html

  subroutine join(acc, n, item)          ! (string-join details "; ") built incrementally
    character(len=:), allocatable, intent(inout) :: acc
    integer, intent(inout) :: n
    character(len=*), intent(in) :: item
    if (n > 0) then
       acc = acc//'; '//item
    else
       acc = item
    end if
    n = n + 1
  end subroutine join

  function tool_label(k, found) result(r)
    character(len=*), intent(in) :: k
    logical, intent(out) :: found
    character(len=:), allocatable :: r
    found = .true.
    select case (k)
    case ('webSearch');          r = 'Searched web'
    case ('xSearch');            r = 'Searched X'
    case ('xUserSearch');        r = 'Searched X users'
    case ('browsePage');         r = 'Browsed'
    case ('conversationSearch'); r = 'Searching conversations'
    case ('viewImage');          r = 'View Image'
    case ('initTerminalSession'); r = 'Connected to computer'
    case ('chatroomSend');       r = 'Sent to All'
    case ('bash');               r = 'Ran command'
    case ('editFile');           r = 'Wrote file'
    case ('readFile');           r = 'Read file'
    case ('listDir');            r = 'Listed directory'
    case ('imageSearch');        r = 'Searched images'
    case ('mcp');                r = 'Tool'
    case default
       r = ''; found = .false.
    end select
    ! select case compares blank-padded: reject a key that only matched with trailing blanks
    if (found .and. len_trim(k) /= len(k)) then
       r = ''; found = .false.
    end if
  end function tool_label

  logical function is_build_kind(k)
    character(len=*), intent(in) :: k
    is_build_kind = streq(k, 'bash') .or. streq(k, 'editFile') .or. streq(k, 'readFile') .or. streq(k, 'listDir') .or. &
                    streq(k, 'imageSearch') .or. streq(k, 'mcp')
  end function is_build_kind

  function event_html(ev) result(r)
    integer, intent(in) :: ev
    character(len=:), allocatable :: r
    character(len=:), allocatable :: label, msg, ks
    integer :: ty, k, a, m, qv, q, res, instr
    logical :: found
    ty = jget(ev, 'type')
    if (node_is(ty, 'summary')) then
       r = '<div class="ev summary">'//escn(jget(ev, 'text'))//'</div>'//NL
    else if (node_is(ty, 'tool')) then
       k = jget(ev, 'kind')
       a = hsh(jget(ev, 'args'))
       found = .false.
       ks = ''
       if (jis_str(k)) then
          ks = jstr(k)
          label = tool_label(ks, found)
       end if
       if (.not. found) label = py_str(k)
       if (node_is(k, 'chatroomSend')) then
          m = jget(a, 'message')
          if (.not. truthy(m)) then
             msg = ''
          else if (jis_str(m)) then
             msg = jstr(m)
          else
             msg = py_str(m)
          end if
          r = '<div class="ev chatroom"><span class="label">Sent to All</span><div class="msg">'//esc(msg)//'</div></div>'//NL
       else if (jis_str(k) .and. is_build_kind(ks)) then
          r = build_tool_html(label, ks, a, hsh(jget(ev, 'results')))
       else
          qv = jget(a, 'query')
          if (.not. jis_null(qv)) then
             q = qv
          else
             q = 0
             if (truthy(jget(a, 'url'))) then
                q = jget(a, 'url')
             else if (truthy(jget(a, 'previewUrl'))) then
                q = jget(a, 'previewUrl')
             else if (truthy(jget(a, 'preview_url'))) then
                q = jget(a, 'preview_url')
             end if
          end if
          res = hsh(jget(ev, 'results'))
          instr = jget(a, 'instructions')
          r = '<div class="ev tool"><span class="label">'//esc(label)//'</span> <span class="q">'//link_node(q)//'</span>'// &
              count_suffix(jlen(lst(jget(res, 'items'))))
          if (jis_str(instr)) r = r//'<div class="prev">'//esc(jstr(instr))//'</div>'
          r = r//NL//results_html(res)//'</div>'//NL
       end if
    else if (node_is(ty, 'tool_result')) then
       res = hsh(jget(ev, 'results'))
       r = '<div class="ev">tool result '//escn(jget(ev, 'toolCallId'))//': '//itoa(jlen(lst(jget(res, 'items'))))// &
           ' items</div>'//NL
    else if (node_is(ty, 'text')) then
       r = '<div class="ev">['//escn(jget(ev, 'channel'))//'] '//esc(one_line_n(jget(ev, 'text'), 400))//'</div>'//NL
    else
       r = '<div class="ev">'//escn(ty)//'</div>'//NL
    end if
  end function event_html

  function image_surfaces_html(turn) result(r)
    integer, intent(in) :: turn
    character(len=:), allocatable :: r
    character(len=16), parameter :: labels(3) = [character(len=16) :: 'Generated image', 'Image edit URI', 'Image attachment']
    character(len=18), parameter :: keys(3) = [character(len=18) :: 'generatedImageUrls', 'imageEditUris', 'imageAttachments']
    type(strbuf) :: b
    integer :: e, v
    logical :: any
    any = .false.
    do e = 1, 3
       v = jfirst(lst(jget(turn, trim(keys(e)))))
       do while (v > 0)
          any = .true.
          call sb_add(b, '<div class="image-surface"><span class="label">'//esc(trim(labels(e)))//'</span> ')
          if (nurl(v)) then
             call sb_add(b, link_node(v))
          else if (jis_str(v)) then
             call sb_add(b, '<code>'//esc(jstr(v))//'</code>')
          else
             call sb_add(b, pre_block(pretty_json_value(v)))
          end if
          call sb_add(b, '</div>'//NL)
          v = jnext(v)
       end do
    end do
    if (any) then
       r = '<div class="image-surfaces">'//NL//sb_str(b)//'</div>'//NL
    else
       r = ''
    end if
  end function image_surfaces_html

  ! reply text with <sup> citation marks at code-point offsets, escaped
  function reply_html(text, cits, idx) result(r)
    character(len=*), intent(in) :: text, idx
    integer, intent(in) :: cits
    character(len=:), allocatable :: r
    type(strbuf) :: b
    integer :: n, k, ci, lastcp, lastb, off, nb
    integer(int64) :: o64
    n = cp_count(text)
    lastcp = 0; lastb = 1; k = 0
    ci = jfirst(cits)
    do while (ci > 0)
       k = k + 1
       o64 = 0
       if (jis_int(jget(ci, 'offset'))) o64 = jint(jget(ci, 'offset'))
       off = int(min(max(o64, 0_int64), int(n, int64)))
       off = max(off, lastcp)
       nb = cp_skip(text, lastb, off - lastcp)
       if (nb > lastb) call sb_add(b, esc(text(lastb:nb-1)))
       call sb_add(b, '<sup class="cite"><a href="#cite-'//idx//'-'//itoa(k)//'">['//itoa(k)//']</a></sup>')
       lastcp = off; lastb = nb
       ci = jnext(ci)
    end do
    if (lastb <= len(text)) call sb_add(b, esc(text(lastb:)))
    r = sb_str(b)
  end function reply_html

  ! (hash-ref att-paths fid #f): a string fid looked up among the object's decoded keys
  subroutine att_lookup(att, fid, rel, found)
    integer, intent(in) :: att, fid
    character(len=:), allocatable, intent(out) :: rel
    logical, intent(out) :: found
    character(len=:), allocatable :: key
    integer :: q
    found = .false.; rel = ''
    if (.not. jis_obj(att) .or. .not. jis_str(fid)) return
    key = jstr(fid)
    q = jfirst(att)
    do while (q > 0)
       if (streq(jname(q), key)) then
          found = jis_str(q)
          if (found) then
             rel = jstr(q)
          else
             rel = ''
          end if
       end if
       q = jnext(q)
    end do
  end subroutine att_lookup

end module fx_html
