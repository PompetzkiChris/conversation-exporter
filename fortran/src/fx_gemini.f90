! fx_gemini.f90 — Gemini lane: batchexecute hNvQHb payload -> transcript.json, page checks.
! Field map and semantics mirror racket/gemini.rkt exactly (see the comment block there).
module fx_gemini
  use iso_fortran_env, only: int64
  use fx_util
  use fx_json
  implicit none
  private
  public :: gemini_url_id, gemini_parse_batchexecute, gemini_transcript, gemini_dom_checks, &
            text_item, turn_summary, utf8_prefix, write_repr, to_lower, seen_has, seen_add

  type :: text_item
     character(len=:), allocatable :: s
  end type text_item

  type :: turn_summary
     logical :: human
     integer :: index
     character(len=:), allocatable :: text
  end type turn_summary

contains

  ! https://gemini.google.com/[u/N/](app|gem/X)/<hex> -> lowercase id, '' if not a Gemini link
  function gemini_url_id(url) result(id)
    character(len=*), intent(in) :: url
    character(len=:), allocatable :: id
    character(len=:), allocatable :: u, rest
    integer :: k, i
    id = ''
    u = str_trim_ws(url)
    if (starts_with(u, 'https://gemini.google.com/')) then
       rest = u(27:)
    else if (starts_with(u, 'http://gemini.google.com/')) then
       rest = u(26:)
    else
       return
    end if
    if (starts_with(rest, 'u/')) then
       k = index(rest(3:), '/'); if (k == 0) return
       rest = rest(3+k:)
    end if
    if (starts_with(rest, 'app/')) then
       rest = rest(5:)
    else if (starts_with(rest, 'gem/')) then
       k = index(rest(5:), '/'); if (k == 0) return
       rest = rest(5+k:)
    else
       return
    end if
    i = 0
    do while (i < len(rest))
       if (index('0123456789abcdefABCDEF', rest(i+1:i+1)) == 0) exit
       i = i + 1
    end do
    if (i >= 8) id = to_lower(rest(1:i))
  end function gemini_url_id

  function to_lower(s) result(r)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: r
    integer :: i, c
    r = s
    do i = 1, len(s)
       c = iachar(s(i:i))
       if (c >= 65 .and. c <= 90) r(i:i) = achar(c + 32)
    end do
  end function to_lower

  ! raw rt=c body -> parsed hNvQHb payload (first one found), null() if none
  integer function gemini_parse_batchexecute(raw) result(payload)
    character(len=*), intent(in) :: raw
    integer :: v, e
    integer :: a, z
    character(len=:), allocatable :: line
    payload = 0
    a = 1
    do while (a <= len(raw) .and. .not. (payload > 0))
       z = index(raw(a:), achar(10))
       if (z == 0) then
          line = raw(a:); a = len(raw) + 1
       else
          line = raw(a:a+z-2); a = a + z
       end if
       if (len(line) > 0) then
          if (line(len(line):len(line)) == achar(13)) line = line(1:len(line)-1)
       end if
       line = str_trim_ws(line)
       if (.not. starts_with(line, '[')) cycle
       v = jparse(line)
       if (.not. (v > 0)) cycle
       e = jfirst(v)
       do while ((e > 0))
          if (jlen(e) >= 3) then
             if (jequal_str(jat(e, [0]), 'wrb.fr') .and. jequal_str(jat(e, [1]), 'hNvQHb') .and. jis_str(jat(e, [2]))) then
                payload = jparse(jstr(jat(e, [2])))
                if ((payload > 0)) exit
             end if
          end if
          e = jnext(e)
       end do
    end do
  end function gemini_parse_batchexecute

  ! str or null
  subroutine put_str_or_null(w, p)
    type(jw), intent(inout) :: w
    integer, intent(in) :: p
    if (jis_str(p)) then
       call jw_str(w, jstr(p))
    else
       call jw_null(w)
    end if
  end subroutine put_str_or_null

  function iso_of(t) result(r)
    integer, intent(in) :: t
    character(len=:), allocatable :: r
    integer :: sec, nan
    sec = jat(t, [4, 0]); nan = jat(t, [4, 1])
    if (jis_int(sec)) then
       if (jis_int(nan)) then
          r = epoch_to_iso(jint(sec), jint(nan))
       else
          r = epoch_to_iso(jint(sec), 0_int64)
       end if
    else
       r = ''
    end if
  end function iso_of

  subroutine put_iso(w, iso)
    type(jw), intent(inout) :: w
    character(len=*), intent(in) :: iso
    if (len(iso) > 0) then
       call jw_str(w, iso)
    else
       call jw_null(w)
    end if
  end subroutine put_iso

  ! Build transcript.json text from the payload list (pages in order).  Also returns the
  ! per-turn summaries needed by the page checks.
  subroutine gemini_transcript(payloads, npay, source_url, title, out_text, summaries, nturns)
    integer, intent(in) :: payloads(:)
    integer, intent(in) :: npay
    character(len=*), intent(in) :: source_url, title
    character(len=:), allocatable, intent(out) :: out_text
    type(turn_summary), allocatable, intent(out) :: summaries(:)
    integer, intent(out) :: nturns
    integer, allocatable :: newest_first(:), turns(:)
    integer :: c, t
    type(jw) :: w
    integer :: n, i, k
    character(len=:), allocatable :: cid, first_iso, last_iso
    ! collect turns (newest first across pages), then reverse
    n = 0
    do k = 1, npay
       n = n + jlen(jat(payloads(k), [0]))
    end do
    allocate(newest_first(n), turns(n))
    i = 0
    do k = 1, npay
       c = jfirst(jat(payloads(k), [0]))
       do while ((c > 0))
          i = i + 1; newest_first(i) = c
          c = jnext(c)
       end do
    end do
    do i = 1, n
       turns(i) = newest_first(n - i + 1)
    end do
    cid = ''
    do i = 1, n
       if (jis_str(jat(turns(i), [0, 0]))) then
          cid = jstr(jat(turns(i), [0, 0])); exit
       end if
    end do
    nturns = 2*n
    allocate(summaries(max(nturns, 1)))
    if (n > 0) then
       first_iso = iso_of(turns(1)); last_iso = iso_of(turns(n))
    else
       first_iso = ''; last_iso = ''
    end if

    call jw_begin_obj(w)
    call jw_key(w, 'conversation')
    call jw_begin_obj(w)
      call jw_key(w, 'conversationId')
      if (len(cid) > 0) then
         call jw_str(w, cid)
      else
         call jw_null(w)
      end if
      call jw_key(w, 'createTime'); call put_iso(w, first_iso)
      call jw_key(w, 'isPublic'); call jw_null(w)
      call jw_key(w, 'modifyTime'); call put_iso(w, last_iso)
      call jw_key(w, 'platform'); call jw_str(w, 'gemini')
      call jw_key(w, 'sourceUrl'); call jw_str(w, source_url)
      call jw_key(w, 'title')
      if (len(title) > 0) then
         call jw_str(w, title)
      else
         call jw_null(w)
      end if
    call jw_end_obj(w)
    call jw_key(w, 'turns')
    call jw_begin_arr(w)
    do i = 1, n
       t = turns(i)
       call write_human(w, t, 2*(i-1), summaries(2*i-1))
       call write_assistant(w, t, 2*(i-1)+1, summaries(2*i))
    end do
    call jw_end_arr(w)
    call jw_end_obj(w)
    out_text = jw_result(w)
  end subroutine gemini_transcript

  subroutine write_human(w, t, idx, sm)
    type(jw), intent(inout) :: w
    integer, intent(in) :: t
    integer, intent(in) :: idx
    type(turn_summary), intent(out) :: sm
    integer :: user, a, flag
    character(len=:), allocatable :: text
    user = jat(t, [2])
    call jw_begin_obj(w)
    call jw_key(w, 'attachments')
    call jw_begin_arr(w)
    a = jfirst(jat(user, [0, 4, 0, 4]))
    if (.not. jis_arr(jat(user, [0, 4, 0, 4]))) a = 0
    do while ((a > 0))
       if (jis_arr(a)) then
          call jw_begin_obj(w)
          call jw_key(w, 'fileId');   call put_str_or_null(w, jat(a, [2]))
          call jw_key(w, 'fileName'); call put_str_or_null(w, jat(a, [2]))
          call jw_key(w, 'mimeType'); call put_str_or_null(w, jat(a, [11]))
          call jw_key(w, 'url');      call put_str_or_null(w, jat(a, [3]))
          call jw_end_obj(w)
       end if
       a = jnext(a)
    end do
    call jw_end_arr(w)
    call jw_key(w, 'citations'); call jw_begin_arr(w); call jw_end_arr(w)
    call jw_key(w, 'createTime'); call put_iso(w, iso_of(t))
    call jw_key(w, 'index'); call jw_int(w, int(idx, int64))
    call jw_key(w, 'inputFlag')
    if (jlen(user) <= 8) then
       call jw_str(w, 'absent')
    else
       flag = jat(user, [8])
       if (jis_false(flag)) then
          call jw_str(w, 'false')
       else if (jis_null(flag)) then
          call jw_str(w, 'null')
       else
          call jw_str(w, jraw_text(flag))
       end if
    end if
    call jw_key(w, 'model'); call jw_null(w)
    call jw_key(w, 'parentResponseId'); call jw_null(w)
    call jw_key(w, 'responseId'); call put_str_or_null(w, jat(t, [0, 1]))
    call jw_key(w, 'sender'); call jw_str(w, 'human')
    call jw_key(w, 'sources'); call jw_null(w)
    text = ''
    if (jis_str(jat(user, [0, 0]))) text = jstr(jat(user, [0, 0]))
    call jw_key(w, 'text'); call jw_str(w, text)
    call jw_key(w, 'thinking'); call jw_null(w)
    call jw_end_obj(w)
    sm%human = .true.; sm%index = idx; sm%text = text
  end subroutine write_human

  subroutine write_assistant(w, t, idx, sm)
    type(jw), intent(inout) :: w
    integer, intent(in) :: t
    integer, intent(in) :: idx
    type(turn_summary), intent(out) :: sm
    integer :: cands, cand, c, sel, q
    character(len=:), allocatable :: text, think
    integer :: ncand
    cands = jat(t, [3, 0])
    ncand = 0
    if (jis_arr(cands)) ncand = jlen(cands)
    sel = jat(t, [3, 3])
    cand = 0
    c = jfirst(cands)
    if (.not. jis_arr(cands)) c = 0
    do while ((c > 0))
       if (same_scalar(jat(c, [0]), sel)) then
          cand = c; exit
       end if
       c = jnext(c)
    end do
    if (.not. (cand > 0) .and. ncand > 0) cand = jfirst(cands)

    call jw_begin_obj(w)
    call jw_key(w, 'attachments'); call jw_begin_arr(w); call jw_end_arr(w)
    call jw_key(w, 'candidates'); call jw_int(w, int(ncand, int64))
    call jw_key(w, 'citations'); call write_citations(w, cand)
    call jw_key(w, 'createTime'); call put_iso(w, iso_of(t))
    call jw_key(w, 'index'); call jw_int(w, int(idx, int64))
    call jw_key(w, 'model'); call put_str_or_null(w, jat(t, [3, 21]))
    call jw_key(w, 'parentResponseId'); call put_str_or_null(w, jat(t, [0, 1]))
    call jw_key(w, 'responseId'); call put_str_or_null(w, jat(cand, [0]))
    call jw_key(w, 'sender'); call jw_str(w, 'assistant')
    call jw_key(w, 'sources')
    call jw_begin_obj(w)
      call jw_key(w, 'embeds'); call write_embeds(w, cand)
      call jw_key(w, 'searchQueries')
      call jw_begin_arr(w)
      q = jfirst(jat(t, [3, 1]))
      if (.not. jis_arr(jat(t, [3, 1]))) q = 0
      do while ((q > 0))
         if (jis_str(jat(q, [0]))) call jw_str(w, jstr(jat(q, [0])))
         q = jnext(q)
      end do
      call jw_end_arr(w)
      call jw_key(w, 'webSearchResults'); call write_web_results(w, cand)
    call jw_end_obj(w)
    text = ''
    if (jis_str(jat(cand, [1, 0]))) text = jstr(jat(cand, [1, 0]))
    call jw_key(w, 'text'); call jw_str(w, text)
    call jw_key(w, 'thinking')
    if (jis_str(jat(cand, [37, 0, 0]))) then
       think = jstr(jat(cand, [37, 0, 0]))
       call jw_begin_obj(w)
       call jw_key(w, 'durationMs'); call jw_null(w)
       call jw_key(w, 'endTime'); call jw_null(w)
       call jw_key(w, 'mainRollout'); call jw_null(w)
       call jw_key(w, 'rollouts')
       call jw_begin_arr(w)
         call jw_begin_obj(w)
         call jw_key(w, 'events'); call write_thinking_events(w, think)
         call jw_key(w, 'id'); call jw_str(w, 'Gemini')
         call jw_key(w, 'role'); call jw_null(w)
         call jw_end_obj(w)
       call jw_end_arr(w)
       call jw_key(w, 'startTime'); call jw_null(w)
       call jw_key(w, 'text'); call jw_str(w, think)
       call jw_end_obj(w)
    else
       call jw_null(w)
    end if
    call jw_end_obj(w)
    sm%human = .false.; sm%index = idx; sm%text = text
  end subroutine write_assistant

  logical function same_scalar(a, b)
    integer, intent(in) :: a, b
    same_scalar = .false.
    if (jis_str(a) .and. jis_str(b)) then
       same_scalar = (jstr(a) == jstr(b))
    else if (jis_null(a) .and. jis_null(b)) then
       same_scalar = .true.
    else if (jis_int(a) .and. jis_int(b)) then
       same_scalar = (jint(a) == jint(b))
    end if
  end function same_scalar

  ! "**Title**\n\nbody\n\n**Title2**" -> header / thought events
  subroutine write_thinking_events(w, s)
    type(jw), intent(inout) :: w
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: body, part
    integer :: a, i, nl
    body = str_trim_ws(s)
    call jw_begin_arr(w)
    a = 1; i = 1
    do while (i <= len(body) + 1)
       if (i <= len(body)) then
          if (body(i:i) == achar(10)) then
             nl = 0
             do while (i + nl <= len(body))
                if (body(i+nl:i+nl) /= achar(10)) exit
                nl = nl + 1
             end do
             if (nl >= 2) then
                part = body(a:i-1); call emit_part(part)
                i = i + nl; a = i
                cycle
             end if
             i = i + nl
             cycle
          end if
          i = i + 1
       else
          part = body(a:len(body)); call emit_part(part)
          exit
       end if
    end do
    call jw_end_arr(w)
  contains
    subroutine emit_part(p0)
      character(len=*), intent(in) :: p0
      character(len=:), allocatable :: p
      p = str_trim_ws(p0)
      if (len(p) == 0) return
      call jw_begin_obj(w)
      if (len(p) >= 5 .and. starts_with(p, '**') .and. ends_with(p, '**')) then
         call jw_key(w, 'channel'); call jw_str(w, 'header')
         call jw_key(w, 'text'); call jw_str(w, p(3:len(p)-2))
      else
         call jw_key(w, 'channel'); call jw_str(w, 'thought')
         call jw_key(w, 'text'); call jw_str(w, p)
      end if
      call jw_key(w, 'type'); call jw_str(w, 'text')
      call jw_end_obj(w)
    end subroutine emit_part
  end subroutine write_thinking_events

  ! citations-of: for c in cand[2][1] (arrays), for src in c[2] (arrays)
  subroutine write_citations(w, cand)
    type(jw), intent(inout) :: w
    integer, intent(in) :: cand
    integer :: c, src, spans, v
    call jw_begin_arr(w)
    c = first_child_of_array(jat(cand, [2, 1]))
    do while ((c > 0))
       if (jis_arr(c)) then
          spans = jat(c, [0, 3])
          src = first_child_of_array(jat(c, [2]))
          do while ((src > 0))
             if (jis_arr(src)) then
                call jw_begin_obj(w)
                call jw_key(w, 'end')
                v = jat(spans, [0, 1])
                if (jis_int(v)) then
                   call jw_int(w, jint(v))
                else
                   call jw_null(w)
                end if
                call jw_key(w, 'preview'); call put_str_or_null(w, jat(src, [3]))
                call jw_key(w, 'start')
                v = jat(spans, [0, 0])
                if (jis_int(v)) then
                   call jw_int(w, jint(v))
                else
                   call jw_null(w)
                end if
                call jw_key(w, 'text'); call put_str_or_null(w, jat(c, [0, 0]))
                call jw_key(w, 'title'); call put_str_or_null(w, jat(src, [1]))
                call jw_key(w, 'url'); call put_str_or_null(w, jat(src, [0]))
                call jw_end_obj(w)
             end if
             src = jnext(src)
          end do
       end if
       c = jnext(c)
    end do
    call jw_end_arr(w)
  end subroutine write_citations

  integer function first_child_of_array(p) result(q)
    integer, intent(in) :: p
    q = 0
    if (jis_arr(p)) q = jfirst(p)
  end function first_child_of_array

  ! webSearchResults: distinct {preview,title,url} from citations whose url is a string
  subroutine write_web_results(w, cand)
    type(jw), intent(inout) :: w
    integer, intent(in) :: cand
    integer :: c, src
    type(text_item), allocatable :: seen(:)
    integer :: ns
    character(len=:), allocatable :: key
    type(jw) :: tmp
    allocate(seen(64)); ns = 0
    call jw_begin_arr(w)
    c = first_child_of_array(jat(cand, [2, 1]))
    do while ((c > 0))
       if (jis_arr(c)) then
          src = first_child_of_array(jat(c, [2]))
          do while ((src > 0))
             if (jis_arr(src) .and. jis_str(jat(src, [0]))) then
                tmp = jw()
                call jw_begin_obj(tmp)
                call jw_key(tmp, 'preview'); call put_str_or_null(tmp, jat(src, [3]))
                call jw_key(tmp, 'title'); call put_str_or_null(tmp, jat(src, [1]))
                call jw_key(tmp, 'url'); call put_str_or_null(tmp, jat(src, [0]))
                call jw_end_obj(tmp)
                key = sb_str(tmp%b)
                if (.not. seen_has(seen, ns, key)) then
                   call seen_add(seen, ns, key)
                   call jw_begin_obj(w)
                   call jw_key(w, 'preview'); call put_str_or_null(w, jat(src, [3]))
                   call jw_key(w, 'title'); call put_str_or_null(w, jat(src, [1]))
                   call jw_key(w, 'url'); call put_str_or_null(w, jat(src, [0]))
                   call jw_end_obj(w)
                end if
             end if
             src = jnext(src)
          end do
       end if
       c = jnext(c)
    end do
    call jw_end_arr(w)
  end subroutine write_web_results

  ! embeds: walk cand[12]; a list [title id url channel …] with an http(s) url at [2] is one embed
  subroutine write_embeds(w, cand)
    type(jw), intent(inout) :: w
    integer, intent(in) :: cand
    type(text_item), allocatable :: seen(:)
    integer :: ns
    allocate(seen(16)); ns = 0
    call jw_begin_arr(w)
    call walk(jat(cand, [12]))
    call jw_end_arr(w)
  contains
    recursive subroutine walk(v)
      integer, intent(in) :: v
      integer :: c
      type(jw) :: tmp
      character(len=:), allocatable :: key, u
      if (jis_arr(v)) then
         if (jlen(v) >= 3) then
            if (jis_str(jat(v, [0])) .and. jis_str(jat(v, [2]))) then
               u = jstr(jat(v, [2]))
               if (starts_with(u, 'http://') .or. starts_with(u, 'https://')) then
                  tmp = jw()
                  call jw_begin_obj(tmp)
                  call jw_key(tmp, 'channel'); call put_str_or_null(tmp, jat(v, [3]))
                  call jw_key(tmp, 'id'); call put_str_or_null(tmp, jat(v, [1]))
                  call jw_key(tmp, 'title'); call jw_str(tmp, jstr(jat(v, [0])))
                  call jw_key(tmp, 'url'); call jw_str(tmp, u)
                  call jw_end_obj(tmp)
                  key = sb_str(tmp%b)
                  if (.not. seen_has(seen, ns, key)) then
                     call seen_add(seen, ns, key)
                     call jw_begin_obj(w)
                     call jw_key(w, 'channel'); call put_str_or_null(w, jat(v, [3]))
                     call jw_key(w, 'id'); call put_str_or_null(w, jat(v, [1]))
                     call jw_key(w, 'title'); call jw_str(w, jstr(jat(v, [0])))
                     call jw_key(w, 'url'); call jw_str(w, u)
                     call jw_end_obj(w)
                  end if
                  return
               end if
            end if
         end if
         c = jfirst(v)
         do while ((c > 0))
            call walk(c); c = jnext(c)
         end do
      else if (jis_obj(v)) then
         c = jfirst(v)
         do while ((c > 0))
            call walk(c); c = jnext(c)
         end do
      end if
    end subroutine walk
  end subroutine write_embeds

  logical function seen_has(seen, ns, key)
    type(text_item), intent(in) :: seen(:)
    integer, intent(in) :: ns
    character(len=*), intent(in) :: key
    integer :: i
    seen_has = .false.
    do i = 1, ns
       if (seen(i)%s == key) then
          seen_has = .true.; return
       end if
    end do
  end function seen_has

  subroutine seen_add(seen, ns, key)
    type(text_item), allocatable, intent(inout) :: seen(:)
    integer, intent(inout) :: ns
    character(len=*), intent(in) :: key
    type(text_item), allocatable :: tmp(:)
    if (ns >= size(seen)) then
       allocate(tmp(2*size(seen))); tmp(1:ns) = seen(1:ns); call move_alloc(tmp, seen)
    end if
    ns = ns + 1; seen(ns)%s = key
  end subroutine seen_add

  ! ---------------------------------------------------------------- page checks

  ! norm: drop * _ ` # >, NBSP -> space, collapse ASCII whitespace, trim
  function norm(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, c
    logical :: sp
    sp = .false.
    i = 1
    do while (i <= len(s))
       c = iachar(s(i:i))
       if (c == 42 .or. c == 95 .or. c == 96 .or. c == 35 .or. c == 62) then
          i = i + 1; cycle
       end if
       if (c == 194 .and. i < len(s)) then
          if (iachar(s(i+1:i+1)) == 160) then
             sp = .true.; i = i + 2; cycle
          end if
       end if
       if (c == 32 .or. (c >= 9 .and. c <= 13)) then
          sp = .true.
       else
          if (sp) call sb_add(o, ' ')
          sp = .false.
          call sb_add(o, s(i:i))
       end if
       i = i + 1
    end do
    r = str_trim_ws(sb_str(o))
  end function norm

  ! first n Unicode characters of s (UTF-8)
  function utf8_prefix(s, n) result(r)
    character(len=*), intent(in) :: s
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    integer :: i, k, c
    i = 1; k = 0
    do while (i <= len(s))
       c = iachar(s(i:i))
       if (c < 128 .or. c >= 192) then
          if (k == n) exit
          k = k + 1
       end if
       i = i + 1
    end do
    r = s(1:i-1)
  end function utf8_prefix

  function prefix(s, n) result(r)
    character(len=*), intent(in) :: s
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    r = utf8_prefix(norm(s), n)
  end function prefix

  ! Racket ~s rendering of a string
  function write_repr(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, c
    character(len=16), parameter :: hx = '0123456789abcdef'
    call sb_add(o, '"')
    do i = 1, len(s)
       c = iachar(s(i:i))
       select case (c)
       case (34); call sb_add(o, '\"')
       case (92); call sb_add(o, '\\')
       case (10); call sb_add(o, '\n')
       case (13); call sb_add(o, '\r')
       case (9);  call sb_add(o, '\t')
       case (0:8, 11, 12, 14:31)
          call sb_add(o, '\u00'//hx(c/16+1:c/16+1)//hx(mod(c,16)+1:mod(c,16)+1))
       case default
          call sb_add(o, s(i:i))
       end select
    end do
    call sb_add(o, '"')
    r = sb_str(o)
  end function write_repr

! a reply that opens with a list item: the page renders the number or bullet as list markup, not as text
  function strip_list_marker(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: i, j
    r = s
    i = 1
    do while (i <= len(s))
       if (index(' '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13), s(i:i)) == 0) exit
       i = i + 1
    end do
    if (i > len(s)) return
    if (index('-*+', s(i:i)) > 0) then
       j = i + 1
    else if (index('0123456789', s(i:i)) > 0) then
       j = i
       do while (j <= len(s))
          if (index('0123456789', s(j:j)) == 0) exit
          j = j + 1
       end do
       if (j > len(s)) return
       if (s(j:j) /= '.' .and. s(j:j) /= ')') return
       j = j + 1
    else
       return
    end if
    if (j > len(s)) return
    if (index(' '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13), s(j:j)) == 0) return
    do while (j <= len(s))
       if (index(' '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13), s(j:j)) == 0) exit
       j = j + 1
    end do
    r = s(j:)
  end function strip_list_marker
  function first_line(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: a, z
    character(len=:), allocatable :: ln
    a = 1
    do while (a <= len(s))
       z = index(s(a:), achar(10))
       if (z == 0) then
          ln = s(a:); a = len(s) + 1
       else
          ln = s(a:a+z-2); a = a + z
       end if
       ln = str_trim_ws(ln)
       if (len(ln) > 0) then
          r = ln; return
       end if
    end do
    r = ''
  end function first_line

  ! returns the verification "checks" array text pieces via writer and failure count
  subroutine gemini_dom_checks(w, summaries, nturns, dom, nfailed, failed_names, failed_idx, nchecks)
    type(jw), intent(inout) :: w
    type(turn_summary), intent(in) :: summaries(:)
    integer, intent(in) :: nturns
    integer, intent(in) :: dom
    integer, intent(out) :: nfailed, nchecks
    type(text_item), allocatable, intent(out) :: failed_names(:)
    integer, allocatable, intent(out) :: failed_idx(:)
    integer :: dq, dr
    integer, allocatable :: hi(:), ai(:)
    integer :: nh, na, i, k, ndq, ndr, stable
    character(len=:), allocatable :: want, got
    allocate(hi(max(nturns,1)), ai(max(nturns,1)), failed_names(4*max(nturns,1)+8), failed_idx(4*max(nturns,1)+8))
    nh = 0; na = 0
    do i = 1, nturns
       if (summaries(i)%human) then
          nh = nh + 1; hi(nh) = i
       else
          na = na + 1; ai(na) = i
       end if
    end do
    dq = child_named(dom, 'queries'); dr = child_named(dom, 'responses')
    ndq = jlen(dq); ndr = jlen(dr)
    nfailed = 0; nchecks = 0
    stable = int(jint(child_named(dom, 'stableRounds')))
    call jw_begin_arr(w)
    call check('dom-reached-top', stable >= 1, itoa(int(jint(child_named(dom, 'rounds'))))//' scroll rounds, stable '// &
               itoa(stable)//', '//i64toa(jint(child_named(dom, 'ms')))//' ms', -1)
    call check('human-turn-count', ndq == nh, 'page '//itoa(ndq)//', api '//itoa(nh), -1)
    call check('assistant-turn-count', ndr == na, 'page '//itoa(ndr)//', api '//itoa(na), -1)
    k = min(ndq, nh)
    do i = 0, k - 1
       want = prefix(summaries(hi(nh - i))%text, 60)
       got = norm(jstr(jat(dq, [ndq - 1 - i])))
       call check('human-text', index(got, want) > 0, 'api starts '//write_repr(want), summaries(hi(nh - i))%index)
    end do
    k = min(ndr, na)
    do i = 0, k - 1
       want = utf8_prefix(norm(strip_list_marker(first_line(summaries(ai(na - i))%text))), 40)
       got = norm(jstr(child_named(jat(dr, [ndr - 1 - i]), 'text')))
       call check('assistant-text', len(want) == 0 .or. index(got, want) > 0, &
                  'api first line starts '//write_repr(want), summaries(ai(na - i))%index)
    end do
    call jw_end_arr(w)
  contains
    subroutine check(name, ok, detail, turn)
      character(len=*), intent(in) :: name, detail
      logical, intent(in) :: ok
      integer, intent(in) :: turn
      nchecks = nchecks + 1
      call jw_begin_obj(w)
      call jw_key(w, 'detail'); call jw_str(w, detail)
      call jw_key(w, 'name'); call jw_str(w, name)
      call jw_key(w, 'ok'); call jw_bool(w, ok)
      call jw_key(w, 'turnIndex')
      if (turn >= 0) then
         call jw_int(w, int(turn, int64))
      else
         call jw_null(w)
      end if
      call jw_end_obj(w)
      if (.not. ok) then
         nfailed = nfailed + 1
         failed_names(nfailed)%s = name; failed_idx(nfailed) = turn
      end if
    end subroutine check
  end subroutine gemini_dom_checks

  integer function child_named(p, name)
    integer, intent(in) :: p
    character(len=*), intent(in) :: name
    child_named = jget(p, name)
  end function child_named

end module fx_gemini
