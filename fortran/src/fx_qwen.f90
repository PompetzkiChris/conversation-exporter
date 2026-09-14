! fx_qwen.f90 — Qwen lane: /api/v2/chats/<id> payload -> transcript.json, page checks.
! Mirrors racket/qwen.rkt (field map, branch walk, off-branch versions, checks, warnings).
module fx_qwen
  use iso_fortran_env, only: int64
  use fx_util
  use fx_json
  use fx_gemini, only: text_item, utf8_prefix, write_repr, to_lower
  implicit none
  private
  public :: qwen_chat_id, qwen_transcript, qwen_dom_checks, qturn

  type :: qturn
     logical :: human
     integer :: index
     character(len=:), allocatable :: text, id
  end type qturn

contains

  ! https://chat.qwen.ai/c/<uuid> -> lowercase uuid, '' otherwise
  function qwen_chat_id(url) result(id)
    character(len=*), intent(in) :: url
    character(len=:), allocatable :: id
    character(len=:), allocatable :: u, rest
    integer :: i
    id = ''
    u = str_trim_ws(url)
    if (starts_with(u, 'https://chat.qwen.ai/c/')) then
       rest = u(24:)
    else if (starts_with(u, 'http://chat.qwen.ai/c/')) then
       rest = u(23:)
    else
       return
    end if
    if (len(rest) < 36) return
    do i = 1, 36
       if (index('0123456789abcdefABCDEF-', rest(i:i)) == 0) return
    end do
    id = to_lower(rest(1:36))
  end function qwen_chat_id

  subroutine put_raw_or_null(w, p)
    type(jw), intent(inout) :: w
    integer, intent(in) :: p
    call jcanon_value(w, p)
  end subroutine put_raw_or_null

  subroutine put_obj_or_null(w, p)
    type(jw), intent(inout) :: w
    integer, intent(in) :: p
    if (jis_obj(p)) then
       call jcanon_value(w, p)
    else
       call jw_null(w)
    end if
  end subroutine put_obj_or_null

  subroutine put_epoch(w, p)
    type(jw), intent(inout) :: w
    integer, intent(in) :: p
    if (jtype(p) == J_NUM) then
       call jw_str(w, epoch_to_iso(jint_floor(p), 0_int64))
    else
       call jw_null(w)
    end if
  end subroutine put_epoch

  function epoch_text(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    if (jtype(p) == J_NUM) then
       r = epoch_to_iso(jint_floor(p), 0_int64)
    else
       r = ''
    end if
  end function epoch_text

  integer(int64) function jint_floor(p)
    integer, intent(in) :: p
    real(8) :: x
    integer :: ios
    character(len=64) :: t
    if (jis_int(p)) then
       jint_floor = jint(p)
    else
       t = jraw_text(p)
       read(t, *, iostat=ios) x
       jint_floor = int(floor(x), int64)
    end if
  end function jint_floor

  function answer_text(m) result(r)
    integer, intent(in) :: m
    character(len=:), allocatable :: r
    integer :: c
    logical :: any
    type(strbuf) :: o
    any = .false.
    c = jfirst(jget(m, 'content_list'))
    if (.not. jis_arr(jget(m, 'content_list'))) c = 0
    do while (c > 0)
       if (jis_obj(c)) then
          if (jstr(jget(c, 'phase')) == 'answer') then
             any = .true.
             if (jis_str(jget(c, 'content'))) call sb_add(o, jstr(jget(c, 'content')))
          end if
       end if
       c = jnext(c)
    end do
    if (any) then
       r = sb_str(o)
    else if (jis_str(jget(m, 'content'))) then
       r = jstr(jget(m, 'content'))
    else
       r = ''
    end if
  end function answer_text

  ! thinking events; returns .false. if there are none
  logical function has_events(m)
    integer, intent(in) :: m
    integer :: c, ex, t1, t2
    has_events = .false.
    c = jfirst(jget(m, 'content_list'))
    if (.not. jis_arr(jget(m, 'content_list'))) c = 0
    do while (c > 0)
       if (jis_obj(c)) then
          select case (jstr(jget(c, 'phase')))
          case ('thinking_summary')
             ex = jget(c, 'extra')
             t1 = first_str(jget(jget(ex, 'summary_title'), 'content'))
             t2 = first_str(jget(jget(ex, 'summary_thought'), 'content'))
             if (t1 > 0 .or. t2 > 0) then
                has_events = .true.; return
             end if
          case ('web_search')
             has_events = .true.; return
          end select
       end if
       c = jnext(c)
    end do
  contains
    integer function first_str(arr)
      integer, intent(in) :: arr
      integer :: q
      first_str = 0
      if (.not. jis_arr(arr)) return
      q = jfirst(arr)
      do while (q > 0)
         if (jis_str(q)) then
            first_str = q; return
         end if
         q = jnext(q)
      end do
    end function first_str
  end function has_events

  subroutine write_thinking(w, m)
    type(jw), intent(inout) :: w
    integer, intent(in) :: m
    integer :: c, ex, titles, thoughts, nt, nh, i, wi
    if (.not. has_events(m)) then
       call jw_null(w); return
    end if
    call jw_begin_obj(w)
    call jw_key(w, 'durationMs'); call jw_null(w)
    call jw_key(w, 'endTime'); call jw_null(w)
    call jw_key(w, 'mainRollout'); call jw_null(w)
    call jw_key(w, 'rollouts')
    call jw_begin_arr(w)
    call jw_begin_obj(w)
    call jw_key(w, 'events')
    call jw_begin_arr(w)
    c = jfirst(jget(m, 'content_list'))
    do while (c > 0)
       if (jis_obj(c)) then
          ex = jget(c, 'extra')
          select case (jstr(jget(c, 'phase')))
          case ('thinking_summary')
             titles = jget(jget(ex, 'summary_title'), 'content')
             thoughts = jget(jget(ex, 'summary_thought'), 'content')
             nt = 0; nh = 0
             if (jis_arr(titles)) nt = jlen(titles)
             if (jis_arr(thoughts)) nh = jlen(thoughts)
             do i = 0, max(nt, nh) - 1
                if (i < nt) then
                   if (jis_str(jat(titles, [i]))) then
                      call jw_begin_obj(w)
                      call jw_key(w, 'channel'); call jw_str(w, 'header')
                      call jw_key(w, 'text'); call jw_str(w, jstr(jat(titles, [i])))
                      call jw_key(w, 'type'); call jw_str(w, 'text')
                      call jw_end_obj(w)
                   end if
                end if
                if (i < nh) then
                   if (jis_str(jat(thoughts, [i]))) then
                      call jw_begin_obj(w)
                      call jw_key(w, 'channel'); call jw_str(w, 'thought')
                      call jw_key(w, 'text'); call jw_str(w, jstr(jat(thoughts, [i])))
                      call jw_key(w, 'type'); call jw_str(w, 'text')
                      call jw_end_obj(w)
                   end if
                end if
             end do
          case ('web_search')
             call jw_begin_obj(w)
             call jw_key(w, 'args')
             call jw_begin_obj(w)
             call jw_key(w, 'displayPosition'); call put_raw_or_null(w, jget(ex, 'display_position'))
             call jw_end_obj(w)
             call jw_key(w, 'kind'); call jw_str(w, 'web_search')
             call jw_key(w, 'results')
             call jw_begin_arr(w)
             wi = jfirst(jget(ex, 'web_search_info'))
             if (.not. jis_arr(jget(ex, 'web_search_info'))) wi = 0
             do while (wi > 0)
                if (jis_obj(wi)) then
                   call jw_begin_obj(w)
                   call jw_key(w, 'date'); call put_raw_or_null(w, jget(wi, 'date'))
                   call jw_key(w, 'snippet'); call put_raw_or_null(w, jget(wi, 'snippet'))
                   call jw_key(w, 'title'); call put_raw_or_null(w, jget(wi, 'title'))
                   call jw_key(w, 'url'); call put_raw_or_null(w, jget(wi, 'url'))
                   call jw_end_obj(w)
                end if
                wi = jnext(wi)
             end do
             call jw_end_arr(w)
             call jw_key(w, 'toolCallId'); call jw_null(w)
             call jw_key(w, 'type'); call jw_str(w, 'tool')
             call jw_end_obj(w)
          end select
       end if
       c = jnext(c)
    end do
    call jw_end_arr(w)
    call jw_key(w, 'id'); call jw_str(w, 'Qwen')
    call jw_key(w, 'role'); call jw_null(w)
    call jw_end_obj(w)
    call jw_end_arr(w)
    call jw_key(w, 'startTime'); call jw_null(w)
    call jw_end_obj(w)
  end subroutine write_thinking

  subroutine write_web_results(w, m)
    type(jw), intent(inout) :: w
    integer, intent(in) :: m
    integer :: c, wi
    call jw_begin_arr(w)
    c = jfirst(jget(m, 'content_list'))
    if (.not. jis_arr(jget(m, 'content_list'))) c = 0
    do while (c > 0)
       if (jis_obj(c)) then
          if (jstr(jget(c, 'phase')) == 'web_search') then
             wi = jfirst(jget(jget(c, 'extra'), 'web_search_info'))
             if (.not. jis_arr(jget(jget(c, 'extra'), 'web_search_info'))) wi = 0
             do while (wi > 0)
                if (jis_obj(wi)) then
                   call jw_begin_obj(w)
                   call jw_key(w, 'preview'); call put_raw_or_null(w, jget(wi, 'snippet'))
                   call jw_key(w, 'title'); call put_raw_or_null(w, jget(wi, 'title'))
                   call jw_key(w, 'url'); call put_raw_or_null(w, jget(wi, 'url'))
                   call jw_end_obj(w)
                end if
                wi = jnext(wi)
             end do
          end if
       end if
       c = jnext(c)
    end do
    call jw_end_arr(w)
  end subroutine write_web_results

  ! payload root -> transcript text + turn summaries
  subroutine qwen_transcript(payload, source_url, out_text, turns_out, nturns, offb_ids, noff, messages_total)
    integer, intent(in) :: payload
    character(len=*), intent(in) :: source_url
    character(len=:), allocatable, intent(out) :: out_text
    type(qturn), allocatable, intent(out) :: turns_out(:)
    integer, intent(out) :: nturns, noff, messages_total
    type(text_item), allocatable, intent(out) :: offb_ids(:)
    integer :: data, hist, msgs, cur, m, n, i, k, p, q, parent
    integer, allocatable :: chain(:), branch(:)
    logical, allocatable :: onbranch(:)
    type(jw) :: w
    type(text_item), allocatable :: seen(:)
    logical :: user, dup
    data = jget(payload, 'data')
    hist = jget(jget(data, 'chat'), 'history')
    msgs = jget(hist, 'messages')
    messages_total = jlen(msgs)
    allocate(chain(max(messages_total, 1)), seen(max(messages_total, 1)))
    ! walk currentId -> root
    n = 0
    if (jis_str(jget(hist, 'currentId'))) then
       m = jget(msgs, jstr(jget(hist, 'currentId')))
       do while (m > 0 .and. n < messages_total)
          dup = .false.
          do k = 1, n
             if (chain(k) == m) dup = .true.
          end do
          if (dup) exit
          n = n + 1; chain(n) = m
          if (jis_str(jget(m, 'parentId'))) then
             m = jget(msgs, jstr(jget(m, 'parentId')))
          else
             m = 0
          end if
       end do
    end if
    allocate(branch(max(n, 1)))
    do i = 1, n
       branch(i) = chain(n - i + 1)
    end do
    nturns = n
    allocate(turns_out(max(n, 1)))

    call jw_begin_obj(w)
    call jw_key(w, 'conversation')
    call jw_begin_obj(w)
      call jw_key(w, 'conversationId'); call put_raw_or_null(w, jget(data, 'id'))
      call jw_key(w, 'createTime'); call put_epoch(w, jget(data, 'created_at'))
      call jw_key(w, 'isPublic')
      if (jis_str(jget(data, 'share_id'))) then
         call jw_bool(w, .true.)
      else
         call jw_null(w)
      end if
      call jw_key(w, 'messagesTotal'); call jw_int(w, int(messages_total, int64))
      call jw_key(w, 'modifyTime'); call put_epoch(w, jget(data, 'updated_at'))
      call jw_key(w, 'offBranchMessages')
      call jw_begin_arr(w)
      allocate(offb_ids(max(messages_total, 1))); noff = 0
      q = jfirst(msgs)
      do while (q > 0)
         dup = .false.
         do k = 1, n
            if (branch(k) == q) dup = .true.
         end do
         if (.not. dup) then
            noff = noff + 1
            offb_ids(noff)%s = jstr(jget(q, 'id'))
            call jw_begin_obj(w)
            call jw_key(w, 'createTime'); call put_epoch(w, jget(q, 'timestamp'))
            call jw_key(w, 'error'); call put_obj_or_null(w, jget(q, 'error'))
            call jw_key(w, 'id'); call put_raw_or_null(w, jget(q, 'id'))
            call jw_key(w, 'parentId'); call put_raw_or_null(w, jget(q, 'parentId'))
            call jw_key(w, 'role'); call put_raw_or_null(w, jget(q, 'role'))
            call jw_key(w, 'text')
            if (jstr(jget(q, 'role')) == 'user') then
               call jw_str(w, str_or_empty(jget(q, 'content')))
            else
               call jw_str(w, answer_text(q))
            end if
            call jw_end_obj(w)
         end if
         q = jnext(q)
      end do
      call jw_end_arr(w)
      call jw_key(w, 'platform'); call jw_str(w, 'qwen')
      call jw_key(w, 'sourceUrl'); call jw_str(w, source_url)
      call jw_key(w, 'title'); call put_raw_or_null(w, jget(data, 'title'))
    call jw_end_obj(w)
    call jw_key(w, 'turns')
    call jw_begin_arr(w)
    do i = 1, n
       m = branch(i)
       user = (jstr(jget(m, 'role')) == 'user')
       call jw_begin_obj(w)
       call jw_key(w, 'attachments')
       call jw_begin_arr(w)
       if (user .and. jis_arr(jget(m, 'files'))) then
          q = jfirst(jget(m, 'files'))
          do while (q > 0)
             if (jis_obj(q)) then
                call jw_begin_obj(w)
                call jw_key(w, 'fileId'); call put_raw_or_null(w, jget(q, 'id'))
                call jw_key(w, 'fileName'); call put_raw_or_null(w, jget(q, 'name'))
                call jw_key(w, 'mimeType'); call put_raw_or_null(w, jget(q, 'file_type'))
                call jw_key(w, 'sizeBytes'); call put_raw_or_null(w, jget(q, 'size'))
                call jw_key(w, 'url'); call put_raw_or_null(w, jget(q, 'url'))
                call jw_end_obj(w)
             end if
             q = jnext(q)
          end do
       end if
       call jw_end_arr(w)
       call jw_key(w, 'citations'); call jw_begin_arr(w); call jw_end_arr(w)
       call jw_key(w, 'createTime'); call put_epoch(w, jget(m, 'timestamp'))
       call jw_key(w, 'error'); call put_obj_or_null(w, jget(m, 'error'))
       call jw_key(w, 'featureConfig'); call put_obj_or_null(w, jget(m, 'feature_config'))
       call jw_key(w, 'index'); call jw_int(w, int(i - 1, int64))
       call jw_key(w, 'model')
       if (user) then
          call jw_null(w)
       else if (jis_str(jget(m, 'modelName'))) then
          call jw_str(w, jstr(jget(m, 'modelName')))
       else
          call put_raw_or_null(w, jget(m, 'model'))
       end if
       call jw_key(w, 'parentResponseId'); call put_raw_or_null(w, jget(m, 'parentId'))
       call jw_key(w, 'responseId'); call put_raw_or_null(w, jget(m, 'id'))
       call jw_key(w, 'sender')
       if (user) then
          call jw_str(w, 'human')
       else
          call jw_str(w, 'assistant')
       end if
       call jw_key(w, 'siblings')
       parent = 0
       if (jis_str(jget(m, 'parentId'))) parent = jget(msgs, jstr(jget(m, 'parentId')))
       if (parent > 0) then
          if (jis_arr(jget(parent, 'childrenIds'))) then
             call jw_int(w, int(jlen(jget(parent, 'childrenIds')), int64))
          else
             call jw_int(w, 0_int64)
          end if
       else
          call jw_int(w, 1_int64)
       end if
       call jw_key(w, 'sources')
       if (user) then
          call jw_null(w)
       else
          call jw_begin_obj(w)
          call jw_key(w, 'webSearchResults'); call write_web_results(w, m)
          call jw_end_obj(w)
       end if
       turns_out(i)%human = user
       turns_out(i)%index = i - 1
       turns_out(i)%id = jstr(jget(m, 'id'))
       if (user) then
          turns_out(i)%text = str_or_empty(jget(m, 'content'))
       else
          turns_out(i)%text = answer_text(m)
       end if
       call jw_key(w, 'text'); call jw_str(w, turns_out(i)%text)
       call jw_key(w, 'thinking')
       if (user) then
          call jw_null(w)
       else
          call write_thinking(w, m)
       end if
       call jw_key(w, 'usage')
       if (user) then
          call jw_null(w)
       else
          call put_obj_or_null(w, jget(m, 'usage'))
       end if
       call jw_end_obj(w)
    end do
    call jw_end_arr(w)
    call jw_end_obj(w)
    out_text = jw_result(w)
  end subroutine qwen_transcript

  function str_or_empty(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    r = ''
    if (jis_str(p)) r = jstr(p)
  end function str_or_empty

  ! ---------------------------------------------------------------- page checks

  ! norm: drop * _ ` # > | -, NBSP -> space, collapse ASCII whitespace, trim
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
       if (c == 42 .or. c == 95 .or. c == 96 .or. c == 35 .or. c == 62 .or. c == 124 .or. c == 45) then
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
          if (sp .and. o%n > 0) call sb_add(o, ' ')
          sp = .false.
          call sb_add(o, s(i:i))
       end if
       i = i + 1
    end do
    r = str_trim_ws(sb_str(o))
  end function norm

  function prefix(s, n) result(r)
    character(len=*), intent(in) :: s
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    r = utf8_prefix(norm(s), n)
  end function prefix

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
  ! a leaked reasoning delimiter (<think>, </think>, "</think" glued to the answer) is not shown by the page
  function strip_think(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, k
    i = 1
    do while (i <= len(s))
       k = 0
       if (i + 6 <= len(s)) then
          if (s(i:i+6) == '</think') k = 7
       end if
       if (k == 0 .and. i + 5 <= len(s)) then
          if (s(i:i+5) == '<think') k = 6
       end if
       if (k > 0) then
          i = i + k
          if (i <= len(s)) then
             if (s(i:i) == '>') i = i + 1
          end if
          cycle
       end if
       call sb_add(o, s(i:i)); i = i + 1
    end do
    r = sb_str(o)
  end function strip_think
  ! first line whose norm is non-empty (the line itself)
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
       if (len(norm(ln)) > 0) then
          r = ln; return
       end if
    end do
    r = ''
  end function first_line

  subroutine qwen_dom_checks(w, turns, nturns, transcript_root, dom, nfailed, fnames, fidx, nchecks, warnings, nwarn)
    type(jw), intent(inout) :: w
    type(qturn), intent(in) :: turns(:)
    integer, intent(in) :: nturns, transcript_root, dom
    integer, intent(out) :: nfailed, nchecks, nwarn
    type(text_item), allocatable, intent(out) :: fnames(:), warnings(:)
    integer, allocatable, intent(out) :: fidx(:)
    integer :: du, da, d, i, k, off, offs, nh, na, seen_hit, q
    logical :: scrolled, found, ok
    character(len=:), allocatable :: want, did, qt
    integer :: ai
    allocate(fnames(4*max(nturns,1) + 64), fidx(4*max(nturns,1) + 64), warnings(max(nturns,1) + 8))
    nfailed = 0; nchecks = 0; nwarn = 0
    du = jget(dom, 'users'); da = jget(dom, 'assistants')
    scrolled = .not. jis_false(jget(dom, 'scrolled'))
    offs = jget(jget(transcript_root, 'conversation'), 'offBranchMessages')
    nh = 0; na = 0
    do i = 1, nturns
       if (turns(i)%human) then
          nh = nh + 1
       else
          na = na + 1
       end if
    end do
    call jw_begin_arr(w)
    ! rendered replies
    d = jfirst(da)
    do while (d > 0)
       did = jstr(jget(d, 'id'))
       ai = 0
       do i = 1, nturns
          if (.not. turns(i)%human .and. turns(i)%id == did) then
             ai = i; exit
          end if
       end do
       off = 0
       q = jfirst(offs)
       do while (q > 0)
          if (jstr(jget(q, 'id')) == did) then
             off = q; exit
          end if
          q = jnext(q)
       end do
       if (ai > 0) then
          want = prefix(strip_list_marker(first_line(strip_think(turns(ai)%text))), 40)
       else if (off > 0) then
          want = prefix(strip_list_marker(first_line(strip_think(jstr(jget(off, 'text'))))), 40)
       else
          want = ''
       end if
       if (off > 0 .and. ai == 0) then
          nwarn = nwarn + 1
          warnings(nwarn)%s = 'the page shows reply '//did//', an alternate version of the reply on the API''s current branch (parent '// &
                              jstr(jget(off, 'parentId'))//'); both are in the export'
       end if
       ok = (ai > 0 .or. off > 0) .and. (len(want) == 0 .or. index(norm(jstr(jget(d, 'text'))), want) > 0)
       if (ai > 0) then
          call check('page-assistant-in-api', ok, 'id '//did//'; api first line starts '//write_repr(want), turns(ai)%index)
       else if (off > 0) then
          call check('page-assistant-in-api', ok, 'id '//did//' (off-branch version); api first line starts '//write_repr(want), -1)
       else
          call check('page-assistant-in-api', ok, 'id '//did//' not in the API message tree', -1)
       end if
       d = jnext(d)
    end do
    ! rendered user messages
    q = jfirst(du)
    do while (q > 0)
       qt = jstr(q)
       found = .false.; k = -1
       do i = 1, nturns
          if (.not. turns(i)%human) cycle
          want = prefix(turns(i)%text, 60)
          if (len(norm(qt)) == 0) then
             found = (len(want) == 0)
          else
             found = (len(want) > 0 .and. index(norm(qt), want) > 0)
          end if
          if (found) then
             k = turns(i)%index; exit
          end if
       end do
       call check('page-user-in-api', found, 'page text starts '//write_repr(prefix(qt, 60)), k)
       q = jnext(q)
    end do
    if (scrolled) then
       call check('dom-reached-top', jint(jget(dom, 'stableRounds')) >= 1, i64toa(jint(jget(dom, 'rounds')))//' top rounds, '// &
                  i64toa(jint(jget(dom, 'steps')))//' steps down, '//i64toa(jint(jget(dom, 'ms')))//' ms', -1)
       do i = 1, nturns
          if (turns(i)%human) cycle
          seen_hit = 0
          d = jfirst(da)
          do while (d > 0)
             if (jstr(jget(d, 'id')) == turns(i)%id) seen_hit = 1
             d = jnext(d)
          end do
          call check('api-assistant-seen-on-page', seen_hit == 1, 'message id '//turns(i)%id, turns(i)%index)
       end do
       call check('api-user-count-seen', jlen(du) >= nh, 'page '//itoa(jlen(du))//', api '//itoa(nh), -1)
    else
       nwarn = nwarn + 1
       warnings(nwarn)%s = 'read-only snapshot of a shared Qwen window: it showed '//itoa(jlen(da))//' of '//itoa(na)// &
                           ' replies and '//itoa(jlen(du))//' of '//itoa(nh)//' user messages; coverage not checked'
    end if
    call jw_end_arr(w)
  contains
    subroutine check(name, okv, detail, turn)
      character(len=*), intent(in) :: name, detail
      logical, intent(in) :: okv
      integer, intent(in) :: turn
      nchecks = nchecks + 1
      call jw_begin_obj(w)
      call jw_key(w, 'detail'); call jw_str(w, detail)
      call jw_key(w, 'name'); call jw_str(w, name)
      call jw_key(w, 'ok'); call jw_bool(w, okv)
      call jw_key(w, 'turnIndex')
      if (turn >= 0) then
         call jw_int(w, int(turn, int64))
      else
         call jw_null(w)
      end if
      call jw_end_obj(w)
      if (.not. okv) then
         nfailed = nfailed + 1; fnames(nfailed)%s = name; fidx(nfailed) = turn
      end if
    end subroutine check
  end subroutine qwen_dom_checks

end module fx_qwen
