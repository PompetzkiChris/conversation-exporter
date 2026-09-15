! fx_grok.f90 — Grok API JSON (chunk or legacy format) -> transcript.json.
! Port of racket/transcript.rkt, itself a byte-for-byte port of fixtures/reference_transcript.py:
! Python truthiness, dict.get, `a or b`, str() rendering, citation offsets in characters,
! round-half-even thinking durations and parent-chain ordering are all reproduced.
module fx_grok
  use iso_fortran_env, only: int64, real64
  use fx_util
  use fx_json
  implicit none
  private
  public :: grok_transcript, grok_format, truthy, py_str, grok_merge_ws_results

  character(len=*), parameter :: ASSET_BASE = 'https://assets.grok.com/'
  character(len=*), parameter :: SOLO_ROLLOUT = 'Grok'

  integer, parameter :: EV_SUMMARY = 1, EV_TEXT = 2, EV_TOOL = 3, EV_RESULT = 4, EV_UNKNOWN = 5
  integer, parameter :: RES_NULL = 0, RES_WEB = 1, RES_X = 2, RES_KIND = 3, RES_KINDNULL = 4

  type :: text_item_g
     character(len=:), allocatable :: s
  end type text_item_g

  type :: gevent
     integer :: kind = 0
     character(len=:), allocatable :: text, channel, tkind
     logical :: tkind_null = .false.
     integer :: channel_node = 0        ! non-string truthy channel (dumped raw)
     integer :: callid = 0              ! raw node
     integer :: args = 0                ! raw node; 0 with args_empty -> {}
     logical :: args_empty = .false.
     integer :: res_mode = RES_NULL
     character(len=:), allocatable :: res_kind
     integer :: res_items = 0           ! array node normalized as web/x
     integer :: raw = 0
  end type gevent

  type :: grollout
     character(len=:), allocatable :: name
     integer :: role = 0                 ! 0 null, 1 Leader, 2 Agent
     integer, allocatable :: ev(:)
     integer :: nev = 0
  end type grollout

  type :: gcit
     integer :: offset
     integer :: citation_node = 0        ! raw citationId (chunk) ; -1 => use citation_num
     integer(int64) :: citation_num = 0
     logical :: citation_zero = .false.
     integer :: card_node = 0
     character(len=:), allocatable :: card_str   ! legacy lower-cased hex
     integer :: kind_node = 0
     character(len=:), allocatable :: kind_str
     logical :: kind_null = .true.
     integer :: url_node = 0
  end type gcit

  ! searched-image card of a legacy reply (reference strip_images)
  type :: gimg
     integer :: offset = 0
     character(len=:), allocatable :: cid, image_id, size
     logical :: has_id = .false., has_size = .false.
     integer :: img = 0                  ! the card's "image" object, 0 = none
  end type gimg
  type(gimg), allocatable :: imgs(:)
  integer :: nimgs = 0

  ! per-turn state
  type(gevent), allocatable :: evs(:)
  integer :: nevs
  type(grollout), allocatable :: rls(:)
  integer :: nrls
  integer, allocatable :: unattr(:)
  integer :: nunattr
  character(len=:), allocatable :: main_name
  logical :: main_null
  integer :: nids
  ! card map: callid raw text -> event index
  type(text_item_g), allocatable :: card_keys(:)
  integer, allocatable :: card_ev(:)
  integer :: ncards


  type :: ws_key
     character(len=:), allocatable :: s
  end type ws_key
  type(ws_key), allocatable, save :: ws_ids(:)
  integer, allocatable, save :: ws_ce(:)
  integer, save :: nws = 0

contains

  ! ---------------------------------------------------------------- python-ish helpers

  logical function truthy(p)
    integer, intent(in) :: p
    select case (jtype(p))
    case (0, J_NULL, J_FALSE)
       truthy = .false.
    case (J_STR)
       truthy = (len(jstr(p)) > 0)
    case (J_NUM)
       truthy = .not. is_zero_num(p)
    case (J_ARR, J_OBJ)
       truthy = (jlen(p) > 0)
    case default
       truthy = .true.
    end select
  end function truthy

  logical function is_zero_num(p)
    integer, intent(in) :: p
    real(real64) :: x
    integer :: ios
    character(len=:), allocatable :: t
    t = jraw_text(p)
    read(t, *, iostat=ios) x
    is_zero_num = (ios == 0 .and. x == 0.0_real64)
  end function is_zero_num

  ! Python str(): strings as-is, None/True/False, ints, lists/dicts via repr
  recursive function py_str(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    integer :: c, n, i, j, t
    integer, allocatable :: kids(:)
    type(strbuf) :: o
    select case (jtype(p))
    case (J_STR)
       r = jstr(p)
    case (0, J_NULL)
       r = 'None'
    case (J_TRUE)
       r = 'True'
    case (J_FALSE)
       r = 'False'
    case (J_NUM)
       r = jraw_text(p)
    case (J_ARR)
       call sb_add(o, '[')
       c = jfirst(p); i = 0
       do while (c > 0)
          if (i > 0) call sb_add(o, ', ')
          call sb_add(o, py_repr(c)); i = i + 1
          c = jnext(c)
       end do
       call sb_add(o, ']'); r = sb_str(o)
    case (J_OBJ)
       n = jlen(p); allocate(kids(n))
       c = jfirst(p); i = 0
       do while (c > 0)
          i = i + 1; kids(i) = c; c = jnext(c)
       end do
       do i = 2, n
          t = kids(i); j = i - 1
          do while (j >= 1)
             if (.not. lgt(jname(kids(j)), jname(t))) exit
             kids(j+1) = kids(j); j = j - 1
          end do
          kids(j+1) = t
       end do
       call sb_add(o, '{')
       do i = 1, n
          if (i > 1) call sb_add(o, ', ')
          call sb_add(o, "'"//jname(kids(i))//"': "//py_repr(kids(i)))
       end do
       call sb_add(o, '}'); r = sb_str(o)
    case default
       r = ''
    end select
  end function py_str

  recursive function py_repr(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    if (jis_str(p)) then
       r = "'"//jstr(p)//"'"
    else
       r = py_str(p)
    end if
  end function py_repr

  ! `a or b`: first truthy, else the last
  integer function py_or2(a, b)
    integer, intent(in) :: a, b
    if (truthy(a)) then
       py_or2 = a
    else
       py_or2 = b
    end if
  end function py_or2

  function str_or_empty(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    r = ''
    if (jis_str(p)) r = jstr(p)
  end function str_or_empty

  integer function list_or_empty(p)       ! handle if truthy list, else 0
    integer, intent(in) :: p
    list_or_empty = 0
    if (jis_arr(p) .and. truthy(p)) list_or_empty = p
  end function list_or_empty

  integer function hash_or_empty(p)
    integer, intent(in) :: p
    hash_or_empty = 0
    if (jis_obj(p) .and. truthy(p)) hash_or_empty = p
  end function hash_or_empty

  logical function jhas(h, key)
    integer, intent(in) :: h
    character(len=*), intent(in) :: key
    jhas = (jget(h, key) > 0)
  end function jhas

  integer function nchars(s)
    character(len=*), intent(in) :: s
    integer :: i, c
    nchars = 0
    do i = 1, len(s)
       c = iachar(s(i:i))
       if (c < 128 .or. c >= 192) nchars = nchars + 1
    end do
  end function nchars

  ! canonical identity text of a scalar node (for dict keys)
  function key_text(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    if (jis_str(p)) then
       r = 's:'//jstr(p)
    else if (jis_null(p)) then
       r = 'n:'
    else
       r = 'r:'//jraw_text(p)
    end if
  end function key_text

  ! ---------------------------------------------------------------- time

  ! RFC3339 -> microseconds since epoch; ok=.false. if unparsable
  subroutine parse_time_us(p, us, ok)
    integer, intent(in) :: p
    integer(int64), intent(out) :: us
    logical, intent(out) :: ok
    character(len=:), allocatable :: t
    integer :: i, n
    integer(int64) :: y, mo, d, hh, mi, ss, frac, tzh, tzm, tzs, off, days
    integer :: sign, k
    ok = .false.; us = 0
    if (.not. jis_str(p)) return
    t = jstr(p); n = len(t)
    if (n < 10) return
    if (.not. (dig(t,1,4) .and. t(5:5) == '-' .and. dig(t,6,7) .and. t(8:8) == '-' .and. dig(t,9,10))) return
    y = num(t,1,4); mo = num(t,6,7); d = num(t,9,10)
    hh = 0; mi = 0; ss = 0; frac = 0; sign = 1; tzh = 0; tzm = 0; tzs = 0
    i = 11
    if (i <= n) then
       if (t(i:i) == 'T' .or. t(i:i) == ' ') then
          if (i + 5 > n) return
          if (.not. (dig(t,i+1,i+2) .and. t(i+3:i+3) == ':' .and. dig(t,i+4,i+5))) return
          hh = num(t,i+1,i+2); mi = num(t,i+4,i+5); i = i + 6
          if (i <= n) then
             if (t(i:i) == ':') then
                if (i + 2 > n) return
                if (.not. dig(t,i+1,i+2)) return
                ss = num(t,i+1,i+2); i = i + 3
                if (i <= n) then
                   if (t(i:i) == '.' .or. t(i:i) == ',') then
                      k = i + 1
                      do while (k <= n)
                         if (index('0123456789', t(k:k)) == 0) exit
                         k = k + 1
                      end do
                      if (k == i + 1) return
                      frac = frac6(t(i+1:k-1)); i = k
                   end if
                end if
             end if
          end if
       end if
    end if
    if (i <= n) then
       if (t(i:i) == 'Z' .or. t(i:i) == 'z') then
          i = i + 1
       else if (t(i:i) == '+' .or. t(i:i) == '-') then
          if (t(i:i) == '-') sign = -1
          if (i + 2 > n) return
          if (.not. dig(t,i+1,i+2)) return
          tzh = num(t,i+1,i+2); i = i + 3
          if (i <= n) then
             if (t(i:i) == ':') i = i + 1
          end if
          if (i + 1 > n) return
          if (.not. dig(t,i,i+1)) return
          tzm = num(t,i,i+1); i = i + 2
          if (i <= n) then
             if (t(i:i) == ':') then
                if (i + 2 > n) return
                if (.not. dig(t,i+1,i+2)) return
                tzs = num(t,i+1,i+2); i = i + 3
             end if
          end if
       end if
    end if
    if (i <= n) return
    if (mo < 1 .or. mo > 12 .or. d < 1 .or. d > 31 .or. hh > 23 .or. mi > 59 .or. ss > 59 .or. tzh > 23 .or. tzm > 59) return
    off = sign * (tzh*3600 + tzm*60 + tzs)
    days = dfc(y, mo, d)
    us = (days*86400 + hh*3600 + mi*60 + ss - off) * 1000000_int64 + frac
    ok = .true.
  contains
    logical function dig(s, a, b)
      character(len=*), intent(in) :: s
      integer, intent(in) :: a, b
      integer :: q
      dig = .false.
      if (b > len(s)) return
      do q = a, b
         if (index('0123456789', s(q:q)) == 0) return
      end do
      dig = .true.
    end function dig
    integer(int64) function num(s, a, b)
      character(len=*), intent(in) :: s
      integer, intent(in) :: a, b
      integer :: q
      num = 0
      do q = a, b
         num = num*10 + (iachar(s(q:q)) - 48)
      end do
    end function num
    integer(int64) function frac6(s)
      character(len=*), intent(in) :: s
      character(len=6) :: f
      integer :: q
      f = '000000'
      do q = 1, min(6, len(s))
         f(q:q) = s(q:q)
      end do
      frac6 = num(f, 1, 6)
    end function frac6
    integer(int64) function dfc(yy, mm, dd)
      integer(int64), intent(in) :: yy, mm, dd
      integer(int64) :: y2, era, yoe, mp, doy, doe
      y2 = yy; if (mm <= 2) y2 = yy - 1
      if (y2 >= 0) then
         era = y2 / 400
      else
         era = (y2 - 399) / 400
      end if
      yoe = y2 - era*400
      if (mm > 2) then
         mp = mm - 3
      else
         mp = mm + 9
      end if
      doy = (153*mp + 2)/5 + dd - 1
      doe = yoe*365 + yoe/4 - yoe/100 + doy
      dfc = era*146097 + doe - 719468
    end function dfc
  end subroutine parse_time_us

  subroutine put_duration(w, a, b)
    type(jw), intent(inout) :: w
    integer, intent(in) :: a, b
    integer(int64) :: ta, tb
    logical :: oka, okb
    real(real64) :: ms, fl
    integer(int64) :: r
    call parse_time_us(a, ta, oka); call parse_time_us(b, tb, okb)
    if (.not. (oka .and. okb)) then
       call jw_null(w); return
    end if
    ms = (real(tb - ta, real64) / 1000000.0_real64) * 1000.0_real64
    fl = floor(ms)
    if (ms - fl > 0.5_real64) then
       r = int(fl, int64) + 1
    else if (ms - fl < 0.5_real64) then
       r = int(fl, int64)
    else
       r = int(fl, int64)
       if (mod(r, 2_int64) /= 0) r = r + 1
    end if
    call jw_int(w, r)
  end subroutine put_duration

  ! ---------------------------------------------------------------- tool helpers

  ! first_key: known kinds first, else smallest key; skips toolUsageCardId/toolCallId
  subroutine first_key(d, kname, knull, body)
    integer, intent(in) :: d
    character(len=:), allocatable, intent(out) :: kname
    logical, intent(out) :: knull
    integer, intent(out) :: body
    character(len=20), parameter :: pri(9) = [character(len=20) :: 'webSearch', 'xPost', 'xSearch', 'xUserSearch', &
         'browsePage', 'viewImage', 'chatroomSend', 'initTerminalSession', 'conversationSearch']
    integer :: c, k, chosen
    character(len=:), allocatable :: nm, best
    kname = ''; knull = .true.; body = 0
    if (.not. jis_obj(d)) return
    chosen = 0
    do k = 1, 9
       c = jfirst(d)
       do while (c > 0)
          if (jname(c) == trim(pri(k))) then
             chosen = c; exit
          end if
          c = jnext(c)
       end do
       if (chosen > 0) exit
    end do
    if (chosen == 0) then
       best = ''
       c = jfirst(d)
       do while (c > 0)
          nm = jname(c)
          if (nm /= 'toolUsageCardId' .and. nm /= 'toolCallId') then
             if (chosen == 0) then
                chosen = c; best = nm
             else if (llt(nm, best)) then
                chosen = c; best = nm
             end if
          end if
          c = jnext(c)
       end do
    end if
    if (chosen == 0) return
    kname = jname(chosen); knull = .false.
    body = chosen
    if (jis_null(chosen)) body = 0
  end subroutine first_key

  ! card_args: body.args if object; else body if non-empty object; else 0 (null)
  integer function card_args(body)
    integer, intent(in) :: body
    card_args = 0
    if (.not. jis_obj(body)) return
    if (jis_obj(jget(body, 'args'))) then
       card_args = jget(body, 'args')
    else if (jlen(body) > 0) then
       card_args = body
    end if
  end function card_args

  ! <xai:tool_args><![CDATA[...]]></xai:tool_args> -> its "args" object, else 0 ({} for the caller)
  integer function args_from_xml(text)
    character(len=*), intent(in) :: text
    integer :: a, b, d
    character(len=*), parameter :: open_t = '<xai:tool_args><![CDATA[', close_t = ']]></xai:tool_args>'
    args_from_xml = 0
    a = index(text, open_t)
    if (a == 0) return
    b = index(text(a+len(open_t):), close_t)
    if (b == 0) return
    d = jparse(text(a+len(open_t):a+len(open_t)+b-2))
    if (jis_obj(d)) then
       if (jis_obj(jget(d, 'args'))) args_from_xml = jget(d, 'args')
    end if
  end function args_from_xml

  ! ---------------------------------------------------------------- rollouts

  subroutine reset_turn()
    if (allocated(evs)) deallocate(evs)
    if (allocated(rls)) deallocate(rls)
    if (allocated(unattr)) deallocate(unattr)
    if (allocated(card_keys)) deallocate(card_keys)
    if (allocated(card_ev)) deallocate(card_ev)
    allocate(evs(64), rls(8), unattr(16), card_keys(16), card_ev(16))
    nevs = 0; nrls = 0; nunattr = 0; ncards = 0; nimgs = 0
  end subroutine reset_turn

  subroutine make_rollouts(ids)
    integer, intent(in) :: ids
    integer :: c, dummy
    nids = 0
    main_null = .true.; main_name = ''
    if (ids > 0) then
       nids = jlen(ids)
       if (nids > 0) then
          c = jfirst(ids)
          main_null = .false.; main_name = py_str(c)
       end if
       c = jfirst(ids)
       do while (c > 0)
          dummy = rollout_get(c, '')
          c = jnext(c)
       end do
    end if
  end subroutine make_rollouts

  ! rollouts-get: name = py-or(name0, main, "")   (name0 given as node, or literal when node==0 and lit/=' ')
  integer function rollout_get(node, lit) result(k)
    integer, intent(in) :: node
    character(len=*), intent(in) :: lit
    character(len=:), allocatable :: nm
    type(grollout), allocatable :: tmp(:)
    integer :: i
    if (node > 0) then
       if (truthy(node)) then
          nm = py_str(node)
       else if (.not. main_null .and. len(main_name) > 0) then
          nm = main_name
       else
          nm = ''
       end if
    else if (len(lit) > 0) then
       nm = lit
    else if (.not. main_null .and. len(main_name) > 0) then
       nm = main_name
    else
       nm = ''
    end if
    do i = 1, nrls
       if (rls(i)%name == nm) then
          k = i; return
       end if
    end do
    if (nrls >= size(rls)) then
       allocate(tmp(2*size(rls))); tmp(1:nrls) = rls(1:nrls); call move_alloc(tmp, rls)
    end if
    nrls = nrls + 1; k = nrls
    rls(k)%name = nm
    allocate(rls(k)%ev(16)); rls(k)%nev = 0
    if (nids == 0) then
       rls(k)%role = 0
    else if (.not. main_null .and. nm == main_name) then
       rls(k)%role = 1
    else
       rls(k)%role = 2
    end if
  end function rollout_get

  integer function new_event(kind)
    integer, intent(in) :: kind
    type(gevent), allocatable :: tmp(:)
    if (nevs >= size(evs)) then
       allocate(tmp(2*size(evs))); tmp(1:nevs) = evs(1:nevs); call move_alloc(tmp, evs)
    end if
    nevs = nevs + 1
    evs(nevs) = gevent()
    evs(nevs)%kind = kind
    new_event = nevs
  end function new_event

  ! add-event!: rollout node (0 or "" -> unattributed / solo)
  subroutine add_event(rnode, e)
    integer, intent(in) :: rnode, e
    integer :: k
    integer, allocatable :: tmp(:)
    logical :: empty
    empty = .true.
    if (rnode > 0) then
       if (.not. jis_null(rnode)) then
          empty = .false.
          if (jis_str(rnode)) empty = (len(jstr(rnode)) == 0)
       end if
    end if
    if (empty) then
       if (nids == 0) then
          k = rollout_get(0, SOLO_ROLLOUT)
          call push(rls(k)%ev, rls(k)%nev, e)
       else
          if (nunattr >= size(unattr)) then
             allocate(tmp(2*size(unattr))); tmp(1:nunattr) = unattr(1:nunattr); call move_alloc(tmp, unattr)
          end if
          nunattr = nunattr + 1; unattr(nunattr) = e
       end if
    else
       k = rollout_get(rnode, '')
       call push(rls(k)%ev, rls(k)%nev, e)
    end if
  contains
    subroutine push(arr, n, v)
      integer, allocatable, intent(inout) :: arr(:)
      integer, intent(inout) :: n
      integer, intent(in) :: v
      integer, allocatable :: t2(:)
      if (n >= size(arr)) then
         allocate(t2(2*size(arr))); t2(1:n) = arr(1:n); call move_alloc(t2, arr)
      end if
      n = n + 1; arr(n) = v
    end subroutine push
  end subroutine add_event

  subroutine add_tool(rnode, card_id_node, kname, knull, args)
    integer, intent(in) :: rnode, card_id_node, args
    character(len=*), intent(in) :: kname
    logical, intent(in) :: knull
    integer :: e, i
    type(text_item_g), allocatable :: tk(:)
    integer, allocatable :: te(:)
    character(len=:), allocatable :: key
    e = new_event(EV_TOOL)
    evs(e)%tkind = kname; evs(e)%tkind_null = knull
    evs(e)%callid = card_id_node
    if (truthy(args)) then
       evs(e)%args = args
    else
       evs(e)%args_empty = .true.
    end if
    call add_event(rnode, e)
    key = key_text(card_id_node)
    do i = 1, ncards
       if (card_keys(i)%s == key) then
          card_ev(i) = e; return
       end if
    end do
    if (ncards >= size(card_keys)) then
       allocate(tk(2*size(card_keys)), te(2*size(card_keys)))
       tk(1:ncards) = card_keys(1:ncards); te(1:ncards) = card_ev(1:ncards)
       call move_alloc(tk, card_keys); call move_alloc(te, card_ev)
    end if
    ncards = ncards + 1; card_keys(ncards)%s = key; card_ev(ncards) = e
  end subroutine add_tool

  subroutine add_result(rnode, call_id_node, mode, rkind, items)
    integer, intent(in) :: rnode, call_id_node, mode, items
    character(len=*), intent(in) :: rkind
    integer :: i, e
    character(len=:), allocatable :: key
    key = key_text(call_id_node)
    do i = 1, ncards
       if (card_keys(i)%s == key) then
          e = card_ev(i)
          evs(e)%res_mode = mode; evs(e)%res_kind = rkind; evs(e)%res_items = items
          return
       end if
    end do
    e = new_event(EV_RESULT)
    evs(e)%callid = call_id_node
    evs(e)%res_mode = mode; evs(e)%res_kind = rkind; evs(e)%res_items = items
    call add_event(rnode, e)
  end subroutine add_result

  ! ---------------------------------------------------------------- writers

  subroutine write_norm_web(w, items)
    type(jw), intent(inout) :: w
    integer, intent(in) :: items
    integer :: c, l
    call jw_begin_arr(w)
    l = list_or_empty(items)
    c = jfirst(l)
    do while (c > 0)
       call jw_begin_obj(w)
       call jw_key(w, 'preview')
       if (jhas(c, 'snippet')) then
          call jcanon_value(w, jget(c, 'snippet'))
       else
          call jcanon_value(w, jget(c, 'preview'))
       end if
       call jw_key(w, 'title'); call jcanon_value(w, jget(c, 'title'))
       call jw_key(w, 'url'); call jcanon_value(w, jget(c, 'url'))
       call jw_end_obj(w)
       c = jnext(c)
    end do
    call jw_end_arr(w)
  end subroutine write_norm_web

  subroutine write_norm_x(w, items)
    type(jw), intent(inout) :: w
    integer, intent(in) :: items
    integer :: c, l
    call jw_begin_arr(w)
    l = list_or_empty(items)
    c = jfirst(l)
    do while (c > 0)
       call jw_begin_obj(w)
       call jw_key(w, 'createTime'); call jcanon_value(w, jget(c, 'createTime'))
       call jw_key(w, 'name'); call jcanon_value(w, jget(c, 'name'))
       call jw_key(w, 'postId'); call jcanon_value(w, jget(c, 'postId'))
       call jw_key(w, 'text'); call jcanon_value(w, jget(c, 'text'))
       call jw_key(w, 'username')
       if (jhas(c, 'userhandle')) then
          call jcanon_value(w, jget(c, 'userhandle'))
       else
          call jcanon_value(w, jget(c, 'username'))
       end if
       call jw_end_obj(w)
       c = jnext(c)
    end do
    call jw_end_arr(w)
  end subroutine write_norm_x

  integer function items_len(items)
    integer, intent(in) :: items
    items_len = jlen(list_or_empty(items))
  end function items_len

  subroutine write_results(w, e)
    type(jw), intent(inout) :: w
    integer, intent(in) :: e
    select case (evs(e)%res_mode)
    case (RES_NULL)
       call jw_null(w)
    case (RES_WEB)
       call jw_begin_obj(w)
       call jw_key(w, 'items'); call write_norm_web(w, evs(e)%res_items)
       call jw_key(w, 'kind'); call jw_str(w, 'web')
       call jw_end_obj(w)
    case (RES_X)
       call jw_begin_obj(w)
       call jw_key(w, 'items'); call write_norm_x(w, evs(e)%res_items)
       call jw_key(w, 'kind'); call jw_str(w, 'x')
       call jw_end_obj(w)
    case (RES_KIND)
       call jw_begin_obj(w)
       call jw_key(w, 'items'); call jw_begin_arr(w); call jw_end_arr(w)
       call jw_key(w, 'kind'); call jw_str(w, evs(e)%res_kind)
       call jw_end_obj(w)
    case (RES_KINDNULL)
       call jw_begin_obj(w)
       call jw_key(w, 'items'); call jw_begin_arr(w); call jw_end_arr(w)
       call jw_key(w, 'kind'); call jw_null(w)
       call jw_end_obj(w)
    end select
  end subroutine write_results

  subroutine write_event(w, e)
    type(jw), intent(inout) :: w
    integer, intent(in) :: e
    call jw_begin_obj(w)
    select case (evs(e)%kind)
    case (EV_SUMMARY)
       call jw_key(w, 'text'); call jw_str(w, evs(e)%text)
       call jw_key(w, 'type'); call jw_str(w, 'summary')
    case (EV_TEXT)
       call jw_key(w, 'channel')
       if (evs(e)%channel_node > 0) then
          call jcanon_value(w, evs(e)%channel_node)
       else
          call jw_str(w, evs(e)%channel)
       end if
       call jw_key(w, 'text'); call jw_str(w, evs(e)%text)
       call jw_key(w, 'type'); call jw_str(w, 'text')
    case (EV_TOOL)
       call jw_key(w, 'args')
       if (evs(e)%args_empty) then
          call jw_begin_obj(w); call jw_end_obj(w)
       else
          call jcanon_value(w, evs(e)%args)
       end if
       call jw_key(w, 'kind')
       if (evs(e)%tkind_null) then
          call jw_null(w)
       else
          call jw_str(w, evs(e)%tkind)
       end if
       call jw_key(w, 'results'); call write_results(w, e)
       call jw_key(w, 'toolCallId'); call jcanon_value(w, evs(e)%callid)
       call jw_key(w, 'type'); call jw_str(w, 'tool')
    case (EV_RESULT)
       call jw_key(w, 'results'); call write_results(w, e)
       call jw_key(w, 'toolCallId'); call jcanon_value(w, evs(e)%callid)
       call jw_key(w, 'type'); call jw_str(w, 'tool_result')
    case (EV_UNKNOWN)
       call jw_key(w, 'raw'); call jcanon_value(w, evs(e)%raw)
       call jw_key(w, 'type'); call jw_str(w, 'unknown')
    end select
    call jw_end_obj(w)
  end subroutine write_event

  integer function result_rows(e)
    integer, intent(in) :: e
    result_rows = 0
    select case (evs(e)%res_mode)
    case (RES_WEB, RES_X)
       result_rows = items_len(evs(e)%res_items)
    end select
  end function result_rows

  ! ---------------------------------------------------------------- per format

  subroutine assistant_from_chunks(r, text, cits, ncits)
    integer, intent(in) :: r
    character(len=:), allocatable, intent(out) :: text
    type(gcit), allocatable, intent(out) :: cits(:)
    integer, intent(out) :: ncits
    type(strbuf) :: parts
    integer :: c, meta, rname, t, ch, card, res, body, e, cit, pos
    logical :: is_main, knull
    character(len=:), allocatable :: chs, txt, kname
    allocate(cits(16)); ncits = 0; pos = 0
    c = jfirst(list_or_empty(jget(r, 'outputChunks')))
    do while (c > 0)
       meta = hash_or_empty(jget(c, 'metadata'))
       rname = jget(meta, 'rolloutId')
       if (.not. truthy(rname)) rname = 0
       is_main = (rname == 0)
       if (.not. is_main .and. .not. main_null) is_main = (py_str(rname) == main_name)
       if (jhas(c, 'text')) then
          t = jget(c, 'text')
          ch = jget(t, 'channel')
          txt = str_or_empty(jget(t, 'text'))
          chs = ''
          if (truthy(ch) .and. jis_str(ch)) chs = jstr(ch)
          if (chs == 'CHANNEL_ASSISTANT_RESPONSE' .and. truthy(ch)) then
             if (is_main) then
                call sb_add(parts, txt); pos = pos + nchars(txt)
             end if
          else if (chs == 'CHANNEL_ASSISTANT_NOTETAKER_SUMMARY' .and. truthy(ch)) then
             e = new_event(EV_SUMMARY); evs(e)%text = txt; call add_event(rname, e)
          else
             e = new_event(EV_TEXT); evs(e)%text = txt
             if (truthy(ch)) then
                if (jis_str(ch)) then
                   evs(e)%channel = jstr(ch)
                else
                   evs(e)%channel = ''; evs(e)%channel_node = ch
                end if
             else
                evs(e)%channel = ''
             end if
             call add_event(rname, e)
          end if
       else if (jhas(c, 'toolUsageCard')) then
          card = jget(c, 'toolUsageCard')
          call first_key(card, kname, knull, body)
          call add_tool(rname, jget(card, 'toolUsageCardId'), kname, knull, card_args(body))
       else if (jhas(c, 'toolResult')) then
          res = jget(c, 'toolResult')
          call first_key(res, kname, knull, body)
          if (knull) then
             call add_result(rname, jget(res, 'toolCallId'), RES_NULL, '', 0)
          else if (kname == 'webSearch') then
             call add_result(rname, jget(res, 'toolCallId'), RES_WEB, 'web', jget(body, 'webpages'))
          else if (kname == 'xPost') then
             call add_result(rname, jget(res, 'toolCallId'), RES_X, 'x', jget(body, 'posts'))
          else
             call add_result(rname, jget(res, 'toolCallId'), RES_KIND, kname, 0)
          end if
       else if (jhas(c, 'renderCitation')) then
          cit = jget(c, 'renderCitation')
          if (is_main) then
             call grow_cits(cits, ncits)
             ncits = ncits + 1
             cits(ncits) = gcit(offset=pos)
             if (jis_null(jget(cit, 'citationId'))) then
                cits(ncits)%citation_zero = .true.
             else
                cits(ncits)%citation_node = jget(cit, 'citationId')
             end if
             cits(ncits)%card_node = jget(cit, 'id')
             cits(ncits)%kind_node = jget(cit, 'kind'); cits(ncits)%kind_null = .false.
             cits(ncits)%url_node = jget(cit, 'url')
          end if
       else if (jhas(c, 'uiLayout')) then
          continue
       else
          e = new_event(EV_UNKNOWN); evs(e)%raw = c; call add_event(rname, e)
       end if
       c = jnext(c)
    end do
    text = sb_str(parts)
  end subroutine assistant_from_chunks

  subroutine grow_cits(cits, n)
    type(gcit), allocatable, intent(inout) :: cits(:)
    integer, intent(in) :: n
    type(gcit), allocatable :: tmp(:)
    if (n >= size(cits)) then
       allocate(tmp(2*size(cits))); tmp(1:n) = cits(1:n); call move_alloc(tmp, cits)
    end if
  end subroutine grow_cits

  function join_text(v) result(r)
    integer, intent(in) :: v
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: c
    c = jfirst(list_or_empty(v))
    do while (c > 0)
       if (jis_str(c)) call sb_add(o, jstr(c))
       c = jnext(c)
    end do
    r = sb_str(o)
  end function join_text

  logical function tags_have(tags, s)
    integer, intent(in) :: tags
    character(len=*), intent(in) :: s
    integer :: c
    tags_have = .false.
    c = jfirst(tags)
    do while (c > 0)
       if (jequal_str(c, s)) then
          tags_have = .true.; return
       end if
       c = jnext(c)
    end do
  end function tags_have

  subroutine assistant_from_steps(r, text, cits, ncits)
    integer, intent(in) :: r
    character(len=:), allocatable, intent(out) :: text
    type(gcit), allocatable, intent(out) :: cits(:)
    integer, intent(out) :: ncits
    integer :: s, rname, tags, card, body, args0, tr, e, c, cm
    logical :: knull
    character(len=:), allocatable :: kname, xml, msg
    type(strbuf) :: ch
    ! card map for citations
    type(text_item_g), allocatable :: ckeys(:)
    integer, allocatable :: cval(:)
    integer :: nck, i, j
    allocate(cits(16)); ncits = 0
    s = jfirst(list_or_empty(jget(r, 'steps')))
    do while (s > 0)
       rname = jget(s, 'rolloutId')
       if (.not. truthy(rname)) rname = 0
       tags = list_or_empty(jget(s, 'tags'))
       if (tags_have(tags, 'summary')) then
          e = new_event(EV_SUMMARY); evs(e)%text = join_text(jget(s, 'text')); call add_event(rname, e)
       else if (tags_have(tags, 'tool_usage_card')) then
          xml = join_text(jget(s, 'text'))
          card = jfirst(list_or_empty(jget(s, 'toolUsageCards')))
          do while (card > 0)
             call first_key(card, kname, knull, body)
             args0 = card_args(body)
             if (args0 == 0) args0 = args_from_xml(xml)
             call add_tool(rname, jget(card, 'toolUsageCardId'), kname, knull, args0)
             card = jnext(card)
          end do
          tr = jfirst(list_or_empty(jget(s, 'toolUsageResults')))
          do while (tr > 0)
             if (jhas(tr, 'webSearchResults')) then
                call add_result(rname, jget(tr, 'toolUsageCardId'), RES_WEB, 'web', &
                                jget(hash_or_empty(jget(tr, 'webSearchResults')), 'results'))
             else if (jhas(tr, 'xSearchResults')) then
                call add_result(rname, jget(tr, 'toolUsageCardId'), RES_X, 'x', &
                                jget(hash_or_empty(jget(tr, 'xSearchResults')), 'results'))
             else
                call first_key(tr, kname, knull, body)
                if (knull) then
                   call add_result(rname, jget(tr, 'toolUsageCardId'), RES_KINDNULL, '', 0)
                else
                   call add_result(rname, jget(tr, 'toolUsageCardId'), RES_KIND, kname, 0)
                end if
             end if
             tr = jnext(tr)
          end do
       else if (tags_have(tags, 'raw_function_result')) then
          continue
       else
          call sb_clear(ch)
          c = jfirst(tags); i = 0
          do while (c > 0)
             if (i > 0) call sb_add(ch, ',')
             call sb_add(ch, py_str(c)); i = i + 1
             c = jnext(c)
          end do
          e = new_event(EV_TEXT); evs(e)%channel = sb_str(ch); evs(e)%text = join_text(jget(s, 'text'))
          call add_event(rname, e)
       end if
       s = jnext(s)
    end do
    ! citation cards
    allocate(ckeys(16), cval(16)); nck = 0
    c = jfirst(list_or_empty(jget(r, 'cardAttachmentsJson')))
    do while (c > 0)
       if (jis_str(c)) then
          cm = jparse(jstr(c))
       else
          cm = c
       end if
       if (jis_obj(cm)) then
          if (truthy(jget(cm, 'id'))) then
             j = 0
             do i = 1, nck
                if (ckeys(i)%s == to_lower_s(py_str(jget(cm, 'id')))) j = i
             end do
             if (j == 0) then
                if (nck >= size(ckeys)) call grow_map(ckeys, cval, nck)
                nck = nck + 1; j = nck
                ckeys(j)%s = to_lower_s(py_str(jget(cm, 'id')))
             end if
             cval(j) = cm
          end if
       end if
       c = jnext(c)
    end do
    ! reply text: strip citation markup, record offsets (in characters)
    if (truthy(jget(r, 'message'))) then
       if (jis_str(jget(r, 'message'))) then
          msg = jstr(jget(r, 'message'))
       else
          msg = py_str(jget(r, 'message'))
       end if
    else
       msg = ''
    end if
    call strip_citations(msg, text, cits, ncits, ckeys, cval, nck)
    call strip_images(text, cits, ncits, ckeys, cval, nck)
  contains
    subroutine grow_map(k, v, n)
      type(text_item_g), allocatable, intent(inout) :: k(:)
      integer, allocatable, intent(inout) :: v(:)
      integer, intent(in) :: n
      type(text_item_g), allocatable :: k2(:)
      integer, allocatable :: v2(:)
      allocate(k2(2*size(k)), v2(2*size(k))); k2(1:n) = k(1:n); v2(1:n) = v(1:n)
      call move_alloc(k2, k); call move_alloc(v2, v)
    end subroutine grow_map
  end subroutine assistant_from_steps

  function to_lower_s(s) result(r)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: r
    integer :: i, c
    r = s
    do i = 1, len(s)
       c = iachar(s(i:i))
       if (c >= 65 .and. c <= 90) r(i:i) = achar(c + 32)
    end do
  end function to_lower_s

  subroutine strip_citations(msg, text, cits, ncits, ckeys, cval, nck)
    character(len=*), intent(in) :: msg
    character(len=:), allocatable, intent(out) :: text
    type(gcit), allocatable, intent(inout) :: cits(:)
    integer, intent(inout) :: ncits
    type(text_item_g), intent(in) :: ckeys(:)
    integer, intent(in) :: cval(:), nck
    character(len=*), parameter :: p1 = '<grok:render card_id="'
    character(len=*), parameter :: p2 = '" card_type="citation_card" type="render_inline_citation"><argument name="citation_id">'
    character(len=*), parameter :: p3 = '</argument></grok:render>'
    type(strbuf) :: o
    integer :: last, i, a, h, dg, pos, k, card
    character(len=:), allocatable :: cid
    integer(int64) :: num
    last = 1; pos = 0; i = 1
    do
       a = index(msg(i:), p1)
       if (a == 0) exit
       a = i + a - 1
       h = a + len(p1)
       do while (h <= len(msg))
          if (index('0123456789abcdefABCDEF', msg(h:h)) == 0) exit
          h = h + 1
       end do
       if (h == a + len(p1) .or. h + len(p2) - 1 > len(msg)) then
          i = a + 1; cycle
       end if
       if (msg(h:h+len(p2)-1) /= p2) then
          i = a + 1; cycle
       end if
       dg = h + len(p2)
       k = dg
       do while (k <= len(msg))
          if (index('0123456789', msg(k:k)) == 0) exit
          k = k + 1
       end do
       if (k == dg .or. k + len(p3) - 1 > len(msg)) then
          i = a + 1; cycle
       end if
       if (msg(k:k+len(p3)-1) /= p3) then
          i = a + 1; cycle
       end if
       ! a match [a, k+len(p3)-1]
       if (a > last) then
          call sb_add(o, msg(last:a-1)); pos = pos + nchars(msg(last:a-1))
       end if
       cid = to_lower_s(msg(a+len(p1):h-1))
       read(msg(dg:k-1), *) num
       call grow_cits(cits, ncits)
       ncits = ncits + 1
       cits(ncits) = gcit(offset=pos)
       cits(ncits)%citation_node = -1; cits(ncits)%citation_num = num
       cits(ncits)%card_str = cid
       card = 0
       do k = 1, nck
          if (ckeys(k)%s == cid) card = cval(k)
       end do
       if (card > 0) then
          cits(ncits)%kind_null = .true.
          if (jis_int(jget(card, 'kind'))) then
             select case (jint(jget(card, 'kind')))
             case (0); cits(ncits)%kind_str = 'CITATION_KIND_UNSPECIFIED'; cits(ncits)%kind_null = .false.
             case (1); cits(ncits)%kind_str = 'CITATION_KIND_WEB_PAGE';    cits(ncits)%kind_null = .false.
             case (2); cits(ncits)%kind_str = 'CITATION_KIND_X_POST';      cits(ncits)%kind_null = .false.
             end select
          end if
          cits(ncits)%url_node = jget(card, 'url')
       else
          cits(ncits)%kind_null = .true.; cits(ncits)%url_node = 0
       end if
       last = index(msg(a:), p3) + a - 1 + len(p3)
       i = last
    end do
    if (last <= len(msg)) call sb_add(o, msg(last:))
    text = sb_str(o)
  end subroutine strip_citations

  ! Remove image-card markup from the (citation-stripped) reply text.  Each card becomes an imgs entry at its
  ! character offset; citation offsets at or after the end of a card move back by its length.
  subroutine strip_images(text, cits, ncits, ckeys, cval, nck)
    character(len=:), allocatable, intent(inout) :: text
    type(gcit), allocatable, intent(inout) :: cits(:)
    integer, intent(in) :: ncits
    type(text_item_g), intent(in) :: ckeys(:)
    integer, intent(in) :: cval(:), nck
    character(len=*), parameter :: p1 = '<grok:render card_id="'
    character(len=*), parameter :: p2 = '" card_type="image_card" type="render_searched_image">'
    character(len=*), parameter :: pa = '<argument name="', pb = '">', pc = '</argument>', p3 = '</grok:render>'
    type(strbuf) :: o
    integer, allocatable :: cut_end(:), cut_n(:)
    integer :: last, i, a, h, k, e, nm, ve, card, removed, ncut, n, j
    character(len=:), allocatable :: nm_s, val
    type(gimg), allocatable :: grow(:)
    if (index(text, p1) == 0) return
    allocate(cut_end(16), cut_n(16)); ncut = 0
    if (.not. allocated(imgs)) allocate(imgs(8))
    last = 1; i = 1; removed = 0
    do
       a = index(text(i:), p1)
       if (a == 0) exit
       a = i + a - 1
       h = a + len(p1)
       do while (h <= len(text))
          if (index('0123456789abcdefABCDEF', text(h:h)) == 0) exit
          h = h + 1
       end do
       if (h == a + len(p1) .or. h + len(p2) - 1 > len(text)) then
          i = a + 1; cycle
       end if
       if (text(h:h+len(p2)-1) /= p2) then
          i = a + 1; cycle
       end if
       ! zero or more <argument name="[a-z_]+">[^<]*</argument>, then </grok:render>
       k = h + len(p2)
       do
          if (k + len(pa) - 1 > len(text)) exit
          if (text(k:k+len(pa)-1) /= pa) exit
          nm = k + len(pa)
          do while (nm <= len(text))
             if (index('abcdefghijklmnopqrstuvwxyz_', text(nm:nm)) == 0) exit
             nm = nm + 1
          end do
          if (nm == k + len(pa) .or. nm + len(pb) - 1 > len(text)) exit
          if (text(nm:nm+len(pb)-1) /= pb) exit
          ve = nm + len(pb)
          do while (ve <= len(text))
             if (text(ve:ve) == '<') exit
             ve = ve + 1
          end do
          if (ve + len(pc) - 1 > len(text)) exit
          if (text(ve:ve+len(pc)-1) /= pc) exit
          k = ve + len(pc)
       end do
       if (k + len(p3) - 1 > len(text)) then
          i = a + 1; cycle
       end if
       if (text(k:k+len(p3)-1) /= p3) then
          i = a + 1; cycle
       end if
       e = k + len(p3) - 1                    ! the card is text(a:e)
       if (a > last) call sb_add(o, text(last:a-1))
       if (nimgs >= size(imgs)) then
          allocate(grow(2*size(imgs))); grow(1:nimgs) = imgs(1:nimgs); call move_alloc(grow, imgs)
       end if
       nimgs = nimgs + 1
       imgs(nimgs) = gimg()
       imgs(nimgs)%offset = nchars(text(1:a-1)) - removed
       imgs(nimgs)%cid = to_lower_s(text(a+len(p1):h-1))
       ! arguments (a repeated name: the last one wins)
       j = h + len(p2)
       do while (j < k)
          nm = j + len(pa)
          ve = index(text(nm:), pb) + nm - 1
          nm_s = text(nm:ve-1)
          val = text(ve+len(pb):index(text(ve:), pc) + ve - 2)
          if (nm_s == 'image_id') then
             imgs(nimgs)%image_id = val; imgs(nimgs)%has_id = .true.
          else if (nm_s == 'size') then
             imgs(nimgs)%size = val; imgs(nimgs)%has_size = .true.
          end if
          j = index(text(ve:), pc) + ve - 1 + len(pc)
       end do
       card = 0
       do n = 1, nck
          if (ckeys(n)%s == imgs(nimgs)%cid) card = cval(n)
       end do
       if (card > 0) then
          if (jis_obj(jget(card, 'image'))) imgs(nimgs)%img = jget(card, 'image')
       end if
       n = nchars(text(a:e))
       if (ncut >= size(cut_end)) then
          cut_end = [cut_end, cut_end]; cut_n = [cut_n, cut_n]
       end if
       ncut = ncut + 1; cut_end(ncut) = nchars(text(1:e)); cut_n(ncut) = n
       removed = removed + n
       last = e + 1; i = e + 1
    end do
    if (ncut == 0) return
    if (last <= len(text)) call sb_add(o, text(last:))
    text = sb_str(o)
    if (.not. allocated(cits)) return
    do j = 1, ncits
       n = 0
       do k = 1, ncut
          if (cut_end(k) <= cits(j)%offset) n = n + cut_n(k)
       end do
       cits(j)%offset = cits(j)%offset - n
    end do
  end subroutine strip_images

  subroutine write_images(w)
    type(jw), intent(inout) :: w
    integer :: i
    call jw_begin_arr(w)
    do i = 1, nimgs
       call jw_begin_obj(w)
       call jw_key(w, 'cardId'); call jw_str(w, imgs(i)%cid)
       call jw_key(w, 'imageId')
       if (imgs(i)%has_id) then
          call jw_str(w, imgs(i)%image_id)
       else
          call jw_null(w)
       end if
       call jw_key(w, 'link'); call img_field('link')
       call jw_key(w, 'offset'); call jw_int(w, int(imgs(i)%offset, int64))
       call jw_key(w, 'size')
       if (imgs(i)%has_size) then
          call jw_str(w, imgs(i)%size)
       else
          call jw_null(w)
       end if
       call jw_key(w, 'source'); call img_field('source')
       call jw_key(w, 'thumbnail'); call img_field('thumbnail')
       call jw_key(w, 'title'); call img_field('title')
       call jw_key(w, 'url'); call img_field('original')
       call jw_end_obj(w)
    end do
    call jw_end_arr(w)
  contains
    subroutine img_field(k)
      character(len=*), intent(in) :: k
      if (imgs(i)%img > 0) then
         call jcanon_value(w, jget(imgs(i)%img, k))
      else
         call jw_null(w)
      end if
    end subroutine img_field
  end subroutine write_images

  subroutine write_citations(w, cits, ncits)
    type(jw), intent(inout) :: w
    type(gcit), intent(in) :: cits(:)
    integer, intent(in) :: ncits
    integer :: i
    call jw_begin_arr(w)
    do i = 1, ncits
       call jw_begin_obj(w)
       call jw_key(w, 'cardId')
       if (allocated(cits(i)%card_str)) then
          call jw_str(w, cits(i)%card_str)
       else
          call jcanon_value(w, cits(i)%card_node)
       end if
       call jw_key(w, 'citationId')
       if (cits(i)%citation_zero) then
          call jw_int(w, 0_int64)
       else if (cits(i)%citation_node == -1) then
          call jw_int(w, cits(i)%citation_num)
       else
          call jcanon_value(w, cits(i)%citation_node)
       end if
       call jw_key(w, 'kind')
       if (allocated(cits(i)%kind_str) .and. .not. cits(i)%kind_null) then
          call jw_str(w, cits(i)%kind_str)
       else if (cits(i)%kind_node > 0 .and. .not. allocated(cits(i)%card_str)) then
          call jcanon_value(w, cits(i)%kind_node)
       else
          call jw_null(w)
       end if
       call jw_key(w, 'offset'); call jw_int(w, int(cits(i)%offset, int64))
       call jw_key(w, 'url'); call jcanon_value(w, cits(i)%url_node)
       call jw_end_obj(w)
    end do
    call jw_end_arr(w)
  end subroutine write_citations

  ! ---------------------------------------------------------------- turns

  subroutine write_attachments(w, r)
    type(jw), intent(inout) :: w
    integer, intent(in) :: r
    integer :: fid, m, a, c, key, pk, v
    call jw_begin_arr(w)
    fid = jfirst(list_or_empty(jget(r, 'fileAttachments')))
    do while (fid > 0)
       m = 0; a = 0
       c = jfirst(list_or_empty(jget(r, 'fileAttachmentsMetadata')))
       do while (c > 0)
          if (key_text(jget(c, 'fileMetadataId')) == key_text(fid)) m = c
          c = jnext(c)
       end do
       c = jfirst(list_or_empty(jget(r, 'fileAttachmentAssetMetadata')))
       do while (c > 0)
          if (key_text(jget(c, 'assetId')) == key_text(fid)) a = c
          c = jnext(c)
       end do
       if (truthy(jget(a, 'key'))) then
          key = jget(a, 'key')
       else if (truthy(jget(m, 'fileUri'))) then
          key = jget(m, 'fileUri')
       else
          key = 0
       end if
       pk = jget(a, 'previewImageKey')
       call jw_begin_obj(w)
       call jw_key(w, 'contentUrl')
       if (key > 0) then
          call jw_str(w, ASSET_BASE//py_str(key))
       else
          call jw_null(w)
       end if
       call jw_key(w, 'createTime'); call jcanon_value(w, py_or2(jget(m, 'createTime'), jget(a, 'createTime')))
       call jw_key(w, 'fileId'); call jcanon_value(w, fid)
       call jw_key(w, 'fileName')
       v = first_truthy3(jget(m, 'fileName'), jget(a, 'name'))
       if (v > 0) then
          call jcanon_value(w, v)
       else
          call jw_str(w, '')
       end if
       call jw_key(w, 'mimeType')
       v = first_truthy3(jget(m, 'fileMimeType'), jget(a, 'mimeType'))
       if (v > 0) then
          call jcanon_value(w, v)
       else
          call jw_str(w, '')
       end if
       call jw_key(w, 'previewUrl')
       if (truthy(pk)) then
          call jw_str(w, ASSET_BASE//py_str(pk))
       else
          call jw_null(w)
       end if
       call jw_key(w, 'sizeBytes'); call jcanon_value(w, jget(a, 'sizeBytes'))
       call jw_end_obj(w)
       fid = jnext(fid)
    end do
    call jw_end_arr(w)
  contains
    integer function first_truthy3(x, y)
      integer, intent(in) :: x, y
      first_truthy3 = 0
      if (truthy(x)) then
         first_truthy3 = x
      else if (truthy(y)) then
         first_truthy3 = y
      end if
    end function first_truthy3
  end subroutine write_attachments

  subroutine write_turn(w, r, idx)
    type(jw), intent(inout) :: w
    integer, intent(in) :: r, idx
    character(len=:), allocatable :: sender, text
    integer :: c, layout, ids, i, j, rows
    type(gcit), allocatable :: cits(:)
    integer :: ncits
    logical :: human
    type(strbuf) :: tb
    human = jequal_str(jget(r, 'sender'), 'human')
    call reset_turn()
    ncits = 0
    if (human) then
       if (list_or_empty(jget(r, 'inputChunks')) > 0) then
          c = jfirst(jget(r, 'inputChunks'))
          do while (c > 0)
             if (jhas(c, 'text')) call sb_add(tb, str_or_empty(jget(jget(c, 'text'), 'text')))
             c = jnext(c)
          end do
          text = sb_str(tb)
       else if (truthy(jget(r, 'message'))) then
          if (jis_str(jget(r, 'message'))) then
             text = jstr(jget(r, 'message'))
          else
             text = py_str(jget(r, 'message'))
          end if
       else
          text = ''
       end if
    else
       layout = hash_or_empty(jget(r, 'uiLayout'))
       c = jfirst(list_or_empty(jget(r, 'outputChunks')))
       do while (c > 0)
          if (jhas(c, 'uiLayout')) layout = jget(c, 'uiLayout')
          c = jnext(c)
       end do
       if (truthy(jget(layout, 'rolloutIds'))) then
          ids = list_or_empty(jget(layout, 'rolloutIds'))
       else
          ids = list_or_empty(jget(hash_or_empty(jget(hash_or_empty(jget(r, 'metadata')), 'ui_layout')), 'rollout_ids'))
       end if
       call make_rollouts(ids)
       if (truthy(jget(r, 'outputChunks'))) then
          call assistant_from_chunks(r, text, cits, ncits)
       else
          call assistant_from_steps(r, text, cits, ncits)
       end if
    end if

    call jw_begin_obj(w)
    call jw_key(w, 'attachments'); call write_attachments(w, r)
    call jw_key(w, 'citations')
    if (human) then
       call jw_begin_arr(w); call jw_end_arr(w)
    else
       call write_citations(w, cits, ncits)
    end if
    call jw_key(w, 'createTime'); call jcanon_value(w, jget(r, 'createTime'))
    if (truthy(jget(r, 'generatedImageUrls'))) then
       call jw_key(w, 'generatedImageUrls'); call jcanon_value(w, jget(r, 'generatedImageUrls'))
    end if
    if (truthy(jget(r, 'imageAttachments'))) then
       call jw_key(w, 'imageAttachments'); call jcanon_value(w, jget(r, 'imageAttachments'))
    end if
    if (truthy(jget(r, 'imageEditUris'))) then
       call jw_key(w, 'imageEditUris'); call jcanon_value(w, jget(r, 'imageEditUris'))
    end if
    if (.not. human .and. nimgs > 0) then
       call jw_key(w, 'images'); call write_images(w)
    end if
    call jw_key(w, 'index'); call jw_int(w, int(idx, int64))
    call jw_key(w, 'model')
    if (truthy(jget(r, 'model'))) then
       call jcanon_value(w, jget(r, 'model'))
    else
       call jw_null(w)
    end if
    call jw_key(w, 'parentResponseId'); call jcanon_value(w, jget(r, 'parentResponseId'))
    call jw_key(w, 'responseId'); call jcanon_value(w, jget(r, 'responseId'))
    call jw_key(w, 'sender'); call jcanon_value(w, jget(r, 'sender'))
    call jw_key(w, 'sources')
    if (human) then
       call jw_null(w)
    else
       rows = 0
       do i = 1, nrls
          do j = 1, rls(i)%nev
             rows = rows + result_rows(rls(i)%ev(j))
          end do
          if (.not. main_null .and. rls(i)%name == main_name) then
             do j = 1, nunattr
                rows = rows + result_rows(unattr(j))
             end do
          else if (main_null .and. rls(i)%name == '') then
             do j = 1, nunattr
                rows = rows + result_rows(unattr(j))
             end do
          end if
       end do
       call jw_begin_obj(w)
       call jw_key(w, 'toolResultRows'); call jw_int(w, int(rows, int64))
       call jw_key(w, 'webSearchResults'); call write_norm_web(w, jget(r, 'webSearchResults'))
       call jw_key(w, 'xposts'); call write_norm_x(w, jget(r, 'xposts'))
       call jw_end_obj(w)
    end if
    call jw_key(w, 'text'); call jw_str(w, text)
    call jw_key(w, 'thinking')
    if (human) then
       call jw_null(w)
    else
       call jw_begin_obj(w)
       call jw_key(w, 'durationMs'); call put_duration(w, jget(r, 'thinkingStartTime'), jget(r, 'thinkingEndTime'))
       call jw_key(w, 'endTime'); call jcanon_value(w, jget(r, 'thinkingEndTime'))
       call jw_key(w, 'mainRollout')
       if (main_null) then
          call jw_null(w)
       else
          call jcanon_value(w, jfirst(ids))
       end if
       call jw_key(w, 'rollouts')
       call jw_begin_arr(w)
       do i = 1, nrls
          call jw_begin_obj(w)
          call jw_key(w, 'events')
          call jw_begin_arr(w)
          do j = 1, rls(i)%nev
             call write_event(w, rls(i)%ev(j))
          end do
          if (rls(i)%name == main_key()) then
             do j = 1, nunattr
                call write_event(w, unattr(j))
             end do
          end if
          call jw_end_arr(w)
          call jw_key(w, 'id'); call jw_str(w, rls(i)%name)
          call jw_key(w, 'role')
          select case (rls(i)%role)
          case (1); call jw_str(w, 'Leader')
          case (2); call jw_str(w, 'Agent')
          case default; call jw_null(w)
          end select
          call jw_end_obj(w)
       end do
       call jw_end_arr(w)
       call jw_key(w, 'startTime'); call jcanon_value(w, jget(r, 'thinkingStartTime'))
       call jw_end_obj(w)
    end if
    call jw_end_obj(w)
  contains
    function main_key() result(mk)
      character(len=:), allocatable :: mk
      if (main_null) then
         mk = ''
      else
         mk = main_name
      end if
    end function main_key
  end subroutine write_turn

  ! parent-chain ordering: DFS from roots (children in array order), then orphans
  subroutine order_responses(responses, order, n)
    integer, intent(in) :: responses
    integer, allocatable, intent(out) :: order(:)
    integer, intent(out) :: n
    integer, allocatable :: rs(:), stack(:)
    type(text_item_g), allocatable :: rid(:), pid(:)
    logical, allocatable :: seen(:)
    integer :: m, c, i, j, sp, top, k
    logical :: parent_known
    m = jlen(responses)
    allocate(rs(max(m,1)), rid(max(m,1)), pid(max(m,1)), seen(max(m,1)), order(max(m,1)), stack(max(4*m,4)))
    c = jfirst(responses); i = 0
    do while (c > 0)
       i = i + 1; rs(i) = c
       rid(i)%s = key_text(jget(c, 'responseId')); pid(i)%s = key_text(jget(c, 'parentResponseId'))
       c = jnext(c)
    end do
    seen = .false.; n = 0; sp = 0
    ! roots in array order, pushed so the first root is on top
    do i = m, 1, -1
       parent_known = .false.
       do j = 1, m
          if (rid(j)%s == pid(i)%s) then
             parent_known = .true.; exit
          end if
       end do
       if (.not. parent_known) then
          sp = sp + 1; stack(sp) = i
       end if
    end do
    do while (sp > 0)
       top = stack(sp); sp = sp - 1
       ! by-id maps to the last response with this id
       k = top
       do j = 1, m
          if (rid(j)%s == rid(top)%s) k = j
       end do
       if (id_seen(rid(top)%s)) cycle
       call mark(rid(top)%s)
       n = n + 1; order(n) = rs(k)
       do j = m, 1, -1
          if (pid(j)%s == rid(top)%s) then
             if (sp >= size(stack)) call grow_stack()
             sp = sp + 1; stack(sp) = j
          end if
       end do
    end do
    do i = 1, m
       if (.not. id_seen(rid(i)%s)) then
          call mark(rid(i)%s)
          n = n + 1; order(n) = rs(i)
       end if
    end do
  contains
    logical function id_seen(s)
      character(len=*), intent(in) :: s
      integer :: q
      id_seen = .false.
      do q = 1, m
         if (seen(q) .and. rid(q)%s == s) then
            id_seen = .true.; return
         end if
      end do
    end function id_seen
    subroutine mark(s)
      character(len=*), intent(in) :: s
      integer :: q
      do q = 1, m
         if (rid(q)%s == s) seen(q) = .true.
      end do
    end subroutine mark
    subroutine grow_stack()
      integer, allocatable :: t2(:)
      allocate(t2(2*size(stack))); t2(1:sp) = stack(1:sp); call move_alloc(t2, stack)
    end subroutine grow_stack
  end subroutine order_responses

  ! off(i): order(i) is not on the current branch.  Where a response has several children (an edited message, a
  ! regenerated reply), the last child in array order is the version the page shows; every earlier child and all
  ! of its descendants are off the branch.  Children of an unknown parent (roots) are never split.
  subroutine off_branch_flags(responses, order, n, off)
    integer, intent(in) :: responses, n
    integer, intent(in) :: order(:)
    logical, allocatable, intent(out) :: off(:)
    type(text_item_g), allocatable :: rid(:), pid(:)
    logical, allocatable :: offr(:)
    integer, allocatable :: stack(:)
    integer :: m, c, i, j, last, sp, top
    m = jlen(responses)
    allocate(off(max(n,1)), rid(max(m,1)), pid(max(m,1)), offr(max(m,1)), stack(max(m*m+m,4)))
    off = .false.; offr = .false.
    c = jfirst(responses); i = 0
    do while (c > 0)
       i = i + 1
       rid(i)%s = key_text(jget(c, 'responseId')); pid(i)%s = key_text(jget(c, 'parentResponseId'))
       c = jnext(c)
    end do
    sp = 0
    do i = 1, m
       ! children of response i, only once per distinct id (the by-id entry is the last with this id)
       if (any([(rid(j)%s == rid(i)%s, j = i + 1, m)])) cycle
       last = 0
       do j = 1, m
          if (pid(j)%s == rid(i)%s) last = j
       end do
       if (last == 0) cycle
       do j = 1, last - 1
          if (pid(j)%s == rid(i)%s .and. rid(j)%s /= rid(last)%s) then
             sp = sp + 1; stack(sp) = j
          end if
       end do
    end do
    do while (sp > 0)
       top = stack(sp); sp = sp - 1
       if (any(offr(1:m) .and. [(rid(j)%s == rid(top)%s, j = 1, m)])) cycle
       do j = 1, m
          if (rid(j)%s == rid(top)%s) offr(j) = .true.
       end do
       do j = m, 1, -1
          if (pid(j)%s == rid(top)%s) then
             sp = sp + 1; stack(sp) = j
          end if
       end do
    end do
    do i = 1, n
       c = order(i)
       do j = 1, m
          if (offr(j) .and. rid(j)%s == key_text(jget(c, 'responseId'))) off(i) = .true.
       end do
    end do
  end subroutine off_branch_flags

  ! "chunk" if any response has non-empty outputChunks
  function grok_format(d) result(fmt)
    integer, intent(in) :: d
    character(len=:), allocatable :: fmt
    integer :: r
    fmt = 'legacy'
    r = jfirst(list_or_empty(jget(d, 'responses')))
    do while (r > 0)
       if (truthy(jget(r, 'outputChunks'))) then
          fmt = 'chunk'; return
       end if
       r = jnext(r)
    end do
  end function grok_format

  ! d: payload with "responses"; conv: conversation object (0 -> use synth_conv_id); source_url '' -> null
  subroutine grok_transcript(d, conv, synth_conv_id, source_url, out_text)
    integer, intent(in) :: d, conv
    character(len=*), intent(in) :: synth_conv_id, source_url
    character(len=:), allocatable, intent(out) :: out_text
    type(jw) :: w
    integer, allocatable :: order(:)
    logical, allocatable :: off(:)
    integer :: n, i, k, cv
    cv = hash_or_empty(conv)
    call jw_begin_obj(w)
    call jw_key(w, 'conversation')
    call jw_begin_obj(w)
    call jw_key(w, 'conversationId')
    if (conv == 0 .and. len(synth_conv_id) > 0) then
       call jw_str(w, synth_conv_id)
    else
       call jcanon_value(w, jget(cv, 'conversationId'))
    end if
    call jw_key(w, 'createTime'); call jcanon_value(w, jget(cv, 'createTime'))
    call jw_key(w, 'isPublic')
    if (jhas(d, 'isPublic')) then
       call jcanon_value(w, jget(d, 'isPublic'))
    else
       call jw_null(w)
    end if
    call jw_key(w, 'modifyTime'); call jcanon_value(w, jget(cv, 'modifyTime'))
    call jw_key(w, 'sourceUrl')
    if (len(source_url) > 0) then
       call jw_str(w, source_url)
    else
       call jw_null(w)
    end if
    call jw_key(w, 'title'); call jcanon_value(w, jget(cv, 'title'))
    call jw_end_obj(w)
    call order_responses(list_or_empty(jget(d, 'responses')), order, n)
    call off_branch_flags(list_or_empty(jget(d, 'responses')), order, n, off)
    ! offBranchTurns (sorted before "turns"): edited messages and regenerated replies, only when there are any
    if (count(off(1:n)) > 0) then
       call jw_key(w, 'offBranchTurns')
       call jw_begin_arr(w)
       k = 0
       do i = 1, n
          if (.not. off(i)) cycle
          call write_turn(w, order(i), k); k = k + 1
       end do
       call jw_end_arr(w)
    end if
    call jw_key(w, 'turns')
    call jw_begin_arr(w)
    k = 0
    do i = 1, n
       if (off(i)) cycle
       call write_turn(w, order(i), k); k = k + 1
    end do
    call jw_end_arr(w)
    call jw_end_obj(w)
    out_text = jw_result(w)
  end subroutine grok_transcript

  ! ------------------------------------------------------------------ WebSocket tool results
  ! grok.com's REST responses carry no output for agent tools such as bash (results null).  The page receives it over
  ! wss://grok.com/ws/mgw as conversation.history.item events: item.x_grok.output_chunks[].tool_result
  ! {tool_call_id, code_execution {stdout, exit_code}}.  frames: JSON array of the frames the page got.
  ! Fills the results of tool events that have none with {exitCode, items [], kind "code", stdout}; same rules as
  ! racket/transcript.rkt merge-ws-tool-results! (turns[].thinking.rollouts[].events[] only; last frame wins).
  subroutine grok_merge_ws_results(text, frames, out_text, nmerged)
    character(len=*), intent(in) :: text
    integer, intent(in) :: frames
    character(len=:), allocatable, intent(out) :: out_text
    integer, intent(out) :: nmerged
    type(jw) :: w
    integer :: f, ev, xg, ch, tr, t, i
    nmerged = 0
    nws = 0
    if (allocated(ws_ids)) deallocate(ws_ids)
    if (allocated(ws_ce)) deallocate(ws_ce)
    allocate(ws_ids(256), ws_ce(256))
    f = jfirst(frames)
    do while (f > 0)
       ev = jget(f, 'event')
       if (jis_obj(f) .and. jis_obj(ev)) then
          if (jequal_str(jget(ev, 'type'), 'conversation.history.item')) then
             xg = jget(jget(ev, 'item'), 'x_grok')
             if (jis_obj(jget(ev, 'item')) .and. jis_obj(xg)) then
                if (jis_arr(jget(xg, 'output_chunks'))) then
                   ch = jfirst(jget(xg, 'output_chunks'))
                   do while (ch > 0)
                      tr = 0
                      if (jis_obj(ch)) tr = jget(ch, 'tool_result')
                      if (jis_obj(tr)) then
                         if (jis_str(jget(tr, 'tool_call_id')) .and. jis_obj(jget(tr, 'code_execution'))) &
                            call ws_put(jstr(jget(tr, 'tool_call_id')), jget(tr, 'code_execution'))
                      end if
                      ch = jnext(ch)
                   end do
                end if
             end if
          end if
       end if
       f = jnext(f)
    end do
    t = jparse(text)
    call ws_write(w, t, 0, nmerged)
    out_text = jw_result(w)
  contains
    subroutine ws_put(id, ce)
      character(len=*), intent(in) :: id
      integer, intent(in) :: ce
      type(ws_key), allocatable :: tmp(:)
      integer, allocatable :: tmpc(:)
      do i = 1, nws
         if (ws_ids(i)%s == id) then
            ws_ce(i) = ce; return
         end if
      end do
      if (nws >= size(ws_ids)) then
         allocate(tmp(2*nws), tmpc(2*nws)); tmp(1:nws) = ws_ids(1:nws); tmpc(1:nws) = ws_ce(1:nws)
         call move_alloc(tmp, ws_ids); call move_alloc(tmpc, ws_ce)
      end if
      nws = nws + 1; ws_ids(nws)%s = id; ws_ce(nws) = ce
    end subroutine ws_put
  end subroutine grok_merge_ws_results

  integer function ws_find(p)
    integer, intent(in) :: p
    integer :: i
    ws_find = 0
    if (.not. jis_str(p)) return
    do i = 1, nws
       if (ws_ids(i)%s == jstr(p)) then
          ws_find = ws_ce(i); return
       end if
    end do
  end function ws_find

  ! level 0 transcript, 1 turn, 2 thinking, 3 rollout, 4 event
  recursive subroutine ws_write(w, p, level, nmerged)
    type(jw), intent(inout) :: w
    integer, intent(in) :: p, level
    integer, intent(inout) :: nmerged
    integer, allocatable :: kids(:)
    integer :: n, i, j, t, c, ce
    character(len=:), allocatable :: k
    logical :: replace, wrote
    if (.not. jis_obj(p)) then
       call jcanon_value(w, p); return
    end if
    n = jlen(p); allocate(kids(max(n,1)))
    c = jfirst(p); i = 0
    do while (c > 0)
       i = i + 1; kids(i) = c; c = jnext(c)
    end do
    do i = 2, n
       t = kids(i); j = i - 1
       do while (j >= 1)
          if (.not. lgt(jname(kids(j)), jname(t))) exit
          kids(j+1) = kids(j); j = j - 1
       end do
       kids(j+1) = t
    end do
    ce = 0
    if (level == 4) then
       if (jequal_str(jget(p, 'type'), 'tool') .and. (jget(p, 'results') == 0 .or. jis_null(jget(p, 'results')))) &
          ce = ws_find(jget(p, 'toolCallId'))
    end if
    replace = (ce > 0)
    if (replace) nmerged = nmerged + 1
    call jw_begin_obj(w)
    wrote = .false.
    do i = 1, n
       k = jname(kids(i))
       if (replace .and. .not. wrote .and. lgt(k, 'results')) then
          call put_code(); wrote = .true.
       end if
       call jw_key(w, k)
       if (replace .and. k == 'results') then
          call put_code_value(); wrote = .true.
       else if (level == 0 .and. (k == 'turns' .or. k == 'offBranchTurns') .and. jis_arr(kids(i))) then
          call each(kids(i), 1)
       else if (level == 1 .and. k == 'thinking' .and. jis_obj(kids(i))) then
          call ws_write(w, kids(i), 2, nmerged)
       else if (level == 2 .and. k == 'rollouts' .and. jis_arr(kids(i))) then
          call each(kids(i), 3)
       else if (level == 3 .and. k == 'events' .and. jis_arr(kids(i))) then
          call each(kids(i), 4)
       else
          call jcanon_value(w, kids(i))
       end if
    end do
    if (replace .and. .not. wrote) call put_code()
    call jw_end_obj(w)
  contains
    recursive subroutine each(arr, lv)
      integer, intent(in) :: arr, lv
      integer :: e
      call jw_begin_arr(w)
      e = jfirst(arr)
      do while (e > 0)
         call ws_write(w, e, lv, nmerged)
         e = jnext(e)
      end do
      call jw_end_arr(w)
    end subroutine each
    subroutine put_code()
      call jw_key(w, 'results')
      call put_code_value()
    end subroutine put_code
    subroutine put_code_value()
      call jw_begin_obj(w)
      call jw_key(w, 'exitCode'); call opt(jget(ce, 'exit_code'))
      call jw_key(w, 'items'); call jw_begin_arr(w); call jw_end_arr(w)
      call jw_key(w, 'kind'); call jw_str(w, 'code')
      call jw_key(w, 'stdout'); call opt(jget(ce, 'stdout'))
      call jw_end_obj(w)
    end subroutine put_code_value
    subroutine opt(v)
      integer, intent(in) :: v
      if (v == 0) then
        call jw_null(w)
      else
        call jcanon_value(w, v)
      end if
    end subroutine opt
  end subroutine ws_write

end module fx_grok
