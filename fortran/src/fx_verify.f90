! fx_verify.f90 — SPEC section 5 verifier for Grok (port of racket/verify.rkt, itself a port of
! shared/reference_verify.py) + the legacy citation enrichment from DOM chips.
! Difference from the Racket verifier: no Unicode NFC normalization (grok.com text observed NFC).
module fx_verify
  use iso_fortran_env, only: int64
  use fx_util
  use fx_json
  use fx_net, only: sha256_hex
  use fx_grok, only: truthy, py_str
  implicit none
  private
  public :: verify_capture, enrich_citations, word, wordlist, safe_file_name, stability_record, api_consistency_record, strip_env_text

  type :: word
     character(len=:), allocatable :: s
  end type word

  type :: wordlist
     type(word), allocatable :: w(:)
     integer :: n = 0
  end type wordlist

  ! report
  type(strbuf), allocatable :: recs(:)
  character(len=:), allocatable :: rec_name(:)
  type(word), allocatable :: rnames(:)
  logical, allocatable :: rok(:)
  integer, allocatable :: rturn(:)          ! -1 = null
  integer :: nrec
  type(word), allocatable :: warns(:)
  integer :: nwarn

  character(len=*), parameter :: PUA_E000 = achar(238)//achar(128)//achar(128)   ! U+E000, a private-use placeholder
  character(len=*), parameter :: PAGE_NOTE = 'observed grok.com behaviour (share page, 2026-09-02): chatroom messages '// &
      'whose outputChunk is streamed AFTER the last CHANNEL_ASSISTANT_RESPONSE chunk of the turn are not rendered in the '// &
      'Thoughts panel (test conversation turns 7 and 15); the API export keeps them, the page omits them'

contains

  ! ---------------------------------------------------------------- text

  ! decode one UTF-8 code point at s(i:), returns length
  integer function cp_at(s, i, cp)
    character(len=*), intent(in) :: s
    integer, intent(in) :: i
    integer, intent(out) :: cp
    integer :: c0
    c0 = ubyte(s(i:i))
    if (c0 < 128) then
       cp = c0; cp_at = 1
    else if (c0 >= 240 .and. i + 3 <= len(s)) then
       cp = iand(c0, 7)*262144 + iand(ubyte(s(i+1:i+1)), 63)*4096 + iand(ubyte(s(i+2:i+2)), 63)*64 + iand(ubyte(s(i+3:i+3)), 63)
       cp_at = 4
    else if (c0 >= 224 .and. i + 2 <= len(s)) then
       cp = iand(c0, 15)*4096 + iand(ubyte(s(i+1:i+1)), 63)*64 + iand(ubyte(s(i+2:i+2)), 63); cp_at = 3
    else if (c0 >= 192 .and. i + 1 <= len(s)) then
       cp = iand(c0, 31)*64 + iand(ubyte(s(i+1:i+1)), 63); cp_at = 2
    else
       cp = c0; cp_at = 1
    end if
  end function cp_at

  integer function ubyte(ch)
    character, intent(in) :: ch
    ubyte = iachar(ch)
    if (ubyte < 0) ubyte = ubyte + 256
  end function ubyte

  ! Python str.isspace
  logical function py_space(cp)
    integer, intent(in) :: cp
    select case (cp)
    case (9:13, 28:32, 133, 160, 5760, 8192:8202, 8232, 8233, 8239, 8287, 12288)
       py_space = .true.
    case default
       py_space = .false.
    end select
  end function py_space

  function as_text(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    if (jis_null(p)) then
       r = ''
    else if (jis_str(p)) then
       r = jstr(p)
    else
       r = py_str(p)
    end if
  end function as_text

  ! remove \r, U+200B, U+2060, U+FEFF
  function drop_cr_zw(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, k, cp
    i = 1
    do while (i <= len(s))
       k = cp_at(s, i, cp)
       if (.not. (cp == 13 .or. cp == 8203 .or. cp == 8288 .or. cp == 65279)) call sb_add(o, s(i:i+k-1))
       i = i + k
    end do
    r = sb_str(o)
  end function drop_cr_zw

  subroutine py_split(s, wl)
    character(len=*), intent(in) :: s
    type(wordlist), intent(out) :: wl
    integer :: i, k, cp, start
    allocate(wl%w(max(16, len(s)/4 + 1))); wl%n = 0
    start = 0; i = 1
    do while (i <= len(s))
       k = cp_at(s, i, cp)
       if (py_space(cp)) then
          if (start > 0) call push(s(start:i-1))
          start = 0
       else if (start == 0) then
          start = i
       end if
       i = i + k
    end do
    if (start > 0) call push(s(start:))
  contains
    subroutine push(t)
      character(len=*), intent(in) :: t
      type(word), allocatable :: tmp(:)
      if (wl%n >= size(wl%w)) then
         allocate(tmp(2*size(wl%w))); tmp(1:wl%n) = wl%w(1:wl%n); call move_alloc(tmp, wl%w)
      end if
      wl%n = wl%n + 1; wl%w(wl%n)%s = t
    end subroutine push
  end subroutine py_split

  function join_words(wl, a, b) result(r)
    type(wordlist), intent(in) :: wl
    integer, intent(in) :: a, b          ! 1-based inclusive range
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    do i = max(1, a), min(wl%n, b)
       if (i > max(1, a)) call sb_add(o, ' ')
       call sb_add(o, wl%w(i)%s)
    end do
    r = sb_str(o)
  end function join_words

  function normv(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    r = norm_s(as_text(p))
  end function normv

  function norm_s(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(wordlist) :: wl
    call py_split(drop_cr_zw(s), wl)
    r = join_words(wl, 1, wl%n)
  end function norm_s

  ! markdown stripped words
  subroutine md_words(p, wl)
    integer, intent(in) :: p
    type(wordlist), intent(out) :: wl
    call md_words_s(as_text(p), wl)
  end subroutine md_words

  subroutine md_words_s(s0, wl)
    character(len=*), intent(in) :: s0
    type(wordlist), intent(out) :: wl
    character(len=:), allocatable :: s
    s = drop_cr_zw(s0)
    s = drop_math_delims(s)
    s = md_link(s)
    s = replace_all(s, '|', ' ')        ! markdown table pipes: the page renders a table, not the pipes
    s = md_lines(s)
    s = protect_code_spans(s)
    s = md_esc(s)
    s = md_chars(s)
    s = replace_all(s, PUA_E000, '\')
    call py_split(s, wl)
  end subroutine md_words_s

  ! inline code: backslashes inside are literal, not markdown escapes (the backticks go, the backslashes stay)
  function protect_code_spans(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, j
    i = 1
    do while (i <= len(s))
       if (s(i:i) == '`') then
          j = i + 1
          do while (j <= len(s))
             if (s(j:j) == '`' .or. s(j:j) == achar(10)) exit
             j = j + 1
          end do
          if (j <= len(s)) then
             if (s(j:j) == '`') then
                call sb_add(o, replace_all(s(i+1:j-1), '\', PUA_E000))
                i = j + 1
                cycle
             end if
          end if
       end if
       call sb_add(o, s(i:i)); i = i + 1
    end do
    r = sb_str(o)
  end function protect_code_spans

  ! \( \) \[ \] $$ are math delimiters: the page shows the TeX between them (extract.js reads formulas as TeX)
  function drop_math_delims(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    i = 1
    do while (i <= len(s))
       if (i < len(s)) then
          if (s(i:i) == '\' .and. index('()[]', s(i+1:i+1)) > 0) then
             i = i + 2; cycle
          end if
          if (s(i:i+1) == '$$') then
             i = i + 2; cycle
          end if
       end if
       call sb_add(o, s(i:i)); i = i + 1
    end do
    r = sb_str(o)
  end function drop_math_delims

  ! !?\[([^\]]*)\]\([^)]*\)  -> \1
  function md_link(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, j, k, l
    i = 1
    do while (i <= len(s))
       j = 0
       if (s(i:i) == '[') then
          j = i
       else if (s(i:i) == '!' .and. i < len(s)) then
          if (s(i+1:i+1) == '[') j = i + 1
       end if
       if (j > 0) then
          k = index(s(j+1:), ']')
          if (k > 0) then
             k = j + k
             if (k < len(s)) then
                if (s(k+1:k+1) == '(') then
                   l = index(s(k+2:), ')')
                   if (l > 0) then
                      call sb_add(o, s(j+1:k-1))
                      i = k + 1 + l + 1
                      cycle
                   end if
                end if
             end if
          end if
       end if
       call sb_add(o, s(i:i)); i = i + 1
    end do
    r = sb_str(o)
  end function md_link

  ! line-anchored rules: horizontal rule, list marker, heading hashes, quote marker
  function md_lines(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: a, z
    character(len=:), allocatable :: ln
    a = 1
    do
       z = index(s(a:), achar(10))
       if (z == 0) then
          ln = s(a:)
       else
          ln = s(a:a+z-2)
       end if
       if (is_rule(ln)) ln = ''
       ln = strip_prefix_mark(ln)
       ln = strip_prefix_char(ln, '#', .true.)
       ln = strip_prefix_char(ln, '>', .false.)
       call sb_add(o, ln)
       if (z == 0) exit
       call sb_add(o, achar(10))
       a = a + z
       if (a > len(s)) exit
    end do
    if (len(s) > 0) then
       if (s(len(s):len(s)) == achar(10) .and. o%n > 0) then
          ! the loop above already emitted the final newline; nothing follows it
       end if
    end if
    r = sb_str(o)
  contains
    logical function is_rule(t)
      character(len=*), intent(in) :: t
      integer :: q, cnt
      is_rule = .false.; cnt = 0
      do q = 1, len(t)
         select case (t(q:q))
         case (' ', achar(9))
         case ('-', '*', '_', '=')
            cnt = cnt + 1
         case default
            return
         end select
      end do
      ! the first marker must come after only spaces/tabs, which holds for any such line
      is_rule = (cnt >= 3)
    end function is_rule
    function strip_prefix_mark(t) result(u)
      character(len=*), intent(in) :: t
      character(len=:), allocatable :: u
      integer :: q, q2
      u = t
      q = 1
      do while (q <= len(t))
         if (t(q:q) /= ' ' .and. t(q:q) /= achar(9)) exit
         q = q + 1
      end do
      if (q > len(t)) return
      if (t(q:q) == '-' .or. t(q:q) == '*' .or. t(q:q) == '+') then
         q2 = q + 1
      else if (index('0123456789', t(q:q)) > 0) then
         q2 = q
         do while (q2 <= len(t))
            if (index('0123456789', t(q2:q2)) == 0) exit
            q2 = q2 + 1
         end do
         if (q2 > len(t)) return
         if (t(q2:q2) /= '.' .and. t(q2:q2) /= ')') return
         q2 = q2 + 1
      else
         return
      end if
      if (q2 > len(t)) return
      if (t(q2:q2) /= ' ' .and. t(q2:q2) /= achar(9)) return
      do while (q2 <= len(t))
         if (t(q2:q2) /= ' ' .and. t(q2:q2) /= achar(9)) exit
         q2 = q2 + 1
      end do
      u = t(q2:)
    end function strip_prefix_mark
    function strip_prefix_char(t, ch, many) result(u)
      character(len=*), intent(in) :: t
      character, intent(in) :: ch
      logical, intent(in) :: many
      character(len=:), allocatable :: u
      integer :: q
      u = t
      q = 1
      do while (q <= len(t))
         if (t(q:q) /= ' ' .and. t(q:q) /= achar(9)) exit
         q = q + 1
      end do
      if (q > len(t)) return
      if (t(q:q) /= ch) return
      q = q + 1
      if (many) then
         do while (q <= len(t))
            if (t(q:q) /= ch) exit
            q = q + 1
         end do
      end if
      do while (q <= len(t))
         if (t(q:q) /= ' ' .and. t(q:q) /= achar(9)) exit
         q = q + 1
      end do
      u = t(q:)
    end function strip_prefix_char
  end function md_lines

  function md_esc(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    i = 1
    do while (i <= len(s))
       if (s(i:i) == '\' .and. i < len(s)) then
          if (index('\`*_{}[]()#+-.!>|~', s(i+1:i+1)) > 0) then
             call sb_add(o, s(i+1:i+1)); i = i + 2; cycle
          end if
       end if
       call sb_add(o, s(i:i)); i = i + 1
    end do
    r = sb_str(o)
  end function md_esc

  function md_chars(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    do i = 1, len(s)
       if (index('*_#>`', s(i:i)) == 0) call sb_add(o, s(i:i))
    end do
    r = sb_str(o)
  end function md_chars

  integer function nchars(s)
    character(len=*), intent(in) :: s
    integer :: i, c
    nchars = 0
    do i = 1, len(s)
       c = ubyte(s(i:i))
       if (c < 128 .or. c >= 192) nchars = nchars + 1
    end do
  end function nchars

  function head_chars(s, n) result(r)
    character(len=*), intent(in) :: s
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    integer :: i, k, c
    i = 1; k = 0
    do while (i <= len(s))
       c = ubyte(s(i:i))
       if (c < 128 .or. c >= 192) then
          if (k == n) exit
          k = k + 1
       end if
       i = i + 1
    end do
    r = s(1:i-1)
  end function head_chars

  function lower_s(s) result(r)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: r
    integer :: i, c
    r = s
    do i = 1, len(s)
       c = iachar(s(i:i))
       if (c >= 65 .and. c <= 90) r(i:i) = achar(c + 32)
    end do
  end function lower_s

  function urlkey(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    integer :: i
    r = lower_s(normv(p))
    i = 1
    do while (i <= len(r))
       if (r(i:i) < 'a' .or. r(i:i) > 'z') exit
       i = i + 1
    end do
    if (i > 1 .and. i + 2 <= len(r)) then
       if (r(i:i+2) == '://') r = r(i+3:)
    end if
    if (starts_with(r, 'www.')) r = r(5:)
    do while (len(r) > 0)
       if (r(len(r):len(r)) /= '/') exit
       r = r(1:len(r)-1)
    end do
  end function urlkey

  ! 'Thought for 1m 21s' -> seconds; -1 when unparsable
  integer(int64) function duration_seconds(p)
    integer, intent(in) :: p
    character(len=:), allocatable :: s
    integer :: st, i
    integer(int64) :: h, m, sec
    logical :: gh, gm, gs
    duration_seconds = -1
    if (.not. (jis_str(p) .and. truthy(p))) return
    s = strip_ends(jstr(p))
    do st = 1, len(s) + 1
       if (try_at(st)) then
          if (gh .or. gm .or. gs) duration_seconds = 3600*h + 60*m + sec
          return
       end if
    end do
  contains
    logical function try_at(i0)
      integer, intent(in) :: i0
      integer :: q
      h = 0; m = 0; sec = 0; gh = .false.; gm = .false.; gs = .false.
      q = i0
      call grp(q, 'h', gh, h)
      call spaces(q)
      call grp(q, 'm', gm, m)
      call spaces(q)
      call grp(q, 's', gs, sec)
      call spaces(q)
      try_at = (q == len(s) + 1)
    end function try_at
    subroutine grp(q, letter, got, v)
      integer, intent(inout) :: q
      character, intent(in) :: letter
      logical, intent(out) :: got
      integer(int64), intent(out) :: v
      integer :: e, ios
      got = .false.; v = 0
      e = q
      do while (e <= len(s))
         if (index('0123456789', s(e:e)) == 0) exit
         e = e + 1
      end do
      if (e == q .or. e > len(s)) return
      if (s(e:e) /= letter) return
      read(s(q:e-1), *, iostat=ios) v
      got = .true.; q = e + 1
    end subroutine grp
    subroutine spaces(q)
      integer, intent(inout) :: q
      do while (q <= len(s))
         if (index(' '//achar(9)//achar(10)//achar(13)//achar(12)//achar(11), s(q:q)) == 0) exit
         q = q + 1
      end do
    end subroutine spaces
  end function duration_seconds

  function strip_ends(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: a, b, k, cp, j
    a = 1
    do while (a <= len(s))
       k = cp_at(s, a, cp)
       if (.not. py_space(cp)) exit
       a = a + k
    end do
    b = len(s)
    do while (b >= a)
       j = b
       do while (j > a .and. ubyte(s(j:j)) >= 128 .and. ubyte(s(j:j)) < 192)
          j = j - 1
       end do
       k = cp_at(s, j, cp)
       if (.not. py_space(cp)) exit
       b = j - 1
    end do
    r = s(a:b)
  end function strip_ends

  integer function chip_size(chip)
    integer, intent(in) :: chip
    character(len=:), allocatable :: t
    integer :: e, b, ios, v
    chip_size = 1
    t = normv(jget(chip, 'text'))
    e = len(t)
    do while (e >= 1)
       if (index(' '//achar(9)//achar(10)//achar(13)//achar(12)//achar(11), t(e:e)) == 0) exit
       e = e - 1
    end do
    b = e
    do while (b >= 1)
       if (index('0123456789', t(b:b)) == 0) exit
       b = b - 1
    end do
    if (b == e .or. b < 1) return
    if (t(b:b) /= '+') return
    read(t(b+1:e), *, iostat=ios) v
    if (ios == 0) chip_size = 1 + v
  end function chip_size

  function safe_file_name(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    character(len=len(s)) :: t
    integer :: i
    t = s
    do i = 1, len(s)
       if (index('\/:*?"<>|', s(i:i)) > 0 .or. (iachar(s(i:i)) >= 0 .and. iachar(s(i:i)) < 32)) t(i:i) = '_'
    end do
    r = str_trim_ws(t)
    if (len(r) == 0) r = 'file'
  end function safe_file_name

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

  ! ---------------------------------------------------------------- report plumbing

  subroutine rep_reset()
    if (allocated(recs)) deallocate(recs)
    if (allocated(rnames)) deallocate(rnames)
    if (allocated(rok)) deallocate(rok)
    if (allocated(rturn)) deallocate(rturn)
    if (allocated(warns)) deallocate(warns)
    allocate(recs(256), rnames(256), rok(256), rturn(256), warns(64))
    nrec = 0; nwarn = 0
  end subroutine rep_reset

  ! a record is finished JSON text (any key order; re-sorted on output)
  subroutine rep_add(name, turn, ok, text)
    character(len=*), intent(in) :: name, text
    integer, intent(in) :: turn
    logical, intent(in) :: ok
    type(strbuf), allocatable :: t1(:)
    type(word), allocatable :: t2(:)
    logical, allocatable :: t3(:)
    integer, allocatable :: t4(:)
    if (nrec >= size(recs)) then
       allocate(t1(2*nrec), t2(2*nrec), t3(2*nrec), t4(2*nrec))
       t1(1:nrec) = recs(1:nrec); t2(1:nrec) = rnames(1:nrec); t3(1:nrec) = rok(1:nrec); t4(1:nrec) = rturn(1:nrec)
       call move_alloc(t1, recs); call move_alloc(t2, rnames); call move_alloc(t3, rok); call move_alloc(t4, rturn)
    end if
    nrec = nrec + 1
    call sb_clear(recs(nrec)); call sb_add(recs(nrec), text)
    rnames(nrec)%s = name; rok(nrec) = ok; rturn(nrec) = turn
  end subroutine rep_add

  subroutine rep_warn(msg)
    character(len=*), intent(in) :: msg
    type(word), allocatable :: tmp(:)
    if (nwarn >= size(warns)) then
       allocate(tmp(2*nwarn)); tmp(1:nwarn) = warns(1:nwarn); call move_alloc(tmp, warns)
    end if
    nwarn = nwarn + 1; warns(nwarn)%s = msg
  end subroutine rep_warn

  ! compact JSON helpers for record text
  function q(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    r = jquote(s)
  end function q

  function raw(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    type(jw) :: w
    call jcanon_value(w, p)
    r = sb_str(w%b)
  end function raw

  function bool(v) result(r)
    logical, intent(in) :: v
    character(len=:), allocatable :: r
    if (v) then
       r = 'true'
    else
       r = 'false'
    end if
  end function bool

  function turnj(i) result(r)
    integer, intent(in) :: i
    character(len=:), allocatable :: r
    if (i < 0) then
       r = 'null'
    else
       r = itoa(i)
    end if
  end function turnj

  function words_json(wl, a, b) result(r)    ! python slice [a, b) 0-based, clamped
    type(wordlist), intent(in) :: wl
    integer, intent(in) :: a, b
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, lo, hi
    lo = max(0, min(wl%n, a)); hi = max(lo, min(wl%n, b))
    call sb_add(o, '[')
    do i = lo + 1, hi
       if (i > lo + 1) call sb_add(o, ',')
       call sb_add(o, q(wl%w(i)%s))
    end do
    call sb_add(o, ']')
    r = sb_str(o)
  end function words_json

  logical function words_equal(a, b)
    type(wordlist), intent(in) :: a, b
    integer :: i
    words_equal = (a%n == b%n)
    if (.not. words_equal) return
    do i = 1, a%n
       if (a%w(i)%s /= b%w(i)%s) then
          words_equal = .false.; return
       end if
    end do
  end function words_equal

  function first_diff(ew, dw) result(r)
    type(wordlist), intent(in) :: ew, dw
    character(len=:), allocatable :: r
    integer :: n, pos, k
    n = min(ew%n, dw%n)
    pos = n
    do k = 1, n
       if (ew%w(k)%s /= dw%w(k)%s) then
          pos = k - 1; exit
       end if
    end do
    if (pos == ew%n .and. pos == dw%n) then
       r = 'null'; return
    end if
    r = '{"position":'//itoa(pos)//',"expected":'//words_json(ew, max(0, pos-8), pos+8)//',"actual":'// &
        words_json(dw, max(0, pos-8), pos+8)//',"expectedWords":'//itoa(ew%n)//',"actualWords":'//itoa(dw%n)//'}'
  end function first_diff

  ! "team.An": the API glues consecutive reply messages without a space; compare with those splits made
  logical function glued_subsequence(small, big)
    type(wordlist), intent(in) :: small, big
    type(wordlist) :: s2, b2
    call py_split(glue_split(join_words(small, 1, small%n)), s2)
    call py_split(glue_split(join_words(big, 1, big%n)), b2)
    glued_subsequence = subsequence(s2, b2)
  end function glued_subsequence

  function glue_split(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    do i = 1, len(s)
       call sb_add(o, s(i:i))
       if (i < len(s)) then
          if (index('.!?', s(i:i)) > 0 .and. s(i+1:i+1) >= 'A' .and. s(i+1:i+1) <= 'Z') call sb_add(o, ' ')
       end if
    end do
    r = sb_str(o)
  end function glue_split

  logical function subsequence(small, big)
    type(wordlist), intent(in) :: small, big
    integer :: i, j
    i = 1; j = 1
    do while (i <= small%n)
       if (j > big%n) then
          subsequence = .false.; return
       end if
       if (small%w(i)%s == big%w(j)%s) i = i + 1
       j = j + 1
    end do
    subsequence = .true.
  end function subsequence

  function missing_spans(ew, dw) result(r)
    type(wordlist), intent(in) :: ew, dw
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, j, start, cnt
    call sb_add(o, '['); cnt = 0
    i = 0; j = 0; start = -1
    do while (i < ew%n)
       if (j < dw%n) then
          if (ew%w(i+1)%s == dw%w(j+1)%s) then
             if (start >= 0) call span(start, i)
             start = -1; i = i + 1; j = j + 1; cycle
          end if
       end if
       if (start < 0) start = i
       i = i + 1
    end do
    if (start >= 0) call span(start, ew%n)
    call sb_add(o, ']')
    r = sb_str(o)
  contains
    subroutine span(a, b)
      integer, intent(in) :: a, b
      if (cnt >= 20) return
      if (cnt > 0) call sb_add(o, ',')
      cnt = cnt + 1
      call sb_add(o, '{"fromWord":'//itoa(a)//',"toWord":'//itoa(b-1)//',"text":'// &
                     q(head_chars(join_words(ew, a+1, b), 300))//'}')
    end subroutine span
  end function missing_spans

  ! ---------------------------------------------------------------- capture access

  integer function art_at(arts, i)
    integer, intent(in) :: arts, i
    art_at = 0
    if (i >= 0 .and. i < jlen(arts)) art_at = jat(arts, [i])
  end function art_at

  integer function panel(cap, key, idx)
    integer, intent(in) :: cap, idx
    character(len=*), intent(in) :: key
    integer :: d
    d = jget(cap, key)
    panel = 0
    if (jis_obj(d)) panel = jget(d, itoa(idx))
  end function panel

  integer function obj_or0(p)
    integer, intent(in) :: p
    obj_or0 = 0
    if (jis_obj(p) .and. truthy(p)) obj_or0 = p
  end function obj_or0

  integer function list_or0(p)
    integer, intent(in) :: p
    list_or0 = 0
    if (jis_arr(p) .and. truthy(p)) list_or0 = p
  end function list_or0

  logical function is_human(t)
    integer, intent(in) :: t
    is_human = jequal_str(jget(t, 'sender'), 'human')
  end function is_human

  logical function is_asst(t)
    integer, intent(in) :: t
    is_asst = jequal_str(jget(t, 'sender'), 'assistant')
  end function is_asst

  integer function tindex(t)
    integer, intent(in) :: t
    tindex = int(jint(jget(t, 'index')))
  end function tindex

  ! ---------------------------------------------------------------- checks

  subroutine check_turn_count(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, a
    type(strbuf) :: es, as
    logical :: ok
    integer :: nt, na
    nt = jlen(list_or0(jget(t, 'turns'))); na = jlen(list_or0(jget(cap, 'articles')))
    call sb_add(es, '['); tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (es%n > 1) call sb_add(es, ',')
       if (is_human(tu)) then
          call sb_add(es, '"user"')
       else
          call sb_add(es, '"assistant"')
       end if
       tu = jnext(tu)
    end do
    call sb_add(es, ']')
    call sb_add(as, '['); a = jfirst(list_or0(jget(cap, 'articles')))
    do while (a > 0)
       if (as%n > 1) call sb_add(as, ',')
       call sb_add(as, raw(jget(a, 'role')))
       a = jnext(a)
    end do
    call sb_add(as, ']')
    ok = (nt == na) .and. (sb_str(es) == sb_str(as))
    call rep_add('turn-count', -1, ok, '{"name":"turn-count","turnIndex":null,"expected":{"count":'//itoa(nt)// &
         ',"senders":'//sb_str(es)//'},"actual":{"count":'//itoa(na)//',"senders":'//sb_str(as)//'},"ok":'//bool(ok)//'}')
  end subroutine check_turn_count

  subroutine check_user_text(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, a, i
    character(len=:), allocatable :: e, d, mode, fd
    type(wordlist) :: ew, dw
    logical :: exact, ok
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_human(tu)) then
          i = tindex(tu); a = art_at(list_or0(jget(cap, 'articles')), i)
          e = normv(jget(tu, 'text')); d = normv(jget(a, 'text'))
          exact = (e == d)
          call md_words(jget(tu, 'text'), ew); call md_words(jget(a, 'text'), dw)
          ok = exact .or. words_equal(ew, dw)
          if (exact) then
             mode = 'exact'
          else if (ok) then
             mode = 'markdown-stripped'
          else
             mode = 'mismatch'
          end if
          if (ok) then
             fd = 'null'
          else
             fd = first_diff(ew, dw)
          end if
          call rep_add('user-text', i, ok, '{"name":"user-text","turnIndex":'//itoa(i)// &
               ',"expected":{"chars":'//itoa(nchars(e))//',"sha256":'//q(sha256_hex(e))//',"head":'//q(head_chars(e, 80))// &
               ',"words":'//itoa(ew%n)//'},"actual":{"chars":'//itoa(nchars(d))//',"sha256":'//q(sha256_hex(d))// &
               ',"head":'//q(head_chars(d, 80))//',"words":'//itoa(dw%n)//'},"ok":'//bool(ok)//',"mode":'//q(mode)// &
               ',"firstDiff":'//fd//'}')
       end if
       tu = jnext(tu)
    end do
  end subroutine check_user_text

  subroutine check_assistant_text(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, a, i
    type(wordlist) :: ew, dw
    logical :: eq, sub
    character(len=:), allocatable :: extra, fd
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu); a = art_at(list_or0(jget(cap, 'articles')), i)
          call md_words(jget(tu, 'text'), ew); call md_words(jget(a, 'text'), dw)
          eq = words_equal(ew, dw)
          sub = eq .or. subsequence(dw, ew)
          if (.not. sub) sub = glued_subsequence(dw, ew)
          if (eq) then
             fd = 'null'; extra = ''
          else
             fd = first_diff(ew, dw)
             if (sub) then
                extra = ',"domIsSubsequenceOfApi":true,"classification":"page-omits-content","missingFromDom":'// &
                        missing_spans(ew, dw)
             else
                extra = ',"domIsSubsequenceOfApi":false,"classification":"mismatch","missingFromDom":'//missing_spans(ew, dw)
             end if
          end if
          call rep_add('assistant-text', i, sub, '{"name":"assistant-text","turnIndex":'//itoa(i)// &
               ',"expected":{"words":'//itoa(ew%n)//',"sha256":'//q(sha256_hex(join_words(ew, 1, ew%n)))// &
               '},"actual":{"words":'//itoa(dw%n)//',"sha256":'//q(sha256_hex(join_words(dw, 1, dw%n)))// &
               '},"ok":'//bool(sub)//',"firstDiff":'//fd// &
               ',"rule":"DOM words == API words, or DOM words a subsequence of API words (page omission -> warning)"'//extra//'}')
          if (.not. eq .and. sub) call rep_warn('turn '//itoa(i)//': the page renders '//itoa(dw%n)//' of '//itoa(ew%n)// &
               ' reply words; the export keeps the full API text (see assistant-text.missingFromDom)')
       end if
       tu = jnext(tu)
    end do
  end subroutine check_assistant_text

  function fileid_from_preview(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    character(len=:), allocatable :: s
    integer :: i, k
    logical :: okk
    r = 'null'
    s = as_text(p)
    do i = 1, len(s) - 37 - 13
       if (s(i:i) /= '/') cycle
       okk = .true.
       do k = i + 1, i + 36
          if (index('0123456789abcdef-', s(k:k)) == 0) then
             okk = .false.; exit
          end if
       end do
       if (.not. okk) cycle
       if (s(i+37:i+50) == '/preview-image') then
          r = q(s(i+1:i+36)); return
       end if
    end do
  end function fileid_from_preview

  subroutine check_attachments(t, cap, att_dir)
    integer, intent(in) :: t, cap
    character(len=*), intent(in) :: att_dir
    integer :: tu, i, api, dom, x, d, nexp, nact
    type(strbuf) :: en, ei, ep, es, an, ai, ap, asz
    logical :: size_ok, names_ok, all_named, ok
    character(len=:), allocatable :: path, en_s, an_s
    integer(int64) :: sz
    integer :: k
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_human(tu)) then
          i = tindex(tu)
          api = list_or0(jget(tu, 'attachments'))
          dom = list_or0(jget(art_at(list_or0(jget(cap, 'articles')), i), 'attachments'))
          if (jlen(api) > 0 .or. jlen(dom) > 0) then
             call sb_clear(en); call sb_clear(ei); call sb_clear(ep); call sb_clear(es)
             call sb_clear(an); call sb_clear(ai); call sb_clear(ap); call sb_clear(asz)
             call sb_add(en, '['); call sb_add(ei, '['); call sb_add(ep, '['); call sb_add(es, '[')
             call sb_add(an, '['); call sb_add(ai, '['); call sb_add(ap, '['); call sb_add(asz, '[')
             size_ok = .true.; nexp = 0
             x = jfirst(api)
             do while (x > 0)
                if (nexp > 0) then
                   call sb_add(en, ','); call sb_add(ei, ','); call sb_add(ep, ','); call sb_add(es, ','); call sb_add(asz, ',')
                end if
                nexp = nexp + 1
                call sb_add(en, raw(jget(x, 'fileName'))); call sb_add(ei, raw(jget(x, 'fileId')))
                call sb_add(ep, raw(jget(x, 'previewUrl'))); call sb_add(es, raw(jget(x, 'sizeBytes')))
                path = att_dir//'\'//itoa(i)//'-'//safe_file_name(py_str(jget(x, 'fileId')))//'-'// &
                       safe_file_name(py_str(jget(x, 'fileName')))
                if (len(att_dir) == 0) then
                   call sb_add(asz, '"not checked"')
                else if (file_exists(path)) then
                   sz = file_size(path)
                   if (.not. (jis_int(jget(x, 'sizeBytes')) .and. jint(jget(x, 'sizeBytes')) == sz)) size_ok = .false.
                   call sb_add(asz, i64toa(sz))
                else
                   size_ok = .false.; call sb_add(asz, 'null')
                end if
                x = jnext(x)
             end do
             nact = 0; all_named = .true.
             d = jfirst(dom)
             do while (d > 0)
                if (nact > 0) then
                   call sb_add(an, ','); call sb_add(ai, ','); call sb_add(ap, ',')
                end if
                nact = nact + 1
                call sb_add(an, raw(jget(d, 'name'))); call sb_add(ai, fileid_from_preview(jget(d, 'previewSrc')))
                call sb_add(ap, raw(jget(d, 'previewSrc')))
                if (jis_null(jget(d, 'name'))) all_named = .false.
                d = jnext(d)
             end do
             call sb_add(en, ']'); call sb_add(ei, ']'); call sb_add(ep, ']'); call sb_add(es, ']')
             call sb_add(an, ']'); call sb_add(ai, ']'); call sb_add(ap, ']'); call sb_add(asz, ']')
             names_ok = (nexp == nact)
             if (names_ok) then
                x = jfirst(api); d = jfirst(dom)
                do k = 1, nexp
                   if (.not. jis_null(jget(d, 'name'))) then
                      if (key_text(jget(d, 'name')) /= key_text(jget(x, 'fileName'))) names_ok = .false.
                   end if
                   x = jnext(x); d = jnext(d)
                end do
             end if
             ok = names_ok .and. (sb_str(ei) == sb_str(ai)) .and. (sb_str(ep) == sb_str(ap)) .and. size_ok
             if (all_named) then
                an_s = 'dom tooltip'
             else
                an_s = 'api (the page shows the name only in a hover tooltip)'
             end if
             call rep_add('attachments', i, ok, '{"name":"attachments","turnIndex":'//itoa(i)// &
                  ',"expected":{"names":'//sb_str(en)//',"fileIds":'//sb_str(ei)//',"previewUrls":'//sb_str(ep)// &
                  ',"sizes":'//sb_str(es)//'},"actual":{"names":'//sb_str(an)//',"fileIds":'//sb_str(ai)// &
                  ',"previewUrls":'//sb_str(ap)//',"sizes":'//sb_str(asz)//'},"ok":'//bool(ok)// &
                  ',"sizeCheck":'//q(trim(merge('files                               ', &
                                                'not performed (no --attachments DIR)', len(att_dir) > 0)))// &
                  ',"nameSource":'//q(an_s)// &
                  ',"rule":"fileIds and previewUrls equal; DOM names equal when present; sizes equal when files are given"}')
          end if
       end if
       tu = jnext(tu)
    end do
  end subroutine check_attachments

  subroutine check_thought_label(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, i, label, dur
    integer(int64) :: secs, delta
    logical :: ok
    character(len=:), allocatable :: secs_j, delta_j
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu)
          label = jget(art_at(list_or0(jget(cap, 'articles')), i), 'thoughtLabel')
          secs = duration_seconds(label)
          dur = jget(obj_or0(jget(tu, 'thinking')), 'durationMs')
          if (secs < 0) then
             secs_j = 'null'
          else
             secs_j = i64toa(secs)
          end if
          if (secs < 0 .or. .not. jis_int(dur)) then
             delta_j = 'null'; ok = .false.
          else
             delta = abs(secs*1000 - jint(dur)); delta_j = i64toa(delta); ok = (delta <= 2000)
          end if
          call rep_add('thought-label', i, ok, '{"name":"thought-label","turnIndex":'//itoa(i)// &
               ',"expected":{"durationMs":'//raw(dur)//',"toleranceMs":2000},"actual":{"label":'//raw(label)// &
               ',"seconds":'//secs_j//',"deltaMs":'//delta_j//'},"ok":'//bool(ok)//'}')
       end if
       tu = jnext(tu)
    end do
  end subroutine check_thought_label

  subroutine sort_unique(items, n)
    type(word), intent(inout) :: items(:)
    integer, intent(inout) :: n
    integer :: i, j, m
    character(len=:), allocatable :: tmp
    do i = 2, n
       j = i
       do while (j > 1)
          if (.not. lgt(items(j-1)%s, items(j)%s)) exit
          tmp = items(j)%s; items(j)%s = items(j-1)%s; items(j-1)%s = tmp; j = j - 1
       end do
    end do
    m = 0
    do i = 1, n
       if (m > 0) then
          if (items(m)%s == items(i)%s) cycle
       end if
       m = m + 1; items(m)%s = items(i)%s
    end do
    n = m
  end subroutine sort_unique

  function strlist_json(items, n) result(r)
    type(word), intent(in) :: items(:)
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    call sb_add(o, '[')
    do i = 1, n
       if (i > 1) call sb_add(o, ',')
       call sb_add(o, q(items(i)%s))
    end do
    call sb_add(o, ']')
    r = sb_str(o)
  end function strlist_json

  function strlist_pyrepr(items, n) result(r)
    type(word), intent(in) :: items(:)
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    call sb_add(o, '[')
    do i = 1, n
       if (i > 1) call sb_add(o, ', ')
       call sb_add(o, "'"//items(i)%s//"'")
    end do
    call sb_add(o, ']')
    r = sb_str(o)
  end function strlist_pyrepr

  subroutine check_rollouts(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, i, rl, pan, sec, na, nd, nm, nx, k, j
    type(word), allocatable :: api(:), dom(:), miss(:), extra(:)
    logical :: ok, found, same
    character(len=:), allocatable :: tail
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu)
          allocate(api(64), dom(64)); na = 0; nd = 0
          rl = jfirst(list_or0(jget(obj_or0(jget(tu, 'thinking')), 'rollouts')))
          do while (rl > 0)
             if (truthy(jget(rl, 'events'))) then
                na = na + 1; api(na)%s = py_str(jget(rl, 'id'))
             end if
             rl = jnext(rl)
          end do
          pan = obj_or0(panel(cap, 'thoughtsByArticle', i))
          call sort_unique(api, na)
          sec = jfirst(list_or0(jget(pan, 'sections')))
          do while (sec > 0)
             if (truthy(jget(sec, 'rollout'))) then
                nd = nd + 1; dom(nd)%s = py_str(jget(sec, 'rollout'))
             else if (na == 1) then            ! a page that names no agent shows the single rollout of a solo turn
                nd = nd + 1; dom(nd)%s = api(1)%s
             end if
             sec = jnext(sec)
          end do
          call sort_unique(dom, nd)
          allocate(miss(max(na,1)), extra(max(nd,1))); nm = 0; nx = 0
          do k = 1, nd
             found = .false.
             do j = 1, na
                if (api(j)%s == dom(k)%s) found = .true.
             end do
             if (.not. found) then
                nx = nx + 1; extra(nx)%s = dom(k)%s
             end if
          end do
          do k = 1, na
             found = .false.
             do j = 1, nd
                if (dom(j)%s == api(k)%s) found = .true.
             end do
             if (.not. found) then
                nm = nm + 1; miss(nm)%s = api(k)%s
             end if
          end do
          ok = (nx == 0)
          same = (strlist_json(api, na) == strlist_json(dom, nd))
          tail = ''
          if (.not. same) tail = ',"missingInDom":'//strlist_json(miss, nm)//',"extraInDom":'//strlist_json(extra, nx)// &
                                 ',"note":'//q(PAGE_NOTE)
          call rep_add('rollouts', i, ok, '{"name":"rollouts","turnIndex":'//itoa(i)//',"expected":'//strlist_json(api, na)// &
               ',"actual":'//strlist_json(dom, nd)//',"ok":'//bool(ok)// &
               ',"rule":"every DOM rollout exists in the API; API rollouts absent from the page -> warning"'//tail//'}')
          if (.not. same .and. nm > 0 .and. nx == 0) call rep_warn('turn '//itoa(i)//': page omits rollout(s) '// &
               strlist_pyrepr(miss, nm)//' that the API contains; the export keeps them')
          deallocate(api, dom, miss, extra)
       end if
       tu = jnext(tu)
    end do
  end subroutine check_rollouts

  subroutine check_summaries(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, i, rl, ev, pan, sec, r, na, nd, nmiss, k
    type(word), allocatable :: api(:), dom(:), miss(:)
    character(len=:), allocatable :: text
    logical :: ok
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu)
          allocate(api(64), dom(64), miss(64)); na = 0; nd = 0; nmiss = 0
          rl = jfirst(list_or0(jget(obj_or0(jget(tu, 'thinking')), 'rollouts')))
          do while (rl > 0)
             ev = jfirst(list_or0(jget(rl, 'events')))
             do while (ev > 0)
                if (jequal_str(jget(ev, 'type'), 'summary')) call push(api, na, normv(jget(ev, 'text')))
                ev = jnext(ev)
             end do
             rl = jnext(rl)
          end do
          pan = obj_or0(panel(cap, 'thoughtsByArticle', i))
          text = normv(jget(pan, 'innerText'))
          sec = jfirst(list_or0(jget(pan, 'sections')))
          do while (sec > 0)
             r = jfirst(list_or0(jget(sec, 'rows')))
             do while (r > 0)
                if (jequal_str(jget(r, 'type'), 'summary')) call push(dom, nd, normv(jget(r, 'text')))
                r = jnext(r)
             end do
             sec = jnext(sec)
          end do
          do k = 1, na
             if (len(api(k)%s) > 0 .and. index(text, api(k)%s) == 0) call push(miss, nmiss, api(k)%s)
          end do
          ok = (nmiss == 0 .and. nd == na)
          call rep_add('summaries', i, ok, '{"name":"summaries","turnIndex":'//itoa(i)//',"expected":{"count":'//itoa(na)// &
               ',"texts":'//strlist_json(api, na)//'},"actual":{"count":'//itoa(nd)//',"texts":'//strlist_json(dom, nd)// &
               '},"ok":'//bool(ok)//',"missingFromPanelText":'//strlist_json(miss, nmiss)//'}')
          deallocate(api, dom, miss)
       end if
       tu = jnext(tu)
    end do
  contains
    subroutine push(arr, n, v)
      type(word), allocatable, intent(inout) :: arr(:)
      integer, intent(inout) :: n
      character(len=*), intent(in) :: v
      type(word), allocatable :: tmp(:)
      if (n >= size(arr)) then
         allocate(tmp(2*size(arr))); tmp(1:n) = arr(1:n); call move_alloc(tmp, arr)
      end if
      n = n + 1; arr(n)%s = v
    end subroutine push
  end subroutine check_summaries

  subroutine check_chatroom(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, i, rl, ev, pan, sec, r, nmsg, ndom, k, j, nnf
    type(wordlist) :: hayw, w
    type(wordlist), allocatable :: apiw(:)
    character(len=:), allocatable :: hay
    type(strbuf) :: res, dres
    logical :: dom_ok, found, inapi, ok
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu)
          pan = obj_or0(panel(cap, 'thoughtsByArticle', i))
          call md_words(jget(pan, 'innerText'), hayw)
          hay = ' '//join_words(hayw, 1, hayw%n)//' '
          allocate(apiw(64)); nmsg = 0; nnf = 0
          call sb_clear(res); call sb_add(res, '[')
          rl = jfirst(list_or0(jget(obj_or0(jget(tu, 'thinking')), 'rollouts')))
          do while (rl > 0)
             ev = jfirst(list_or0(jget(rl, 'events')))
             do while (ev > 0)
                if (jequal_str(jget(ev, 'type'), 'tool') .and. jequal_str(jget(ev, 'kind'), 'chatroomSend')) then
                   k = jget(obj_or0(jget(ev, 'args')), 'message')
                   if (truthy(k)) then
                      call md_words(k, w)
                   else
                      call md_words_s('', w)
                   end if
                   if (w%n > 0) then
                      found = index(hay, ' '//join_words(w, 1, w%n)//' ') > 0
                   else
                      found = .true.
                   end if
                   if (.not. found) nnf = nnf + 1
                   nmsg = nmsg + 1
                   if (nmsg > size(apiw)) call grow_wl(apiw)
                   apiw(nmsg) = w
                   if (nmsg > 1) call sb_add(res, ',')
                   call sb_add(res, '{"rollout":'//raw(jget(rl, 'id'))//',"words":'//itoa(w%n)//',"head":'// &
                                    q(join_words(w, 1, 8))//',"found":'//bool(found)//'}')
                end if
                ev = jnext(ev)
             end do
             rl = jnext(rl)
          end do
          call sb_add(res, ']')
          dom_ok = .true.; ndom = 0
          call sb_clear(dres); call sb_add(dres, '[')
          sec = jfirst(list_or0(jget(pan, 'sections')))
          do while (sec > 0)
             r = jfirst(list_or0(jget(sec, 'rows')))
             do while (r > 0)
                if (jequal_str(jget(r, 'type'), 'chatroom')) then
                   call md_words(jget(r, 'message'), w)
                   if (w%n > 0) then
                      inapi = .false.
                      do j = 1, nmsg
                         if (words_equal(apiw(j), w)) inapi = .true.
                      end do
                   else
                      inapi = .true.
                   end if
                   if (.not. inapi) dom_ok = .false.
                   ndom = ndom + 1
                   if (ndom > 1) call sb_add(dres, ',')
                   call sb_add(dres, '{"words":'//itoa(w%n)//',"head":'//q(join_words(w, 1, 8))//',"inApi":'//bool(inapi)//'}')
                end if
                r = jnext(r)
             end do
             sec = jnext(sec)
          end do
          call sb_add(dres, ']')
          ok = dom_ok .and. ndom <= nmsg
          if (nnf > 0 .or. ndom /= nmsg) then
             call rep_add('chatroom', i, ok, '{"name":"chatroom","turnIndex":'//itoa(i)//',"expected":{"count":'// &
                  itoa(nmsg)//'},"actual":{"count":'//itoa(ndom)//',"messages":'//sb_str(res)//',"domMessages":'// &
                  sb_str(dres)//'},"ok":'//bool(ok)//',"rule":"every DOM chatroom message equals an API chatroomSend '// &
                  'message; API messages absent from the page -> warning","note":'//q(PAGE_NOTE)//'}')
             if (ok) call rep_warn('turn '//itoa(i)//': page renders '//itoa(ndom)//' of '//itoa(nmsg)// &
                                   ' chatroom messages; the export keeps all of them')
          else
             call rep_add('chatroom', i, ok, '{"name":"chatroom","turnIndex":'//itoa(i)//',"expected":{"count":'// &
                  itoa(nmsg)//'},"actual":{"count":'//itoa(ndom)//',"messages":'//sb_str(res)//',"domMessages":'// &
                  sb_str(dres)//'},"ok":'//bool(ok)//',"rule":"every DOM chatroom message equals an API chatroomSend '// &
                  'message; API messages absent from the page -> warning"}')
          end if
          deallocate(apiw)
       end if
       tu = jnext(tu)
    end do
  contains
    subroutine grow_wl(a)
      type(wordlist), allocatable, intent(inout) :: a(:)
      type(wordlist), allocatable :: tmp(:)
      allocate(tmp(2*size(a))); tmp(1:size(a)) = a; call move_alloc(tmp, a)
    end subroutine grow_wl
  end subroutine check_chatroom

  ! ---- tool rows
  type(word) function dom_kind(k)
    character(len=*), intent(in) :: k
    select case (k)
    case ('webSearch');                dom_kind%s = 'Searched web'
    case ('xSearch', 'xUserSearch');   dom_kind%s = 'Searched '//achar(240)//achar(157)//achar(149)//achar(143)
    case ('browsePage');               dom_kind%s = 'Browsed'
    case default;                      dom_kind%s = ''
    end select
  end function dom_kind

  logical function is_dom_kind_value(p)
    integer, intent(in) :: p
    character(len=:), allocatable :: s
    is_dom_kind_value = .false.
    if (.not. jis_str(p)) return
    s = jstr(p)
    is_dom_kind_value = (s == 'Searched web' .or. s == 'Browsed' .or. &
                         s == 'Searched '//achar(240)//achar(157)//achar(149)//achar(143))
  end function is_dom_kind_value

  subroutine check_tool_rows(t, cap)
    integer, intent(in) :: t, cap
    type :: trow
       character(len=:), allocatable :: rkey, rsort1, kind, key, apikind, rollout_raw, count_raw, urls_raw
       integer :: nurls = 0
       logical :: is_web = .false.
    end type trow
    type(trow), allocatable :: er(:), ar(:)
    integer :: tu, i, pan, rl, ev, args, res, items, it, sec, r, ne, na, k, j, nm, nx, used_k, ro, sole, nsole
    logical, allocatable :: used(:), matched_e(:), matched_a(:)
    type(strbuf) :: miss, extra, mism, urls
    character(len=:), allocatable :: kind, key
    integer :: web_rows_e, web_urls_e, web_rows_a, web_urls_a, nmism
    logical :: ok
    integer, allocatable :: mi(:), xi(:)
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu)
          pan = panel(cap, 'sourcesByArticle', i)
          allocate(er(64)); ne = 0
          rl = jfirst(list_or0(jget(obj_or0(jget(tu, 'thinking')), 'rollouts')))
          do while (rl > 0)
             ev = jfirst(list_or0(jget(rl, 'events')))
             do while (ev > 0)
                if (jequal_str(jget(ev, 'type'), 'tool') .and. jis_str(jget(ev, 'kind'))) then
                   kind = jstr(jget(ev, 'kind'))
                   if (len(dom_kind_s(kind)) > 0) then
                      args = obj_or0(jget(ev, 'args')); res = obj_or0(jget(ev, 'results'))
                      items = list_or0(jget(res, 'items'))
                      if (kind == 'browsePage') then
                         key = urlkey(jget(args, 'url'))
                      else
                         key = normv(jget(args, 'query'))
                      end if
                      if (ne >= size(er)) call grow_rows(er)
                      ne = ne + 1
                      er(ne)%rollout_raw = raw(jget(rl, 'id'))
                      er(ne)%rsort1 = py_str(jget(rl, 'id'))
                      er(ne)%kind = dom_kind_s(kind); er(ne)%apikind = kind; er(ne)%key = key
                      er(ne)%is_web = (kind == 'webSearch')
                      if (er(ne)%is_web) then
                         er(ne)%count_raw = itoa(jlen(items))
                         call sb_clear(urls); call sb_add(urls, '[')
                         it = jfirst(items); k = 0
                         do while (it > 0)
                            if (k > 0) call sb_add(urls, ',')
                            call sb_add(urls, raw(jget(it, 'url'))); k = k + 1
                            it = jnext(it)
                         end do
                         call sb_add(urls, ']'); er(ne)%urls_raw = sb_str(urls); er(ne)%nurls = k
                      else
                         er(ne)%count_raw = 'null'; er(ne)%urls_raw = 'null'; er(ne)%nurls = 0
                      end if
                      er(ne)%rkey = key_text(jget(rl, 'id'))//achar(1)//er(ne)%kind//achar(1)//key
                   end if
                end if
                ev = jnext(ev)
             end do
             rl = jnext(rl)
          end do
          if (.not. (ne == 0 .and. .not. truthy(pan))) then
             allocate(ar(64)); na = 0
             ! a page that names no agent (canvas replies) shows the single rollout of a solo turn
             sole = 0; nsole = 0
             rl = jfirst(list_or0(jget(obj_or0(jget(tu, 'thinking')), 'rollouts')))
             do while (rl > 0)
                if (truthy(jget(rl, 'events'))) then
                   if (nsole == 0) then
                      sole = jget(rl, 'id'); nsole = 1
                   else if (key_text(jget(rl, 'id')) /= key_text(sole)) then
                      nsole = 2
                   end if
                end if
                rl = jnext(rl)
             end do
             if (nsole /= 1) sole = 0
             sec = jfirst(list_or0(jget(obj_or0(pan), 'sections')))
             do while (sec > 0)
                r = jfirst(list_or0(jget(sec, 'rows')))
                do while (r > 0)
                   if (is_dom_kind_value(jget(r, 'kind'))) then
                      ro = jget(sec, 'rollout')
                      if (.not. truthy(ro)) ro = sole
                      if (na >= size(ar)) call grow_rows(ar)
                      na = na + 1
                      ar(na)%kind = jstr(jget(r, 'kind'))
                      if (ar(na)%kind == 'Browsed') then
                         if (truthy(jget(r, 'url'))) then
                            ar(na)%key = urlkey(jget(r, 'url'))
                         else
                            ar(na)%key = urlkey(jget(r, 'query'))
                         end if
                      else
                         ar(na)%key = normv(jget(r, 'query'))
                      end if
                      ar(na)%rollout_raw = raw(ro)
                      ar(na)%rsort1 = py_str(ro)
                      ar(na)%count_raw = raw(jget(r, 'count'))
                      call sb_clear(urls); call sb_add(urls, '[')
                      it = jfirst(list_or0(jget(r, 'results'))); k = 0
                      do while (it > 0)
                         if (k > 0) call sb_add(urls, ',')
                         call sb_add(urls, raw(jget(it, 'url'))); k = k + 1
                         it = jnext(it)
                      end do
                      call sb_add(urls, ']'); ar(na)%urls_raw = sb_str(urls); ar(na)%nurls = k
                      ar(na)%is_web = (ar(na)%kind == 'Searched web')
                      ar(na)%rkey = key_text(ro)//achar(1)//ar(na)%kind//achar(1)//ar(na)%key
                   end if
                   r = jnext(r)
                end do
                sec = jnext(sec)
             end do
             ! multiset differences
             allocate(matched_e(max(ne,1)), matched_a(max(na,1)))
             matched_e = .false.; matched_a = .false.
             do k = 1, ne
                do j = 1, na
                   if (.not. matched_a(j) .and. ar(j)%rkey == er(k)%rkey) then
                      matched_a(j) = .true.; matched_e(k) = .true.; exit
                   end if
                end do
             end do
             allocate(mi(max(ne,1)), xi(max(na,1))); nm = 0; nx = 0
             do k = 1, ne
                if (.not. matched_e(k)) then
                   nm = nm + 1; mi(nm) = k
                end if
             end do
             do j = 1, na
                if (.not. matched_a(j)) then
                   nx = nx + 1; xi(nx) = j
                end if
             end do
             call sort_rows(er, mi, nm); call sort_rows(ar, xi, nx)
             call sb_clear(miss); call sb_add(miss, '[')
             do k = 1, nm
                if (k > 1) call sb_add(miss, ',')
                call sb_add(miss, '{"rollout":'//er(mi(k))%rollout_raw//',"kind":'//q(er(mi(k))%kind)//',"key":'//q(er(mi(k))%key)//'}')
             end do
             call sb_add(miss, ']')
             call sb_clear(extra); call sb_add(extra, '[')
             do k = 1, nx
                if (k > 1) call sb_add(extra, ',')
                call sb_add(extra, '{"rollout":'//ar(xi(k))%rollout_raw//',"kind":'//q(ar(xi(k))%kind)//',"key":'//q(ar(xi(k))%key)//'}')
             end do
             call sb_add(extra, ']')
             allocate(used(max(na,1))); used = .false.
             call sb_clear(mism); call sb_add(mism, '['); nmism = 0
             do k = 1, ne
                if (er(k)%kind /= 'Searched web') cycle
                used_k = 0
                do j = 1, na
                   if (.not. used(j) .and. ar(j)%rkey == er(k)%rkey) then
                      used_k = j; exit
                   end if
                end do
                if (used_k == 0) cycle
                used(used_k) = .true.
                if (ar(used_k)%count_raw /= er(k)%count_raw .or. ar(used_k)%urls_raw /= er(k)%urls_raw) then
                   if (nmism > 0) call sb_add(mism, ',')
                   nmism = nmism + 1
                   call sb_add(mism, '{"rollout":'//er(k)%rollout_raw//',"query":'//q(er(k)%key)//',"apiCount":'// &
                        er(k)%count_raw//',"domCount":'//ar(used_k)%count_raw//',"apiUrls":'//er(k)%urls_raw// &
                        ',"domUrls":'//ar(used_k)%urls_raw//'}')
                end if
             end do
             call sb_add(mism, ']')
             web_rows_e = 0; web_urls_e = 0; web_rows_a = 0; web_urls_a = 0
             do k = 1, ne
                if (er(k)%kind == 'Searched web') web_rows_e = web_rows_e + 1
                if (er(k)%is_web .and. er(k)%nurls > 0) web_urls_e = web_urls_e + er(k)%nurls
             end do
             do j = 1, na
                if (ar(j)%kind == 'Searched web') then
                   web_rows_a = web_rows_a + 1; web_urls_a = web_urls_a + ar(j)%nurls
                end if
             end do
             ok = (nm == 0 .and. nx == 0 .and. nmism == 0)
             call rep_add('tool-rows', i, ok, '{"name":"tool-rows","turnIndex":'//itoa(i)//',"expected":{"rows":'//itoa(ne)// &
                  ',"webSearchRows":'//itoa(web_rows_e)//',"webResultUrls":'//itoa(web_urls_e)//'},"actual":{"rows":'// &
                  itoa(na)//',"webSearchRows":'//itoa(web_rows_a)//',"webResultUrls":'//itoa(web_urls_a)// &
                  ',"domRowsTotal":'//raw(jget(obj_or0(pan), 'rowCount'))//'},"ok":'//bool(ok)//',"missingInDom":'// &
                  sb_str(miss)//',"extraInDom":'//sb_str(extra)//',"urlMismatches":'//sb_str(mism)//'}')
             deallocate(ar, matched_e, matched_a, mi, xi, used)
          end if
          deallocate(er)
       end if
       tu = jnext(tu)
    end do
  contains
    function dom_kind_s(k) result(r)
      character(len=*), intent(in) :: k
      character(len=:), allocatable :: r
      type(word) :: w
      w = dom_kind(k); r = w%s
    end function dom_kind_s
    subroutine grow_rows(a)
      type(trow), allocatable, intent(inout) :: a(:)
      type(trow), allocatable :: tmp(:)
      allocate(tmp(2*size(a))); tmp(1:size(a)) = a; call move_alloc(tmp, a)
    end subroutine grow_rows
    ! sort indices by (py-str rollout, kind, key)
    subroutine sort_rows(rows, idx, n)
      type(trow), intent(in) :: rows(:)
      integer, intent(inout) :: idx(:)
      integer, intent(in) :: n
      integer :: a1, b1, tmpi
      do a1 = 2, n
         b1 = a1
         do while (b1 > 1)
            if (.not. row_lt(rows(idx(b1)), rows(idx(b1-1)))) exit
            tmpi = idx(b1); idx(b1) = idx(b1-1); idx(b1-1) = tmpi; b1 = b1 - 1
         end do
      end do
    end subroutine sort_rows
    logical function row_lt(x, y)
      type(trow), intent(in) :: x, y
      if (x%rsort1 /= y%rsort1) then
         row_lt = llt(x%rsort1, y%rsort1)
      else if (x%kind /= y%kind) then
         row_lt = llt(x%kind, y%kind)
      else
         row_lt = llt(x%key, y%key)
      end if
    end function row_lt
  end subroutine check_tool_rows

  subroutine check_citations(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, i, cits, chips, c, eff, ncit, nchip, k, j, ngroups_nohref
    type(strbuf) :: urls, dist, hrefs_all, texts, bad, unver
    type(word), allocatable :: api_urls(:), distinct(:), hrefs(:)
    integer :: nu, nd, nh, nbad, nunv
    logical :: have_urls, ok, found
    character(len=:), allocatable :: mode
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu)
          cits = list_or0(jget(tu, 'citations'))
          chips = list_or0(jget(art_at(list_or0(jget(cap, 'articles')), i), 'citationChips'))
          ncit = jlen(cits); nchip = jlen(chips)
          if (ncit > 0 .or. nchip > 0) then
             allocate(api_urls(max(ncit,1)), distinct(max(ncit,1)), hrefs(max(nchip,1)))
             nu = 0; nd = 0; nh = 0; have_urls = .false.
             call sb_clear(urls); call sb_add(urls, '[')
             c = jfirst(cits)
             do while (c > 0)
                if (nu > 0) call sb_add(urls, ',')
                nu = nu + 1; api_urls(nu)%s = key_text(jget(c, 'url'))
                call sb_add(urls, raw(jget(c, 'url')))
                if (truthy(jget(c, 'url'))) then
                   have_urls = .true.
                   found = .false.
                   do k = 1, nd
                      if (distinct(k)%s == api_urls(nu)%s) found = .true.
                   end do
                   if (.not. found) then
                      nd = nd + 1; distinct(nd)%s = api_urls(nu)%s
                   end if
                end if
                c = jnext(c)
             end do
             call sb_add(urls, ']')
             eff = 0; ngroups_nohref = 0
             call sb_clear(hrefs_all); call sb_add(hrefs_all, '[')
             call sb_clear(texts); call sb_add(texts, '[')
             c = jfirst(chips); k = 0
             do while (c > 0)
                eff = eff + chip_size(c)
                if (truthy(jget(c, 'href'))) then
                   nh = nh + 1; hrefs(nh)%s = key_text(jget(c, 'href'))
                else
                   ngroups_nohref = ngroups_nohref + 1
                end if
                if (k > 0) then
                   call sb_add(hrefs_all, ','); call sb_add(texts, ',')
                end if
                k = k + 1
                call sb_add(hrefs_all, raw(jget(c, 'href'))); call sb_add(texts, q(normv(jget(c, 'text'))))
                c = jnext(c)
             end do
             call sb_add(hrefs_all, ']'); call sb_add(texts, ']')
             if (have_urls) then
                call sb_clear(bad); call sb_add(bad, '['); nbad = 0
                c = jfirst(chips)
                do while (c > 0)
                   if (truthy(jget(c, 'href'))) then
                      found = .false.
                      do j = 1, nu
                         if (api_urls(j)%s == key_text(jget(c, 'href'))) found = .true.
                      end do
                      if (.not. found) then
                         if (nbad > 0) call sb_add(bad, ',')
                         nbad = nbad + 1; call sb_add(bad, raw(jget(c, 'href')))
                      end if
                   end if
                   c = jnext(c)
                end do
                call sb_add(bad, ']')
                call sb_clear(unver); call sb_add(unver, '['); nunv = 0
                do k = 1, nd
                   found = .false.
                   do j = 1, nh
                      if (hrefs(j)%s == distinct(k)%s) found = .true.
                   end do
                   if (.not. found) then
                      if (nunv > 0) call sb_add(unver, ',')
                      nunv = nunv + 1
                      call sb_add(unver, q(distinct(k)%s(3:)))
                   end if
                end do
                call sb_add(unver, ']')
                ok = (eff == nd .or. eff == ncit) .and. nbad == 0 .and. (nunv == 0 .or. ngroups_nohref > 0)
                call rep_add('citations', i, ok, '{"name":"citations","turnIndex":'//itoa(i)//',"expected":{"citations":'// &
                     itoa(ncit)//',"distinctUrls":'//itoa(nd)//',"urls":'//sb_str(urls)//'},"actual":{"chips":'//itoa(nchip)// &
                     ',"effectiveChips":'//itoa(eff)//',"chipTexts":'//sb_str(texts)//',"chipHrefs":'//sb_str(hrefs_all)// &
                     '},"ok":'//bool(ok)//',"hrefNotInApi":'//sb_str(bad)//',"apiUrlsWithoutHref":'//sb_str(unver)// &
                     ',"rule":"effectiveChips == distinct API URLs, or == citations (one chip per citation); every href in API URLs"}')
             else
                ok = (eff > 0 .and. eff <= ncit)
                if (eff == ncit) then
                   mode = 'one-to-one'
                else
                   mode = 'ambiguous'
                end if
                call rep_add('citations', i, ok, '{"name":"citations","turnIndex":'//itoa(i)//',"expected":{"citations":'// &
                     itoa(ncit)//',"distinctUrls":null,"urls":'//sb_str(urls)//'},"actual":{"chips":'//itoa(nchip)// &
                     ',"effectiveChips":'//itoa(eff)//',"chipTexts":'//sb_str(texts)//',"chipHrefs":'//sb_str(hrefs_all)// &
                     '},"ok":'//bool(ok)//',"rule":"legacy transcript (no API URLs): 0 < effectiveChips <= citations; '// &
                     'hrefs reported for enrichment","enrichment":{"mode":'//q(mode)//',"chipHrefs":'//sb_str(hrefs_all)// &
                     ',"chipTexts":'//sb_str(texts)//'}}')
                if (mode == 'ambiguous') call rep_warn('turn '//itoa(i)//': '//itoa(ncit)//' citation(s) but '//itoa(eff)// &
                     ' chip(s); citation URLs cannot be assigned from the DOM, left null')
             end if
             deallocate(api_urls, distinct, hrefs)
          end if
       end if
       tu = jnext(tu)
    end do
  end subroutine check_citations

  subroutine check_expansion(cap)
    integer, intent(in) :: cap
    character(len=8), parameter :: keys(2) = [character(len=8) :: 'thoughts', 'sources ']
    character(len=17), parameter :: fields(2) = [character(len=17) :: 'thoughtsByArticle', 'sourcesByArticle ']
    integer :: f, d, c, n, k, j, tmpi, ios, v, pan, rc
    integer, allocatable :: ks(:), nodes(:)
    character(len=:), allocatable :: kname
    logical :: ok
    do f = 1, 2
       d = obj_or0(jget(cap, trim(fields(f))))
       n = jlen(d)
       if (n == 0) cycle
       allocate(ks(n), nodes(n)); k = 0
       c = jfirst(d)
       do while (c > 0)
          kname = jname(c)
          read(kname, *, iostat=ios) v
          if (ios == 0 .and. len(kname) > 0 .and. verify(kname, '0123456789') == 0) then
             k = k + 1; ks(k) = v; nodes(k) = c
          end if
          c = jnext(c)
       end do
       do j = 2, k
          tmpi = j
          do while (tmpi > 1)
             if (ks(tmpi-1) <= ks(tmpi)) exit
             call swap(tmpi)
             tmpi = tmpi - 1
          end do
       end do
       do j = 1, k
          pan = nodes(j)
          if (jis_null(pan)) cycle
          rc = jget(pan, 'remainingCollapsed')
          ok = jis_int(rc)
          if (ok) ok = (jint(rc) == 0)
          call rep_add('expansion-complete', ks(j), ok, '{"name":"expansion-complete","turnIndex":'//itoa(ks(j))// &
               ',"expected":{"panel":'//q(trim(keys(f)))//',"remainingCollapsed":0},"actual":{"panel":'//q(trim(keys(f)))// &
               ',"remainingCollapsed":'//raw(rc)//',"rows":'//raw(jget(pan, 'rowCount'))//',"links":'// &
               raw(jget(pan, 'linkCount'))//'},"ok":'//bool(ok)//'}')
       end do
       deallocate(ks, nodes)
    end do
  contains
    subroutine swap(p)
      integer, intent(in) :: p
      integer :: t1, t2
      t1 = ks(p); ks(p) = ks(p-1); ks(p-1) = t1
      t2 = nodes(p); nodes(p) = nodes(p-1); nodes(p-1) = t2
    end subroutine swap
  end subroutine check_expansion

  subroutine check_api_presence(t, cap)
    integer, intent(in) :: t, cap
    integer :: tu, i, rl
    logical :: has
    tu = jfirst(list_or0(jget(t, 'turns')))
    do while (tu > 0)
       if (is_asst(tu)) then
          i = tindex(tu)
          has = .false.
          rl = jfirst(list_or0(jget(obj_or0(jget(tu, 'thinking')), 'rollouts')))
          do while (rl > 0)
             if (truthy(jget(rl, 'events'))) has = .true.
             rl = jnext(rl)
          end do
          if (has .and. .not. truthy(panel(cap, 'thoughtsByArticle', i))) &
             call rep_warn('turn '//itoa(i)//': API has thinking events but the DOM capture has no Thoughts panel')
       end if
       tu = jnext(tu)
    end do
  end subroutine check_api_presence

  ! ---------------------------------------------------------------- assembly

  ! verification.json text; extra_fields_json: pre-rendered '"api":"ok","dom":"ok (1 round)","rounds":1' (may be '')
  function verify_capture(t, cap, att_dir, tname, cname, tool, extra_fields_json, nfailed_out, ntotal_out, extra_checks) &
       result(out)
    integer, intent(in) :: t, cap
    character(len=*), intent(in) :: att_dir, tname, cname, tool, extra_fields_json
    character(len=*), intent(in), optional :: extra_checks   ! comma-joined JSON check records (stability, api-consistency)
    integer, intent(out) :: nfailed_out, ntotal_out
    character(len=:), allocatable :: out
    type(strbuf) :: doc, byname, failed, checks, wl
    type(word), allocatable :: names(:)
    integer :: i, j, nn, nf, cnt, okc
    logical :: dup
    call rep_reset()
    call check_turn_count(t, cap)
    call check_user_text(t, cap)
    call check_assistant_text(t, cap)
    call check_attachments(t, cap, att_dir)
    call check_thought_label(t, cap)
    call check_rollouts(t, cap)
    call check_summaries(t, cap)
    call check_chatroom(t, cap)
    call check_tool_rows(t, cap)
    call check_citations(t, cap)
    call check_expansion(cap)
    call check_api_presence(t, cap)
    if (present(extra_checks)) then
       if (len(extra_checks) > 0) call add_extra(extra_checks)
    end if
    call sb_add(checks, '[')
    nf = 0
    call sb_add(failed, '[')
    do i = 1, nrec
       if (i > 1) call sb_add(checks, ',')
       call sb_add(checks, sb_str(recs(i)))
       if (.not. rok(i)) then
          if (nf > 0) call sb_add(failed, ',')
          nf = nf + 1
          call sb_add(failed, '{"name":'//q(rnames(i)%s)//',"turnIndex":'//turnj(rturn(i))//'}')
       end if
    end do
    call sb_add(checks, ']'); call sb_add(failed, ']')
    allocate(names(max(nrec,1))); nn = 0
    do i = 1, nrec
       dup = .false.
       do j = 1, nn
          if (names(j)%s == rnames(i)%s) dup = .true.
       end do
       if (.not. dup) then
          nn = nn + 1; names(nn)%s = rnames(i)%s
       end if
    end do
    call sb_add(byname, '{')
    do j = 1, nn
       cnt = 0; okc = 0
       do i = 1, nrec
          if (rnames(i)%s == names(j)%s) then
             cnt = cnt + 1
             if (rok(i)) okc = okc + 1
          end if
       end do
       if (j > 1) call sb_add(byname, ',')
       call sb_add(byname, q(names(j)%s)//':{"total":'//itoa(cnt)//',"ok":'//itoa(okc)//',"failed":'//itoa(cnt-okc)//'}')
    end do
    call sb_add(byname, '}')
    call sb_add(wl, '[')
    do i = 1, nwarn
       if (i > 1) call sb_add(wl, ',')
       call sb_add(wl, q(warns(i)%s))
    end do
    call sb_add(wl, ']')
    call sb_add(doc, '{"tool":'//q(tool)//',"checks":'//sb_str(checks)//',"byName":'//sb_str(byname)// &
                     ',"summary":{"total":'//itoa(nrec)//',"ok":'//itoa(nrec-nf)//',"failed":'//itoa(nf)//'}'// &
                     ',"failedChecks":'//sb_str(failed)//',"warnings":'//sb_str(wl)//',"ok":'//bool(nf == 0)// &
                     ',"transcript":'//q(tname)//',"capture":'//q(cname))
    if (len(extra_fields_json) > 0) call sb_add(doc, ','//extra_fields_json)
    call sb_add(doc, '}')
    block
      type(jw) :: w
      call jcanon_value(w, jparse(sb_str(doc)))
      out = jw_result(w)
    end block
    nfailed_out = nf; ntotal_out = nrec
  contains
    subroutine add_extra(txt)
      character(len=*), intent(in) :: txt
      integer :: arr, e, ti
      arr = jparse('['//txt//']')
      e = jfirst(arr)
      do while (e > 0)
         ti = -1
         if (jis_int(jget(e, 'turnIndex'))) ti = int(jint(jget(e, 'turnIndex')))
         call rep_add(jstr(jget(e, 'name')), ti, jis_true(jget(e, 'ok')), raw(e))
         e = jnext(e)
      end do
    end subroutine add_extra
  end function verify_capture

  ! ---------------------------------------------------------------- stability (check 11) / api-consistency (check 13)

  ! canonical text of the capture without capturedAt, env and articles[*].html
  function strip_env_text(cap) result(r)
    integer, intent(in) :: cap
    character(len=:), allocatable :: r
    type(jw) :: w
    integer, allocatable :: kids(:)
    integer :: n, i, c, a
    logical :: done_articles
    n = 0
    if (jis_obj(cap)) n = jlen(cap)
    allocate(kids(max(n,1)))
    n = 0
    if (jis_obj(cap)) then
       c = jfirst(cap)
       do while (c > 0)
          if (jname(c) /= 'capturedAt' .and. jname(c) /= 'env' .and. jname(c) /= 'articles') then
             n = n + 1; kids(n) = c
          end if
          c = jnext(c)
       end do
    end if
    call sort_kids(kids, n)
    call jw_begin_obj(w)
    done_articles = .false.
    do i = 1, n
       if (.not. done_articles .and. lgt(jname(kids(i)), 'articles')) then
          call put_articles(); done_articles = .true.
       end if
       call jw_key(w, jname(kids(i))); call jcanon_value(w, kids(i))
    end do
    if (.not. done_articles) call put_articles()
    call jw_end_obj(w)
    r = jw_result(w)
  contains
    subroutine put_articles()
      integer :: ak(256), m, j, q
      integer, allocatable :: akk(:)
      call jw_key(w, 'articles')
      call jw_begin_arr(w)
      a = 0
      if (jis_obj(cap)) a = jget(cap, 'articles')
      if (jis_arr(a)) then
         q = jfirst(a)
         do while (q > 0)
            if (jis_obj(q)) then
               allocate(akk(max(jlen(q),1))); m = 0
               c = jfirst(q)
               do while (c > 0)
                  if (jname(c) /= 'html') then
                     m = m + 1; akk(m) = c
                  end if
                  c = jnext(c)
               end do
               call sort_kids(akk, m)
               call jw_begin_obj(w)
               do j = 1, m
                  call jw_key(w, jname(akk(j))); call jcanon_value(w, akk(j))
               end do
               call jw_end_obj(w)
               deallocate(akk)
            else
               call jw_begin_obj(w); call jw_end_obj(w)
            end if
            q = jnext(q)
         end do
      end if
      call jw_end_arr(w)
      ak(1) = 0
    end subroutine put_articles
  end function strip_env_text

  character(len=4) function jkind(p)
    integer, intent(in) :: p
    if (jis_obj(p)) then
       jkind = 'dict'
    else if (jis_arr(p)) then
       jkind = 'list'
    else if (jis_str(p)) then
       jkind = 'str'
    else if (jis_true(p) .or. jis_false(p)) then
       jkind = 'bool'
    else if (p == 0 .or. jis_null(p)) then
       jkind = 'none'
    else
       jkind = 'int'
    end if
  end function jkind

  ! diff-paths: records as comma-joined compact JSON; n = number pushed
  subroutine diff_paths(a, b, limit, out, n, perm_filter, nperm)
    integer, intent(in) :: a, b, limit
    type(strbuf), intent(inout) :: out
    integer, intent(out) :: n
    logical, intent(in) :: perm_filter      ! api-consistency: count citation url/kind paths separately
    integer, intent(out) :: nperm
    integer :: ntot, wlimit
    n = 0; nperm = 0; ntot = 0
    wlimit = limit
    if (perm_filter) wlimit = 100000
    call walk(a, b, '')
  contains
    recursive subroutine walk(x, y, path)
      integer, intent(in) :: x, y
      character(len=*), intent(in) :: path
      integer, allocatable :: kx(:)
      character(len=:), allocatable :: k
      type(word), allocatable :: keys(:)
      integer :: nk, c, i, cx, cy, lx, ly
      if (ntot >= wlimit) return
      if (jkind(x) /= jkind(y)) then
         if (len(path) == 0) then
            call push('/', s120(x), s120(y))
         else
            call push(path, s120(x), s120(y))
         end if
      else if (jis_obj(x)) then
         allocate(keys(jlen(x) + jlen(y))); nk = 0
         c = jfirst(x)
         do while (c > 0)
            nk = nk + 1; keys(nk)%s = jname(c); c = jnext(c)
         end do
         c = jfirst(y)
         do while (c > 0)
            nk = nk + 1; keys(nk)%s = jname(c); c = jnext(c)
         end do
         call sort_unique(keys, nk)
         do i = 1, nk
            k = keys(i)%s
            cx = jget(x, k); cy = jget(y, k)
            if (cx == 0 .or. cy == 0) then
               call push(path//'/'//k, trim(merge('present', 'absent ', cx > 0)), trim(merge('present', 'absent ', cy > 0)))
            else
               call walk(cx, cy, path//'/'//k)
            end if
         end do
      else if (jis_arr(x)) then
         lx = jlen(x); ly = jlen(y)
         if (lx /= ly) call push(path, 'len '//itoa(lx), 'len '//itoa(ly))
         cx = jfirst(x); cy = jfirst(y); i = 0
         do while (cx > 0 .and. cy > 0)
            call walk(cx, cy, path//'/'//itoa(i))
            cx = jnext(cx); cy = jnext(cy); i = i + 1
         end do
      else if (scalar_differs(x, y)) then
         call push(path, s120(x), s120(y))
      end if
    end subroutine walk
    logical function scalar_differs(x, y)
      integer, intent(in) :: x, y
      if (jis_str(x)) then
         scalar_differs = (jstr(x) /= jstr(y))
      else
         scalar_differs = (jraw_text(x) /= jraw_text(y))
      end if
    end function scalar_differs
    function s120(p) result(r)
      integer, intent(in) :: p
      character(len=:), allocatable :: r
      r = head_chars(py_str(p), 120)
    end function s120
    subroutine push(path, va, vb)
      character(len=*), intent(in) :: path, va, vb
      if (ntot >= wlimit) return
      ntot = ntot + 1
      if (perm_filter) then
         if (is_cit_path(path)) then
            nperm = nperm + 1; return
         end if
      end if
      if (n >= limit) return
      if (n > 0) call sb_add(out, ',')
      n = n + 1
      call sb_add(out, '{"path":'//q(path)//',"a":'//q(va)//',"b":'//q(vb)//'}')
    end subroutine push
  end subroutine diff_paths

  ! ^/turns/[0-9]+/citations/[0-9]+/(url|kind)$
  logical function is_cit_path(p)
    character(len=*), intent(in) :: p
    integer :: i
    is_cit_path = .false.
    if (.not. starts_with(p, '/turns/')) return
    i = 8
    if (.not. digits()) return
    if (.not. (i + 10 <= len(p))) return
    if (p(i:i+10) /= '/citations/') return
    i = i + 11
    if (.not. digits()) return
    is_cit_path = (p(i:) == '/url' .or. p(i:) == '/kind')
  contains
    logical function digits()
      integer :: s0
      s0 = i
      do while (i <= len(p))
         if (index('0123456789', p(i:i)) == 0) exit
         i = i + 1
      end do
      digits = (i > s0)
    end function digits
  end function is_cit_path

  function stability_record(cap1, cap2, name1, name2) result(r)
    integer, intent(in) :: cap1, cap2
    character(len=*), intent(in) :: name1, name2
    character(len=:), allocatable :: r, s1, s2
    type(strbuf) :: diffs
    integer :: n, np
    logical :: ok
    s1 = strip_env_text(cap1); s2 = strip_env_text(cap2)
    ok = (s1 == s2)
    if (.not. ok) call diff_paths(jparse(s1), jparse(s2), 40, diffs, n, .false., np)
    r = '{"name":"dom-stability","turnIndex":null,"expected":{"sha256":'//q(sha256_hex(s1))//',"bytes":'//itoa(len(s1))// &
        ',"file":'//q(name1)//'},"actual":{"sha256":'//q(sha256_hex(s2))//',"bytes":'//itoa(len(s2))//',"file":'//q(name2)// &
        '},"ok":'//bool(ok)//',"ignored":["capturedAt","env","articles[*].html"],"differences":['//sb_str(diffs)//']}'
  end function stability_record

  ! t_chunk / t_legacy: transcript handles (0 = unavailable)
  function api_consistency_record(t_chunk, t_legacy) result(r)
    integer, intent(in) :: t_chunk, t_legacy
    character(len=:), allocatable :: r, s1, s2
    type(strbuf) :: diffs
    integer :: n, np
    if (.not. (jis_obj(t_chunk) .and. jis_obj(t_legacy))) then
       r = '{"name":"api-consistency","turnIndex":null,"rule":"transcripts built from the chunk and the legacy payload differ '// &
           'only in citations[].url/kind","expected":{"format":"chunk","available":'//bool(jis_obj(t_chunk))// &
           '},"actual":{"format":"legacy","available":'//bool(jis_obj(t_legacy))//'},"ok":false,"differences":[],'// &
           '"note":"one of the two payloads was not available, the cross-check could not be performed"}'
       return
    end if
    block
      type(jw) :: w1, w2
      call jcanon_value(w1, t_chunk); s1 = jw_result(w1)
      call jcanon_value(w2, t_legacy); s2 = jw_result(w2)
    end block
    call diff_paths(t_chunk, t_legacy, 40, diffs, n, .true., np)
    r = '{"name":"api-consistency","turnIndex":null,"rule":"transcripts built from the chunk and the legacy payload differ '// &
        'only in citations[].url/kind","expected":{"format":"chunk","sha256":'//q(sha256_hex(s1))//',"bytes":'//itoa(len(s1))// &
        '},"actual":{"format":"legacy","sha256":'//q(sha256_hex(s2))//',"bytes":'//itoa(len(s2))//'},"ok":'//bool(n == 0)// &
        ',"permittedDifferences":'//itoa(np)//',"differences":['//sb_str(diffs)//']}'
  end function api_consistency_record

  ! ---------------------------------------------------------------- citation enrichment
  ! Legacy fallback: citations the API left without url get url/kind from DOM chips (one-to-one only).
  subroutine enrich_citations(t, cap, chip_links, out_text, notes, nnotes)
    integer, intent(in) :: t, cap, chip_links
    character(len=:), allocatable, intent(out) :: out_text
    type(word), allocatable, intent(out) :: notes(:)
    integer, intent(out) :: nnotes
    type(jw) :: w
    integer :: top, c
    allocate(notes(64)); nnotes = 0
    call jw_begin_obj(w)
    ! top-level keys: conversation, turns (+ any others, sorted)
    call write_sorted_obj_top(w, t)
    call jw_end_obj(w)
    out_text = jw_result(w)
  contains
    subroutine write_sorted_obj_top(w2, obj)
      type(jw), intent(inout) :: w2
      integer, intent(in) :: obj
      integer, allocatable :: kids(:)
      integer :: n, i2, j2, tt
      n = jlen(obj); allocate(kids(n))
      c = jfirst(obj); i2 = 0
      do while (c > 0)
         i2 = i2 + 1; kids(i2) = c; c = jnext(c)
      end do
      call sort_kids(kids, n)
      do i2 = 1, n
         call jw_key(w2, jname(kids(i2)))
         if (jname(kids(i2)) == 'turns') then
            call jw_begin_arr(w2)
            tt = jfirst(kids(i2))
            do while (tt > 0)
               call write_turn(w2, tt)
               tt = jnext(tt)
            end do
            call jw_end_arr(w2)
         else
            call jcanon_value(w2, kids(i2))
         end if
      end do
    end subroutine write_sorted_obj_top

    subroutine write_turn(w2, tu)
      type(jw), intent(inout) :: w2
      integer, intent(in) :: tu
      integer, allocatable :: kids(:)
      integer :: n, i2, cits, ncit, nnull, chips, eff, ch, pos, sz, k2, m, entry, e2, idx
      type(word), allocatable :: assigned(:)
      logical, allocatable :: has_assigned(:)
      integer, allocatable :: covered(:), ids(:)
      integer :: nmem, chip_k, tmp1, a1, b1, nfilled, links
      n = jlen(tu); allocate(kids(n))
      c = jfirst(tu); i2 = 0
      do while (c > 0)
         i2 = i2 + 1; kids(i2) = c; c = jnext(c)
      end do
      call sort_kids(kids, n)
      cits = list_or0(jget(tu, 'citations')); ncit = jlen(cits)
      nnull = 0
      c = jfirst(cits)
      do while (c > 0)
         if (.not. truthy(jget(c, 'url'))) nnull = nnull + 1
         c = jnext(c)
      end do
      allocate(assigned(max(ncit,1)))
      allocate(has_assigned(max(ncit,1))); has_assigned = .false.
      if (is_asst(tu) .and. ncit > 0 .and. nnull > 0) then
         idx = tindex(tu)
         chips = list_or0(jget(art_at(list_or0(jget(cap, 'articles')), idx), 'citationChips'))
         eff = 0
         ch = jfirst(chips)
         do while (ch > 0)
            eff = eff + chip_size(ch); ch = jnext(ch)
         end do
         if (eff /= ncit) then
            call note('turn '//itoa(idx)//': '//itoa(nnull)//' of '//itoa(ncit)//' citation(s) unresolved by the API, '// &
                      itoa(eff)//' effective chip(s) for '//itoa(ncit)//' citations (ambiguous), left null')
         else
            links = list_or0(jget(obj_or0(chip_links), itoa(idx)))
            pos = 0; chip_k = 0
            ch = jfirst(chips)
            do while (ch > 0)
               sz = chip_size(ch)
               if (sz == 1 .and. truthy(jget(ch, 'href'))) then
                  if (jis_str(jget(ch, 'href'))) then
                     assigned(pos+1)%s = jstr(jget(ch, 'href')); has_assigned(pos+1) = .true.
                  end if
               else if (sz == 1) then
                  call note('turn '//itoa(idx)//': chip '//itoa(chip_k)//' ('//repr(normv(jget(ch, 'text')))// &
                            ') has no href and no revealed members; citation '//itoa(pos)//' left null')
               else
                  entry = 0
                  e2 = jfirst(links)
                  do while (e2 > 0)
                     if (jis_int(jget(e2, 'chip'))) then
                        if (jint(jget(e2, 'chip')) == chip_k) then
                           entry = e2; exit
                        end if
                     end if
                     e2 = jnext(e2)
                  end do
                  nmem = jlen(list_or0(jget(entry, 'links')))
                  if (nmem /= sz) then
                     call note('turn '//itoa(idx)//': group chip '//itoa(chip_k)//' ('//repr(normv(jget(ch, 'text')))// &
                               ') covers '//itoa(sz)//' citations but revealed '//itoa(nmem)//' member link(s); left null')
                  else
                     allocate(covered(sz), ids(sz))
                     do k2 = 1, sz
                        covered(k2) = pos + k2 - 1
                        m = jat(cits, [covered(k2)])
                        ids(k2) = 0
                        if (jis_int(jget(m, 'citationId'))) ids(k2) = int(jint(jget(m, 'citationId')))
                     end do
                     do a1 = 2, sz          ! stable sort covered by citationId
                        b1 = a1
                        do while (b1 > 1)
                           if (ids(b1-1) <= ids(b1)) exit
                           tmp1 = ids(b1); ids(b1) = ids(b1-1); ids(b1-1) = tmp1
                           tmp1 = covered(b1); covered(b1) = covered(b1-1); covered(b1-1) = tmp1
                           b1 = b1 - 1
                        end do
                     end do
                     m = jfirst(list_or0(jget(entry, 'links')))
                     do k2 = 1, sz
                        if (jis_str(jget(m, 'href'))) then
                           assigned(covered(k2)+1)%s = jstr(jget(m, 'href')); has_assigned(covered(k2)+1) = .true.
                        end if
                        m = jnext(m)
                     end do
                     call note('turn '//itoa(idx)//': group chip '//itoa(chip_k)//' ('//repr(normv(jget(ch, 'text')))// &
                               ') expanded to '//itoa(nmem)//' member link(s), paired with citations '//intlist(covered, sz)// &
                               ' by ascending citationId')
                     deallocate(covered, ids)
                  end if
               end if
               pos = pos + sz; chip_k = chip_k + 1
               ch = jnext(ch)
            end do
            nfilled = 0
            c = jfirst(cits); k2 = 0
            do while (c > 0)
               k2 = k2 + 1
               if (has_assigned(k2)) then
                  if (len(assigned(k2)%s) > 0 .and. .not. truthy(jget(c, 'url'))) nfilled = nfilled + 1
               end if
               c = jnext(c)
            end do
            call note('turn '//itoa(idx)//': '//itoa(nfilled)//' of '//itoa(nnull)// &
                      ' API-unresolved citation URL(s) filled from the DOM chips (fallback, kind CITATION_KIND_WEB_PAGE)')
         end if
      end if
      call jw_begin_obj(w2)
      do i2 = 1, n
         call jw_key(w2, jname(kids(i2)))
         if (jname(kids(i2)) == 'citations' .and. jis_arr(kids(i2))) then
            call jw_begin_arr(w2)
            c = jfirst(kids(i2)); k2 = 0
            do while (c > 0)
               k2 = k2 + 1
               if (k2 <= size(has_assigned)) then
                  if (has_assigned(k2) .and. .not. truthy(jget(c, 'url'))) then
                     call write_cit(w2, c, assigned(k2)%s)
                     c = jnext(c); cycle
                  end if
               end if
               call jcanon_value(w2, c)
               c = jnext(c)
            end do
            call jw_end_arr(w2)
         else
            call jcanon_value(w2, kids(i2))
         end if
      end do
      call jw_end_obj(w2)
    end subroutine write_turn

    subroutine write_cit(w2, cit, url)
      type(jw), intent(inout) :: w2
      integer, intent(in) :: cit
      character(len=*), intent(in) :: url
      integer, allocatable :: kids(:)
      integer :: n, i2, cc
      logical :: had_kind, had_url
      n = jlen(cit); allocate(kids(n))
      cc = jfirst(cit); i2 = 0
      do while (cc > 0)
         i2 = i2 + 1; kids(i2) = cc; cc = jnext(cc)
      end do
      call sort_kids(kids, n)
      had_kind = (jget(cit, 'kind') > 0); had_url = (jget(cit, 'url') > 0)
      call jw_begin_obj(w2)
      ! keys in sorted order; kind/url added if absent
      do i2 = 1, n
         if (.not. had_kind .and. lgt(jname(kids(i2)), 'kind')) then
            call jw_key(w2, 'kind'); call jw_str(w2, 'CITATION_KIND_WEB_PAGE'); had_kind = .true.
         end if
         if (.not. had_url .and. lgt(jname(kids(i2)), 'url')) then
            call jw_key(w2, 'url'); call jw_str(w2, url); had_url = .true.
         end if
         call jw_key(w2, jname(kids(i2)))
         if (jname(kids(i2)) == 'kind') then
            call jw_str(w2, 'CITATION_KIND_WEB_PAGE')
         else if (jname(kids(i2)) == 'url') then
            call jw_str(w2, url)
         else
            call jcanon_value(w2, kids(i2))
         end if
      end do
      if (.not. had_kind) then
         call jw_key(w2, 'kind'); call jw_str(w2, 'CITATION_KIND_WEB_PAGE')
      end if
      if (.not. had_url) then
         call jw_key(w2, 'url'); call jw_str(w2, url)
      end if
      call jw_end_obj(w2)
    end subroutine write_cit

    subroutine note(msg)
      character(len=*), intent(in) :: msg
      type(word), allocatable :: tmp(:)
      if (nnotes >= size(notes)) then
         allocate(tmp(2*size(notes))); tmp(1:nnotes) = notes(1:nnotes); call move_alloc(tmp, notes)
      end if
      nnotes = nnotes + 1; notes(nnotes)%s = msg
    end subroutine note
  end subroutine enrich_citations

  subroutine sort_kids(kids, n)
    integer, intent(inout) :: kids(:)
    integer, intent(in) :: n
    integer :: i, j, t
    do i = 2, n
       t = kids(i); j = i - 1
       do while (j >= 1)
          if (.not. lgt(jname(kids(j)), jname(t))) exit
          kids(j+1) = kids(j); j = j - 1
       end do
       kids(j+1) = t
    end do
  end subroutine sort_kids

  ! Racket ~s of a string
  function repr(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    r = '"'//replace_all(replace_all(s, '\', '\\'), '"', '\"')//'"'
  end function repr

  ! Racket list printing: (1 2 3)
  function intlist(a, n) result(r)
    integer, intent(in) :: a(:), n
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    call sb_add(o, '(')
    do i = 1, n
       if (i > 1) call sb_add(o, ' ')
       call sb_add(o, itoa(a(i)))
    end do
    call sb_add(o, ')')
    r = sb_str(o)
  end function intlist

end module fx_verify
