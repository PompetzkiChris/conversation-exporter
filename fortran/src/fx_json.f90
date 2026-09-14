! fx_json.f90 — fast JSON reader (flat node arena, zero-copy) + canonical JSON writer.
!
! Reader: every parsed document keeps its text; nodes are rows in one arena of integer
! arrays (kind, slice start/end, first child, next sibling, child count, key slice).
! Handles are plain integers; 0 means "absent" and behaves like JSON null, which mirrors
! Racket's `at` returning 'null.  Strings are decoded only when asked for.
! (json-fortran was used first: correct, but 9.7 s to read one 3 MB Gemini response because
!  it grows long strings one character at a time.)
!
! Writer canonical form (identical to racket/jsonw.rkt and fixtures/reference_transcript.py):
!   keys sorted by code point, 1 space per level, ",\n" between items, ": " after keys,
!   {} and [] when empty, strings escape only " \ and < 0x20 (\n \r \t \b \f, else \u00xx),
!   integers only, trailing newline.
module fx_json
  use iso_fortran_env, only: int64, int32
  use fx_util
  implicit none
  private
  public :: J_NULL, J_FALSE, J_TRUE, J_NUM, J_STR, J_ARR, J_OBJ
  public :: jparse, jat, jlen, jtype, jis_str, jis_arr, jis_obj, jis_int, jis_null, jis_false, jis_true, &
            jstr, jint, jnext, jfirst, jname, jequal_str, jget, jcanon_value, jraw_text
  public :: jw, jw_begin_obj, jw_end_obj, jw_begin_arr, jw_end_arr, jw_key, jw_str, jw_int, &
            jw_null, jw_bool, jw_elem, jw_result, jw_raw, jquote

  integer, parameter :: J_NULL = 1, J_FALSE = 2, J_TRUE = 3, J_NUM = 4, J_STR = 5, J_ARR = 6, J_OBJ = 7

  type :: doc_text
     character(len=:), allocatable :: s
  end type doc_text

  integer, allocatable, save :: nk(:), na(:), nb(:), nfirst(:), nnext(:), ncount(:), nka(:), nkb(:), ndoc(:)
  integer, save :: nn = 0
  type(doc_text), allocatable, save :: docs(:)
  integer, save :: ndocs = 0

  type :: jw
     type(strbuf) :: b
     integer :: level = 0
     integer :: cnt(0:256) = 0
     logical :: after_key = .false.
  end type jw

contains

  subroutine grow_nodes(need)
    integer, intent(in) :: need
    integer :: cap
    if (.not. allocated(nk)) then
       cap = max(65536, need)
       allocate(nk(cap), na(cap), nb(cap), nfirst(cap), nnext(cap), ncount(cap), nka(cap), nkb(cap), ndoc(cap))
       return
    end if
    if (need <= size(nk)) return
    cap = max(need, 2*size(nk))
    call g(nk); call g(na); call g(nb); call g(nfirst); call g(nnext); call g(ncount); call g(nka); call g(nkb); call g(ndoc)
  contains
    subroutine g(v)
      integer, allocatable, intent(inout) :: v(:)
      integer, allocatable :: t(:)
      allocate(t(cap)); t(1:nn) = v(1:nn); call move_alloc(t, v)
    end subroutine g
  end subroutine grow_nodes

  integer function new_node(kind, a, b, d)
    integer, intent(in) :: kind, a, b, d
    call grow_nodes(nn + 1)
    nn = nn + 1
    nk(nn) = kind; na(nn) = a; nb(nn) = b; nfirst(nn) = 0; nnext(nn) = 0; ncount(nn) = 0
    nka(nn) = 0; nkb(nn) = -1; ndoc(nn) = d
    new_node = nn
  end function new_node

  ! parse text -> root handle (0 on failure)
  integer function jparse(text) result(root)
    character(len=*), intent(in) :: text
    integer, parameter :: MAXD = 4096
    integer :: stk(MAXD), last(MAXD), sp, i, n, d, node, parent, ka, kb, c
    type(doc_text), allocatable :: tmp(:)
    logical :: expect_key
    root = 0
    if (.not. allocated(docs)) allocate(docs(64))
    if (ndocs >= size(docs)) then
       allocate(tmp(2*size(docs))); tmp(1:ndocs) = docs(1:ndocs); call move_alloc(tmp, docs)
    end if
    ndocs = ndocs + 1; d = ndocs
    docs(d)%s = text
    n = len(text)
    sp = 0; i = 1; ka = 0; kb = -1
    expect_key = .false.
    do
       call skip_ws()
       if (i > n) exit
       c = iachar(docs(d)%s(i:i))
       if (sp > 0) then
          if (c == 44) then
             i = i + 1
             if (nk(stk(sp)) == J_OBJ) expect_key = .true.
             cycle
          else if (c == 93 .or. c == 125) then
             i = i + 1
             nb(stk(sp)) = i - 1
             sp = sp - 1
             expect_key = .false.
             if (sp == 0) exit
             cycle
          end if
          if (expect_key) then
             if (c /= 34) then
                root = 0; return
             end if
             ka = i + 1
             call scan_string()
             kb = i - 2
             call skip_ws()
             if (i > n) then
                root = 0; return
             end if
             if (docs(d)%s(i:i) /= ':') then
                root = 0; return
             end if
             i = i + 1
             expect_key = .false.
             cycle
          end if
       end if
       select case (c)
       case (123)
          node = new_node(J_OBJ, i, i, d)
          call attach(node)
          i = i + 1
          sp = sp + 1
          if (sp > MAXD) then
             root = 0; return
          end if
          stk(sp) = node; last(sp) = 0
          call skip_ws()
          if (i <= n) then
             if (docs(d)%s(i:i) == '}') then
                nb(node) = i; i = i + 1; sp = sp - 1
                if (sp == 0) exit
                cycle
             end if
          end if
          expect_key = .true.
       case (91)
          node = new_node(J_ARR, i, i, d)
          call attach(node)
          i = i + 1
          sp = sp + 1
          if (sp > MAXD) then
             root = 0; return
          end if
          stk(sp) = node; last(sp) = 0
       case (34)
          node = new_node(J_STR, i + 1, 0, d)
          call scan_string()
          nb(node) = i - 2
          call attach(node)
       case (116)
          node = new_node(J_TRUE, i, i + 3, d); call attach(node); i = i + 4
       case (102)
          node = new_node(J_FALSE, i, i + 4, d); call attach(node); i = i + 5
       case (110)
          node = new_node(J_NULL, i, i + 3, d); call attach(node); i = i + 4
       case default
          node = new_node(J_NUM, i, i, d)
          do while (i <= n)
             c = iachar(docs(d)%s(i:i))
             if (.not. ((c >= 48 .and. c <= 57) .or. c == 45 .or. c == 43 .or. c == 46 .or. c == 101 .or. c == 69)) exit
             i = i + 1
          end do
          nb(node) = i - 1
          if (nb(node) < na(node)) then
             root = 0; return
          end if
          call attach(node)
       end select
       if (sp == 0) exit
    end do
  contains
    subroutine skip_ws()
      do while (i <= n)
         c = iachar(docs(d)%s(i:i))
         if (c /= 32 .and. c /= 10 .and. c /= 13 .and. c /= 9) exit
         i = i + 1
      end do
    end subroutine skip_ws
    subroutine scan_string()
      integer :: j
      j = i + 1
      do while (j <= n)
         if (docs(d)%s(j:j) == '\') then
            j = j + 2
         else if (docs(d)%s(j:j) == '"') then
            exit
         else
            j = j + 1
         end if
      end do
      i = j + 1
    end subroutine scan_string
    subroutine attach(node)
      integer, intent(in) :: node
      if (sp == 0) then
         root = node; return
      end if
      parent = stk(sp)
      if (nk(parent) == J_OBJ) then
         nka(node) = ka; nkb(node) = kb
      end if
      if (last(sp) == 0) then
         nfirst(parent) = node
      else
         nnext(last(sp)) = node
      end if
      last(sp) = node
      ncount(parent) = ncount(parent) + 1
    end subroutine attach
  end function jparse

  integer function jtype(p)
    integer, intent(in) :: p
    if (p <= 0) then
       jtype = 0
    else
       jtype = nk(p)
    end if
  end function jtype

  logical function jis_str(p);   integer, intent(in) :: p; jis_str = (jtype(p) == J_STR);  end function
  logical function jis_arr(p);   integer, intent(in) :: p; jis_arr = (jtype(p) == J_ARR);  end function
  logical function jis_obj(p);   integer, intent(in) :: p; jis_obj = (jtype(p) == J_OBJ);  end function
  logical function jis_false(p); integer, intent(in) :: p; jis_false = (jtype(p) == J_FALSE); end function
  logical function jis_true(p);  integer, intent(in) :: p; jis_true = (jtype(p) == J_TRUE);  end function
  logical function jis_null(p);  integer, intent(in) :: p; jis_null = (jtype(p) == 0 .or. jtype(p) == J_NULL); end function

  logical function jis_int(p)
    integer, intent(in) :: p
    jis_int = .false.
    if (jtype(p) == J_NUM) jis_int = (scan(docs(ndoc(p))%s(na(p):nb(p)), '.eE') == 0)
  end function jis_int

  integer function jlen(p)
    integer, intent(in) :: p
    jlen = 0
    if (jtype(p) == J_ARR .or. jtype(p) == J_OBJ) jlen = ncount(p)
  end function jlen

  integer function jfirst(p)
    integer, intent(in) :: p
    jfirst = 0
    if (jtype(p) == J_ARR .or. jtype(p) == J_OBJ) jfirst = nfirst(p)
  end function jfirst

  integer function jnext(p)
    integer, intent(in) :: p
    jnext = 0
    if (p > 0) jnext = nnext(p)
  end function jnext

  integer function jat(p, idx) result(q)
    integer, intent(in) :: p
    integer, intent(in) :: idx(:)
    integer :: k, j
    q = p
    do k = 1, size(idx)
       if (jtype(q) /= J_ARR) then
          q = 0; return
       end if
       if (idx(k) < 0 .or. idx(k) >= ncount(q)) then
          q = 0; return
       end if
       q = nfirst(q)
       do j = 1, idx(k)
          q = nnext(q)
       end do
    end do
  end function jat

  integer function jget(p, key) result(q)
    integer, intent(in) :: p
    character(len=*), intent(in) :: key
    q = 0
    if (jtype(p) /= J_OBJ) return
    q = nfirst(p)
    do while (q > 0)
       if (nkb(q) - nka(q) + 1 == len(key)) then
          if (docs(ndoc(q))%s(nka(q):nkb(q)) == key) return
       end if
       q = nnext(q)
    end do
  end function jget

  function jname(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    r = ''
    if (p <= 0) return
    if (nkb(p) >= nka(p)) r = json_unescape(docs(ndoc(p))%s(nka(p):nkb(p)))
  end function jname

  function jstr(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    r = ''
    if (jtype(p) /= J_STR) return
    if (nb(p) >= na(p)) r = json_unescape(docs(ndoc(p))%s(na(p):nb(p)))
  end function jstr

  function jint(p) result(r)
    integer, intent(in) :: p
    integer(int64) :: r
    integer :: ios
    r = 0
    if (jis_int(p)) read(docs(ndoc(p))%s(na(p):nb(p)), *, iostat=ios) r
  end function jint

  logical function jequal_str(p, s)
    integer, intent(in) :: p
    character(len=*), intent(in) :: s
    jequal_str = .false.
    if (jtype(p) == J_STR) jequal_str = (jstr(p) == s)
  end function jequal_str

  function jraw_text(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    if (p <= 0) then
       r = 'null'
    else if (nk(p) == J_STR) then
       r = '"'//docs(ndoc(p))%s(na(p):nb(p))//'"'
    else
       r = docs(ndoc(p))%s(na(p):nb(p))
    end if
  end function jraw_text

  ! ---------------------------------------------------------------- canonical writer

  subroutine indent(w, n)
    type(jw), intent(inout) :: w
    integer, intent(in) :: n
    if (n > 0) call sb_add(w%b, repeat(' ', n))
  end subroutine indent

  subroutine jw_elem(w)
    type(jw), intent(inout) :: w
    if (w%after_key) then
       w%after_key = .false.; return
    end if
    if (w%level == 0) return
    if (w%cnt(w%level) == 0) then
       call sb_add(w%b, achar(10))
    else
       call sb_add(w%b, ','//achar(10))
    end if
    w%cnt(w%level) = w%cnt(w%level) + 1
    call indent(w, w%level)
  end subroutine jw_elem

  subroutine jw_begin_obj(w)
    type(jw), intent(inout) :: w
    call jw_elem(w)
    call sb_add(w%b, '{')
    w%level = w%level + 1
    w%cnt(w%level) = 0
  end subroutine jw_begin_obj

  subroutine jw_end_obj(w)
    type(jw), intent(inout) :: w
    if (w%cnt(w%level) > 0) then
       call sb_add(w%b, achar(10)); call indent(w, w%level - 1)
    end if
    call sb_add(w%b, '}')
    w%level = w%level - 1
  end subroutine jw_end_obj

  subroutine jw_begin_arr(w)
    type(jw), intent(inout) :: w
    call jw_elem(w)
    call sb_add(w%b, '[')
    w%level = w%level + 1
    w%cnt(w%level) = 0
  end subroutine jw_begin_arr

  subroutine jw_end_arr(w)
    type(jw), intent(inout) :: w
    if (w%cnt(w%level) > 0) then
       call sb_add(w%b, achar(10)); call indent(w, w%level - 1)
    end if
    call sb_add(w%b, ']')
    w%level = w%level - 1
  end subroutine jw_end_arr

  subroutine jw_key(w, k)
    type(jw), intent(inout) :: w
    character(len=*), intent(in) :: k
    call jw_elem(w)
    call put_string(w%b, k)
    call sb_add(w%b, ': ')
    w%after_key = .true.
  end subroutine jw_key

  subroutine jw_str(w, s)
    type(jw), intent(inout) :: w
    character(len=*), intent(in) :: s
    call jw_elem(w)
    call put_string(w%b, s)
  end subroutine jw_str

  subroutine jw_int(w, i)
    type(jw), intent(inout) :: w
    integer(int64), intent(in) :: i
    call jw_elem(w)
    call sb_add(w%b, i64toa(i))
  end subroutine jw_int

  subroutine jw_null(w)
    type(jw), intent(inout) :: w
    call jw_elem(w)
    call sb_add(w%b, 'null')
  end subroutine jw_null

  subroutine jw_bool(w, v)
    type(jw), intent(inout) :: w
    logical, intent(in) :: v
    call jw_elem(w)
    if (v) then
       call sb_add(w%b, 'true')
    else
       call sb_add(w%b, 'false')
    end if
  end subroutine jw_bool

  subroutine jw_raw(w, text)
    type(jw), intent(inout) :: w
    character(len=*), intent(in) :: text
    call jw_elem(w)
    call sb_add(w%b, text)
  end subroutine jw_raw

  function jw_result(w) result(r)
    type(jw), intent(in) :: w
    character(len=:), allocatable :: r
    r = sb_str(w%b) // achar(10)
  end function jw_result

  function jquote(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: b
    call put_string(b, s)
    r = sb_str(b)
  end function jquote

  subroutine put_string(b, s)
    type(strbuf), intent(inout) :: b
    character(len=*), intent(in) :: s
    integer :: i, c, run
    character(len=16), parameter :: hx = '0123456789abcdef'
    call sb_add(b, '"')
    run = 1
    do i = 1, len(s)
       c = iachar(s(i:i))
       if (c == 34 .or. c == 92 .or. (c >= 0 .and. c < 32)) then
          if (i > run) call sb_add(b, s(run:i-1))
          select case (c)
          case (34); call sb_add(b, '\"')
          case (92); call sb_add(b, '\\')
          case (10); call sb_add(b, '\n')
          case (13); call sb_add(b, '\r')
          case (9);  call sb_add(b, '\t')
          case (8);  call sb_add(b, '\b')
          case (12); call sb_add(b, '\f')
          case default
             call sb_add(b, '\u00'//hx(c/16+1:c/16+1)//hx(mod(c,16)+1:mod(c,16)+1))
          end select
          run = i + 1
       end if
    end do
    if (run <= len(s)) call sb_add(b, s(run:))
    call sb_add(b, '"')
  end subroutine put_string

  recursive subroutine jcanon_value(w, p)
    type(jw), intent(inout) :: w
    integer, intent(in) :: p
    integer, allocatable :: kids(:)
    integer :: n, i, j, t, c
    select case (jtype(p))
    case (0, J_NULL)
       call jw_null(w)
    case (J_TRUE)
       call jw_bool(w, .true.)
    case (J_FALSE)
       call jw_bool(w, .false.)
    case (J_NUM)
       call jw_int(w, jint(p))
    case (J_STR)
       call jw_str(w, jstr(p))
    case (J_ARR)
       call jw_begin_arr(w)
       c = nfirst(p)
       do while (c > 0)
          call jcanon_value(w, c); c = nnext(c)
       end do
       call jw_end_arr(w)
    case (J_OBJ)
       n = ncount(p)
       allocate(kids(n))
       c = nfirst(p); i = 0
       do while (c > 0)
          i = i + 1; kids(i) = c; c = nnext(c)
       end do
       do i = 2, n
          t = kids(i); j = i - 1
          do while (j >= 1)
             if (.not. key_gt(kids(j), t)) exit
             kids(j+1) = kids(j); j = j - 1
          end do
          kids(j+1) = t
       end do
       call jw_begin_obj(w)
       do i = 1, n
          call jw_key(w, jname(kids(i)))
          call jcanon_value(w, kids(i))
       end do
       call jw_end_obj(w)
    end select
  end subroutine jcanon_value

  logical function key_gt(x, y)
    integer, intent(in) :: x, y
    character(len=:), allocatable :: a, b
    integer :: i, ca, cb
    a = jname(x); b = jname(y)
    do i = 1, min(len(a), len(b))
       ca = iachar(a(i:i)); cb = iachar(b(i:i))
       if (ca < 0) ca = ca + 256
       if (cb < 0) cb = cb + 256
       if (ca /= cb) then
          key_gt = (ca > cb); return
       end if
    end do
    key_gt = (len(a) > len(b))
  end function key_gt

end module fx_json
