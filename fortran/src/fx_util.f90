! fx_util.f90 — growable strings, files, directories, time, JSON string decoding.
module fx_util
  use iso_fortran_env, only: int64, int32, int8, output_unit
  use iso_c_binding
  implicit none
  private
  public :: dir_exists, default_exports_dir, file_size, arg_count, arg_utf8, env_utf8, spawn_detached
  public :: strbuf, sb_add, sb_str, sb_clear, itoa, i64toa, read_file, write_file, file_exists, &
            mkdir_p, starts_with, ends_with, str_trim_ws, epoch_to_iso, now_iso_utc, now_ms, &
            json_unescape, exe_dir, log_line, replace_all, local_stamp, sleep_ms

  type, public :: strbuf
     character(len=:), allocatable :: s
     integer :: n = 0
  end type strbuf

  ! File system through the wide-character Windows API: names are UTF-8 in this program and UTF-16 on disk,
  ! so non-ASCII file names (attachments, titles) come out exactly as the Racket version writes them.
  interface
     function CreateDirectoryW(path, sa) bind(C, name='CreateDirectoryW')
       import :: c_int16_t, c_ptr, c_int
       integer(c_int16_t), intent(in) :: path(*)
       type(c_ptr), value :: sa
       integer(c_int) :: CreateDirectoryW
     end function
     function GetFileAttributesW(path) bind(C, name='GetFileAttributesW')
       import :: c_int16_t, c_int
       integer(c_int16_t), intent(in) :: path(*)
       integer(c_int) :: GetFileAttributesW
     end function
     function CreateFileW(path, access, share, sa, disposition, flags, templ) bind(C, name='CreateFileW')
       import :: c_int16_t, c_int, c_ptr, c_intptr_t
       integer(c_int16_t), intent(in) :: path(*)
       integer(c_int), value :: access, share, disposition, flags
       type(c_ptr), value :: sa, templ
       integer(c_intptr_t) :: CreateFileW
     end function
     function WriteFile(h, buf, n, done, ov) bind(C, name='WriteFile')
       import :: c_intptr_t, c_char, c_int, c_ptr
       integer(c_intptr_t), value :: h
       character(kind=c_char), intent(in) :: buf(*)
       integer(c_int), value :: n
       integer(c_int), intent(out) :: done
       type(c_ptr), value :: ov
       integer(c_int) :: WriteFile
     end function
     function ReadFile(h, buf, n, done, ov) bind(C, name='ReadFile')
       import :: c_intptr_t, c_char, c_int, c_ptr
       integer(c_intptr_t), value :: h
       character(kind=c_char), intent(inout) :: buf(*)
       integer(c_int), value :: n
       integer(c_int), intent(out) :: done
       type(c_ptr), value :: ov
       integer(c_int) :: ReadFile
     end function
     function GetFileSizeEx(h, sz) bind(C, name='GetFileSizeEx')
       import :: c_intptr_t, c_int64_t, c_int
       integer(c_intptr_t), value :: h
       integer(c_int64_t), intent(out) :: sz
       integer(c_int) :: GetFileSizeEx
     end function
     function CloseHandle(h) bind(C, name='CloseHandle')
       import :: c_intptr_t, c_int
       integer(c_intptr_t), value :: h
       integer(c_int) :: CloseHandle
     end function
     function MultiByteToWideChar(cp, fl, s, n, w, wn) bind(C, name='MultiByteToWideChar')
       import :: c_char, c_int16_t, c_int
       integer(c_int), value :: cp, fl, n, wn
       character(kind=c_char), intent(in) :: s(*)
       integer(c_int16_t), intent(inout) :: w(*)
       integer(c_int) :: MultiByteToWideChar
     end function
     function WideCharToMultiByte(cp, fl, w, wn, s, n, d, u) bind(C, name='WideCharToMultiByte')
       import :: c_int16_t, c_char, c_int, c_ptr
       integer(c_int), value :: cp, fl, wn, n
       integer(c_int16_t), intent(in) :: w(*)
       character(kind=c_char), intent(inout) :: s(*)
       type(c_ptr), value :: d, u
       integer(c_int) :: WideCharToMultiByte
     end function
     function GetCommandLineW() bind(C, name='GetCommandLineW')
       import :: c_ptr
       type(c_ptr) :: GetCommandLineW
     end function
     function CommandLineToArgvW(cmd, n) bind(C, name='CommandLineToArgvW')
       import :: c_ptr, c_int
       type(c_ptr), value :: cmd
       integer(c_int), intent(out) :: n
       type(c_ptr) :: CommandLineToArgvW
     end function
     function LocalFree(h) bind(C, name='LocalFree')
       import :: c_ptr
       type(c_ptr), value :: h
       type(c_ptr) :: LocalFree
     end function
     function lstrlenW(s) bind(C, name='lstrlenW')
       import :: c_ptr, c_int
       type(c_ptr), value :: s
       integer(c_int) :: lstrlenW
     end function
     function GetEnvironmentVariableW(name, buf, n) bind(C, name='GetEnvironmentVariableW')
       import :: c_int16_t, c_int
       integer(c_int16_t), intent(in) :: name(*)
       integer(c_int16_t), intent(inout) :: buf(*)
       integer(c_int), value :: n
       integer(c_int) :: GetEnvironmentVariableW
     end function
     function GetModuleFileNameW(h, buf, n) bind(C, name='GetModuleFileNameW')
       import :: c_ptr, c_int16_t, c_int
       type(c_ptr), value :: h
       integer(c_int16_t), intent(inout) :: buf(*)
       integer(c_int), value :: n
       integer(c_int) :: GetModuleFileNameW
     end function
     function CreateProcessW(app, cmd, pa, ta, inherit, flags, env, dir, si, pi) bind(C, name='CreateProcessW')
       import :: c_ptr, c_int, c_int16_t, c_int8_t
       type(c_ptr), value :: app, pa, ta, env, dir
       integer(c_int16_t), intent(inout) :: cmd(*)
       integer(c_int), value :: inherit, flags
       integer(c_int8_t), intent(inout) :: si(*), pi(*)
       integer(c_int) :: CreateProcessW
     end function
     subroutine Sleep(ms) bind(C, name='Sleep')
       import :: c_int32_t
       integer(c_int32_t), value :: ms
     end subroutine
  end interface

contains

  subroutine sb_add(b, t)
    type(strbuf), intent(inout) :: b
    character(len=*), intent(in) :: t
    integer :: need, cap
    character(len=:), allocatable :: tmp
    if (.not. allocated(b%s)) then
       allocate(character(len=max(4096, 2*len(t))) :: b%s)
       b%n = 0
    end if
    need = b%n + len(t)
    if (need > len(b%s)) then
       cap = max(need, 2*len(b%s))
       allocate(character(len=cap) :: tmp)
       if (b%n > 0) tmp(1:b%n) = b%s(1:b%n)
       call move_alloc(tmp, b%s)
    end if
    if (len(t) > 0) b%s(b%n+1:need) = t
    b%n = need
  end subroutine sb_add

  function sb_str(b) result(r)
    type(strbuf), intent(in) :: b
    character(len=:), allocatable :: r
    if (allocated(b%s)) then
       r = b%s(1:b%n)
    else
       r = ''
    end if
  end function sb_str

  subroutine sb_clear(b)
    type(strbuf), intent(inout) :: b
    b%n = 0
  end subroutine sb_clear

  function itoa(i) result(r)
    integer, intent(in) :: i
    character(len=:), allocatable :: r
    character(len=24) :: t
    write(t, '(I0)') i
    r = trim(t)
  end function itoa

  function i64toa(i) result(r)
    integer(int64), intent(in) :: i
    character(len=:), allocatable :: r
    character(len=24) :: t
    write(t, '(I0)') i
    r = trim(t)
  end function i64toa

  logical function starts_with(s, p)
    character(len=*), intent(in) :: s, p
    starts_with = .false.
    if (len(s) >= len(p)) starts_with = (s(1:len(p)) == p)
  end function starts_with

  logical function ends_with(s, p)
    character(len=*), intent(in) :: s, p
    ends_with = .false.
    if (len(s) >= len(p)) ends_with = (s(len(s)-len(p)+1:) == p)
  end function ends_with

  ! trims ASCII whitespace and U+00A0 (C2 A0) from both ends
  function str_trim_ws(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: a, z
    a = 1; z = len(s)
    do
       if (a > z) exit
       if (is_ws1(s(a:a))) then
          a = a + 1
       else if (a < z .and. s(a:a) == achar(194) .and. s(min(a+1,z):min(a+1,z)) == achar(160)) then
          a = a + 2
       else
          exit
       end if
    end do
    do
       if (z < a) exit
       if (is_ws1(s(z:z))) then
          z = z - 1
       else if (z > a .and. s(z:z) == achar(160) .and. s(z-1:z-1) == achar(194)) then
          z = z - 2
       else
          exit
       end if
    end do
    if (z >= a) then
       r = s(a:z)
    else
       r = ''
    end if
  contains
    logical function is_ws1(c)
      character, intent(in) :: c
      integer :: k
      k = iachar(c)
      is_ws1 = (k == 32 .or. (k >= 9 .and. k <= 13))
    end function is_ws1
  end function str_trim_ws

  function replace_all(s, a, b) result(r)
    character(len=*), intent(in) :: s, a, b
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    i = 1
    do while (i <= len(s))
       if (i + len(a) - 1 <= len(s)) then
          if (s(i:i+len(a)-1) == a) then
             call sb_add(o, b); i = i + len(a); cycle
          end if
       end if
       call sb_add(o, s(i:i)); i = i + 1
    end do
    r = sb_str(o)
  end function replace_all

  ! UTF-8 path -> NUL-terminated UTF-16, with the \\?\ prefix on absolute drive paths (no 260-character limit)
  subroutine wide_path(path, w)
    character(len=*), intent(in) :: path
    integer(c_int16_t), allocatable, intent(out) :: w(:)
    character(len=:), allocatable :: p
    integer(c_int) :: n
    integer(c_int16_t) :: dummy(1)
    integer :: i
    p = path
    do i = 1, len(p)
       if (p(i:i) == '/') p(i:i) = '\'
    end do
    if (len(p) >= 3) then
       if (p(2:3) == ':\') p = '\\?\'//p
    end if
    if (len(p) == 0) then
       allocate(w(1)); w(1) = 0; return
    end if
    n = MultiByteToWideChar(65001_c_int, 0_c_int, p, int(len(p), c_int), dummy, 0_c_int)
    allocate(w(n + 1))
    n = MultiByteToWideChar(65001_c_int, 0_c_int, p, int(len(p), c_int), w, n)
    w(n + 1) = 0
  end subroutine wide_path

  integer(c_int) function attributes(path)
    character(len=*), intent(in) :: path
    integer(c_int16_t), allocatable :: w(:)
    call wide_path(path, w)
    attributes = GetFileAttributesW(w)
  end function attributes

  logical function dir_exists(path)
    character(len=*), intent(in) :: path
    integer(c_int) :: a
    a = attributes(path)
    dir_exists = (a /= -1 .and. iand(a, 16_c_int) /= 0)
  end function dir_exists

  ! the author's machine keeps exports in the project tree; everyone else gets Documents\Exporter\exports
  function default_exports_dir() result(r)
    character(len=:), allocatable :: r
    character(len=1024) :: home
    if (dir_exists('C:\ClaudeOutput\grok-export-claude\exports')) then
       r = 'C:\ClaudeOutput\grok-export-claude\exports'
    else
       r = env_utf8('USERPROFILE')//'\Documents\Exporter\exports'
    end if
  end function default_exports_dir

  logical function file_exists(path)
    character(len=*), intent(in) :: path
    integer(c_int) :: a
    a = attributes(path)
    file_exists = (a /= -1 .and. iand(a, 16_c_int) == 0)
  end function file_exists

  ! size in bytes, -1 when the file cannot be opened
  integer(int64) function file_size(path)
    character(len=*), intent(in) :: path
    integer(c_int16_t), allocatable :: w(:)
    integer(c_intptr_t) :: h
    integer(c_int64_t) :: sz
    integer(c_int) :: r
    file_size = -1
    call wide_path(path, w)
    h = CreateFileW(w, int(z'80000000', c_int), 7_c_int, c_null_ptr, 3_c_int, 128_c_int, c_null_ptr)
    if (h == -1_c_intptr_t) return
    if (GetFileSizeEx(h, sz) /= 0) file_size = sz
    r = CloseHandle(h)
  end function file_size

  function read_file(path, ok) result(s)
    character(len=*), intent(in) :: path
    logical, intent(out) :: ok
    character(len=:), allocatable :: s
    integer(c_int16_t), allocatable :: w(:)
    integer(c_intptr_t) :: h
    integer(c_int64_t) :: sz
    integer(c_int) :: got, r
    integer(int64) :: pos, want
    ok = .false.
    call wide_path(path, w)
    h = CreateFileW(w, int(z'80000000', c_int), 7_c_int, c_null_ptr, 3_c_int, 128_c_int, c_null_ptr)
    if (h == -1_c_intptr_t) then
       s = ''; return
    end if
    if (GetFileSizeEx(h, sz) == 0) then
       r = CloseHandle(h); s = ''; return
    end if
    allocate(character(len=sz) :: s)
    pos = 0
    do while (pos < sz)
       want = min(sz - pos, 1073741824_int64)
       got = 0
       if (ReadFile(h, s(pos+1:pos+want), int(want, c_int), got, c_null_ptr) == 0 .or. got <= 0) exit
       pos = pos + got
    end do
    r = CloseHandle(h)
    ok = (pos == sz)
  end function read_file

  subroutine write_file(path, s)
    character(len=*), intent(in) :: path, s
    integer(c_int16_t), allocatable :: w(:)
    integer(c_intptr_t) :: h
    integer(c_int) :: done, r
    integer(int64) :: pos, want
    call wide_path(path, w)
    h = CreateFileW(w, int(z'40000000', c_int), 1_c_int, c_null_ptr, 2_c_int, 128_c_int, c_null_ptr)
    if (h == -1_c_intptr_t) then
       call log_line('ERROR: cannot write '//path); return
    end if
    pos = 0
    do while (pos < len(s, kind=int64))
       want = min(len(s, kind=int64) - pos, 1073741824_int64)
       done = 0
       if (WriteFile(h, s(pos+1:pos+want), int(want, c_int), done, c_null_ptr) == 0 .or. done <= 0) then
          call log_line('ERROR: short write to '//path); exit
       end if
       pos = pos + done
    end do
    r = CloseHandle(h)
  end subroutine write_file

  subroutine mkdir_p(path)
    character(len=*), intent(in) :: path
    integer :: i
    integer(c_int) :: rc
    integer(c_int16_t), allocatable :: w(:)
    do i = 3, len(path)
       if (path(i:i) == '\' .or. path(i:i) == '/' .or. i == len(path)) then
          if (i == len(path)) then
             call wide_path(path, w)
          else
             call wide_path(path(1:i-1), w)
          end if
          if (size(w) > 1) rc = CreateDirectoryW(w, c_null_ptr)
       end if
    end do
  end subroutine mkdir_p


  subroutine sleep_ms(ms)
    integer, intent(in) :: ms
    call Sleep(int(ms, c_int32_t))
  end subroutine sleep_ms

  ! days since 1970-01-01 -> civil date (Howard Hinnant's algorithm)
  subroutine civil_from_days(z0, y, m, d)
    integer(int64), intent(in) :: z0
    integer(int64), intent(out) :: y, m, d
    integer(int64) :: z, era, doe, yoe, doy, mp
    z = z0 + 719468_int64
    if (z >= 0) then
       era = z / 146097_int64
    else
       era = (z - 146096_int64) / 146097_int64
    end if
    doe = z - era * 146097_int64
    yoe = (doe - doe/1460_int64 + doe/36524_int64 - doe/146096_int64) / 365_int64
    y = yoe + era * 400_int64
    doy = doe - (365_int64*yoe + yoe/4_int64 - yoe/100_int64)
    mp = (5_int64*doy + 2_int64) / 153_int64
    d = doy - (153_int64*mp + 2_int64)/5_int64 + 1_int64
    if (mp < 10) then
       m = mp + 3
    else
       m = mp - 9
    end if
    if (m <= 2) y = y + 1
  end subroutine civil_from_days

  ! seconds + nanoseconds -> "YYYY-MM-DDTHH:MM:SS.mmmZ"
  function epoch_to_iso(sec, nanos) result(r)
    integer(int64), intent(in) :: sec, nanos
    character(len=:), allocatable :: r
    integer(int64) :: days, rem, y, mo, d, hh, mi, ss, ms
    character(len=32) :: t
    days = sec / 86400_int64
    rem = sec - days*86400_int64
    if (rem < 0) then
       rem = rem + 86400_int64; days = days - 1
    end if
    call civil_from_days(days, y, mo, d)
    hh = rem / 3600; mi = mod(rem, 3600_int64) / 60; ss = mod(rem, 60_int64)
    ms = nanos / 1000000_int64
    write(t, '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,".",I3.3,"Z")') y, mo, d, hh, mi, ss, ms
    r = trim(t)
  end function epoch_to_iso

  function now_ms() result(r)
    integer(int64) :: r
    integer(int64) :: cnt, rate
    call system_clock(cnt, rate)
    r = cnt * 1000_int64 / max(rate, 1_int64)
  end function now_ms

  function now_iso_utc() result(r)
    character(len=:), allocatable :: r
    integer :: v(8)
    integer(int64) :: days, secs
    call date_and_time(values=v)
    ! v(4) = minutes from UTC
    days = days_from_civil(int(v(1),int64), int(v(2),int64), int(v(3),int64))
    secs = days*86400_int64 + v(5)*3600_int64 + v(6)*60_int64 + v(7) - v(4)*60_int64
    r = epoch_to_iso(secs, int(v(8),int64)*1000000_int64)
  end function now_iso_utc

  function local_stamp() result(r)
    character(len=:), allocatable :: r
    integer :: v(8)
    character(len=20) :: t
    call date_and_time(values=v)
    write(t, '(I4.4,I2.2,I2.2,"-",I2.2,I2.2,I2.2)') v(1), v(2), v(3), v(5), v(6), v(7)
    r = trim(t)
  end function local_stamp

  integer(int64) function days_from_civil(y0, m, d)
    integer(int64), intent(in) :: y0, m, d
    integer(int64) :: y, era, yoe, doy, doe, mm
    y = y0
    if (m <= 2) y = y - 1
    if (y >= 0) then
       era = y / 400
    else
       era = (y - 399) / 400
    end if
    yoe = y - era*400
    if (m > 2) then
       mm = m - 3
    else
       mm = m + 9
    end if
    doy = (153*mm + 2)/5 + d - 1
    doe = yoe*365 + yoe/4 - yoe/100 + doy
    days_from_civil = era*146097 + doe - 719468
  end function days_from_civil

  ! Decode a JSON string body (without quotes) into UTF-8 bytes.
  function json_unescape(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i, cp, lo
    if (index(s, '\') == 0) then
       r = s; return
    end if
    i = 1
    do while (i <= len(s))
       if (s(i:i) /= '\' .or. i == len(s)) then
          call sb_add(o, s(i:i)); i = i + 1; cycle
       end if
       select case (s(i+1:i+1))
       case ('"');  call sb_add(o, '"')
       case ('\');  call sb_add(o, '\')
       case ('/');  call sb_add(o, '/')
       case ('b');  call sb_add(o, achar(8))
       case ('f');  call sb_add(o, achar(12))
       case ('n');  call sb_add(o, achar(10))
       case ('r');  call sb_add(o, achar(13))
       case ('t');  call sb_add(o, achar(9))
       case ('u')
          cp = hex4(s, i+2)
          i = i + 6
          if (cp >= int(z'D800') .and. cp <= int(z'DBFF') .and. i+5 <= len(s)) then
             if (s(i:i+1) == '\u') then
                lo = hex4(s, i+2)
                if (lo >= int(z'DC00') .and. lo <= int(z'DFFF')) then
                   cp = 65536 + (cp - int(z'D800'))*1024 + (lo - int(z'DC00'))
                   i = i + 6
                end if
             end if
          end if
          call put_utf8(o, cp)
          cycle
       case default
          call sb_add(o, s(i+1:i+1))
       end select
       i = i + 2
    end do
    r = sb_str(o)
  contains
    integer function hex4(t, p)
      character(len=*), intent(in) :: t
      integer, intent(in) :: p
      integer :: k, v, c
      v = 0
      do k = p, min(p+3, len(t))
         c = iachar(t(k:k))
         if (c >= 48 .and. c <= 57) then
            v = v*16 + c - 48
         else if (c >= 65 .and. c <= 70) then
            v = v*16 + c - 55
         else if (c >= 97 .and. c <= 102) then
            v = v*16 + c - 87
         end if
      end do
      hex4 = v
    end function hex4
  end function json_unescape

  subroutine put_utf8(o, cp)
    type(strbuf), intent(inout) :: o
    integer, intent(in) :: cp
    if (cp < 128) then
       call sb_add(o, achar(cp))
    else if (cp < 2048) then
       call sb_add(o, achar(192 + cp/64) // achar(128 + iand(cp, 63)))
    else if (cp < 65536) then
       call sb_add(o, achar(224 + cp/4096) // achar(128 + iand(cp/64, 63)) // achar(128 + iand(cp, 63)))
    else
       call sb_add(o, achar(240 + cp/262144) // achar(128 + iand(cp/4096, 63)) // &
                      achar(128 + iand(cp/64, 63)) // achar(128 + iand(cp, 63)))
    end if
  end subroutine put_utf8

  ! UTF-16 buffer -> UTF-8
  function utf8_of(w, n) result(s)
    integer(c_int16_t), intent(in) :: w(*)
    integer, intent(in) :: n
    character(len=:), allocatable :: s
    character(kind=c_char) :: dummy(1)
    integer(c_int) :: m
    s = ''
    if (n <= 0) return
    m = WideCharToMultiByte(65001_c_int, 0_c_int, w, int(n, c_int), dummy, 0_c_int, c_null_ptr, c_null_ptr)
    deallocate(s); allocate(character(len=m) :: s)
    m = WideCharToMultiByte(65001_c_int, 0_c_int, w, int(n, c_int), s, m, c_null_ptr, c_null_ptr)
  end function utf8_of

  function exe_dir() result(r)
    character(len=:), allocatable :: r
    integer(c_int16_t) :: buf(32768)
    integer(c_int) :: n
    integer :: k
    n = GetModuleFileNameW(c_null_ptr, buf, 32768_c_int)
    r = utf8_of(buf, int(n))
    k = index(r, '\', back=.true.)
    if (k > 0) then
       r = r(1:k-1)
    else
       r = '.'
    end if
  end function exe_dir

  integer function arg_count()
    type(c_ptr) :: argv, t
    integer(c_int) :: n
    argv = CommandLineToArgvW(GetCommandLineW(), n)
    arg_count = max(0, int(n) - 1)
    t = LocalFree(argv)
  end function arg_count

  ! command-line argument i (1-based) as UTF-8; '' when absent
  function arg_utf8(i) result(r)
    integer, intent(in) :: i
    character(len=:), allocatable :: r
    type(c_ptr) :: argv, t
    type(c_ptr), pointer :: ptrs(:)
    integer(c_int16_t), pointer :: w(:)
    integer(c_int) :: n, l
    r = ''
    argv = CommandLineToArgvW(GetCommandLineW(), n)
    if (.not. c_associated(argv)) return
    if (i >= 0 .and. i < n) then
       call c_f_pointer(argv, ptrs, [n])
       l = lstrlenW(ptrs(i+1))
       call c_f_pointer(ptrs(i+1), w, [max(l,1)])
       r = utf8_of(w, int(l))
    end if
    t = LocalFree(argv)
  end function arg_utf8

  function env_utf8(name) result(r)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: r
    integer(c_int16_t), allocatable :: wn(:)
    integer(c_int16_t) :: buf(32768)
    integer(c_int) :: n
    call wide_name(name, wn)
    n = GetEnvironmentVariableW(wn, buf, 32768_c_int)
    r = ''
    if (n > 0 .and. n < 32768) r = utf8_of(buf, int(n))
  end function env_utf8

  subroutine wide_name(s, w)
    character(len=*), intent(in) :: s
    integer(c_int16_t), allocatable, intent(out) :: w(:)
    integer :: i
    allocate(w(len(s) + 1))
    do i = 1, len(s)
       w(i) = int(iachar(s(i:i)), c_int16_t)
    end do
    w(len(s) + 1) = 0
  end subroutine wide_name

  ! start a program detached (UTF-8 command line), no console window; .true. when it started
  logical function spawn_detached(cmdline) result(ok)
    character(len=*), intent(in) :: cmdline
    integer(c_int16_t), allocatable :: w(:)
    integer(c_int16_t) :: dummy(1)
    integer(c_int8_t) :: si(104), pi(24)
    integer(c_int) :: n, r
    integer(c_intptr_t) :: hp, ht
    n = MultiByteToWideChar(65001_c_int, 0_c_int, cmdline, int(len(cmdline), c_int), dummy, 0_c_int)
    allocate(w(n + 1))
    n = MultiByteToWideChar(65001_c_int, 0_c_int, cmdline, int(len(cmdline), c_int), w, n)
    w(n + 1) = 0
    si = 0; pi = 0
    si(1:4) = transfer(104_c_int32_t, si(1:4))
    r = CreateProcessW(c_null_ptr, w, c_null_ptr, c_null_ptr, 0_c_int, int(z'00000208', c_int), c_null_ptr, c_null_ptr, si, pi)
    ok = (r /= 0)
    if (ok) then
       hp = transfer(pi(1:8), hp); ht = transfer(pi(9:16), ht)
       r = CloseHandle(hp); r = CloseHandle(ht)
    end if
  end function spawn_detached

  subroutine log_line(msg)
    character(len=*), intent(in) :: msg
    write(output_unit, '(A)') msg
    flush(output_unit)
  end subroutine log_line

end module fx_util
