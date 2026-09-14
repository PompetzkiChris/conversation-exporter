! fx_gui.f90 — "Exporter - Herr Pompetzki und Signore Amodei (lol) · Fortran": native Win32 window.
! Top bar: link box, Export, API-only switch, Open folder, Open HTML.  Left: every export on disk, newest first.
! Right: the selected record (verification summary + transcript.md), or the live log while an export runs.
! The export itself runs exporter-f.exe as a child process; its output streams into the reading pane.
module fx_gui_mod
  use iso_c_binding
  use fx_util
  use fx_json
  implicit none
  private
  public :: gui_main

  integer, parameter :: HP = c_intptr_t
  integer(c_int), parameter :: WM_DESTROY = 2, WM_SIZE = 5, WM_SETTEXT = 12, WM_CLOSE = 16, WM_SETFONT = 48, &
       WM_COMMAND = 273, WM_TIMER = 275, WM_GETMINMAXINFO = 36
  integer(c_int), parameter :: LB_ADDSTRING = 384, LB_RESETCONTENT = 388, LB_SETCURSEL = 390, LB_GETCURSEL = 392
  integer(c_int), parameter :: EM_SETSEL = 177, EM_REPLACESEL = 194, EM_SETLIMITTEXT = 197, BM_GETCHECK = 240
  integer(c_int), parameter :: ID_URL = 101, ID_EXPORT = 102, ID_LIST = 103, ID_VIEW = 104, ID_STATUS = 105, &
       ID_SKIPDOM = 106, ID_FOLDER = 107, ID_HTML = 108, ID_TIMER = 1
  integer(c_int), parameter :: WS_CHILD = int(z'40000000', c_int), WS_VISIBLE = int(z'10000000', c_int), &
       WS_BORDER = int(z'00800000', c_int), WS_VSCROLL = int(z'00200000', c_int), WS_HSCROLL = int(z'00100000', c_int), &
       WS_TABSTOP = int(z'00010000', c_int), WS_OVERLAPPEDWINDOW = int(z'00CF0000', c_int), &
       WS_EX_CLIENTEDGE = int(z'00000200', c_int)
  integer(c_int), parameter :: ES_MULTILINE = 4, ES_AUTOVSCROLL = 64, ES_AUTOHSCROLL = 128, ES_READONLY = 2048, &
       LBS_NOTIFY = 1, LBS_NOINTEGRALHEIGHT = 256, BS_DEFPUSHBUTTON = 1, BS_AUTOCHECKBOX = 3, SS_LEFTNOWORDWRAP = 12
  integer(c_int), parameter :: CP_UTF8 = 65001

  type, bind(C) :: wndclassexw
     integer(c_int) :: cbSize, style
     type(c_funptr) :: lpfnWndProc
     integer(c_int) :: cbClsExtra, cbWndExtra
     type(c_ptr) :: hInstance, hIcon, hCursor, hbrBackground, lpszMenuName, lpszClassName, hIconSm
  end type wndclassexw

  type, bind(C) :: msg_t
     type(c_ptr) :: hwnd
     integer(c_int) :: message
     integer(c_intptr_t) :: wParam, lParam
     integer(c_int) :: time, ptx, pty, lPrivate
  end type msg_t

  type, bind(C) :: security_attributes
     integer(c_int) :: nLength
     type(c_ptr) :: lpSecurityDescriptor
     integer(c_int) :: bInheritHandle
  end type security_attributes

  type, bind(C) :: startupinfow
     integer(c_int) :: cb
     type(c_ptr) :: lpReserved, lpDesktop, lpTitle
     integer(c_int) :: dwX, dwY, dwXSize, dwYSize, dwXCountChars, dwYCountChars, dwFillAttribute, dwFlags
     integer(c_short) :: wShowWindow, cbReserved2
     type(c_ptr) :: lpReserved2, hStdInput, hStdOutput, hStdError
  end type startupinfow

  type, bind(C) :: process_information
     type(c_ptr) :: hProcess, hThread
     integer(c_int) :: dwProcessId, dwThreadId
  end type process_information

  type, bind(C) :: win32_find_dataa
     integer(c_int) :: dwFileAttributes
     integer(c_int) :: ft(6)
     integer(c_int) :: nFileSizeHigh, nFileSizeLow, dwReserved0, dwReserved1
     character(kind=c_char) :: cFileName(260)
     character(kind=c_char) :: cAlternateFileName(14)
  end type win32_find_dataa

  type, bind(C) :: rect_t
     integer(c_int) :: left, top, right, bottom
  end type rect_t

  interface
     function GetModuleHandleW(n) bind(C, name='GetModuleHandleW')
       import :: c_ptr; type(c_ptr), value :: n; type(c_ptr) :: GetModuleHandleW
     end function
     function RegisterClassExW(wc) bind(C, name='RegisterClassExW')
       import :: wndclassexw, c_short; type(wndclassexw), intent(in) :: wc; integer(c_short) :: RegisterClassExW
     end function
     function CreateWindowExW(ex, cls, title, style, x, y, w, h, parent, menu, inst, param) bind(C, name='CreateWindowExW')
       import :: c_ptr, c_int
       integer(c_int), value :: ex, style, x, y, w, h
       type(c_ptr), value :: cls, title, parent, menu, inst, param
       type(c_ptr) :: CreateWindowExW
     end function
     function DefWindowProcW(h, m, w, l) bind(C, name='DefWindowProcW')
       import :: c_ptr, c_int, c_intptr_t
       type(c_ptr), value :: h; integer(c_int), value :: m; integer(c_intptr_t), value :: w, l
       integer(c_intptr_t) :: DefWindowProcW
     end function
     function SendMessageW(h, m, w, l) bind(C, name='SendMessageW')
       import :: c_ptr, c_int, c_intptr_t
       type(c_ptr), value :: h; integer(c_int), value :: m; integer(c_intptr_t), value :: w, l
       integer(c_intptr_t) :: SendMessageW
     end function
     function GetMessageW(m, h, a, b) bind(C, name='GetMessageW')
       import :: msg_t, c_ptr, c_int
       type(msg_t) :: m; type(c_ptr), value :: h; integer(c_int), value :: a, b; integer(c_int) :: GetMessageW
     end function
     function IsDialogMessageW(h, m) bind(C, name='IsDialogMessageW')
       import :: msg_t, c_ptr, c_int
       type(c_ptr), value :: h; type(msg_t) :: m; integer(c_int) :: IsDialogMessageW
     end function
     function TranslateMessage(m) bind(C, name='TranslateMessage')
       import :: msg_t, c_int; type(msg_t) :: m; integer(c_int) :: TranslateMessage
     end function
     function DispatchMessageW(m) bind(C, name='DispatchMessageW')
       import :: msg_t, c_intptr_t; type(msg_t) :: m; integer(c_intptr_t) :: DispatchMessageW
     end function
     subroutine PostQuitMessage(code) bind(C, name='PostQuitMessage')
       import :: c_int; integer(c_int), value :: code
     end subroutine
     function ShowWindow(h, cmd) bind(C, name='ShowWindow')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int), value :: cmd; integer(c_int) :: ShowWindow
     end function
     function MoveWindow(h, x, y, w, hh, rep) bind(C, name='MoveWindow')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int), value :: x, y, w, hh, rep; integer(c_int) :: MoveWindow
     end function
     function GetClientRect(h, r) bind(C, name='GetClientRect')
       import :: c_ptr, c_int, rect_t; type(c_ptr), value :: h; type(rect_t) :: r; integer(c_int) :: GetClientRect
     end function
     function EnableWindow(h, en) bind(C, name='EnableWindow')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int), value :: en; integer(c_int) :: EnableWindow
     end function
     function GetWindowTextLengthW(h) bind(C, name='GetWindowTextLengthW')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int) :: GetWindowTextLengthW
     end function
     function GetWindowTextW(h, buf, n) bind(C, name='GetWindowTextW')
       import :: c_ptr, c_int; type(c_ptr), value :: h, buf; integer(c_int), value :: n; integer(c_int) :: GetWindowTextW
     end function
     function SetTimer(h, id, ms, fn) bind(C, name='SetTimer')
       import :: c_ptr, c_int, c_intptr_t
       type(c_ptr), value :: h, fn; integer(c_intptr_t), value :: id; integer(c_int), value :: ms; integer(c_intptr_t) :: SetTimer
     end function
     function KillTimer(h, id) bind(C, name='KillTimer')
       import :: c_ptr, c_int, c_intptr_t
       type(c_ptr), value :: h; integer(c_intptr_t), value :: id; integer(c_int) :: KillTimer
     end function
     function LoadCursorW(inst, name) bind(C, name='LoadCursorW')
       import :: c_ptr; type(c_ptr), value :: inst, name; type(c_ptr) :: LoadCursorW
     end function
     function LoadIconW(inst, name) bind(C, name='LoadIconW')
       import :: c_ptr; type(c_ptr), value :: inst, name; type(c_ptr) :: LoadIconW
     end function
     function GetSysColorBrush(i) bind(C, name='GetSysColorBrush')
       import :: c_ptr, c_int; integer(c_int), value :: i; type(c_ptr) :: GetSysColorBrush
     end function
     function SetProcessDPIAware() bind(C, name='SetProcessDPIAware')
       import :: c_int; integer(c_int) :: SetProcessDPIAware
     end function
     function GetDpiForWindow(h) bind(C, name='GetDpiForWindow')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int) :: GetDpiForWindow
     end function
     function CreateFontW(h, w, esc, ori, wt, it, ul, so, cs, op, cp, q, pf, face) bind(C, name='CreateFontW')
       import :: c_ptr, c_int
       integer(c_int), value :: h, w, esc, ori, wt, it, ul, so, cs, op, cp, q, pf
       type(c_ptr), value :: face
       type(c_ptr) :: CreateFontW
     end function
     function MultiByteToWideChar(cp, fl, s, n, w, wn) bind(C, name='MultiByteToWideChar')
       import :: c_ptr, c_int
       integer(c_int), value :: cp, fl, n, wn; type(c_ptr), value :: s, w; integer(c_int) :: MultiByteToWideChar
     end function
     function WideCharToMultiByte(cp, fl, w, wn, s, n, d, u) bind(C, name='WideCharToMultiByte')
       import :: c_ptr, c_int
       integer(c_int), value :: cp, fl, wn, n; type(c_ptr), value :: w, s, d, u; integer(c_int) :: WideCharToMultiByte
     end function
     function CreatePipe(r, w, sa, sz) bind(C, name='CreatePipe')
       import :: c_ptr, c_int, security_attributes
       type(c_ptr) :: r, w; type(security_attributes) :: sa; integer(c_int), value :: sz; integer(c_int) :: CreatePipe
     end function
     function SetHandleInformation(h, mask, flags) bind(C, name='SetHandleInformation')
       import :: c_ptr, c_int
       type(c_ptr), value :: h; integer(c_int), value :: mask, flags; integer(c_int) :: SetHandleInformation
     end function
     function CreateProcessW(app, cmd, pa, ta, inherit, flags, env, dir, si, pi) bind(C, name='CreateProcessW')
       import :: c_ptr, c_int, startupinfow, process_information
       type(c_ptr), value :: app, cmd, pa, ta, env, dir
       integer(c_int), value :: inherit, flags
       type(startupinfow) :: si; type(process_information) :: pi
       integer(c_int) :: CreateProcessW
     end function
     function CloseHandle(h) bind(C, name='CloseHandle')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int) :: CloseHandle
     end function
     function PeekNamedPipe(h, buf, sz, rd, avail, left) bind(C, name='PeekNamedPipe')
       import :: c_ptr, c_int
       type(c_ptr), value :: h, buf, rd, left; integer(c_int), value :: sz; integer(c_int) :: avail; integer(c_int) :: PeekNamedPipe
     end function
     function ReadFile(h, buf, n, got, ov) bind(C, name='ReadFile')
       import :: c_ptr, c_int
       type(c_ptr), value :: h, buf, ov; integer(c_int), value :: n; integer(c_int) :: got; integer(c_int) :: ReadFile
     end function
     function WaitForSingleObject(h, ms) bind(C, name='WaitForSingleObject')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int), value :: ms; integer(c_int) :: WaitForSingleObject
     end function
     function GetExitCodeProcess(h, code) bind(C, name='GetExitCodeProcess')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int) :: code; integer(c_int) :: GetExitCodeProcess
     end function
     function FindFirstFileA(pat, fd) bind(C, name='FindFirstFileA')
       import :: c_char, c_ptr, win32_find_dataa
       character(kind=c_char) :: pat(*); type(win32_find_dataa) :: fd; type(c_ptr) :: FindFirstFileA
     end function
     function FindNextFileA(h, fd) bind(C, name='FindNextFileA')
       import :: c_ptr, c_int, win32_find_dataa
       type(c_ptr), value :: h; type(win32_find_dataa) :: fd; integer(c_int) :: FindNextFileA
     end function
     function FindClose(h) bind(C, name='FindClose')
       import :: c_ptr, c_int; type(c_ptr), value :: h; integer(c_int) :: FindClose
     end function
     function ShellExecuteW(h, op, file, params, dir, show) bind(C, name='ShellExecuteW')
       import :: c_ptr, c_int
       type(c_ptr), value :: h, op, file, params, dir; integer(c_int), value :: show; type(c_ptr) :: ShellExecuteW
     end function
  end interface

  type :: record
     character(len=:), allocatable :: dir, label
  end type record

  type(c_ptr), save :: hinst, hmain, hurl, hexport, hlist, hview, hstatus, hskip, hfolder, hhtml, hfont, hmono
  type(record), allocatable, save :: recs(:)
  integer, save :: nrecs = 0, dpi = 96
  character(len=:), allocatable, save :: exports_root, exporter_exe, current_dir, pending_log
  logical, save :: running = .false.
  type(c_ptr), save :: child_proc = c_null_ptr, child_out = c_null_ptr
  type(strbuf), save :: runlog

contains

  ! ------------------------------------------------------------------ strings

  ! UTF-8 -> NUL-terminated UTF-16 (target array)
  subroutine to_wide(s, w)
    character(len=*), intent(in) :: s
    integer(c_int16_t), allocatable, target, intent(out) :: w(:)
    character(kind=c_char), allocatable, target :: b(:)
    integer(c_int) :: n
    integer :: i
    allocate(b(max(len(s), 1)))
    do i = 1, len(s)
       b(i) = s(i:i)
    end do
    if (len(s) == 0) then
       allocate(w(1)); w(1) = 0; return
    end if
    n = MultiByteToWideChar(CP_UTF8, 0_c_int, c_loc(b), int(len(s), c_int), c_null_ptr, 0_c_int)
    allocate(w(n + 1))
    n = MultiByteToWideChar(CP_UTF8, 0_c_int, c_loc(b), int(len(s), c_int), c_loc(w), n)
    w(n + 1) = 0
  end subroutine to_wide

  function from_wide(w, n) result(s)
    integer(c_int16_t), target, intent(in) :: w(:)
    integer, intent(in) :: n
    character(len=:), allocatable :: s
    character(kind=c_char), allocatable, target :: b(:)
    integer(c_int) :: m
    integer :: i
    s = ''
    if (n <= 0) return
    m = WideCharToMultiByte(CP_UTF8, 0_c_int, c_loc(w), int(n, c_int), c_null_ptr, 0_c_int, c_null_ptr, c_null_ptr)
    allocate(b(m))
    m = WideCharToMultiByte(CP_UTF8, 0_c_int, c_loc(w), int(n, c_int), c_loc(b), m, c_null_ptr, c_null_ptr)
    deallocate(s); allocate(character(len=m) :: s)
    do i = 1, m
       s(i:i) = b(i)
    end do
  end function from_wide

  function crlf(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    r = replace_all(replace_all(s, achar(13)//achar(10), achar(10)), achar(10), achar(13)//achar(10))
  end function crlf

  function window_text(h) result(s)
    type(c_ptr), intent(in) :: h
    character(len=:), allocatable :: s
    integer(c_int16_t), allocatable, target :: w(:)
    integer(c_int) :: n
    n = GetWindowTextLengthW(h)
    allocate(w(n + 1))
    n = GetWindowTextW(h, c_loc(w), n + 1)
    s = from_wide(w, int(n))
  end function window_text

  subroutine set_text(h, s)
    type(c_ptr), intent(in) :: h
    character(len=*), intent(in) :: s
    integer(c_int16_t), allocatable, target :: w(:)
    integer(HP) :: r
    call to_wide(s, w)
    r = SendMessageW(h, WM_SETTEXT, 0_HP, transfer(c_loc(w), 0_HP))
  end subroutine set_text

  subroutine append_text(h, s)
    type(c_ptr), intent(in) :: h
    character(len=*), intent(in) :: s
    integer(c_int16_t), allocatable, target :: w(:)
    integer(HP) :: r
    call to_wide(s, w)
    r = SendMessageW(h, EM_SETSEL, -1_HP, -1_HP)
    r = SendMessageW(h, EM_REPLACESEL, 0_HP, transfer(c_loc(w), 0_HP))
  end subroutine append_text

  function wptr(s) result(p)          ! leaks a small wide buffer; used for class names and fixed labels only
    character(len=*), intent(in) :: s
    type(c_ptr) :: p
    integer(c_int16_t), pointer :: w(:)
    integer(c_int16_t), allocatable, target :: tmp(:)
    call to_wide(s, tmp)
    allocate(w(size(tmp)))
    w = tmp
    p = c_loc(w)
  end function wptr

  integer function px(v)
    integer, intent(in) :: v
    px = v * dpi / 96
  end function px

  ! ------------------------------------------------------------------ records

  subroutine load_records()
    type(win32_find_dataa) :: fd
    type(c_ptr) :: h
    type(record), allocatable :: tmp(:)
    character(len=:), allocatable :: name
    integer :: i, j, k
    integer(c_int) :: more
    type(record) :: sw
    if (allocated(recs)) deallocate(recs)
    allocate(recs(128)); nrecs = 0
    h = FindFirstFileA(exports_root//'\*'//c_null_char, fd)
    if (transfer(h, 0_HP) == -1_HP) return
    do
       if (iand(fd%dwFileAttributes, 16) /= 0) then
          name = ''
          do k = 1, 260
             if (fd%cFileName(k) == c_null_char) exit
             name = name//fd%cFileName(k)
          end do
          if (name /= '.' .and. name /= '..' .and. file_exists(exports_root//'\'//name//'\manifest.json')) then
             if (nrecs >= size(recs)) then
                allocate(tmp(2*nrecs)); tmp(1:nrecs) = recs(1:nrecs); call move_alloc(tmp, recs)
             end if
             nrecs = nrecs + 1
             recs(nrecs)%dir = name
             recs(nrecs)%label = record_label(name)
          end if
       end if
       more = FindNextFileA(h, fd)
       if (more == 0) exit
    end do
    more = FindClose(h)
    do i = 2, nrecs                      ! newest first (names start with the local timestamp)
       sw = recs(i); j = i - 1
       do while (j >= 1)
          if (.not. llt(recs(j)%dir, sw%dir)) exit
          recs(j+1) = recs(j); j = j - 1
       end do
       recs(j+1) = sw
    end do
  end subroutine load_records

  function record_label(name) result(r)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: r, title, site, stamp, txt
    logical :: ok
    integer :: t
    site = 'grok'
    if (index(name, '-gemini-') > 0) site = 'gemini'
    if (index(name, '-qwen-') > 0) site = 'qwen'
    stamp = name
    if (len(name) >= 15) stamp = name(1:4)//'-'//name(5:6)//'-'//name(7:8)//' '//name(10:11)//':'//name(12:13)
    title = ''
    txt = read_file(exports_root//'\'//name//'\transcript.json', ok)
    if (ok .and. len(txt) > 0) then
       t = jparse(txt)
       if (jis_str(jget(jget(t, 'conversation'), 'title'))) title = jstr(jget(jget(t, 'conversation'), 'title'))
    end if
    if (len(title) == 0) title = name(min(len(name), 17):)
    if (len(title) > 90) title = title(1:90)
    r = stamp//'   '//site//'   '//title
  end function record_label

  subroutine fill_list(select_dir)
    character(len=*), intent(in) :: select_dir
    integer :: i, sel
    integer(HP) :: r
    integer(c_int16_t), allocatable, target :: w(:)
    call load_records()
    r = SendMessageW(hlist, LB_RESETCONTENT, 0_HP, 0_HP)
    sel = -1
    do i = 1, nrecs
       call to_wide(recs(i)%label, w)
       r = SendMessageW(hlist, LB_ADDSTRING, 0_HP, transfer(c_loc(w), 0_HP))
       if (len(select_dir) > 0 .and. recs(i)%dir == select_dir) sel = i - 1
    end do
    if (sel >= 0) then
       r = SendMessageW(hlist, LB_SETCURSEL, int(sel, HP), 0_HP)
       call show_record(sel + 1)
    end if
    call set_text(hstatus, itoa(nrecs)//' exports in '//exports_root)
  end subroutine fill_list

  subroutine show_record(i)
    integer, intent(in) :: i
    character(len=:), allocatable :: d, txt, md
    type(strbuf) :: o
    logical :: ok
    integer :: v, f, m
    if (i < 1 .or. i > nrecs) return
    d = exports_root//'\'//recs(i)%dir
    current_dir = d
    call sb_add(o, recs(i)%label//achar(10))
    call sb_add(o, 'Folder: '//d//achar(10))
    txt = read_file(d//'\manifest.json', ok)
    if (ok) then
       m = jparse(txt)
       call sb_add(o, 'Source: '//jstr(jget(m, 'finalUrl'))//achar(10))
       call sb_add(o, 'Made by: '//jstr(jget(m, 'tool'))//' '//jstr(jget(m, 'version'))//', exit code '// &
                      jraw_text(jget(m, 'exitCode'))//achar(10))
    end if
    txt = read_file(d//'\verification.json', ok)
    if (ok) then
       v = jparse(txt)
       call sb_add(o, 'Verification: '//jraw_text(jget(jget(v, 'summary'), 'total'))//' checks, '// &
                      jraw_text(jget(jget(v, 'summary'), 'failed'))//' failed; API '//jstr(jget(v, 'api'))//'; page '// &
                      jstr(jget(v, 'dom'))//achar(10))
       f = jfirst(jget(v, 'failedChecks'))
       do while (f > 0)
          call sb_add(o, '  FAILED '//jstr(jget(f, 'name'))//' turn '//jraw_text(jget(f, 'turnIndex'))//achar(10))
          f = jnext(f)
       end do
       f = jfirst(jget(v, 'warnings'))
       do while (f > 0)
          call sb_add(o, '  ! '//jstr(f)//achar(10))
          f = jnext(f)
       end do
    end if
    call sb_add(o, repeat('-', 100)//achar(10))
    md = read_file(d//'\transcript.md', ok)
    if (ok) then
       call sb_add(o, md)
    else
       call sb_add(o, '(no transcript.md in this export)'//achar(10))
    end if
    call set_text(hview, crlf(sb_str(o)))
    call EnableWin(hhtml, file_exists(d//'\transcript.html'))
    call EnableWin(hfolder, .true.)
  end subroutine show_record

  subroutine EnableWin(h, on)
    type(c_ptr), intent(in) :: h
    logical, intent(in) :: on
    integer(c_int) :: r
    if (on) then
       r = EnableWindow(h, 1_c_int)
    else
       r = EnableWindow(h, 0_c_int)
    end if
  end subroutine EnableWin

  ! ------------------------------------------------------------------ export run

  subroutine start_export()
    character(len=:), allocatable :: url, cmd
    type(security_attributes) :: sa
    type(startupinfow) :: si
    type(process_information) :: pi
    type(c_ptr) :: rd, wr
    integer(c_int16_t), allocatable, target :: wcmd(:)
    integer(c_int) :: ok
    integer(HP) :: r
    if (running) return
    url = str_trim_ws(window_text(hurl))
    if (len(url) == 0) then
       call set_text(hstatus, 'Paste a grok.com, gemini.google.com or chat.qwen.ai link first'); return
    end if
    cmd = '"'//exporter_exe//'" "'//url//'"'
    if (SendMessageW(hskip, BM_GETCHECK, 0_HP, 0_HP) == 1_HP) cmd = cmd//' --skip-dom'
    sa%nLength = int(c_sizeof(sa), c_int); sa%lpSecurityDescriptor = c_null_ptr; sa%bInheritHandle = 1
    ok = CreatePipe(rd, wr, sa, 0_c_int)
    if (ok == 0) then
       call set_text(hstatus, 'could not create the output pipe'); return
    end if
    ok = SetHandleInformation(rd, 1_c_int, 0_c_int)        ! the read end stays in this process
    si%cb = int(c_sizeof(si), c_int)
    si%lpReserved = c_null_ptr; si%lpDesktop = c_null_ptr; si%lpTitle = c_null_ptr; si%lpReserved2 = c_null_ptr
    si%dwFlags = 257                                        ! STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW
    si%wShowWindow = 0
    si%hStdInput = c_null_ptr; si%hStdOutput = wr; si%hStdError = wr
    call to_wide(cmd, wcmd)
    ok = CreateProcessW(c_null_ptr, c_loc(wcmd), c_null_ptr, c_null_ptr, 1_c_int, int(z'08000000', c_int), &
                        c_null_ptr, c_null_ptr, si, pi)
    ok = ok
    if (CloseHandle(wr) == 0) continue
    if (.not. c_associated(pi%hProcess)) then
       ok = CloseHandle(rd)
       call set_text(hstatus, 'could not start '//exporter_exe); return
    end if
    ok = CloseHandle(pi%hThread)
    child_proc = pi%hProcess; child_out = rd
    running = .true.
    call sb_clear(runlog); pending_log = ''
    call set_text(hview, crlf('Exporting '//url//achar(10)//achar(10)))
    call set_text(hstatus, 'Exporting…')
    call EnableWin(hexport, .false.)
    r = SetTimer(hmain, int(ID_TIMER, HP), 100_c_int, c_null_ptr)
  end subroutine start_export

  subroutine poll_export()
    integer(c_int) :: avail, got, ok, code
    character(kind=c_char), allocatable, target :: buf(:)
    character(len=:), allocatable :: chunk, dir
    integer :: i, k
    integer(HP) :: r
    if (.not. running) return
    do
       avail = 0
       ok = PeekNamedPipe(child_out, c_null_ptr, 0_c_int, c_null_ptr, avail, c_null_ptr)
       if (ok == 0 .or. avail <= 0) exit
       allocate(buf(avail))
       got = 0
       ok = ReadFile(child_out, c_loc(buf), avail, got, c_null_ptr)
       allocate(character(len=max(got,0)) :: chunk)
       do i = 1, got
          chunk(i:i) = buf(i)
       end do
       deallocate(buf)
       pending_log = pending_log//chunk
       call sb_add(runlog, chunk)
       deallocate(chunk)
       k = index(pending_log, achar(10), back=.true.)       ! show whole lines only (no split UTF-8 sequences)
       if (k > 0) then
          call append_text(hview, crlf(pending_log(1:k)))
          pending_log = pending_log(k+1:)
       end if
    end do
    if (WaitForSingleObject(child_proc, 0_c_int) /= 0) return
    ok = GetExitCodeProcess(child_proc, code)
    if (len(pending_log) > 0) call append_text(hview, crlf(pending_log))
    ok = CloseHandle(child_proc); ok = CloseHandle(child_out)
    running = .false.
    r = KillTimer(hmain, int(ID_TIMER, HP))
    call EnableWin(hexport, .true.)
    dir = ''
    chunk = sb_str(runlog)
    k = index(chunk, 'EXPORT_DIR=', back=.true.)
    if (k > 0) then
       dir = chunk(k+11:)
       i = scan(dir, achar(13)//achar(10))
       if (i > 0) dir = dir(1:i-1)
       i = index(dir, '\', back=.true.)
       if (i > 0) dir = dir(i+1:)
    end if
    if (len(dir) > 0) then
       call fill_list(dir)
       call append_text(hview, '')
    end if
    if (code == 0) then
       call set_text(hstatus, 'Finished, exit code 0 (all checks passed)')
    else
       call set_text(hstatus, 'Finished, exit code '//itoa(int(code))//' (see the verification lines at the top)')
    end if
  end subroutine poll_export

  ! ------------------------------------------------------------------ window

  subroutine layout()
    type(rect_t) :: rc
    integer(c_int) :: r, w, h, top, lw
    r = GetClientRect(hmain, rc)
    w = rc%right; h = rc%bottom
    top = px(10)
    r = MoveWindow(hurl, px(10), top, max(px(100), w - px(560)), px(30), 1_c_int)
    r = MoveWindow(hexport, w - px(540), top, px(100), px(30), 1_c_int)
    r = MoveWindow(hskip, w - px(430), top, px(120), px(30), 1_c_int)
    r = MoveWindow(hfolder, w - px(300), top, px(140), px(30), 1_c_int)
    r = MoveWindow(hhtml, w - px(150), top, px(140), px(30), 1_c_int)
    lw = min(px(520), w / 3)
    r = MoveWindow(hlist, px(10), px(50), lw, h - px(84), 1_c_int)
    r = MoveWindow(hview, px(20) + lw, px(50), w - lw - px(30), h - px(84), 1_c_int)
    r = MoveWindow(hstatus, px(10), h - px(28), w - px(20), px(22), 1_c_int)
  end subroutine layout

  recursive function wndproc(hwnd, msg, wparam, lparam) bind(C) result(res)
    type(c_ptr), value :: hwnd
    integer(c_int), value :: msg
    integer(c_intptr_t), value :: wparam, lparam
    integer(c_intptr_t) :: res
    integer :: id, code, sel
    type(c_ptr) :: h
    res = 0
    select case (msg)
    case (WM_SIZE)
       if (c_associated(hlist)) call layout()
    case (WM_COMMAND)
       id = int(iand(wparam, 65535_HP)); code = int(iand(ishft(wparam, -16), 65535_HP))
       select case (id)
       case (1, ID_EXPORT)          ! Enter in the link box arrives as IDOK
          call start_export()
       case (ID_LIST)
          if (code == 1 .and. .not. running) then
             sel = int(SendMessageW(hlist, LB_GETCURSEL, 0_HP, 0_HP))
             if (sel >= 0) call show_record(sel + 1)
          end if
       case (ID_FOLDER)
          if (allocated(current_dir)) h = ShellExecuteW(hwnd, wptr('open'), wptr(current_dir), c_null_ptr, c_null_ptr, 1_c_int)
       case (ID_HTML)
          if (allocated(current_dir)) h = ShellExecuteW(hwnd, wptr('open'), wptr(current_dir//'\transcript.html'), &
                                                        c_null_ptr, c_null_ptr, 1_c_int)
       end select
    case (WM_TIMER)
       call poll_export()
    case (WM_DESTROY)
       call PostQuitMessage(0_c_int)
    case default
       res = DefWindowProcW(hwnd, msg, wparam, lparam)
    end select
  end function wndproc

  function child(cls, text, style, id, exstyle) result(h)
    character(len=*), intent(in) :: cls, text
    integer(c_int), intent(in) :: style, id
    integer(c_int), intent(in), optional :: exstyle
    type(c_ptr) :: h
    integer(c_int) :: ex
    integer(HP) :: r
    ex = 0
    if (present(exstyle)) ex = exstyle
    h = CreateWindowExW(ex, wptr(cls), wptr(text), ior(ior(WS_CHILD, WS_VISIBLE), style), 0, 0, 10, 10, hmain, &
                        transfer(int(id, HP), c_null_ptr), hinst, c_null_ptr)
    r = SendMessageW(h, WM_SETFONT, transfer(hfont, 0_HP), 1_HP)
  end function child

  subroutine gui_main()
    type(wndclassexw), target :: wc
    type(msg_t) :: m
    integer(c_int) :: r
    integer(HP) :: rr
    character(len=*), parameter :: TITLE = 'Exporter - Herr Pompetzki und Signore Amodei (lol) '//achar(194)//achar(183)//' Fortran'
    r = SetProcessDPIAware()
    hinst = GetModuleHandleW(c_null_ptr)
    exporter_exe = exe_dir()//'\exporter-f.exe'
    exports_root = default_exports_dir()
    call mkdir_p(exports_root)
    wc%cbSize = int(c_sizeof(wc), c_int); wc%style = 3
    wc%lpfnWndProc = c_funloc(wndproc)
    wc%cbClsExtra = 0; wc%cbWndExtra = 0
    wc%hInstance = hinst
    wc%hIcon = LoadIconW(c_null_ptr, transfer(32512_HP, c_null_ptr))
    wc%hCursor = LoadCursorW(c_null_ptr, transfer(32512_HP, c_null_ptr))
    wc%hbrBackground = GetSysColorBrush(15_c_int)
    wc%lpszMenuName = c_null_ptr
    wc%lpszClassName = wptr('FxExporterWindow')
    wc%hIconSm = wc%hIcon
    if (RegisterClassExW(wc) == 0) return
    hmain = CreateWindowExW(0_c_int, wptr('FxExporterWindow'), wptr(TITLE), WS_OVERLAPPEDWINDOW, &
                            int(z'80000000', c_int), int(z'80000000', c_int), 1600, 1000, c_null_ptr, c_null_ptr, hinst, c_null_ptr)
    dpi = GetDpiForWindow(hmain)
    if (dpi <= 0) dpi = 96
    r = MoveWindow(hmain, px(60), px(40), px(1500), px(950), 1_c_int)
    hfont = CreateFontW(-px(16), 0_c_int, 0_c_int, 0_c_int, 400_c_int, 0_c_int, 0_c_int, 0_c_int, 1_c_int, 0_c_int, 0_c_int, &
                        5_c_int, 0_c_int, wptr('Segoe UI'))
    hmono = CreateFontW(-px(15), 0_c_int, 0_c_int, 0_c_int, 400_c_int, 0_c_int, 0_c_int, 0_c_int, 1_c_int, 0_c_int, 0_c_int, &
                        5_c_int, 0_c_int, wptr('Consolas'))
    hurl = child('EDIT', '', ior(ior(WS_BORDER, WS_TABSTOP), ES_AUTOHSCROLL), ID_URL, WS_EX_CLIENTEDGE)
    hexport = child('BUTTON', 'Export', ior(WS_TABSTOP, BS_DEFPUSHBUTTON), ID_EXPORT)
    hskip = child('BUTTON', 'API only', ior(WS_TABSTOP, BS_AUTOCHECKBOX), ID_SKIPDOM)
    hfolder = child('BUTTON', 'Open folder', WS_TABSTOP, ID_FOLDER)
    hhtml = child('BUTTON', 'Open HTML', WS_TABSTOP, ID_HTML)
    hlist = child('LISTBOX', '', ior(ior(ior(WS_BORDER, WS_VSCROLL), ior(LBS_NOTIFY, LBS_NOINTEGRALHEIGHT)), WS_TABSTOP), &
                  ID_LIST, WS_EX_CLIENTEDGE)
    hview = child('EDIT', '', ior(ior(ior(WS_BORDER, WS_VSCROLL), ior(WS_HSCROLL, ES_MULTILINE)), ior(ES_READONLY, ES_AUTOVSCROLL)), &
                  ID_VIEW, WS_EX_CLIENTEDGE)
    rr = SendMessageW(hview, WM_SETFONT, transfer(hmono, 0_HP), 1_HP)
    rr = SendMessageW(hview, EM_SETLIMITTEXT, 0_HP, 0_HP)
    hstatus = child('STATIC', '', SS_LEFTNOWORDWRAP, ID_STATUS)
    call EnableWin(hfolder, .false.); call EnableWin(hhtml, .false.)
    call layout()
    call fill_list('')
    call set_text(hview, crlf('Paste a link from grok.com, gemini.google.com or chat.qwen.ai above and press Export.'// &
                              achar(10)//'Pick an export on the left to read it.'//achar(10)))
    r = ShowWindow(hmain, 1_c_int)
    do while (GetMessageW(m, c_null_ptr, 0_c_int, 0_c_int) > 0)
       if (IsDialogMessageW(hmain, m) /= 0) cycle
       r = TranslateMessage(m)
       rr = DispatchMessageW(m)
    end do
  end subroutine gui_main

end module fx_gui_mod

program exporter_f_gui
  use fx_gui_mod
  implicit none
  call gui_main()
end program exporter_f_gui
