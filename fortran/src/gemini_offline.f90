! gemini_offline — parse saved hNvQHb page files into transcript.json (parity test with Racket)
! usage: gemini_offline OUT.json SOURCE_URL TITLE PAGE1.txt [PAGE2.txt ...]
program gemini_offline
  use fx_util
  use fx_json
  use fx_gemini
  implicit none
  character(len=4096) :: a
  character(len=:), allocatable :: out, src, title, raw, text
  integer, allocatable :: pays(:)
  type(turn_summary), allocatable :: sm(:)
  integer :: n, i, np, nt
  logical :: ok
  integer(8) :: t0
  n = command_argument_count()
  if (n < 4) then
     call log_line('usage: gemini_offline OUT.json SOURCE_URL TITLE PAGE1.txt [...]'); stop 1
  end if
  call get_command_argument(1, a); out = trim(a)
  call get_command_argument(2, a); src = trim(a)
  call get_command_argument(3, a); title = trim(a)
  allocate(pays(n - 3)); np = 0
  t0 = now_ms()
  do i = 4, n
     call get_command_argument(i, a)
     raw = read_file(trim(a), ok)
     if (.not. ok) then
        call log_line('cannot read '//trim(a)); stop 1
     end if
     np = np + 1
     pays(np) = gemini_parse_batchexecute(raw)
     if (.not. pays(np) > 0) then
        call log_line('no hNvQHb payload in '//trim(a)); stop 2
     end if
  end do
  call log_line('parse ms '//i64toa(now_ms()-t0)); t0 = now_ms()
  call gemini_transcript(pays, np, src, title, text, sm, nt)
  call log_line('transcript ms '//i64toa(now_ms()-t0))
  call write_file(out, text)
  call log_line('turns '//itoa(nt)//', bytes '//itoa(len(text)))
end program gemini_offline
