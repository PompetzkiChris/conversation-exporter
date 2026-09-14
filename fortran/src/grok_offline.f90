! grok_offline — Grok transcript parity tool.
!   grok_offline share OUT.json SOURCE_URL payload.json            (share / fixture payload as-is)
!   grok_offline conv  OUT.json SOURCE_URL EXPORT_RAW_DIR CONV_ID  (conversation-lane selection from saved raw files)
program grok_offline
  use fx_util
  use fx_json
  use fx_grok
  implicit none
  character(len=4096) :: a
  character(len=:), allocatable :: mode, out, src, path, raw, text, dir, cid, suffix
  integer :: d, meta, legacy, chunk, loaded, chosen, conv
  logical :: ok
  integer(8) :: t0
  call get_command_argument(1, a); mode = trim(a)
  call get_command_argument(2, a); out = trim(a)
  call get_command_argument(3, a); src = trim(a)
  if (src == '-') src = ''
  t0 = now_ms()
  if (mode == 'share') then
     call get_command_argument(4, a); path = trim(a)
     raw = read_file(path, ok)
     d = jparse(raw)
     call grok_transcript(d, jget(d, 'conversation'), '', src, text)
  else
     call get_command_argument(4, a); dir = trim(a)
     call get_command_argument(5, a); cid = trim(a)
     call get_command_argument(6, a); suffix = trim(a)
     meta = load(dir//'\api-conversation'//suffix//'.json')
     legacy = load(dir//'\api-responses'//suffix//'.json')
     chunk = load(dir//'\api-responses_chunk'//suffix//'.json')
     loaded = load(dir//'\api-load-responses'//suffix//'.json')
     chosen = 0
     if (has_responses(chunk) .and. grok_format(chunk) == 'chunk') then
        chosen = chunk
     else if (has_responses(legacy)) then
        chosen = legacy
     else if (has_responses(chunk)) then
        chosen = chunk
     else if (has_responses(loaded)) then
        chosen = loaded
     end if
     if (chosen == 0) then
        call log_line('no responses'); stop 2
     end if
     if (jis_obj(meta) .and. jis_obj(jget(meta, 'conversation'))) then
        conv = jget(meta, 'conversation')
     else if (jis_obj(meta)) then
        conv = meta
     else if (jis_obj(jget(chosen, 'conversation'))) then
        conv = jget(chosen, 'conversation')
     else
        conv = 0
     end if
     call grok_transcript(chosen, conv, cid, src, text)
  end if
  call write_file(out, text)
  call log_line('ms '//i64toa(now_ms() - t0)//', bytes '//itoa(len(text)))
contains
  integer function load(p)
    character(len=*), intent(in) :: p
    character(len=:), allocatable :: s
    logical :: okk
    load = 0
    if (.not. file_exists(p)) return
    s = read_file(p, okk)
    if (okk) load = jparse(s)
  end function load
  logical function has_responses(j)
    integer, intent(in) :: j
    has_responses = .false.
    if (jis_obj(j)) has_responses = (jlen(jget(j, 'responses')) > 0)
  end function has_responses
end program grok_offline
