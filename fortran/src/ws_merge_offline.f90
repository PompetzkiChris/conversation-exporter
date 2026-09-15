! ws_merge_offline — WebSocket tool-result merge parity tool: ws_merge_offline TRANSCRIPT.json WS-HISTORY.json OUT.json
program ws_merge_offline
  use fx_util
  use fx_json
  use fx_grok
  implicit none
  character(len=:), allocatable :: text, out
  logical :: ok
  integer :: n
  call grok_merge_ws_results(read_file(arg_utf8(1), ok), jparse(read_file(arg_utf8(2), ok)), out, n)
  call write_file(arg_utf8(3), out)
  call log_line('merged '//itoa(n))
end program ws_merge_offline