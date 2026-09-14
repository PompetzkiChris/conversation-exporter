! md_offline — transcript.md parity tool: md_offline TRANSCRIPT.json OUT.md
program md_offline
  use fx_util
  use fx_json
  use fx_md
  implicit none
  character(len=4096) :: a, b
  logical :: ok
  call get_command_argument(1, a); call get_command_argument(2, b)
  call write_file(trim(b), transcript_markdown(jparse(read_file(trim(a), ok))))
end program md_offline