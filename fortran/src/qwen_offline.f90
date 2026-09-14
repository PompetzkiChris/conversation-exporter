! qwen_offline — /api/v2/chats/<id> response file -> transcript.json (parity test with Racket)
! usage: qwen_offline OUT.json SOURCE_URL qwen-chat.json
program qwen_offline
  use fx_util
  use fx_json
  use fx_gemini, only: text_item
  use fx_qwen
  implicit none
  character(len=4096) :: a
  character(len=:), allocatable :: out, src, raw, text
  type(qturn), allocatable :: tt(:)
  type(text_item), allocatable :: offs(:)
  integer :: root, nt, noff, total
  logical :: ok
  call get_command_argument(1, a); out = trim(a)
  call get_command_argument(2, a); src = trim(a)
  call get_command_argument(3, a)
  raw = read_file(trim(a), ok)
  root = jparse(raw)
  if (root == 0) then
     call log_line('cannot parse'); stop 2
  end if
  call qwen_transcript(root, src, text, tt, nt, offs, noff, total)
  call write_file(out, text)
  call log_line('turns '//itoa(nt)//', off-branch '//itoa(noff)//', messages '//itoa(total)//', bytes '//itoa(len(text)))
end program qwen_offline
