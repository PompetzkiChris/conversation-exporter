program crypto_test
  use fx_util
  use fx_net
  implicit none
  character(len=:), allocatable :: s
  logical :: ok
  character(len=4096) :: a
  call log_line('abc   '//sha256_hex('abc'))
  call log_line('empty '//sha256_hex(''))
  call log_line('b64   '//base64_decode('aGVsbG8gd29ybGQ='))
  call get_command_argument(1, a)
  s = read_file(trim(a), ok)
  call log_line('file  '//sha256_hex(s))
end program crypto_test
