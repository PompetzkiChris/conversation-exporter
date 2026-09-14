! behavior_offline — behavior-report parity tool (Fortran side of racket/behavior.rkt).
!   behavior_offline TRANSCRIPT.json OUTDIR
! writes OUTDIR\behavior-report.json and OUTDIR\behavior-report.md and prints the manifest record.
program behavior_offline
  use iso_fortran_env, only: int64
  use fx_util
  use fx_json
  use fx_behavior
  implicit none
  character(len=4096) :: a
  character(len=:), allocatable :: tp, od, text
  logical :: ok
  integer :: t
  integer(int64) :: t0
  type(behavior_result) :: res
  type(jw) :: w
  call get_command_argument(1, a); tp = trim(a)
  call get_command_argument(2, a); od = trim(a)
  t0 = now_ms()
  text = read_file(tp, ok)
  if (.not. ok) then
     call log_line('ERROR: cannot read ' // tp); stop 2
  end if
  t = jparse(text)
  call behavior_analyze(t, res)
  call mkdir_p(od)
  if (res%ok) then
     call write_file(od // '\behavior-report.json', res%json)
     call write_file(od // '\behavior-report.md', res%markdown)
  else
     call write_file(od // '\error.txt', res%error)
  end if
  if (res%ok) then
     call behavior_write_manifest_record(w, res, 'ok', now_ms() - t0)
  else
     call behavior_write_manifest_record(w, res, 'error', now_ms() - t0)
  end if
  call log_line(jw_result(w))
  if (.not. res%ok) stop 1
end program behavior_offline
