! verify_offline — Grok verifier / citation-enrichment parity tool.
!   verify_offline verify TRANSCRIPT.json CAPTURE.json OUT.json TOOL [ATTACHMENTS_DIR]
!   verify_offline enrich TRANSCRIPT-API.json CAPTURE.json CHIP-LINKS.json|- OUT.json
program verify_offline
  use fx_util
  use fx_json
  use fx_verify
  implicit none
  character(len=4096) :: a
  character(len=:), allocatable :: mode, tp, cp, out, tool, att, text, tname, cname, lp
  type(word), allocatable :: notes(:)
  logical :: ok
  integer :: t, cap, nf, nt, k, links, nn
  integer(8) :: t0
  call get_command_argument(1, a); mode = trim(a)
  call get_command_argument(2, a); tp = trim(a)
  call get_command_argument(3, a); cp = trim(a)
  t0 = now_ms()
  t = jparse(read_file(tp, ok))
  cap = jparse(read_file(cp, ok))
  if (mode == 'strip') then
     call get_command_argument(4, a); out = trim(a)
     call write_file(out, strip_env_text(cap))
  else if (mode == 'stable' .or. mode == 'consist') then
     call get_command_argument(4, a); out = trim(a)
     if (mode == 'stable') then
        k = index(tp, '\', back=.true.); tname = tp(k+1:)
        k = index(cp, '\', back=.true.); cname = cp(k+1:)
        text = stability_record(t, cap, tname, cname)
     else
        text = api_consistency_record(t, cap)
     end if
     block
       type(jw) :: w
       call jcanon_value(w, jparse(text)); call write_file(out, jw_result(w))
     end block
     call log_line('ms '//i64toa(now_ms() - t0))
  else if (mode == 'verify') then
     call get_command_argument(4, a); out = trim(a)
     call get_command_argument(5, a); tool = trim(a)
     call get_command_argument(6, a); att = trim(a)
     k = index(tp, '\', back=.true.); tname = tp(k+1:)
     k = index(cp, '\', back=.true.); cname = cp(k+1:)
     text = verify_capture(t, cap, att, tname, cname, tool, '', nf, nt)
     call write_file(out, text)
     call log_line('ms '//i64toa(now_ms() - t0)//', checks '//itoa(nt)//', failed '//itoa(nf))
  else
     call get_command_argument(4, a); lp = trim(a)
     call get_command_argument(5, a); out = trim(a)
     links = 0
     if (lp /= '-' .and. file_exists(lp)) links = jparse(read_file(lp, ok))
     call enrich_citations(t, cap, links, text, notes, nn)
     call write_file(out, text)
     do k = 1, nn
        call log_line('note: '//notes(k)%s)
     end do
     call log_line('ms '//i64toa(now_ms() - t0)//', notes '//itoa(nn))
  end if
end program verify_offline
