! html_offline — transcript.html parity tool for fx_html (port of racket/html.rkt).
!   html_offline TRANSCRIPT.json OUT.html [MANIFEST.json|-] [GENERATOR]
!     attachment paths come from MANIFEST.json "attachments": [{"fileId": ..., "path": ...}, ...]
!     (entries whose fileId and path are both strings); GENERATOR defaults to "grok-export-rkt 0.2.0",
!     what grok-export.rkt write-transcript-outputs! passes.
!   html_offline --pretty IN.txt OUT.txt      pretty_json_string of the whole file (md.rkt parity probe)
program html_offline
  use fx_util
  use fx_json
  use fx_html
  implicit none
  character(len=4096) :: a
  character(len=:), allocatable :: tpath, out, mpath, gen, raw, text
  type(strbuf) :: ob
  integer :: t, m, e, fid, pth, att, n
  logical :: ok

  call get_command_argument(1, a); tpath = trim(a)
  call get_command_argument(2, a); out = trim(a)
  if (tpath == '--pretty') then
     call get_command_argument(3, a)
     raw = read_file(out, ok)
     call write_file(trim(a), pretty_json_string(raw))
  else
     call render()
  end if

contains

  subroutine render()
  mpath = '-'
  if (command_argument_count() >= 3) then
     call get_command_argument(3, a); mpath = trim(a)
  end if
  gen = 'grok-export-rkt 0.2.0'
  if (command_argument_count() >= 4) then
     call get_command_argument(4, a); gen = trim(a)
  end if

  raw = read_file(tpath, ok)
  if (.not. ok) then
     call log_line('cannot read '//tpath); stop 2
  end if
  t = jparse(raw)

  ! attachment paths as one JSON object fileId -> path
  call sb_add(ob, '{')
  n = 0
  if (mpath /= '-') then
     raw = read_file(mpath, ok)
     if (ok) then
        m = jparse(raw)
        e = jfirst(jget(m, 'attachments'))
        if (.not. jis_arr(jget(m, 'attachments'))) e = 0
        do while (e > 0)
           fid = jget(e, 'fileId'); pth = jget(e, 'path')
           if (jis_str(fid) .and. jis_str(pth)) then
              if (n > 0) call sb_add(ob, ',')
              call sb_add(ob, jquote(jstr(fid))//':'//jquote(jstr(pth)))
              n = n + 1
           end if
           e = jnext(e)
        end do
     end if
  end if
  call sb_add(ob, '}')
  att = jparse(sb_str(ob))

  call transcript_html(t, att, text, gen)
  call write_file(out, text)
  end subroutine render
end program html_offline
