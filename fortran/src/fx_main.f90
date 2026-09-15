! fx_main.f90 — "Exporter (Fortran)": command-line exporter.
!   exporter-f <url> [--out DIR] [--port N] [--timeout SEC] [--no-launch] [--skip-dom] [--keep-open]
! Output layout and file formats match the Racket app, so its Records reader opens Fortran exports.
program exporter_f
  use iso_fortran_env, only: int64, compiler_version
  use fx_util
  use fx_json
  use fx_net
  use fx_cdp
  use fx_gemini
  use fx_qwen
  use fx_grok
  use fx_verify
  use fx_md
  use fx_html
  use fx_behavior
  implicit none

  character(len=*), parameter :: TOOL = 'exporter-f', VERSION = '0.1.0'
  character(len=:), allocatable :: CHROME
  character(len=:), allocatable :: url, out_dir, profile
  integer :: port, timeout_s
  logical :: no_launch, skip_dom, keep_open
  integer :: code
  type(cdp) :: c, app
  character(len=:), allocatable :: tab_id
  logical :: have_app = .false.
  integer :: owned_port = 0

  ! Grok lane state
  type :: pending_file
     character(len=:), allocatable :: rel, body
  end type pending_file
  type :: round_state
     integer :: k = 0
     logical :: finished = .false., ok = .false., shots = .false.
     integer :: cap = 0, last_seq = 0, nshots = 0
     integer(int64) :: t0 = 0
     character(len=:), allocatable :: error
  end type round_state
  type :: round_rec
     integer :: k = 0, cap = 0
     character(len=:), allocatable :: file, href, rec
  end type round_rec
  type :: file_rec
     character(len=:), allocatable :: path, sha
     integer(int64) :: size = 0
  end type file_rec
  type :: cand_rec
     character(len=:), allocatable :: path, fallback
     integer :: item = 0
  end type cand_rec
  character(len=*), parameter :: FILE_CONTAINER_KEYS = 'files builtFiles built_files outputFiles output_files artifacts assets'
  character(len=*), parameter :: FILE_NAME_KEYS = 'name fileName filename'
  character(len=*), parameter :: FILE_URL_KEYS = 'url downloadUrl download_url fileUrl file_url href uri'
  character(len=*), parameter :: FILE_PATH_KEYS = 'path filePath file_path'
  character(len=*), parameter :: FILE_MIME_KEYS = 'mimeType mime_type contentType content_type'
  character(len=*), parameter :: FILE_SIZE_KEYS = 'sizeBytes size_bytes size'
  character(len=*), parameter :: FILE_DATA_KEYS = 'contentBase64 content_base64 dataBase64 data_base64'
  type(file_rec), allocatable :: files(:)
  type(cand_rec), allocatable :: cands(:)
  type(text_item), allocatable :: notes_list(:)
  type(strbuf) :: reqs, att_list
  integer :: nfiles = 0, nreqs = 0, nnotes = 0, ncand = 0
  character(len=:), allocatable :: sandbox_json, enrich_json, citsrc_json, parsed_fmt, run_error
  integer(int64) :: dom_t0 = 0
  type(cdp) :: c2
  logical :: have_c2 = .false.
  character(len=:), allocatable :: tab2_id, export_dir, api_lane
  character(len=:), allocatable :: js_poll, js_b64, js_chips, js_head, js_tail, js_spoll, js_sres, js_extract, js_wshook, js_wsread
  integer :: ws_frames = 0          ! JSON array of the page's WebSocket history frames (0 = none)
  character(len=:), allocatable :: api_post_ids   ! ' id id ... ': X posts the transcript carries with an author
  type(text_item), allocatable :: gwarn(:)
  type(pending_file), allocatable :: pend(:)
  integer :: ngw = 0, npend = 0, nrounds = 0, chip_links1 = 0
  type(round_rec) :: rounds(8)
  type(text_item), allocatable :: shot_files(:)
  integer :: nshot_files = 0
  type(strbuf) :: att_paths          ! JSON object fileId -> relative path of the downloaded file
  integer :: natt_paths = 0
  type(behavior_result) :: beh
  character(len=16) :: beh_status = 'not_run'
  integer(int64) :: beh_ms = 0

  call parse_args()
  if (gemini_url_id(url) /= '') then
     code = run_gemini()
  else if (qwen_chat_id(url) /= '' .or. url == 'qwen' .or. url == 'qwen:current') then
     code = run_qwen()
  else
     code = run_grok()
  end if
  call log_line('exit code  : '//itoa(code))
  call exit(code)

contains

  ! Chrome for all users, for this user only, or 32-bit Program Files
  function find_chrome() result(r)
    character(len=:), allocatable :: r
    character(len=:), allocatable :: c1, c2, c3
    c1 = env_utf8('ProgramFiles')//'\Google\Chrome\Application\chrome.exe'
    c2 = env_utf8('LOCALAPPDATA')//'\Google\Chrome\Application\chrome.exe'
    c3 = env_utf8('ProgramFiles(x86)')//'\Google\Chrome\Application\chrome.exe'
    if (file_exists(c1)) then
       r = c1
    else if (file_exists(c2)) then
       r = c2
    else if (file_exists(c3)) then
       r = c3
    else
       r = 'C:\Program Files\Google\Chrome\Application\chrome.exe'
    end if
  end function find_chrome

  subroutine parse_args()
    character(len=:), allocatable :: a
    integer :: i, n, ios
    n = arg_count()
    url = ''; port = 9222; timeout_s = 600; no_launch = .false.; skip_dom = .false.; keep_open = .false.
    profile = env_utf8('LOCALAPPDATA')//'\GrokExportClaude\profile'
    CHROME = find_chrome()
    out_dir = default_exports_dir()
    i = 1
    do while (i <= n)
       a = arg_utf8(i)
       select case (a)
       case ('--out');      i = i + 1; out_dir = arg_utf8(i)
       case ('--port');     i = i + 1; a = arg_utf8(i); read(a, *, iostat=ios) port
       case ('--timeout');  i = i + 1; a = arg_utf8(i); read(a, *, iostat=ios) timeout_s
       case ('--profile');  i = i + 1; profile = arg_utf8(i)
       case ('--no-launch'); no_launch = .true.
       case ('--skip-dom');  skip_dom = .true.
       case ('--keep-open'); keep_open = .true.
       case ('--rounds');   i = i + 1          ! accepted for command-line compatibility with the Racket app
       case default
          if (len(url) == 0) url = str_trim_ws(a)
       end select
       i = i + 1
    end do
    if (len(url) == 0) then
       call log_line('usage: exporter-f <url> [--out DIR] [--port N] [--timeout SEC] [--no-launch] [--skip-dom] [--keep-open]')
       call exit(1)
    end if
  end subroutine parse_args

  function js_file(name) result(src)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: src
    logical :: ok
    src = read_file(exe_dir()//'\js\'//name, ok)
    if (.not. ok) src = read_file(exe_dir()//'\..\js\'//name, ok)
    if (.not. ok) then
       call log_line('ERROR: missing page script js\'//name); call exit(1)
    end if
  end function js_file

  logical function ensure_chrome()
    integer :: k
    ensure_chrome = cdp_version_ok(port)
    if (ensure_chrome .or. no_launch) return
    call log_line('Starting Chrome (profile '//profile//') on port '//itoa(port))
    if (.not. spawn_detached('"'//CHROME//'" --remote-debugging-port='//itoa(port)//' "--user-data-dir='// &
         profile//'" --no-first-run --no-default-browser-check --hide-crash-restore-bubble --disable-background-timer-throttling '// &
         '--disable-renderer-backgrounding --disable-backgrounding-occluded-windows --new-window')) then
       call log_line('ERROR: could not start '//CHROME); return
    end if
    do k = 1, 60
       call sleep_ms(500)
       if (cdp_version_ok(port)) then
          ensure_chrome = .true.; return
       end if
    end do
  end function ensure_chrome

  ! ---------------------------------------------------------------- Qwen
  ! Session order: the exporter's own Chrome tabs (ports 9222, 9333: navigated and scrolled freely), else the
  ! Qwen desktop app on 9223, read only (never navigated, scrolled, restarted or screenshotted: another
  ! person or agent may be using that window).

  integer function run_qwen() result(exit_code)
    character(len=:), allocatable :: id, app_ws, app_href, err, state_js, fetch_js, dom_js, dir, chat_url, raw, text, dom_mode
    character(len=:), allocatable :: started, owned_ws
    integer :: v, payload, dom, nt, noff, total, nfailed, nchecks, nwarn, i, root, k, ports(2), tr
    integer(int64) :: t0
    logical :: ok
    type(qturn), allocatable :: tt(:)
    type(text_item), allocatable :: offs(:), fnames(:), warns(:)
    integer, allocatable :: fidx(:)
    type(jw) :: wv, wchecks
    exit_code = 1
    started = now_iso_utc(); t0 = now_ms()
    call run_reset()
    state_js = js_file('qwen-state.js'); fetch_js = js_file('qwen-fetch.js'); dom_js = js_file('qwen-dom.js')
    have_app = .false.; app_href = ''
    call find_qwen_webview(9223, app_ws)
    if (len(app_ws) > 0) then
       if (cdp_connect(app, app_ws, 120000)) then
          v = cdp_eval(app, state_js, .false., err)
          if (v > 0) then
             have_app = jis_true(jget(v, 'signedIn'))
             app_href = jstr(jget(v, 'href'))
          end if
          if (.not. have_app) call cdp_disconnect(app)
       end if
    end if
    id = qwen_chat_id(url)
    if (len(id) == 0) id = qwen_chat_id(app_href)
    if (len(id) == 0) then
       call log_line('ERROR: no chat id: paste a https://chat.qwen.ai/c/<id> link, or open the chat in the Qwen app')
       if (have_app) call cdp_disconnect(app)
       return
    end if
    chat_url = 'https://chat.qwen.ai/c/'//id
    owned_port = 0
    if (.not. skip_dom) then
       ports = [9222, 9333]
       do k = 1, 2
          if (.not. cdp_version_ok(ports(k))) cycle
          call cdp_new_tab(ports(k), chat_url, tab_id, owned_ws, ok)
          if (.not. ok) cycle
          if (.not. cdp_connect(c, owned_ws, 1000000)) then
             call cdp_close_tab(ports(k), tab_id); cycle
          end if
          do i = 1, 41
             v = cdp_eval(c, state_js, .false., err)
             if (v > 0) then
                if (jis_true(jget(v, 'signedIn')) .and. jstr(jget(v, 'host')) == 'chat.qwen.ai' .and. &
                    jint(jget(v, 'assistants')) > 0) then
                   owned_port = ports(k); exit
                end if
             end if
             call sleep_ms(1000)
          end do
          if (owned_port > 0) exit
          call log_line('owned browser on port '//itoa(ports(k))//': not signed in or chat did not render')
          call cdp_disconnect(c); call cdp_close_tab(ports(k), tab_id)
       end do
    end if
    if (owned_port == 0 .and. .not. have_app) then
       call log_line('ERROR: no signed-in Qwen session (exporter browsers 9222/9333, Qwen app 9223)'); return
    end if
    if (owned_port > 0) then
       call log_line('Qwen session: exporter-owned browser, port '//itoa(owned_port))
    else
       call log_line('Qwen session: Qwen desktop app, port 9223 (read-only)')
    end if
    dir = out_dir//'\'//local_stamp()//'-qwen-'//id
    call mkdir_p(dir//'\raw')
    call log_line('Export directory: '//dir)
    if (owned_port > 0) then
       v = cdp_run_function(c, fetch_js, '{"id":'//jquote(id)//'}', err)
    else
       v = cdp_run_function(app, fetch_js, '{"id":'//jquote(id)//'}', err)
    end if
    raw = jstr(jget(v, 'text'))
    call emit(dir, 'raw\qwen-chat.json', raw)
    payload = jparse(raw)
    if (.not. jis_true(jget(payload, 'success'))) then
       call log_line('ERROR: Qwen API: HTTP '//i64toa(jint(jget(v, 'status')))//', no usable payload '//err)
       call close_qwen_sessions(); return
    end if
    call qwen_transcript(payload, chat_url, text, tt, nt, offs, noff, total)
    call emit(dir, 'raw\transcript-api.json', text)
    call emit(dir, 'transcript.json', text)
    call write_outputs(dir, text)
    call log_line('Qwen API: '//itoa(nt)//' turns on the current branch, '//itoa(total)//' messages in total')
    root = jparse(text)
    tr = jfirst(jget(root, 'turns'))
    do while (tr > 0)
       if (jis_obj(jget(tr, 'error'))) call note_add('turn '//i64toa(jint(jget(tr, 'index')))//': Qwen error '// &
            jstr(jget(jget(tr, 'error'), 'code'))//' (stage '//jstr(jget(jget(tr, 'error'), 'stage'))//'): '// &
            jstr(jget(jget(tr, 'error'), 'details')))
       tr = jnext(tr)
    end do
    dom = 0; dom_mode = 'skipped'
    if (.not. skip_dom) then
       if (owned_port > 0) then
          dom = cdp_run_function(c, dom_js, '{"scroll":true,"waitMs":1000,"stableRounds":8,"stepMs":700,"maxMs":900000}', err)
          dom_mode = 'ok (exporter-owned browser, scrolled)'
       else if (qwen_chat_id(app_href) == id) then
          dom = cdp_run_function(app, dom_js, '{"scroll":false}', err)
          dom_mode = 'ok (read-only snapshot of the Qwen app)'
       else
          call log_line('page check skipped: the Qwen app shows '//app_href//' and is never switched')
       end if
       if (dom > 0) then
          call jcanon_value(wv, dom)
          call emit(dir, 'raw\dom-qwen.json', jw_result(wv))
          call log_line('Qwen DOM: '//itoa(jlen(jget(dom, 'users')))//' user messages, '//itoa(jlen(jget(dom, 'assistants')))// &
                        ' replies seen ('//i64toa(jint(jget(dom, 'steps')))//' steps)')
       else
          dom_mode = 'skipped'
          if (len(err) > 0) call log_line('WARNING: DOM lane: '//err)
       end if
    end if
    export_dir = dir
    if (dom > 0) then
       if (owned_port > 0) then
          v = cdp_eval(c, 'document.documentElement.outerHTML', .false., err)
       else
          v = cdp_eval(app, 'document.documentElement.outerHTML', .false., err)
       end if
       if (jis_str(v)) call emit(dir, 'raw\page-initial.html', jstr(v))
    end if
    if (dom > 0 .and. owned_port > 0) ok = screenshot_to(c, dir, 'screenshots/transcript-full.png', .true.)
    nfailed = 0; nchecks = 0; nwarn = 0
    if (dom > 0) then
       call qwen_dom_checks(wchecks, tt, nt, root, dom, nfailed, fnames, fidx, nchecks, warns, nwarn)
    else
       allocate(fnames(1), fidx(1), warns(1))
    end if
    call write_verification(dir, dom > 0, dom_mode, 'raw/dom-qwen.json', wchecks, nchecks, nfailed, fnames, fidx, warns, nwarn)
    if (dom > 0 .and. nfailed == 0 .and. trim(beh_status) /= 'error') then
       exit_code = 0
    else
       exit_code = 2
    end if
    call write_manifest(dir, id, chat_url, 'qwen-api-v2', 'qwen-v2-chat', text, exit_code, started, t0, '')
    call log_line('total ms   : '//i64toa(now_ms() - t0))
    call log_line('EXPORT_DIR='//dir)
    call close_qwen_sessions()
  end function run_qwen

  subroutine close_qwen_sessions()
    if (owned_port > 0) then
       call cdp_disconnect(c)
       if (.not. keep_open) call cdp_close_tab(owned_port, tab_id)
    end if
    if (have_app) call cdp_disconnect(app)
  end subroutine close_qwen_sessions

  subroutine find_qwen_webview(p, ws)
    integer, intent(in) :: p
    character(len=:), allocatable, intent(out) :: ws
    integer :: st, root, t
    character(len=:), allocatable :: body, ty
    ws = ''
    call http_request(p, 'GET', '/json/list', st, body)
    if (st /= 200) return
    root = jparse(body)
    t = jfirst(root)
    do while (t > 0)
       ty = jstr(jget(t, 'type'))
       if ((ty == 'webview' .or. ty == 'page') .and. starts_with(jstr(jget(t, 'url')), 'https://chat.qwen.ai')) then
          ws = jstr(jget(t, 'webSocketDebuggerUrl')); return
       end if
       t = jnext(t)
    end do
  end subroutine find_qwen_webview

  ! verification.json for lanes whose checks come pre-rendered in `checks`
  subroutine write_verification(dir, dom_ok, dom_mode, capture, checks, nchecks, nfailed, fnames, fidx, warns, nwarn)
    character(len=*), intent(in) :: dir, dom_mode, capture
    logical, intent(in) :: dom_ok
    type(jw), intent(in) :: checks
    integer, intent(in) :: nchecks, nfailed, nwarn
    type(text_item), intent(in) :: fnames(:), warns(:)
    integer, intent(in) :: fidx(:)
    type(jw) :: w
    type(text_item), allocatable :: names(:)
    integer :: nn, i, j, root, ck, cnt, fc
    character(len=:), allocatable :: tmpn
    logical :: dup
    root = 0
    if (dom_ok) root = jparse(sb_str(checks%b))
    call jw_begin_obj(w)
    call jw_key(w, 'api'); call jw_str(w, 'ok')
    call jw_key(w, 'byName')
    call jw_begin_obj(w)
    if (dom_ok) then
       allocate(names(max(nchecks,1))); nn = 0
       ck = jfirst(root)
       do while (ck > 0)
          dup = .false.
          do i = 1, nn
             if (names(i)%s == jstr(jget(ck, 'name'))) dup = .true.
          end do
          if (.not. dup) then
             nn = nn + 1; names(nn)%s = jstr(jget(ck, 'name'))
          end if
          ck = jnext(ck)
       end do
       do i = 2, nn
          j = i
          do while (j > 1)
             if (.not. lgt(names(j-1)%s, names(j)%s)) exit
             tmpn = names(j)%s; names(j)%s = names(j-1)%s; names(j-1)%s = tmpn; j = j - 1
          end do
       end do
       do i = 1, nn
          cnt = 0; fc = 0
          ck = jfirst(root)
          do while (ck > 0)
             if (jstr(jget(ck, 'name')) == names(i)%s) then
                cnt = cnt + 1
                if (.not. jis_true(jget(ck, 'ok'))) fc = fc + 1
             end if
             ck = jnext(ck)
          end do
          call jw_key(w, names(i)%s)
          call jw_begin_obj(w)
          call jw_key(w, 'failed'); call jw_int(w, int(fc, int64))
          call jw_key(w, 'total'); call jw_int(w, int(cnt, int64))
          call jw_end_obj(w)
       end do
    end if
    call jw_end_obj(w)
    call jw_key(w, 'capture')
    if (dom_ok) then
       call jw_str(w, capture)
    else
       call jw_null(w)
    end if
    call jw_key(w, 'checks')
    if (dom_ok) then
       call jcanon_value(w, root)
    else
       call jw_begin_arr(w); call jw_end_arr(w)
    end if
    call jw_key(w, 'dom'); call jw_str(w, dom_mode)
    call jw_key(w, 'failedChecks')
    call jw_begin_arr(w)
    do i = 1, nfailed
       call jw_begin_obj(w)
       call jw_key(w, 'name'); call jw_str(w, fnames(i)%s)
       call jw_key(w, 'turnIndex')
       if (fidx(i) >= 0) then
          call jw_int(w, int(fidx(i), int64))
       else
          call jw_null(w)
       end if
       call jw_end_obj(w)
    end do
    call jw_end_arr(w)
    call jw_key(w, 'ok'); call jw_bool(w, dom_ok .and. nfailed == 0)
    call jw_key(w, 'rounds'); call jw_int(w, merge(1_int64, 0_int64, dom_ok))
    call jw_key(w, 'summary')
    call jw_begin_obj(w)
    call jw_key(w, 'failed'); call jw_int(w, int(nfailed, int64))
    call jw_key(w, 'ok'); call jw_int(w, int(nchecks - nfailed, int64))
    call jw_key(w, 'total'); call jw_int(w, int(nchecks, int64))
    call jw_end_obj(w)
    call jw_key(w, 'tool'); call jw_str(w, TOOL)
    call jw_key(w, 'transcript'); call jw_str(w, 'transcript.json')
    call jw_key(w, 'warnings')
    call jw_begin_arr(w)
    do i = 1, nwarn
       call jw_str(w, warns(i)%s)
       call log_line('  ! '//warns(i)%s)
    end do
    call jw_end_arr(w)
    call jw_end_obj(w)
    call emit(dir, 'verification.json', jw_result(w))
    call log_line('verification.json: '//itoa(nchecks)//' checks, '//itoa(nfailed)//' failed')
    do i = 1, nfailed
       call log_line('  FAILED '//fnames(i)%s//' turn '//itoa(fidx(i)))
    end do
  end subroutine write_verification

  ! ---------------------------------------------------------------- Grok
  ! Port of racket/grok-export.rkt run-live-grok: API lane (share_links or conversations, hydration re-fetch,
  ! late retry), attachments, DOM rounds 1 and 2 concurrently in two tabs (hydration redo), SPEC 5 verification
  ! with stability and api-consistency, DOM chip citation fallback.  Panel screenshots, transcript.md/html and the
  ! behavior report are not produced by the Fortran version.

  integer function run_grok() result(exit_code)
    character(len=:), allocatable :: u, wsurl, err, final_url, kind, gid, kind0, id0, dir, conv_id, text, text_final
    character(len=:), allocatable :: started, dom_status, api_status, extra, tool_s, vtext, tc_text, tl_text, reason, suffix
    integer :: data, chunk, legacy, conv, t, tchunk, tlegacy, i, v, nf, nt, nn, pass, vroot, f, r
    integer :: data2, chunk2, legacy2, conv2
    integer(int64) :: t0
    type(word), allocatable :: notes(:)
    type(text_item), allocatable :: fnames(:)
    integer, allocatable :: fidx(:)
    logical :: ok
    exit_code = 1
    started = now_iso_utc(); t0 = now_ms()
    call run_reset()
    npend = 0; have_c2 = .false.
    if (allocated(pend)) deallocate(pend)
    allocate(pend(16))
    js_poll = js_file('grok-poll.js'); js_b64 = js_file('grok-fetch-b64.js'); js_chips = js_file('grok-chip-links.js')
    js_head = js_file('grok-shot-start-head.js'); js_tail = js_file('grok-shot-start-tail.js')
    js_spoll = js_file('grok-shot-poll.js'); js_sres = js_file('grok-shot-result.js'); js_extract = js_file('extract.js')
    js_wshook = js_file('grok-ws-hook.js'); js_wsread = js_file('grok-ws-read.js'); ws_frames = 0; api_post_ids = ''
    u = grok_normalize(trim(url))
    if (len(u) == 0) then
       call log_line('ERROR: not a URL or conversation id: '//trim(url)); return
    end if
    call grok_classify(u, kind0, id0)
    if (.not. ensure_chrome()) then
       call log_line('ERROR: no Chrome DevTools endpoint on port '//itoa(port)); return
    end if
    call cdp_new_tab(port, 'about:blank', tab_id, wsurl, ok)
    if (.not. ok) then
       call log_line('ERROR: could not open a tab'); return
    end if
    if (.not. cdp_connect(c, wsurl, 1000000)) then
       call log_line('ERROR: WebSocket connection to the tab failed'); call finish(); return
    end if
    call prepare_tab(c)
    r = cdp_call(c, 'Page.addScriptToEvaluateOnNewDocument', '{"source":'//jquote(js_wshook)//'}')
    call log_line('Navigating to '//u)
    call navigate(c, u)
    if (.not. wait_conversation(c, final_url)) then
       call finish(); return
    end if
    call capture_ws_history()
    call grok_classify(final_url, kind, gid)
    ! API lane
    data = 0; chunk = 0; legacy = 0; conv = 0; tchunk = 0; tlegacy = 0; api_lane = 'unavailable'
    if (kind == 'share') then
       call api_lane_share(gid, '', data, chunk, legacy)
       if (chunk > 0) then
          call grok_transcript(chunk, jget(chunk, 'conversation'), '', final_url, tc_text); tchunk = jparse(tc_text)
       end if
       if (legacy > 0) then
          call grok_transcript(legacy, jget(legacy, 'conversation'), '', final_url, tl_text); tlegacy = jparse(tl_text)
       end if
       if (data > 0) conv = jget(data, 'conversation')
    else if (kind == 'conversation') then
       call api_lane_conversation(gid, '', data, conv)
    else
       call gwarn_add('final URL '//final_url//' is neither /share/ nor /c/; API lane unavailable')
    end if
    if (data == 0) then
       api_lane = 'unavailable'
       call gwarn_add('API lane unavailable: no endpoint returned a parsable payload with responses; continuing with the DOM lane only')
    end if
    conv_id = ''
    if (jis_str(jget(conv, 'conversationId'))) conv_id = jstr(jget(conv, 'conversationId'))
    if (len(conv_id) == 0 .and. kind == 'conversation') conv_id = gid
    if (len(conv_id) == 0 .and. kind0 == 'conversation') conv_id = id0
    if (len(conv_id) == 0) conv_id = gid
    if (len(conv_id) == 0) conv_id = id0
    if (len(conv_id) == 0) conv_id = 'unknown'
    dir = out_dir//'\'//local_stamp()//'-'//safe_file_name(conv_id)
    call mkdir_p(dir//'\raw')
    export_dir = dir
    call log_line('Export directory: '//dir)
    do i = 1, npend
       call emit(dir, ''//pend(i)%rel, pend(i)%body)
    end do
    npend = 0
    t = 0; text = ''
    if (data > 0) then
       if (kind == 'share') then
          call grok_transcript(data, jget(data, 'conversation'), '', final_url, text)
       else
          call grok_transcript(data, conv, gid, final_url, text)
       end if
       call merge_ws(text)
       t = jparse(text)
       api_post_ids = ' '; call collect_post_ids(t)
       parsed_fmt = grok_format(data)
       call record_sandbox(data, t)
       call emit(dir, 'transcript.json', text)
       call log_line('transcript.json: '//itoa(jlen(jget(t, 'turns')))//' turns ('//grok_format(data)//' format)')
    end if
    if (kind == 'conversation') then
       call query_app_deployment(gid, dir)
    else
       call record_deployment('{"endpoint":null,"queried":false,"reason":'//jquote(kind//' lane: app-deployments takes a conversation id')//'}')
    end if
    ! page HTML as loaded + full-page screenshot
    call capture_page(c, dir)
    ! attachments
    if (t > 0) call download_attachments(t, dir)
    ! DOM lane
    if (skip_dom) then
       call log_line('DOM lane skipped (--skip-dom)')
    else
       call dom_lane(u, dir)
    end if
    ! late API retry for a payload served with its X posts stripped
    if (data > 0 .and. len(xposts_stripped(data)) > 0) then
       do pass = 1, 3
          call log_line('API payload was served unhydrated ('//xposts_stripped(data)//'); retry pass '//itoa(pass)//'/3')
          suffix = '-retry'
          if (pass > 1) suffix = '-retry'//itoa(pass)
          data2 = 0; chunk2 = 0; legacy2 = 0; conv2 = 0
          if (kind == 'share') then
             call api_lane_share(gid, suffix, data2, chunk2, legacy2)
          else if (kind == 'conversation') then
             call api_lane_conversation(gid, suffix, data2, conv2)
          end if
          if (data2 > 0 .and. len(xposts_stripped(data2)) == 0) then
             call log_line('API retry pass '//itoa(pass)//' returned a hydrated payload; the transcript is rebuilt from raw/api-*'// &
                           suffix//'.json')
             if (kind == 'share') then
                tchunk = 0; tlegacy = 0
                if (chunk2 > 0) then
                   call grok_transcript(chunk2, jget(chunk2, 'conversation'), '', final_url, tc_text); tchunk = jparse(tc_text)
                end if
                if (legacy2 > 0) then
                   call grok_transcript(legacy2, jget(legacy2, 'conversation'), '', final_url, tl_text); tlegacy = jparse(tl_text)
                end if
                call grok_transcript(data2, jget(data2, 'conversation'), '', final_url, text)
             else
                call grok_transcript(data2, conv2, gid, final_url, text)
             end if
             call merge_ws(text)
             call note_add('API retry pass '//itoa(pass)//' returned a hydrated payload; the transcript is rebuilt from raw/api-*'// &
                           suffix//'.json (the first-pass payload is kept as raw/api-*.json)')
             data = data2; t = jparse(text); api_post_ids = ' '; call collect_post_ids(t); parsed_fmt = grok_format(data); call record_sandbox(data, t)
             call emit(dir, 'transcript.json', text)
             exit
          else if (pass < 3) then
             call gwarn_add('API retry pass '//itoa(pass)//': payload still unhydrated; next pass in 30 s')
             call sleep_ms(30000)
          else
             call gwarn_add('API retry pass 3: payload still unhydrated; the export keeps the first-pass payload (X posts missing)')
          end if
       end do
    end if
    if (t > 0) call emit(dir, 'raw\transcript-api.json', text)
    ! verification
    tool_s = TOOL//' '//VERSION
    if (skip_dom) then
       dom_status = 'skipped (--skip-dom)'
    else if (nrounds == 0) then
       dom_status = 'unavailable'
    else if (nrounds == 1) then
       dom_status = 'ok (1 round)'
    else
       dom_status = 'ok ('//itoa(nrounds)//' rounds)'
    end if
    api_status = 'ok'
    if (t == 0) api_status = 'unavailable'
    extra = ''
    if (nrounds >= 2) extra = stability_record(rounds(1)%cap, rounds(2)%cap, rounds(1)%file, rounds(2)%file)
    if (api_lane == 'share') then
       if (len(extra) > 0) extra = extra//','
       extra = extra//api_consistency_record(tchunk, tlegacy)
    end if
    if (t > 0 .and. nrounds > 0) then
       vtext = verify_capture(t, rounds(1)%cap, dir//'\attachments', 'transcript.json', rounds(1)%file, tool_s, &
                              '"api":'//jquote(api_status)//',"dom":'//jquote(dom_status)//',"rounds":'//itoa(nrounds), &
                              nf, nt, extra)
    else
       vtext = partial_verification(t > 0, extra, tool_s, api_status, dom_status)
    end if
    call emit(dir, 'verification.json', vtext)
    vroot = jparse(vtext)
    nt = int(jint(jget(jget(vroot, 'summary'), 'total'))); nf = int(jint(jget(jget(vroot, 'summary'), 'failed')))
    call log_line('verification.json: '//itoa(nt)//' checks, '//itoa(nt - nf)//' ok, '//itoa(nf)//' failed, '// &
                  itoa(jlen(jget(vroot, 'warnings')))//' warning(s)')
    allocate(fnames(max(nf,1)), fidx(max(nf,1)))
    f = jfirst(jget(vroot, 'failedChecks')); i = 0
    do while (f > 0)
       i = i + 1
       fnames(i)%s = jstr(jget(f, 'name'))
       fidx(i) = -1
       if (jis_int(jget(f, 'turnIndex'))) fidx(i) = int(jint(jget(f, 'turnIndex')))
       call log_line('  FAILED '//fnames(i)%s//' turn '//pick(fidx(i) >= 0, itoa(fidx(i)), 'null'))
       f = jnext(f)
    end do
    r = jfirst(jget(vroot, 'warnings'))
    do while (r > 0)
       call log_line('  ! '//jstr(r)); r = jnext(r)
    end do
    ! DOM chip fallback for citations the API left null, then the final transcript.json
    text_final = text
    if (t > 0 .and. nrounds > 0) then
       call enrich_citations(t, rounds(1)%cap, chip_links1, text_final, notes, nn)
       block
         type(text_item), allocatable :: ns(:)
         allocate(ns(max(nn,1)))
         do i = 1, nn
            call log_line('citation enrichment: '//notes(i)%s)
            ns(i)%s = notes(i)%s
         end do
         enrich_json = strlist_json(ns, nn)
       end block
       call emit(dir, 'transcript.json', text_final)
    end if
    if (t > 0) then
       call record_citation_sources(t, jparse(text_final))
       call write_outputs(dir, text_final)
    end if
    if (t > 0 .and. jis_true(jget(vroot, 'ok')) .and. trim(beh_status) /= 'error') then
       exit_code = 0
    else if (t > 0 .or. nrounds > 0) then
       exit_code = 2
    else
       exit_code = 1
    end if
    if (t > 0) then
       call write_manifest(dir, conv_id, final_url, api_lane, parsed_fmt, text_final, exit_code, started, t0, '')
    else
       call write_manifest(dir, conv_id, final_url, api_lane, 'null', '', exit_code, started, t0, &
                           'no API data could be parsed and the DOM lane produced no capture')
    end if
    do i = 1, ngw
       call log_line('  - '//gwarn(i)%s)
    end do
    call log_line('total ms   : '//i64toa(now_ms() - t0))
    call log_line('EXPORT_DIR='//dir)
    call finish()
  end function run_grok

  function grok_normalize(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: i
    logical :: uuid
    uuid = (len(s) == 36)
    if (uuid) then
       do i = 1, 36
          if (i == 9 .or. i == 14 .or. i == 19 .or. i == 24) then
             if (s(i:i) /= '-') uuid = .false.
          else if (index('0123456789abcdefABCDEF', s(i:i)) == 0) then
             uuid = .false.
          end if
       end do
    end if
    if (uuid) then
       r = 'https://grok.com/c/'//s
    else if (starts_with(s, 'http://') .or. starts_with(s, 'https://')) then
       r = s
    else
       r = ''
    end if
  end function grok_normalize

  ! ^https?://[^/]+/(share|c)/([^/?#]+)
  subroutine grok_classify(s, kind, id)
    character(len=*), intent(in) :: s
    character(len=:), allocatable, intent(out) :: kind, id
    integer :: a, b, e
    kind = 'unknown'; id = ''
    a = index(s, '://')
    if (a == 0) return
    b = index(s(a+3:), '/')
    if (b == 0) return
    b = a + 2 + b                       ! position of the '/' after the host
    if (starts_with(s(b:), '/share/')) then
       kind = 'share'; a = b + 7
    else if (starts_with(s(b:), '/c/')) then
       kind = 'conversation'; a = b + 3
    else
       return
    end if
    e = a
    do while (e <= len(s))
       if (index('/?#', s(e:e)) > 0) exit
       e = e + 1
    end do
    if (e == a) then
       kind = 'unknown'; return
    end if
    id = s(a:e-1)
  end subroutine grok_classify

  subroutine gwarn_add(msg)
    character(len=*), intent(in) :: msg
    type(text_item), allocatable :: tmp(:)
    call log_line('WARNING: '//msg)
    if (.not. allocated(gwarn)) allocate(gwarn(32))
    if (ngw >= size(gwarn)) then
       allocate(tmp(2*ngw)); tmp(1:ngw) = gwarn(1:ngw); call move_alloc(tmp, gwarn)
    end if
    ngw = ngw + 1; gwarn(ngw)%s = msg
  end subroutine gwarn_add

  subroutine save_raw(rel, body)
    character(len=*), intent(in) :: rel, body
    type(pending_file), allocatable :: tmp(:)
    call log_line('  -> '//rel//' ('//itoa(len(body))//' bytes)')
    if (len(export_dir) > 0) then
       call emit(export_dir, rel, body); return
    end if
    if (npend >= size(pend)) then
       allocate(tmp(2*npend)); tmp(1:npend) = pend(1:npend); call move_alloc(tmp, pend)
    end if
    npend = npend + 1; pend(npend)%rel = rel; pend(npend)%body = body
  end subroutine save_raw

  subroutine prepare_tab(cc)
    type(cdp), intent(inout) :: cc
    integer :: r
    r = cdp_call(cc, 'Page.enable', '{}')
    r = cdp_call(cc, 'Runtime.enable', '{}')
    r = cdp_call(cc, 'Emulation.setDeviceMetricsOverride', '{"width":1600,"height":1200,"deviceScaleFactor":1,"mobile":false}')
  end subroutine prepare_tab

  ! marks the current document, then navigates: wait_conversation ignores the marked (old) document
  subroutine navigate(cc, target)
    type(cdp), intent(inout) :: cc
    character(len=*), intent(in) :: target
    integer :: r
    character(len=:), allocatable :: err
    r = cdp_eval(cc, '(() => { window.__fxOld = true; return true; })()', .false., err)
    r = cdp_call(cc, 'Page.navigate', '{"url":'//jquote(target)//'}')
  end subroutine navigate

  ! articles present and unchanged for 3 polls (0.5 s apart)
  logical function wait_conversation(cc, href) result(ok)
    type(cdp), intent(inout) :: cc
    character(len=:), allocatable, intent(out) :: href
    integer :: v, n, last_n, streak
    integer(int64) :: start, deadline
    logical :: login_said
    character(len=:), allocatable :: err
    ok = .false.; href = ''
    start = now_ms(); deadline = start + int(timeout_s, int64)*1000_int64
    last_n = -1; streak = 0; login_said = .false.
    do
       v = cdp_eval(cc, '(window.__fxOld ? { href: "", n: 0, main: false, signin: false } : '//js_poll//')', .false., err)
       n = 0
       if (v > 0) then
          if (jis_int(jget(v, 'n'))) n = int(jint(jget(v, 'n')))
          if (jis_str(jget(v, 'href'))) href = jstr(jget(v, 'href'))
       end if
       if (n > 0 .and. n == last_n) then
          streak = streak + 1
       else if (n > 0) then
          streak = 1
       else
          streak = 0
       end if
       last_n = n
       if (streak >= 3) then
          call log_line('Conversation rendered: '//itoa(n)//' articles, url '//href//' ('//i64toa(now_ms() - start)//' ms)')
          ok = .true.; return
       end if
       if (now_ms() > deadline) then
          call log_line('ERROR: conversation did not appear within '//itoa(timeout_s)//' s (last url '//href//')')
          return
       end if
       if (.not. login_said .and. n == 0 .and. now_ms() - start > 15000 .and. v > 0) then
          if (jis_true(jget(v, 'signin'))) then
             login_said = .true.
             call log_line('Waiting for you to log in to grok.com in the Chrome window (profile '//profile//')')
          end if
       end if
       call sleep_ms(500)
    end do
  end function wait_conversation

  ! fetch in page context -> status, body
  subroutine page_fetch(target, method, body, status, text)
    character(len=*), intent(in) :: target, method, body
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: text
    character(len=:), allocatable :: init, err
    integer :: v
    if (len(body) > 0) then
       init = '{"credentials":"include","method":'//jquote(method)//',"headers":{"content-type":"application/json"},"body":'// &
              jquote(body)//'}'
    else
       init = '{"credentials":"include","method":'//jquote(method)//'}'
    end if
    v = cdp_eval(c, '(async () => { const r = await fetch('//jquote(target)//', '//init// &
                 '); const t = await r.text(); return { status: r.status, text: t }; })()', .true., err)
    status = 0; text = ''
    if (v == 0) then
       call gwarn_add('API '//method//' '//target//' failed: '//err); return
    end if
    status = int(jint(jget(v, 'status')))
    text = jstr(jget(v, 'text'))
  end subroutine page_fetch

  function xposts_stripped(j) result(reason)
    integer, intent(in) :: j
    character(len=:), allocatable :: reason
    integer :: r, ids, posts
    reason = ''
    if (.not. jis_obj(j)) return
    r = jfirst(arr_or0(jget(j, 'responses')))
    do while (r > 0)
       ids = arr_or0(jget(r, 'xpostIds')); posts = arr_or0(jget(r, 'xposts'))
       if (jlen(ids) > 0 .and. jlen(posts) == 0) then
          reason = 'response '//py_str(jget(r, 'responseId'))//' lists '//itoa(jlen(ids))// &
                   ' xpostIds but carries 0 xposts (X posts not hydrated by the server)'
          return
       end if
       r = jnext(r)
    end do
  end function xposts_stripped

  integer function arr_or0(p)
    integer, intent(in) :: p
    arr_or0 = 0
    if (jis_arr(p)) arr_or0 = p
  end function arr_or0

  logical function has_responses(j)
    integer, intent(in) :: j
    has_responses = .false.
    if (jis_obj(j)) has_responses = (jlen(arr_or0(jget(j, 'responses'))) > 0)
  end function has_responses

  ! one endpoint, saved verbatim as raw/api-<name>.json; hydrated: re-fetch an unhydrated payload up to 3 times
  integer function api_fetch(name, target, method, body, hydrated) result(j)
    character(len=*), intent(in) :: name, target, method, body
    logical, intent(in) :: hydrated
    integer :: status, attempt, k
    integer(int64) :: t1
    character(len=:), allocatable :: text, reason
    j = 0; reason = ''
    do attempt = 1, 3
       t1 = now_ms()
       call page_fetch(target, method, body, status, text)
       call log_line('API '//name//': HTTP '//itoa(status)//', '//itoa(len(text))//' bytes ('//i64toa(now_ms() - t1)//' ms)')
       j = 0
       if (status == 200) then
          k = verify(text, ' '//achar(9)//achar(10)//achar(13))
          if (k > 0) then
             if (text(k:k) == '{' .or. text(k:k) == '[') j = jparse(text)
          end if
          if (j == 0) call gwarn_add('API '//name//': body is not JSON')
       else if (status /= 0) then
          call gwarn_add('API '//name//' returned HTTP '//itoa(status)//' ('//target//')')
       end if
       reason = ''
       if (j > 0 .and. hydrated) reason = xposts_stripped(j)
       if (len(reason) > 0 .and. attempt < 3) then
          if (status /= 0) then
             call save_raw('raw\api-'//name//'.attempt'//itoa(attempt)//'-unhydrated.json', text)
             call req_add(name, target, status, len(text), 'raw/api-'//name//'.attempt'//itoa(attempt)//'-unhydrated.json')
          end if
          call gwarn_add('API '//name//' attempt '//itoa(attempt)//'/3: '//reason//'; re-fetching in 2 s')
          call sleep_ms(2000)
          cycle
       end if
       exit
    end do
    if (len(reason) > 0) call gwarn_add('API '//name//': '//reason//' after 3 attempts; the export keeps the payload as served')
    if (status /= 0) then
       call save_raw('raw\api-'//name//'.json', text)
       call req_add(name, target, status, len(text), 'raw/api-'//name//'.json')
    end if
  end function api_fetch

  subroutine api_lane_share(link_id, suffix, chosen, chunk, legacy)
    character(len=*), intent(in) :: link_id, suffix
    integer, intent(out) :: chosen, chunk, legacy
    character(len=:), allocatable :: base
    api_lane = 'share'
    base = 'https://grok.com/rest/app-chat/share_links/'//link_id
    chunk = api_fetch('share_links_chunk'//suffix, base//'?useChunk=true', 'GET', '', .true.)
    legacy = api_fetch('share_links_legacy'//suffix, base, 'GET', '', .true.)
    if (has_responses(chunk) .and. grok_format(chunk) == 'chunk') then
       chosen = chunk
    else if (has_responses(legacy)) then
       chosen = legacy
    else if (has_responses(chunk)) then
       chosen = chunk
    else
       chosen = 0
    end if
    if (.not. has_responses(chunk)) chunk = 0
    if (.not. has_responses(legacy)) legacy = 0
  end subroutine api_lane_share

  subroutine api_lane_conversation(conv_id, suffix, chosen, conv)
    character(len=*), intent(in) :: conv_id, suffix
    integer, intent(out) :: chosen, conv
    character(len=:), allocatable :: base
    type(strbuf) :: ids
    integer :: meta, legacy, chunk, nodes, loaded, n, k
    api_lane = 'conversation'
    base = 'https://grok.com/rest/app-chat/conversations/'//conv_id
    meta = api_fetch('conversation'//suffix, base, 'GET', '', .false.)
    legacy = api_fetch('responses'//suffix, base//'/responses?includeThreads=false', 'GET', '', .true.)
    chunk = api_fetch('responses_chunk'//suffix, base//'/responses?includeThreads=false&useChunk=true', 'GET', '', .true.)
    nodes = api_fetch('response-node'//suffix, base//'/response-node', 'GET', '', .false.)
    loaded = 0
    if (.not. (has_responses(legacy) .or. has_responses(chunk))) then
       if (jis_obj(nodes)) then
          if (jlen(arr_or0(jget(nodes, 'responseNodes'))) > 0) then
             call sb_add(ids, '{"responseIds":[')
             n = jfirst(jget(nodes, 'responseNodes')); k = 0
             do while (n > 0)
                if (jis_str(jget(n, 'responseId'))) then
                   if (k > 0) call sb_add(ids, ',')
                   call sb_add(ids, jquote(jstr(jget(n, 'responseId')))); k = k + 1
                end if
                n = jnext(n)
             end do
             call sb_add(ids, ']}')
             loaded = api_fetch('load-responses'//suffix, base//'/load-responses', 'POST', sb_str(ids), .true.)
          end if
       end if
    end if
    if (has_responses(chunk) .and. grok_format(chunk) == 'chunk') then
       chosen = chunk
    else if (has_responses(legacy)) then
       chosen = legacy
    else if (has_responses(chunk)) then
       chosen = chunk
    else if (has_responses(loaded)) then
       chosen = loaded
    else
       chosen = 0
    end if
    conv = 0
    if (jis_obj(meta)) then
       if (jis_obj(jget(meta, 'conversation'))) then
          conv = jget(meta, 'conversation')
       else
          conv = meta
       end if
    else if (chosen > 0) then
       if (jis_obj(jget(chosen, 'conversation'))) conv = jget(chosen, 'conversation')
    end if
  end subroutine api_lane_conversation

  subroutine download_attachments(t, dir)
    integer, intent(in) :: t
    character(len=*), intent(in) :: dir
    integer :: tu, a, v, nok, ntot, k, st
    character(len=:), allocatable :: rel, err, bytes
    nok = 0; ntot = 0
    tu = jfirst(arr_or0(jget(t, 'turns')))
    do while (tu > 0)
       a = jfirst(arr_or0(jget(tu, 'attachments')))
       do while (a > 0)
          ntot = ntot + 1
          if (ntot == 1) call mkdir_p(dir//'\attachments')
          rel = 'attachments\'//i64toa(jint(jget(tu, 'index')))//'-'//safe_file_name(py_str(jget(a, 'fileId')))//'-'// &
                safe_file_name(py_str(jget(a, 'fileName')))
          if (.not. jis_str(jget(a, 'contentUrl'))) then
             call gwarn_add('attachment '//rel//' has no contentUrl')
          else
             ! up to 3 attempts: a network error, HTTP 429 or 5xx is retried after 1 s, then 3 s
             do k = 1, 3
                v = cdp_run_function(c, js_b64, jquote(jstr(jget(a, 'contentUrl'))), err)
                if (v > 0) then
                   st = int(jint(jget(v, 'status')))
                   if (.not. (st == 0 .or. st == 429 .or. st >= 500)) exit
                end if
                if (k == 3) exit
                call log_line('attachment '//rel//': attempt '//itoa(k)//' '//pick(v == 0, err, 'HTTP '//itoa(st))//'; retrying')
                call sleep_ms(merge(1000, 3000, k == 1))
             end do
             if (v == 0) then
                call gwarn_add('attachment '//jstr(jget(a, 'contentUrl'))//' download failed: '//err)
             else if (jint(jget(v, 'status')) /= 200) then
                call gwarn_add('attachment '//jstr(jget(a, 'contentUrl'))//': HTTP '//i64toa(jint(jget(v, 'status'))))
             else
                bytes = base64_decode(jstr(jget(v, 'data')))
                call emit(dir, ''//rel, bytes)
                nok = nok + 1
                if (natt_paths > 0) call sb_add(att_paths, ',')
                natt_paths = natt_paths + 1
                call sb_add(att_paths, jquote(py_str(jget(a, 'fileId')))//':'//jquote(replace_all(rel, '\', '/')))
                if (natt_paths > 1) call sb_add(att_list, ',')
                call sb_add(att_list, '{"fileId":'//canon_json(jget(a, 'fileId'))//',"path":'//jquote(replace_all(rel, '\', '/'))//'}')
                if (jis_int(jget(a, 'sizeBytes'))) then
                   if (jint(jget(a, 'sizeBytes')) /= len(bytes, kind=int64)) call gwarn_add('attachment '//rel//': downloaded '// &
                        itoa(len(bytes))//' bytes, API sizeBytes '//i64toa(jint(jget(a, 'sizeBytes'))))
                end if
                call log_line('attachment '//rel//': '//itoa(len(bytes))//' bytes')
             end if
          end if
          a = jnext(a)
       end do
       tu = jnext(tu)
    end do
    if (ntot > 0) call log_line('attachments: '//itoa(nok)//' of '//itoa(ntot)//' downloaded')
  end subroutine download_attachments

  ! revealed X-post results with an empty user handle: the page rendered the unhydrated payload
  function capture_unhydrated(cap) result(reason)
    integer, intent(in) :: cap
    character(len=:), allocatable :: reason
    integer :: n, k, pan, sec, row, res
    character(len=:), allocatable :: s
    character(len=17), parameter :: keys(2) = [character(len=17) :: 'thoughtsByArticle', 'sourcesByArticle ']
    reason = ''; n = 0
    if (.not. jis_obj(cap)) return
    do k = 1, 2
       if (.not. jis_obj(jget(cap, trim(keys(k))))) cycle
       pan = jfirst(jget(cap, trim(keys(k))))
       do while (pan > 0)
          if (jis_obj(pan)) then
             sec = jfirst(arr_or0(jget(pan, 'sections')))
             do while (sec > 0)
                row = jfirst(arr_or0(jget(sec, 'rows')))
                do while (row > 0)
                   res = jfirst(arr_or0(jget(row, 'results')))
                   do while (res > 0)
                      if (jis_str(jget(res, 'url'))) then
                         s = jstr(jget(res, 'url'))
                         if (starts_with(s, 'https://x.com//status/') .or. starts_with(s, 'http://x.com//status/') .or. &
                             starts_with(s, 'https://www.x.com//status/') .or. starts_with(s, 'http://www.x.com//status/')) then
                            if (post_known(s)) n = n + 1
                         end if
                      end if
                      res = jnext(res)
                   end do
                   row = jnext(row)
                end do
                sec = jnext(sec)
             end do
          end if
          pan = jnext(pan)
       end do
    end do
    if (n > 0) reason = itoa(n)//' revealed X-post result(s) have an empty user handle (https://x.com//status/...): '// &
                        'the page was rendered from the unhydrated payload'
  end function capture_unhydrated

  ! An empty-handle post counts only if the transcript carries it with an author (when it carries any): a post
  ! outside that set is one the API also serves without an author (deleted or unavailable), and the page renders
  ! it with an empty handle however often it is reloaded.
  logical function post_known(url)
    character(len=*), intent(in) :: url
    integer :: i, j
    post_known = .true.
    if (.not. allocated(api_post_ids)) return
    if (len_trim(api_post_ids) == 0) return
    i = index(url, '/status/')
    if (i == 0) then
       post_known = .false.; return
    end if
    i = i + 8; j = i
    do while (j <= len(url))
       if (url(j:j) < '0' .or. url(j:j) > '9') exit
       j = j + 1
    end do
    post_known = (j > i) .and. index(api_post_ids, ' '//url(i:j-1)//' ') > 0
  end function post_known

  ! every object with a string postId and a non-empty string username -> ' id ' appended to api_post_ids
  recursive subroutine collect_post_ids(p)
    integer, intent(in) :: p
    integer :: q
    if (jis_obj(p)) then
       if (jis_str(jget(p, 'postId')) .and. jis_str(jget(p, 'username'))) then
          if (len(jstr(jget(p, 'username'))) > 0) then
             if (index(api_post_ids, ' '//jstr(jget(p, 'postId'))//' ') == 0) &
                api_post_ids = api_post_ids//jstr(jget(p, 'postId'))//' '
          end if
       end if
    end if
    if (jis_obj(p) .or. jis_arr(p)) then
       q = jfirst(p)
       do while (q > 0)
          call collect_post_ids(q)
          q = jnext(q)
       end do
    end if
  end subroutine collect_post_ids
  ! starts extract.js as a background promise on the page
  logical function extractor_start(cc, k, shots)
    type(cdp), intent(inout) :: cc
    integer, intent(in) :: k
    logical, intent(in) :: shots
    integer :: v
    character(len=:), allocatable :: err, opts
    if (shots) then
       opts = '{"round":'//itoa(k)//',"settleMs":700,"maxPasses":25,"pauseForShot":true,"shotTimeoutMs":180000}'
    else
       opts = '{"round":'//itoa(k)//',"settleMs":700,"maxPasses":25}'
    end if
    v = cdp_eval(cc, js_head//js_extract//')('//opts//js_tail, .false., err)
    extractor_start = jis_true(v)
    if (extractor_start) then
       call log_line('DOM round '//itoa(k)//': extractor started in the background')
    else
       call gwarn_add('DOM round '//itoa(k)//': the extractor did not start on the page '//err)
    end if
  end function extractor_start

  ! one poll; .true. when the round has finished (capture or error)
  logical function round_step(cc, st, dir) result(done)
    type(cdp), intent(inout) :: cc
    type(round_state), intent(inout) :: st
    character(len=*), intent(in) :: dir
    integer :: v, seq, cap, art
    character(len=:), allocatable :: err
    done = .true.
    if (st%finished) return
    v = cdp_eval(cc, js_spoll, .false., err)
    if (v == 0) then
       st%error = 'poll failed: '//err; st%finished = .true.; return
    end if
    seq = 0
    if (jis_int(jget(v, 'seq'))) seq = int(jint(jget(v, 'seq')))
    if (seq > 0 .and. seq /= st%last_seq) then        ! a panel held open for a screenshot: photograph, then release
       st%last_seq = seq
       art = -1
       if (jis_int(jget(v, 'article'))) art = int(jint(jget(v, 'article')))
       if (st%shots .and. art >= 0 .and. jis_str(jget(v, 'panel'))) then
          if (len(jstr(jget(v, 'panel'))) > 0) then
             if (screenshot_to(cc, dir, 'screenshots/turn'//itoa(art)//'-'//jstr(jget(v, 'panel'))//'.png', .false.)) &
                st%nshots = st%nshots + 1
          end if
       end if
       v = cdp_eval(cc, '(() => { window.__grokShotDone = '//itoa(seq)//'; return true; })()', .false., err)
       v = cdp_eval(cc, js_spoll, .false., err)
    end if
    if (jis_true(jget(v, 'done'))) then
       st%finished = .true.
       if (jis_str(jget(v, 'error'))) then
          st%error = 'extractor threw: '//jstr(jget(v, 'error'))
       else
          cap = cdp_eval(cc, js_sres, .false., err)
          if (jis_obj(cap)) then
             st%cap = cap; st%ok = .true.
          else
             st%error = 'the extractor returned no capture '//err
          end if
       end if
    else
       done = .false.
    end if
  end function round_step

  subroutine dom_lane(target, dir)
    character(len=*), intent(in) :: target, dir
    type(round_state) :: s1, s2
    character(len=:), allocatable :: ws2, id2, href1, href2, err
    logical :: ok, a, b
    integer(int64) :: t0
    t0 = now_ms(); dom_t0 = t0
    call cdp_new_window(port, 'about:blank', id2, ws2, ok)
    if (ok) ok = cdp_connect(c2, ws2, 1000000)
    if (.not. ok) then
       call gwarn_add('second tab for the concurrent DOM round could not be opened; the rounds run one after the other')
       call round_sequential(c, target, 1, dir, 1, .true.)
       if (nrounds > 0) call after_round1(dir, nshot_files)
       call round_sequential(c, target, 2, dir, 1, .false.)
       return
    end if
    have_c2 = .true.; tab2_id = id2
    call prepare_tab(c2)
    call log_line('DOM lane: rounds 1 and 2 run at the same time in two tabs')
    call navigate(c2, target)
    call navigate(c, target)
    ok = wait_conversation(c, href1)
    if (ok) ok = wait_conversation(c2, href2)
    s1%k = 1; s2%k = 2; s1%error = ''; s2%error = ''
    if (ok) then
       ! only the drawn tab gets fast screenshots: give the panel shots to that round
       s2%shots = tab_renders(c2)
       s1%shots = .not. s2%shots .and. tab_renders(c)
       if (.not. (s1%shots .or. s2%shots)) s2%shots = .true.
       call log_line('panel screenshots are taken in round '//pick(s1%shots, '1', '2')//'''s tab')
       s1%t0 = now_ms(); s2%t0 = s1%t0
       if (.not. extractor_start(c, 1, s1%shots)) s1%finished = .true.
       if (.not. extractor_start(c2, 2, s2%shots)) s2%finished = .true.
       do
          a = round_step(c, s1, dir)
          b = round_step(c2, s2, dir)
          if (a .and. b) exit
          call sleep_ms(60)
       end do
       call log_line('DOM rounds 1 and 2: '//i64toa(now_ms() - t0)//' ms')
       if (.not. s1%ok) then
          call gwarn_add('DOM round 1 failed: '//s1%error)
       else if (len(capture_unhydrated(s1%cap)) > 0) then
          call reject_round(s1, 1, dir); call sleep_ms(30000); call round_sequential(c, target, 1, dir, 2, s1%shots)
       else
          call round_emit(c, s1, href1, dir)
       end if
       if (nrounds > 0) call after_round1(dir, s1%nshots + s2%nshots)
       if (.not. s2%ok) then
          call gwarn_add('DOM round 2 failed: '//s2%error)
       else if (len(capture_unhydrated(s2%cap)) > 0) then
          call reject_round(s2, 1, dir); call sleep_ms(30000); call round_sequential(c2, target, 2, dir, 2, s2%shots)
       else
          call round_emit(c2, s2, href2, dir)
       end if
    else
       call gwarn_add('DOM lane: the conversation did not render in both tabs')
    end if
    call cdp_disconnect(c2)
    if (.not. keep_open) call cdp_close_tab(port, tab2_id)
    have_c2 = .false.
  end subroutine dom_lane

    subroutine reject_round(st, attempt, dir)
      type(round_state), intent(in) :: st
      integer, intent(in) :: attempt
      character(len=*), intent(in) :: dir
      type(jw) :: w
      call jcanon_value(w, st%cap)
      call emit(dir, 'raw\dom-capture-round'//itoa(st%k)//'.attempt'//itoa(attempt)//'-unhydrated.json', jw_result(w))
      call gwarn_add('DOM round '//itoa(st%k)//' attempt '//itoa(attempt)//'/3: '//capture_unhydrated(st%cap)// &
                     '; re-doing the round in 30 s')
    end subroutine reject_round

  recursive subroutine round_sequential(cc, target, k, dir, first_attempt, shots)
    type(cdp), intent(inout) :: cc
    character(len=*), intent(in) :: target, dir
    integer, intent(in) :: k, first_attempt
    logical, intent(in) :: shots
    type(round_state) :: st
    character(len=:), allocatable :: href, reason
    integer :: attempt
    attempt = first_attempt
    do
       call log_line('DOM round '//itoa(k)//' (attempt '//itoa(attempt)//'): navigating fresh to '//target)
       call navigate(cc, target)
       if (.not. wait_conversation(cc, href)) then
          call gwarn_add('DOM round '//itoa(k)//': the conversation did not render'); return
       end if
       st = round_state()
       st%k = k; st%error = ''; st%shots = shots; st%t0 = now_ms()
       if (.not. extractor_start(cc, k, shots)) return
       do while (.not. round_step(cc, st, dir))
          call sleep_ms(60)
       end do
       if (.not. st%ok) then
          call gwarn_add('DOM round '//itoa(k)//' failed: '//st%error); return
       end if
       reason = capture_unhydrated(st%cap)
       if (len(reason) > 0 .and. attempt < 3) then
          block
          type(jw) :: w
          call jcanon_value(w, st%cap)
          call emit(dir, 'raw\dom-capture-round'//itoa(k)//'.attempt'//itoa(attempt)//'-unhydrated.json', jw_result(w))
          end block
          call gwarn_add('DOM round '//itoa(k)//' attempt '//itoa(attempt)//'/3: '//reason//'; re-doing the round in 30 s')
          call sleep_ms(30000)
          attempt = attempt + 1
          cycle
       end if
       if (len(reason) > 0) call gwarn_add('DOM round '//itoa(k)//': '//reason//' after 3 attempts; keeping the capture as rendered')
       call round_emit(cc, st, href, dir)
       return
    end do
  end subroutine round_sequential

  subroutine round_emit(cc, st, href, dir)
    type(cdp), intent(inout) :: cc
    type(round_state), intent(in) :: st
    character(len=*), intent(in) :: href, dir
    type(jw) :: w, wc
    integer :: v, stats, wn
    character(len=:), allocatable :: err, rel
    rel = 'raw/dom-capture-round'//itoa(st%k)//'.json'
    call jcanon_value(w, st%cap)
    call emit(dir, 'raw\dom-capture-round'//itoa(st%k)//'.json', jw_result(w))
    stats = jget(st%cap, 'stats')
    call log_line('DOM round '//itoa(st%k)//': '//py_str(jget(stats, 'articles'))//' articles, '// &
                  py_str(jget(stats, 'thoughtsPanels'))//' thoughts panels, '//py_str(jget(stats, 'sourcesPanels'))// &
                  ' sources panels, remainingCollapsed '//py_str(jget(stats, 'remainingCollapsedTotal')))
    wn = jfirst(arr_or0(jget(jget(st%cap, 'env'), 'warnings')))
    do while (wn > 0)
       call gwarn_add('DOM round '//itoa(st%k)//': extractor warning: '//py_str(wn)); wn = jnext(wn)
    end do
    v = cdp_eval(cc, js_chips, .true., err)
    if (jis_obj(v)) then
       call jcanon_value(wc, v)
       call emit(dir, 'raw\citation-chips-round'//itoa(st%k)//'.json', jw_result(wc))
       if (st%k == 1) chip_links1 = v
    else
       call gwarn_add('DOM round '//itoa(st%k)//': citation chip helper failed: '//err)
    end if
    v = cdp_eval(cc, 'document.documentElement.outerHTML', .false., err)
    if (jis_str(v)) call emit(dir, 'raw\page-round'//itoa(st%k)//'.html', jstr(v))
    nrounds = nrounds + 1
    rounds(nrounds)%k = st%k; rounds(nrounds)%cap = st%cap; rounds(nrounds)%file = rel; rounds(nrounds)%href = href
    block
      integer :: env
      env = jget(st%cap, 'env')
      rounds(nrounds)%rec = '{"round":'//itoa(st%k)//',"file":'//jquote(rel)//',"finalUrl":'//jquote(href)// &
           ',"extractorMs":'//i64toa(now_ms() - st%t0)//',"roundMs":'//i64toa(now_ms() - dom_t0)// &
           pick(st%shots, ',"screenshotsTaken":'//itoa(st%nshots), '')//',"stats":{"articles":'//canon_json(jget(stats, 'articles'))// &
           ',"userArticles":'//canon_json(jget(stats, 'userArticles'))//',"assistantArticles":'//canon_json(jget(stats, 'assistantArticles'))// &
           ',"thoughtsPanels":'//canon_json(jget(stats, 'thoughtsPanels'))//',"sourcesPanels":'//canon_json(jget(stats, 'sourcesPanels'))// &
           ',"thoughtRows":'//canon_json(jget(stats, 'thoughtRows'))//',"sourceRows":'//canon_json(jget(stats, 'sourceRows'))// &
           ',"thoughtLinks":'//canon_json(jget(stats, 'thoughtLinks'))//',"sourceLinks":'//canon_json(jget(stats, 'sourceLinks'))// &
           ',"remainingCollapsedTotal":'//canon_json(jget(stats, 'remainingCollapsedTotal'))// &
           ',"visibilityState":'//canon_json(jget(env, 'visibilityState'))//',"rafAlive":'//canon_json(jget(env, 'rafAlive'))// &
           ',"forcedOpen":'//canon_json(jget(env, 'forcedOpen'))//',"elapsedMs":'//canon_json(jget(env, 'elapsedMs'))// &
           ',"extractorWarnings":'//pick(jis_arr(jget(env, 'warnings')), canon_json(jget(env, 'warnings')), '[]')//'}}'
    end block
  end subroutine round_emit

  ! verification.json when the DOM lane or the API lane produced nothing (never ok without both)
  function partial_verification(have_t, extra, tool_s, api_status, dom_status) result(r)
    logical, intent(in) :: have_t
    character(len=*), intent(in) :: extra, tool_s, api_status, dom_status
    character(len=:), allocatable :: r
    type(strbuf) :: d, by, fl
    integer :: arr, e, nf, n
    type(jw) :: w
    arr = jparse('['//extra//']')
    call sb_add(by, '{'); call sb_add(fl, '[')
    nf = 0; n = 0
    e = jfirst(arr)
    do while (e > 0)
       n = n + 1
       if (n > 1) call sb_add(by, ',')
       call sb_add(by, jquote(jstr(jget(e, 'name')))//':{"total":1,"ok":'//merge('1', '0', jis_true(jget(e, 'ok')))// &
                   ',"failed":'//merge('0', '1', jis_true(jget(e, 'ok')))//'}')
       if (.not. jis_true(jget(e, 'ok'))) then
          if (nf > 0) call sb_add(fl, ',')
          nf = nf + 1
          call sb_add(fl, '{"name":'//jquote(jstr(jget(e, 'name')))//',"turnIndex":null}')
       end if
       e = jnext(e)
    end do
    call sb_add(by, '}'); call sb_add(fl, ']')
    call sb_add(d, '{"tool":'//jquote(tool_s)//',"transcript":'//merge('"transcript.json"', 'null             ', have_t)// &
                   ',"capture":null,"api":'//jquote(api_status)//',"dom":'//jquote(dom_status)//',"checks":['//extra//']'// &
                   ',"byName":'//sb_str(by)//',"summary":{"total":'//itoa(n)//',"ok":'//itoa(n - nf)//',"failed":'// &
                   itoa(nf)//'},"failedChecks":'//sb_str(fl)//',"warnings":['// &
                   jquote(pick(skip_dom, 'DOM lane skipped (--skip-dom): SPEC 5 checks 1-12 not performed', &
                               'DOM lane produced no capture: SPEC 5 checks 1-12 not performed'))// &
                   '],"note":"the page content was not verified against the API export","ok":'// &
                   merge('true ', 'false', have_t .and. skip_dom .and. nf == 0)//'}')
    call jcanon_value(w, jparse(sb_str(d)))
    r = jw_result(w)
  end function partial_verification

  ! the page's WebSocket history: command output and other agent tool results that the REST responses lack
  subroutine capture_ws_history()
    integer :: v, it, k, fr
    integer(int64) :: t1
    character(len=:), allocatable :: err
    type(strbuf) :: raw, valid
    t1 = now_ms()
    do
       v = cdp_eval(c, js_wsread, .false., err)
       if (jis_true(jget(v, 'done')) .or. now_ms() - t1 > 20000) exit
       call sleep_ms(500)
    end do
    k = 0; ws_frames = 0
    call sb_add(valid, '[')
    call sb_add(raw, '[')
    it = jfirst(jget(v, 'items'))
    do while (it > 0)
       if (jis_str(it)) then
          if (raw%n > 1) call sb_add(raw, ','//achar(10))
          call sb_add(raw, jstr(it))
          fr = 0
          if (looks_json(jstr(it))) fr = jparse(jstr(it))
          if (jis_obj(fr)) then
             if (k > 0) call sb_add(valid, ',')
             call sb_add(valid, jstr(it)); k = k + 1
          end if
       end if
       it = jnext(it)
    end do
    call sb_add(raw, ']'//achar(10)); call sb_add(valid, ']')
    ! kept verbatim: the frames exactly as the server sent them, as one JSON array
    if (raw%n > 3) call save_raw('raw/ws-history.json', sb_str(raw))
    if (k > 0) ws_frames = jparse(sb_str(valid))
    call log_line('WebSocket history: '//itoa(k)//' item(s), done '//pick(jis_true(jget(v, 'done')), 'true', 'false')// &
                  ' ('//i64toa(now_ms() - t1)//' ms)')
  end subroutine capture_ws_history

  subroutine merge_ws(text)
    character(len=:), allocatable, intent(inout) :: text
    character(len=:), allocatable :: merged
    integer :: n
    if (ws_frames <= 0) return
    call grok_merge_ws_results(text, ws_frames, merged, n)
    text = merged
    if (n > 0) call note_add(itoa(n)//' tool result(s) (command output) filled from the page''s WebSocket history (raw/ws-history.json)')
  end subroutine merge_ws

  ! Page.captureScreenshot -> PNG file under dir; rel uses forward slashes (manifest form)
  logical function screenshot_to(cc, dir, rel, beyond) result(ok)
    type(cdp), intent(inout) :: cc
    character(len=*), intent(in) :: dir, rel
    logical, intent(in) :: beyond
    integer :: r
    character(len=:), allocatable :: png
    type(text_item), allocatable :: tmp(:)
    ok = .false.
    if (beyond) then
       r = cdp_call(cc, 'Page.captureScreenshot', '{"format":"png","captureBeyondViewport":true}')
    else
       r = cdp_call(cc, 'Page.captureScreenshot', '{"format":"png"}')
    end if
    if (.not. jis_str(jget(jget(r, 'result'), 'data'))) then
       call gwarn_add('screenshot '//rel//' failed'); return
    end if
    png = base64_decode(jstr(jget(jget(r, 'result'), 'data')))
    if (len(png) < 8) then
       call gwarn_add('screenshot '//rel//': decoded data is not a PNG'); return
    end if
    if (png(1:8) /= achar(137)//'PNG'//achar(13)//achar(10)//achar(26)//achar(10)) then
       call gwarn_add('screenshot '//rel//': decoded data is not a PNG ('//itoa(len(png))//' bytes)'); return
    end if
    call mkdir_p(dir//'\screenshots')
    call emit(dir, ''//replace_all(rel, '/', '\'), png)
    call log_line(rel//': '//itoa(len(png))//' bytes')
    if (.not. allocated(shot_files)) allocate(shot_files(64))
    if (nshot_files >= size(shot_files)) then
       allocate(tmp(2*nshot_files)); tmp(1:nshot_files) = shot_files(1:nshot_files); call move_alloc(tmp, shot_files)
    end if
    nshot_files = nshot_files + 1; shot_files(nshot_files)%s = rel
    ok = .true.
  end function screenshot_to

  ! raw/page-initial.html + screenshots/transcript-full.png
  subroutine capture_page(cc, dir)
    type(cdp), intent(inout) :: cc
    character(len=*), intent(in) :: dir
    integer :: v
    logical :: ok
    character(len=:), allocatable :: err
    v = cdp_eval(cc, 'document.documentElement.outerHTML', .false., err)
    if (jis_str(v)) call emit(dir, 'raw\page-initial.html', jstr(v))
    ok = screenshot_to(cc, dir, 'screenshots/transcript-full.png', .true.)
  end subroutine capture_page

  logical function tab_renders(cc)
    type(cdp), intent(inout) :: cc
    integer :: v
    character(len=:), allocatable :: err
    v = cdp_eval(cc, '(async () => await Promise.race([new Promise((r) => requestAnimationFrame(() => r(true))),'// &
                 ' new Promise((r) => setTimeout(() => r(false), 500))]))()', .true., err)
    tab_renders = jis_true(v)
  end function tab_renders

  ! round 1 owes one screenshot per opened panel; re-open the missing ones in tab 1
  subroutine after_round1(dir, taken)
    character(len=*), intent(in) :: dir
    integer, intent(in) :: taken
    integer :: cap, owed, k, e, a, i, v
    character(len=17), parameter :: keys(2) = [character(len=17) :: 'thoughtsByArticle', 'sourcesByArticle ']
    character(len=8), parameter :: pans(2) = [character(len=8) :: 'thoughts', 'sources ']
    character(len=:), allocatable :: rel, err, head
    logical :: have, ok
    integer :: j
    cap = rounds(1)%cap
    owed = 0
    do k = 1, 2
       e = jfirst(jget(cap, trim(keys(k))))
       do while (e > 0)
          if (truthy(e)) owed = owed + 1
          e = jnext(e)
       end do
    end do
    call log_line('DOM round 1: '//itoa(taken)//' of '//itoa(owed)//' panels photographed inside a round')
    if (taken >= owed) return
    a = jfirst(arr_or0(jget(cap, 'articles')))
    do while (a > 0)
       if (jis_int(jget(a, 'index')) .and. jequal_str(jget(a, 'role'), 'assistant')) then
          i = int(jint(jget(a, 'index')))
          do k = 1, 2
             rel = 'screenshots/turn'//itoa(i)//'-'//trim(pans(k))//'.png'
             have = .false.
             do j = 1, nshot_files
                if (shot_files(j)%s == rel) have = .true.
             end do
             if (have) cycle
             if (.not. truthy(jget(jget(cap, trim(keys(k))), itoa(i)))) cycle
             v = cdp_run_function(c, js_extract, '{"only":{"article":'//itoa(i)//',"panel":"'//trim(pans(k))// &
                                  '"},"settleMs":700,"maxPasses":25}', err)
             v = cdp_eval(c, '(() => { const a = document.querySelector(''aside''); return a ? a.innerText.split(''\n'')[0].trim() : null; })()', &
                          .false., err)
             head = ''
             if (jis_str(v)) head = to_lower(jstr(v))
             if (jequal_str(jget(jget(jget(cap, trim(keys(k))), itoa(i)), 'layout'), 'canvas')) head = trim(pans(k))  ! inline steps
             if (head /= trim(pans(k))) then
                call gwarn_add('screenshot '//rel//' failed: panel '//trim(pans(k))//' for article '//itoa(i)//' is not open')
                cycle
             end if
             ok = screenshot_to(c, dir, rel, .false.)
          end do
       end if
       a = jnext(a)
    end do
    v = cdp_eval(c, '(() => { const b = document.querySelector(''aside button[aria-label="Close"]''); if (b) { b.click(); return true; } return false; })()', &
                 .false., err)
  end subroutine after_round1

  ! transcript.md + transcript.html from the final transcript text
  subroutine write_outputs(dir, text)
    character(len=*), intent(in) :: dir, text
    integer :: t, paths
    character(len=:), allocatable :: html
    t = jparse(text)
    call emit(dir, 'transcript.md', transcript_markdown(t))
    paths = jparse('{'//sb_str(att_paths)//'}')
    call transcript_html(t, paths, html, TOOL//' '//VERSION)
    call emit(dir, 'transcript.html', html)
    call write_behavior_reports(dir, t)
  end subroutine write_outputs

  ! behavior-report.json / .md from the final transcript (additive: never edits a transcript file)
  subroutine write_behavior_reports(dir, t)
    character(len=*), intent(in) :: dir
    integer, intent(in) :: t
    integer(int64) :: t1
    t1 = now_ms()
    call behavior_analyze(t, beh)
    beh_ms = now_ms() - t1
    if (beh%ok) then
       call emit(dir, 'behavior-report.json', beh%json)
       call emit(dir, 'behavior-report.md', beh%markdown)
       beh_status = 'ok'
       call log_line('behavior report: '//itoa(beh%finding_count)//' finding(s), '//itoa(beh%confirmed)//' confirmed, '// &
                     itoa(beh%candidate)//' candidate, '//itoa(beh%not_assessable)//' not assessable')
    else
       beh_status = 'error'
       call log_line('WARNING: behavior report failed: '//beh%error)
    end if
  end subroutine write_behavior_reports

  function pick(cond, a, b) result(r)
    logical, intent(in) :: cond
    character(len=*), intent(in) :: a, b
    character(len=:), allocatable :: r
    if (cond) then
       r = a
    else
       r = b
    end if
  end function pick
  integer function run_gemini() result(exit_code)
    character(len=:), allocatable :: id, wsurl, err, state_js, fetch_js, dom_js, dir, final_url, title, raw, text
    character(len=:), allocatable :: started
    integer :: st, v, pages, pg, np, i, dom, nturns, nfailed, nchecks, tr, r
    integer, allocatable :: pays(:)
    integer(int64) :: deadline, t0
    type(turn_summary), allocatable :: sm(:)
    type(text_item), allocatable :: fnames(:)
    integer, allocatable :: fidx(:)
    type(jw) :: wv, wc
    logical :: ok, signed
    exit_code = 1
    started = now_iso_utc(); t0 = now_ms()
    call run_reset()
    id = gemini_url_id(url)
    state_js = js_file('gemini-state.js'); fetch_js = js_file('gemini-fetch.js'); dom_js = js_file('gemini-dom.js')
    if (.not. ensure_chrome()) then
       call log_line('ERROR: no Chrome DevTools endpoint on port '//itoa(port)); return
    end if
    call cdp_new_tab(port, 'about:blank', tab_id, wsurl, ok)
    if (.not. ok) then
       call log_line('ERROR: could not open a tab'); return
    end if
    if (.not. cdp_connect(c, wsurl, 1000000)) then
       call log_line('ERROR: WebSocket connection to the tab failed'); return
    end if
    call log_line('Navigating to '//trim(url))
    r = cdp_call(c, 'Page.navigate', '{"url":'//jquote(trim(url))//'}')
    ! wait until signed in and the conversation shows
    deadline = now_ms() + int(timeout_s, int64)*1000_int64
    signed = .false.
    do while (now_ms() < deadline)
       v = cdp_eval(c, state_js, .false., err)
       if (v > 0) then
          ! Google's "unusual traffic" interstitial is a CAPTCHA for a person; waiting on it only adds traffic
          if (index(jstr(jget(v, 'href')), 'google.com/sorry') > 0) then
             call log_line('ERROR: Google is showing its unusual-traffic page for this browser ('//jstr(jget(v, 'href'))// &
                           '); open gemini.google.com in the exporter''s Chrome window, clear it there, and export again later')
             call finish(); return
          end if
          if (jstr(jget(v, 'host')) == 'gemini.google.com' .and. jis_true(jget(v, 'signedIn')) .and. &
              jint(jget(v, 'queries')) > 0) then
             signed = .true.; final_url = jstr(jget(v, 'href')); exit
          end if
       end if
       call sleep_ms(1000)
    end do
    if (.not. signed) then
       call log_line('ERROR: the Gemini conversation did not appear (signed in?) within '//itoa(timeout_s)//' s')
       call finish(); return
    end if
    dir = out_dir//'\'//local_stamp()//'-gemini-'//id
    call mkdir_p(dir//'\raw')
    call log_line('Export directory: '//dir)
    ! API lane
    v = cdp_run_function(c, fetch_js, '{"cid":'//jquote(id)//',"pageSize":1000}', err)
    if (v == 0) then
       call log_line('ERROR: Gemini API lane: '//err); call finish(); return
    end if
    title = jstr(jget(v, 'title'))
    pages = jget(v, 'pages')
    np = jlen(pages)
    allocate(pays(max(np,1)))
    pg = jfirst(pages); i = 0
    do while (pg > 0)
       i = i + 1
       raw = jstr(jget(pg, 'text'))
       call emit(dir, 'raw\gemini-hNvQHb-page'//itoa(i)//'.txt', raw)
       pays(i) = gemini_parse_batchexecute(raw)
       if (pays(i) == 0) call log_line('WARNING: page '//itoa(i)//' has no hNvQHb payload')
       pg = jnext(pg)
    end do
    call gemini_transcript(pays, np, final_url, title, text, sm, nturns)
    call emit(dir, 'raw\transcript-api.json', text)
    call emit(dir, 'transcript.json', text)
    call write_outputs(dir, text)
    call log_line('Gemini API: '//itoa(np)//' page(s), '//itoa(nturns)//' turns')
    ! DOM lane: scroll to the very top until nothing more loads
    dom = 0
    if (.not. skip_dom) then
       dom = cdp_run_function(c, dom_js, '{"waitMs":1000,"stableRounds":8,"maxMs":900000}', err)
       if (dom == 0) then
          call log_line('WARNING: DOM lane: '//err)
       else
          call jcanon_value(wv, dom)
          call emit(dir, 'raw\dom-gemini.json', jw_result(wv))
          call log_line('Gemini DOM: '//itoa(jlen(jget(dom, 'queries')))//' user turns, '// &
                        itoa(jlen(jget(dom, 'responses')))//' replies after '//i64toa(jint(jget(dom, 'rounds')))//' scroll rounds')
       end if
    end if
    export_dir = dir
    call capture_page(c, dir)
    ! verification
    nfailed = 0; nchecks = 0
    call jw_begin_obj(wc)
    call jw_key(wc, 'api'); call jw_str(wc, 'ok')
    if (dom > 0) then
       tr = 0
       call jw_key(wc, 'byName')
       call write_by_name(wc, dom, sm, nturns)
       call jw_key(wc, 'capture'); call jw_str(wc, 'raw/dom-gemini.json')
       call jw_key(wc, 'checks')
       call gemini_dom_checks(wc, sm, nturns, dom, nfailed, fnames, fidx, nchecks)
       call jw_key(wc, 'dom'); call jw_str(wc, 'ok (1 round)')
    else
       call jw_key(wc, 'byName'); call jw_begin_obj(wc); call jw_end_obj(wc)
       call jw_key(wc, 'capture'); call jw_null(wc)
       call jw_key(wc, 'checks'); call jw_begin_arr(wc); call jw_end_arr(wc)
       call jw_key(wc, 'dom'); call jw_str(wc, 'skipped')
    end if
    call jw_key(wc, 'failedChecks')
    call jw_begin_arr(wc)
    do i = 1, nfailed
       call jw_begin_obj(wc)
       call jw_key(wc, 'name'); call jw_str(wc, fnames(i)%s)
       call jw_key(wc, 'turnIndex')
       if (fidx(i) >= 0) then
          call jw_int(wc, int(fidx(i), int64))
       else
          call jw_null(wc)
       end if
       call jw_end_obj(wc)
    end do
    call jw_end_arr(wc)
    call jw_key(wc, 'ok'); call jw_bool(wc, dom > 0 .and. nfailed == 0)
    call jw_key(wc, 'rounds'); call jw_int(wc, merge(1_int64, 0_int64, dom > 0))
    call jw_key(wc, 'summary')
    call jw_begin_obj(wc)
    call jw_key(wc, 'failed'); call jw_int(wc, int(nfailed, int64))
    call jw_key(wc, 'ok'); call jw_int(wc, int(nchecks - nfailed, int64))
    call jw_key(wc, 'total'); call jw_int(wc, int(nchecks, int64))
    call jw_end_obj(wc)
    call jw_key(wc, 'tool'); call jw_str(wc, TOOL)
    call jw_key(wc, 'transcript'); call jw_str(wc, 'transcript.json')
    call jw_key(wc, 'warnings'); call jw_begin_arr(wc); call jw_end_arr(wc)
    call jw_end_obj(wc)
    call emit(dir, 'verification.json', jw_result(wc))
    call log_line('verification.json: '//itoa(nchecks)//' checks, '//itoa(nfailed)//' failed')
    do i = 1, nfailed
       call log_line('  FAILED '//fnames(i)%s//' turn '//itoa(fidx(i)))
    end do
    if (dom > 0 .and. nfailed == 0 .and. trim(beh_status) /= 'error') then
       exit_code = 0
    else
       exit_code = 2
    end if
    call write_manifest(dir, 'c_'//id, final_url, 'gemini-batchexecute', 'gemini-hNvQHb', text, exit_code, started, t0, '')
    call log_line('total ms   : '//i64toa(now_ms() - t0))
    call log_line('EXPORT_DIR='//dir)
    call finish()
  end function run_gemini

  subroutine finish()
    call cdp_disconnect(c)
    if (.not. keep_open .and. allocated(tab_id)) call cdp_close_tab(port, tab_id)
  end subroutine finish

  ! byName: per check name {failed,total}, names sorted
  subroutine write_by_name(w, dom, sm, nturns)
    type(jw), intent(inout) :: w
    integer, intent(in) :: dom, nturns
    type(turn_summary), intent(in) :: sm(:)
    type(jw) :: scratch
    type(text_item), allocatable :: fn(:)
    integer, allocatable :: fi(:)
    integer :: nf, nc, k, total, failed
    character(len=*), parameter :: names(5) = [character(len=20) :: 'assistant-text', 'assistant-turn-count', &
                                               'dom-reached-top', 'human-text', 'human-turn-count']
    integer :: nq, nr, nh, na, i
    call gemini_dom_checks(scratch, sm, nturns, dom, nf, fn, fi, nc)
    nq = jlen(jget(dom, 'queries')); nr = jlen(jget(dom, 'responses'))
    nh = 0; na = 0
    do i = 1, nturns
       if (sm(i)%human) then
          nh = nh + 1
       else
          na = na + 1
       end if
    end do
    call jw_begin_obj(w)
    do k = 1, 5
       select case (trim(names(k)))
       case ('assistant-text');  total = min(nr, na)
       case ('human-text');      total = min(nq, nh)
       case default;             total = 1
       end select
       if (total == 0) cycle
       failed = 0
       do i = 1, nf
          if (fn(i)%s == trim(names(k))) failed = failed + 1
       end do
       call jw_key(w, trim(names(k)))
       call jw_begin_obj(w)
       call jw_key(w, 'failed'); call jw_int(w, int(failed, int64))
       call jw_key(w, 'total'); call jw_int(w, int(total, int64))
       call jw_end_obj(w)
    end do
    call jw_end_obj(w)
  end subroutine write_by_name

  ! ---------------------------------------------------------------- run record (manifest.json)
  ! Mirrors racket/grok-export.rkt write-manifest!: every file written (path, size, SHA-256), every API request,
  ! sandbox/deployment, citation enrichment and sources, DOM rounds, verification, behavior, warnings, notes.

  subroutine run_reset()
    nfiles = 0; nreqs = 0; nnotes = 0; ngw = 0; nshot_files = 0; nrounds = 0; natt_paths = 0
    if (allocated(files)) deallocate(files)
    if (allocated(gwarn)) deallocate(gwarn)
    if (allocated(notes_list)) deallocate(notes_list)
    if (allocated(shot_files)) deallocate(shot_files)
    allocate(files(64), gwarn(32), notes_list(16), shot_files(64))
    call sb_clear(reqs); call sb_clear(att_paths); call sb_clear(att_list)
    sandbox_json = 'null'; enrich_json = '[]'; citsrc_json = 'null'; parsed_fmt = 'null'; chip_links1 = 0
    beh_status = 'not_run'; export_dir = ''; run_error = ''; api_lane = 'none'
  end subroutine run_reset

  ! write a file under the export directory and record it; a path written twice keeps one entry (latest bytes)
  subroutine emit(dir, rel0, body)
    character(len=*), intent(in) :: dir, rel0, body
    character(len=:), allocatable :: rel
    type(file_rec), allocatable :: tmp(:)
    integer :: i, j
    rel = replace_all(rel0, '\', '/')
    do while (starts_with(rel, '/'))
       rel = rel(2:)
    end do
    if (index(rel, '/') > 0) call mkdir_p(dir//'\'//replace_all(rel(1:index(rel, '/', back=.true.)-1), '/', '\'))
    call write_file(dir//'\'//replace_all(rel, '/', '\'), body)
    if (.not. allocated(files)) allocate(files(64))
    j = 0
    do i = 1, nfiles
       if (files(i)%path /= rel) then
          j = j + 1; if (j /= i) files(j) = files(i)
       end if
    end do
    nfiles = j
    if (nfiles >= size(files)) then
       allocate(tmp(2*size(files))); tmp(1:nfiles) = files(1:nfiles); call move_alloc(tmp, files)
    end if
    nfiles = nfiles + 1
    files(nfiles)%path = rel; files(nfiles)%size = len(body, kind=int64); files(nfiles)%sha = sha256_hex(body)
  end subroutine emit

  subroutine note_add(msg)
    character(len=*), intent(in) :: msg
    type(text_item), allocatable :: tmp(:)
    call log_line(msg)
    if (.not. allocated(notes_list)) allocate(notes_list(16))
    if (nnotes >= size(notes_list)) then
       allocate(tmp(2*nnotes)); tmp(1:nnotes) = notes_list(1:nnotes); call move_alloc(tmp, notes_list)
    end if
    nnotes = nnotes + 1; notes_list(nnotes)%s = msg
  end subroutine note_add

  subroutine req_add(name, url_s, status, nbytes, file)
    character(len=*), intent(in) :: name, url_s, file
    integer, intent(in) :: status, nbytes
    if (nreqs > 0) call sb_add(reqs, ',')
    nreqs = nreqs + 1
    call sb_add(reqs, '{"name":'//jquote(name)//',"url":'//jquote(url_s)//',"status":'//itoa(status)//',"bytes":'// &
                      itoa(nbytes)//',"file":'//jquote(file)//'}')
  end subroutine req_add

  function strlist_json(items, n) result(r)
    type(text_item), intent(in) :: items(:)
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: i
    call sb_add(o, '[')
    do i = 1, n
       if (i > 1) call sb_add(o, ',')
       call sb_add(o, jquote(items(i)%s))
    end do
    call sb_add(o, ']')
    r = sb_str(o)
  end function strlist_json

  function canon_json(p) result(r)
    integer, intent(in) :: p
    character(len=:), allocatable :: r
    type(jw) :: w
    if (p == 0) then
       r = 'null'; return
    end if
    call jcanon_value(w, p)
    r = sb_str(w%b)
  end function canon_json

  ! SPEC 0.6 item 3: build mode, models, sandbox previewUrls and terminal sessions of a Grok payload + transcript
  subroutine record_sandbox(d, t)
    integer, intent(in) :: d, t
    type(text_item) :: modes(64), models(64), urls(256)
    integer :: nm, nmo, nu, r, tu, rl, ev, a, sessions, v, dep
    character(len=:), allocatable :: dep_json
    nm = 0; nmo = 0; nu = 0; sessions = 0
    r = jfirst(arr_or0(jget(d, 'responses')))
    do while (r > 0)
       call push_distinct(modes, nm, jget(jget(jget(r, 'metadata'), 'request_metadata'), 'model'))
       call push_distinct(modes, nm, jget(jget(r, 'requestMetadata'), 'model'))
       call push_distinct(models, nmo, jget(r, 'model'))
       r = jnext(r)
    end do
    tu = jfirst(arr_or0(jget(t, 'turns')))
    do while (tu > 0)
       rl = jfirst(arr_or0(jget(jget(tu, 'thinking'), 'rollouts')))
       do while (rl > 0)
          ev = jfirst(arr_or0(jget(rl, 'events')))
          do while (ev > 0)
             if (jequal_str(jget(ev, 'type'), 'tool')) then
                a = jget(ev, 'args')
                if (.not. jis_obj(a)) a = 0
                v = jget(a, 'previewUrl')
                if (.not. truthy(v)) v = jget(a, 'preview_url')
                if (jis_str(v)) call push_distinct(urls, nu, v)
                if (jequal_str(jget(ev, 'kind'), 'initTerminalSession')) sessions = sessions + 1
             end if
             ev = jnext(ev)
          end do
          rl = jnext(rl)
       end do
       tu = jnext(tu)
    end do
    dep_json = 'null'
    if (sandbox_json /= 'null') then
       dep = jget(jparse(sandbox_json), 'deployment')
       if (dep > 0) dep_json = canon_json(dep)
    end if
    sandbox_json = '{"mode":'//pick(nm > 0, jquote(first_or(modes, nm)), 'null')//',"modes":'//strlist_json(modes, nm)// &
                   ',"models":'//strlist_json(models, nmo)//',"isBuildMode":'// &
                   pick(nm > 0 .and. first_or(modes, nm) == 'build', 'true', 'false')//',"previewUrls":'//strlist_json(urls, nu)// &
                   ',"previewUrlCount":'//itoa(nu)//',"terminalSessions":'//itoa(sessions)//',"deployment":'//dep_json//'}'
  end subroutine record_sandbox

  function first_or(items, n) result(r)
    type(text_item), intent(in) :: items(:)
    integer, intent(in) :: n
    character(len=:), allocatable :: r
    r = ''
    if (n > 0) r = items(1)%s
  end function first_or

  subroutine push_distinct(items, n, v)
    type(text_item), intent(inout) :: items(:)
    integer, intent(inout) :: n
    integer, intent(in) :: v
    integer :: i
    if (.not. jis_str(v)) return
    if (len(jstr(v)) == 0) return
    do i = 1, n
       if (items(i)%s == jstr(v)) return
    end do
    if (n >= size(items)) return
    n = n + 1; items(n)%s = jstr(v)
  end subroutine push_distinct

  subroutine record_deployment(dep_json)
    character(len=*), intent(in) :: dep_json
    integer :: s, c0
    type(strbuf) :: o
    if (sandbox_json == 'null') then
       sandbox_json = '{"mode":null,"modes":[],"models":[],"isBuildMode":false,"previewUrls":[],"previewUrlCount":0,'// &
                      '"terminalSessions":0,"deployment":null}'
    end if
    s = jparse(sandbox_json)
    call sb_add(o, '{')
    c0 = jfirst(s)
    do while (c0 > 0)
       if (jname(c0) /= 'deployment') then
          if (o%n > 1) call sb_add(o, ',')
          call sb_add(o, jquote(jname(c0))//':'//canon_json(c0))
       end if
       c0 = jnext(c0)
    end do
    call sb_add(o, ',"deployment":'//dep_json//'}')
    sandbox_json = sb_str(o)
  end subroutine record_deployment

  ! GET /rest/app-chat/app-deployments?latest_by_conversation_id=<id>; built files are fetched and kept
  subroutine query_app_deployment(conv_id, dir)
    character(len=*), intent(in) :: conv_id, dir
    character(len=:), allocatable :: target, text
    integer :: status, body, rec, n
    logical :: found
    type(strbuf) :: built
    integer(int64) :: t1
    target = 'https://grok.com/rest/app-chat/app-deployments?latest_by_conversation_id='//conv_id
    t1 = now_ms()
    call page_fetch(target, 'GET', '', status, text)
    if (status == 0) then
       call record_deployment('{"endpoint":'//jquote(target)//',"queried":false,"reason":"request failed"}')
       return
    end if
    call emit(dir, 'raw/api-app-deployments.json', text)
    call req_add('app-deployments', target, status, len(text), 'raw/api-app-deployments.json')
    body = 0
    if (looks_json(text)) body = jparse(text)
    rec = deployment_record_value(body)
    found = (status == 200 .and. (jis_obj(rec) .or. jis_arr(rec)))
    call sb_add(built, '[')
    if (found) then
       ncand = 0
       if (allocated(cands)) deallocate(cands)
       allocate(cands(32))
       call walk_candidates(rec, '$.record')
       do n = 1, ncand
          if (n > 1) call sb_add(built, ',')
          call sb_add(built, deployment_file_result(dir, n, cands(n)))
       end do
    end if
    call sb_add(built, ']')
    call record_deployment('{"endpoint":'//jquote(target)//',"queried":true,"status":'//itoa(status)//',"found":'// &
         pick(found, 'true', 'false')//',"record":'//pick(found, canon_json(rec), 'null')//',"body":'//canon_json(body)// &
         ',"message":'//pick(jis_obj(body), canon_json(jget(body, 'message')), 'null')//',"builtFiles":'//sb_str(built)// &
         ',"note":'//jquote(pick(found, 'deployment record preserved; every supplied file reference has an explicit acquisition outcome', &
                                  'no successful deployment record was returned; no built files were acquired'))// &
         ',"file":"raw/api-app-deployments.json","injected":false,"receiptSource":null}')
    call log_line('app-deployments: HTTP '//itoa(status)//', '//itoa(len(text))//' bytes'// &
                  pick(status == 200, ', deployment record kept in manifest.json', ', no deployment record')// &
                  ' ('//i64toa(now_ms() - t1)//' ms)')
  end subroutine query_app_deployment

  logical function looks_json(text)
    character(len=*), intent(in) :: text
    integer :: k
    looks_json = .false.
    k = verify(text, ' '//achar(9)//achar(10)//achar(13))
    if (k == 0) return
    looks_json = index('{["-0123456789tfn', text(k:k)) > 0
  end function looks_json

  integer function deployment_record_value(body) result(v)
    integer, intent(in) :: body
    character(len=16), parameter :: keys(5) = [character(len=16) :: 'deployment', 'appDeployment', 'app_deployment', 'record', 'data']
    integer :: k, x
    v = body
    if (.not. jis_obj(body)) return
    do k = 1, 5
       x = jget(body, trim(keys(k)))
       if (x > 0 .and. .not. jis_null(x)) then
          if (jis_obj(x) .or. jis_arr(x)) v = x
          return
       end if
    end do
  end function deployment_record_value

  logical function in_keys(k, list)
    character(len=*), intent(in) :: k, list      ! list: space separated
    in_keys = index(' '//list//' ', ' '//k//' ') > 0
  end function in_keys

  logical function file_object(v)
    integer, intent(in) :: v
    integer :: c0
    file_object = .false.
    if (.not. jis_obj(v)) return
    c0 = jfirst(v)
    do while (c0 > 0)
       if (in_keys(jname(c0), FILE_NAME_KEYS//' '//FILE_URL_KEYS//' '//FILE_PATH_KEYS//' '//FILE_DATA_KEYS) .and. &
           .not. jis_null(c0)) then
          file_object = .true.; return
       end if
       c0 = jnext(c0)
    end do
  end function file_object

  subroutine sorted_children(node, kids, n)
    integer, intent(in) :: node
    integer, allocatable, intent(out) :: kids(:)
    integer, intent(out) :: n
    integer :: c0, i, j, t
    n = jlen(node); allocate(kids(max(n,1)))
    c0 = jfirst(node); i = 0
    do while (c0 > 0)
       i = i + 1; kids(i) = c0; c0 = jnext(c0)
    end do
    do i = 2, n
       t = kids(i); j = i - 1
       do while (j >= 1)
          if (.not. lgt(jname(kids(j)), jname(t))) exit
          kids(j+1) = kids(j); j = j - 1
       end do
       kids(j+1) = t
    end do
  end subroutine sorted_children

  recursive subroutine walk_candidates(node, path)
    integer, intent(in) :: node
    character(len=*), intent(in) :: path
    integer, allocatable :: kids(:), fk(:)
    integer :: n, i, m, j, it
    character(len=:), allocatable :: p
    if (jis_obj(node)) then
       call sorted_children(node, kids, n)
       do i = 1, n
          p = path//'.'//jname(kids(i))
          if (in_keys(jname(kids(i)), FILE_CONTAINER_KEYS)) then
             if (jis_arr(kids(i))) then
                it = jfirst(kids(i)); j = 0
                do while (it > 0)
                   call add_cand(p//'['//itoa(j)//']', '', it); j = j + 1; it = jnext(it)
                end do
             else if (file_object(kids(i)) .or. jis_str(kids(i))) then
                call add_cand(p, '', kids(i))
             else if (jis_obj(kids(i))) then
                call sorted_children(kids(i), fk, m)
                do j = 1, m
                   call add_cand(p//'.'//jname(fk(j)), jname(fk(j)), fk(j))
                end do
             end if
          else if (jis_obj(kids(i)) .or. jis_arr(kids(i))) then
             call walk_candidates(kids(i), p)
          end if
       end do
    else if (jis_arr(node)) then
       it = jfirst(node); j = 0
       do while (it > 0)
          call walk_candidates(it, path//'['//itoa(j)//']'); j = j + 1; it = jnext(it)
       end do
    end if
  end subroutine walk_candidates

  subroutine add_cand(path, fallback, item)
    character(len=*), intent(in) :: path, fallback
    integer, intent(in) :: item
    type(cand_rec), allocatable :: tmp(:)
    if (ncand >= size(cands)) then
       allocate(tmp(2*ncand)); tmp(1:ncand) = cands(1:ncand); call move_alloc(tmp, cands)
    end if
    ncand = ncand + 1
    cands(ncand)%path = path; cands(ncand)%fallback = fallback; cands(ncand)%item = item
  end subroutine add_cand

  integer function first_present(h, keys) result(v)
    integer, intent(in) :: h
    character(len=*), intent(in) :: keys
    integer :: a, b
    character(len=:), allocatable :: k
    v = 0
    if (.not. jis_obj(h)) return
    a = 1
    do while (a <= len(keys))
       b = index(keys(a:)//' ', ' ') + a - 1
       k = keys(a:b-1)
       if (jget(h, k) > 0 .and. .not. jis_null(jget(h, k))) then
          v = jget(h, k); return
       end if
       a = b + 1
    end do
  end function first_present

  function reachable_url(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    r = ''
    if (starts_with(s, 'http://') .or. starts_with(s, 'https://') .or. starts_with(s, 'data:')) then
       r = s
    else if (starts_with(s, '/')) then
       r = 'https://grok.com'//s
    end if
  end function reachable_url

  function path_leaf(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r, clean
    integer :: k, e
    r = ''
    k = scan(s, '?#')
    clean = s
    if (k > 0) clean = s(1:k-1)
    e = len(clean)
    do while (e >= 1)
       if (clean(e:e) /= '/' .and. clean(e:e) /= '\') exit
       e = e - 1
    end do
    if (e < 1) return
    k = scan(clean(1:e), '/\', back=.true.)
    r = clean(k+1:e)
  end function path_leaf

  function deployment_file_result(dir, idx, cd) result(r)
    character(len=*), intent(in) :: dir
    integer, intent(in) :: idx
    type(cand_rec), intent(in) :: cd
    character(len=:), allocatable :: r, base, url_s, raw_ref, sfp, name, name_json, rel, bytes, err
    integer :: item, nv, uv, pv, mv, sv, dv, v
    item = cd%item
    nv = first_present(item, FILE_NAME_KEYS); uv = first_present(item, FILE_URL_KEYS)
    pv = first_present(item, FILE_PATH_KEYS); mv = first_present(item, FILE_MIME_KEYS)
    sv = first_present(item, FILE_SIZE_KEYS); dv = first_present(item, FILE_DATA_KEYS)
    raw_ref = ''
    if (jis_str(item)) raw_ref = jstr(item)
    if (jis_str(uv)) then
       url_s = reachable_url(jstr(uv))
    else
       url_s = reachable_url(raw_ref)
    end if
    if (jis_str(pv)) then
       sfp = jstr(pv)
    else if (jis_str(item) .and. len(url_s) == 0) then
       sfp = raw_ref
    else
       sfp = ''
    end if
    if (truthy(nv)) then
       name_json = canon_json(nv); name = py_str(nv)
    else if (len(cd%fallback) > 0) then
       name = cd%fallback; name_json = jquote(name)
    else
       if (len(sfp) > 0) then
          name = path_leaf(sfp)
       else
          name = path_leaf(url_s)
       end if
       if (len(name) == 0) name = 'file'
       name_json = jquote(name)
    end if
    base = '"index":'//itoa(idx)//',"sourcePath":'//jquote(cd%path)//',"source":'//canon_json(item)//',"name":'//name_json// &
           ',"mimeType":'//pick(jis_str(mv), canon_json(mv), 'null')//',"expectedSizeBytes":'//pick(jis_int(sv), canon_json(sv), 'null')// &
           ',"sourceFilePath":'//pick(len(sfp) > 0 .or. jis_str(pv), jquote(sfp), 'null')//',"url":'//pick(len(url_s) > 0, jquote(url_s), 'null')
    if (jis_str(dv)) then
       if (len(jstr(dv)) > 0) then
          r = deployment_written(dir, idx, name, base, sv, base64_decode(jstr(dv)), 'inline-base64', '')
          return
       end if
    end if
    if (len(url_s) == 0) then
       r = '{'//base//',"status":"unavailable","downloaded":false,"error":"deployment response supplied no reachable URL or inline bytes"}'
       return
    end if
    v = cdp_run_function(c, js_b64, jquote(url_s), err)
    if (v == 0) then
       call gwarn_add('deployment file '//itoa(idx)//' ('//name//') download failed: '//err)
       r = '{'//base//',"status":"failed","downloaded":false,"acquisition":"page-fetch","error":'//jquote(err)//'}'
    else if (jint(jget(v, 'status')) /= 200) then
       call gwarn_add('deployment file '//itoa(idx)//' ('//name//'): HTTP '//py_str(jget(v, 'status')))
       r = '{'//base//',"status":"http-error","downloaded":false,"acquisition":"page-fetch","httpStatus":'// &
           canon_json(jget(v, 'status'))//',"error":"deployment file returned a non-200 HTTP status"}'
    else
       r = deployment_written(dir, idx, name, base, sv, base64_decode(jstr(jget(v, 'data'))), 'page-fetch', &
                              ',"httpStatus":'//canon_json(jget(v, 'status')))
    end if
  end function deployment_file_result

    function deployment_written(dir, idx, name, base, sv, b, acquisition, extra) result(o)
      character(len=*), intent(in) :: dir, name, base, b, acquisition, extra
      integer, intent(in) :: idx, sv
      character(len=:), allocatable :: o, rel
      rel = 'deployment-files/'//itoa(idx)//'-'//safe_file_name(name)
      call emit(dir, rel, b)
      o = '{'//base//',"status":"ok","downloaded":true,"acquisition":'//jquote(acquisition)//',"file":'//jquote(rel)// &
          ',"sizeBytes":'//itoa(len(b))//',"sha256":'//jquote(sha256_hex(b))
      if (jis_int(sv)) o = o//',"sizeMatches":'//pick(jint(sv) == len(b, kind=int64), 'true', 'false')
      o = o//extra//'}'
    end function deployment_written

  ! which source filled each citation's url/kind (api-card / api-chunk / dom-chip / none)
  subroutine record_citation_sources(t_api, t_final)
    integer, intent(in) :: t_api, t_final
    type(strbuf) :: turns
    integer :: tu, tf, ca, cf, cx, j, n_card, n_chunk, n_dom, n_none, k
    character(len=:), allocatable :: api_label, src
    logical :: first_turn
    api_label = pick(parsed_fmt == 'chunk', 'api-chunk', 'api-card')
    n_card = 0; n_chunk = 0; n_dom = 0; n_none = 0
    call sb_add(turns, '[')
    first_turn = .true.
    tu = jfirst(arr_or0(jget(t_api, 'turns'))); k = 0
    do while (tu > 0)
       if (jlen(arr_or0(jget(tu, 'citations'))) > 0) then
          if (.not. first_turn) call sb_add(turns, ',')
          first_turn = .false.
          tf = jat(jget(t_final, 'turns'), [k])
          call sb_add(turns, '{"turnIndex":'//canon_json(jget(tu, 'index'))//',"citations":[')
          ca = jfirst(jget(tu, 'citations')); cf = jfirst(arr_or0(jget(tf, 'citations'))); j = 0
          do while (ca > 0)
             if (j > 0) call sb_add(turns, ',')
             cx = cf
             if (cx == 0) cx = ca
             if (truthy(jget(ca, 'url'))) then
                src = api_label
             else if (truthy(jget(cx, 'url'))) then
                src = 'dom-chip'
             else
                src = 'none'
             end if
             select case (src)
             case ('api-card'); n_card = n_card + 1
             case ('api-chunk'); n_chunk = n_chunk + 1
             case ('dom-chip'); n_dom = n_dom + 1
             case default; n_none = n_none + 1
             end select
             call sb_add(turns, '{"cardId":'//canon_json(jget(ca, 'cardId'))//',"citationId":'//canon_json(jget(ca, 'citationId'))// &
                  ',"url":'//canon_json(jget(cx, 'url'))//',"kind":'//canon_json(jget(cx, 'kind'))//',"source":'//jquote(src)//'}')
             j = j + 1
             ca = jnext(ca)
             if (cf > 0) cf = jnext(cf)
          end do
          call sb_add(turns, ']}')
       end if
       k = k + 1
       tu = jnext(tu)
    end do
    call sb_add(turns, ']')
    citsrc_json = '{"rule":"url/kind come from the API (cardAttachmentsJson card id on the legacy format, renderCitation chunk on '// &
                  'the chunk format); DOM chips fill only citations the API left null","summary":{"api-card":'//itoa(n_card)// &
                  ',"api-chunk":'//itoa(n_chunk)//',"dom-chip":'//itoa(n_dom)//',"none":'//itoa(n_none)//'},"turns":'//sb_str(turns)//'}'
  end subroutine record_citation_sources

  subroutine write_manifest(dir, conv_id, final_url, api_lane_s, fmt, transcript_text, exit_code, started, t0, err)
    character(len=*), intent(in) :: dir, conv_id, final_url, api_lane_s, fmt, transcript_text, started, err
    integer, intent(in) :: exit_code
    integer(int64), intent(in) :: t0
    type(strbuf) :: o
    integer :: t, tu, i, v, s, f
    integer :: hum, asst, cits, curl, atts, summ, tools, rl, ev, a
    character(len=4096) :: arg
    character(len=:), allocatable :: vtext, ext
    logical :: ok
    t = 0
    if (len(transcript_text) > 0) t = jparse(transcript_text)
    call sb_add(o, '{"tool":'//jquote(TOOL)//',"version":'//jquote(VERSION)//',"language":"fortran","compilerVersion":'// &
                   jquote(compiler_version()))
    ext = read_file(exe_dir()//'\js\extract.js', ok)
    if (ok) then
       call sb_add(o, ',"extractJsSha256":'//jquote(sha256_hex(ext))//',"extractJsSize":'//itoa(len(ext)))
    else
       call sb_add(o, ',"extractJsSha256":null,"extractJsSize":null')
    end if
    call sb_add(o, ',"args":[')
    do i = 1, arg_count()
       if (i > 1) call sb_add(o, ',')
       call sb_add(o, jquote(arg_utf8(i)))
    end do
    call sb_add(o, '],"options":{"out":'//jquote(out_dir)//',"port":'//itoa(port)//',"chrome":'//jquote(CHROME)//',"profile":'// &
                   jquote(profile)//',"noLaunch":'//pick(no_launch, 'true', 'false')//',"keepOpen":'//pick(keep_open, 'true', 'false')// &
                   ',"timeoutSec":'//itoa(timeout_s)//',"skipDom":'//pick(skip_dom, 'true', 'false')//',"skipApi":false,"rounds":2'// &
                   ',"fromJson":null,"sourceUrl":null,"url":'//jquote(trim(url))//'}')
    call sb_add(o, ',"startedAt":'//jquote(started)//',"finishedAt":'//jquote(now_iso_utc())//',"totalMs":'//i64toa(now_ms() - t0)// &
                   ',"timingsMs":{},"conversationId":'//pick(len(conv_id) > 0, jquote(conv_id), 'null')//',"finalUrl":'// &
                   pick(len(final_url) > 0, jquote(final_url), 'null')//',"apiLane":'//jquote(api_lane_s)//',"parsedFormat":'// &
                   pick(fmt == 'null' .or. len(fmt) == 0, 'null', jquote(fmt))//',"transport":null')
    call sb_add(o, ',"apiRequests":['//sb_str(reqs)//']')
    ! counts
    if (t > 0) then
       hum = 0; asst = 0; cits = 0; curl = 0; atts = 0; summ = 0; tools = 0
       tu = jfirst(arr_or0(jget(t, 'turns')))
       do while (tu > 0)
          if (jequal_str(jget(tu, 'sender'), 'assistant')) asst = asst + 1
          if (jequal_str(jget(tu, 'sender'), 'human')) hum = hum + 1
          atts = atts + jlen(arr_or0(jget(tu, 'attachments')))
          a = jfirst(arr_or0(jget(tu, 'citations')))
          do while (a > 0)
             cits = cits + 1
             if (truthy(jget(a, 'url'))) curl = curl + 1
             a = jnext(a)
          end do
          rl = jfirst(arr_or0(jget(jget(tu, 'thinking'), 'rollouts')))
          do while (rl > 0)
             ev = jfirst(arr_or0(jget(rl, 'events')))
             do while (ev > 0)
                if (jequal_str(jget(ev, 'type'), 'summary')) summ = summ + 1
                if (jequal_str(jget(ev, 'type'), 'tool')) tools = tools + 1
                ev = jnext(ev)
             end do
             rl = jnext(rl)
          end do
          tu = jnext(tu)
       end do
       call sb_add(o, ',"counts":{"turns":'//itoa(jlen(arr_or0(jget(t, 'turns'))))//',"assistantTurns":'//itoa(asst)// &
                      ',"humanTurns":'//itoa(hum)//',"citations":'//itoa(cits)//',"citationsWithUrl":'//itoa(curl)// &
                      ',"attachments":'//itoa(atts)//',"summaries":'//itoa(summ)//',"toolEvents":'//itoa(tools)//'}')
    else
       call sb_add(o, ',"counts":{}')
    end if
    call sb_add(o, ',"domLane":{"rounds":[')
    do i = 1, nrounds
       if (i > 1) call sb_add(o, ',')
       call sb_add(o, rounds(i)%rec)
    end do
    call sb_add(o, '],"screenshots":'//strlist_json(shot_files, nshot_files)//',"citationChipsHelper":'// &
                   pick(chip_links1 > 0, '"raw/citation-chips-round1.json"', 'null')//'}')
    call sb_add(o, ',"sandbox":'//sandbox_json//',"citationEnrichment":'//enrich_json//',"citationSources":'//citsrc_json)
    vtext = read_file(dir//'\verification.json', ok)
    if (ok) then
       v = jparse(vtext); s = jget(v, 'summary')
       call sb_add(o, ',"verification":{"file":"verification.json","ok":'//canon_json(jget(v, 'ok'))//',"total":'// &
                      canon_json(jget(s, 'total'))//',"passed":'//canon_json(jget(s, 'ok'))//',"failed":'//canon_json(jget(s, 'failed'))// &
                      ',"failedChecks":'//pick(jis_arr(jget(v, 'failedChecks')), canon_json(jget(v, 'failedChecks')), '[]')// &
                      ',"api":'//canon_json(jget(v, 'api'))//',"dom":'//canon_json(jget(v, 'dom'))//',"warnings":'// &
                      itoa(jlen(arr_or0(jget(v, 'warnings'))))//'}')
    else
       call sb_add(o, ',"verification":"not run"')
    end if
    block
      type(jw) :: wb
      call behavior_write_manifest_record(wb, beh, trim(beh_status), beh_ms)
      call sb_add(o, ',"behavior":'//sb_str(wb%b))
    end block
    call sb_add(o, ',"attachments":['//sb_str(att_list)//'],"warnings":'//strlist_json(gwarn, ngw)//',"notes":'// &
                   strlist_json(notes_list, nnotes)//',"error":'//pick(len(err) > 0, jquote(err), 'null')//',"exitCode":'// &
                   itoa(exit_code)//',"files":[')
    do f = 1, nfiles
       if (f > 1) call sb_add(o, ',')
       call sb_add(o, '{"path":'//jquote(files(f)%path)//',"sizeBytes":'//i64toa(files(f)%size)//',"sha256":'//jquote(files(f)%sha)//'}')
    end do
    call sb_add(o, ']}')
    block
      type(jw) :: w
      call jcanon_value(w, jparse(sb_str(o)))
      call write_file(dir//'\manifest.json', jw_result(w))
    end block
  end subroutine write_manifest

end program exporter_f
