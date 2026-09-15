! fx_cdp.f90 — Chrome DevTools Protocol over fx_net: targets, calls, page script evaluation.
module fx_cdp
  use iso_fortran_env, only: int64
  use fx_util
  use fx_json
  use fx_net
  implicit none
  private
  public :: cdp, cdp_version_ok, cdp_new_tab, cdp_new_window, cdp_close_tab, cdp_connect, cdp_call, cdp_eval, &
            cdp_run_function, cdp_disconnect

  type :: cdp
     type(ws_conn) :: ws
     integer :: next_id = 1
  end type cdp

contains

  logical function cdp_version_ok(port)
    integer, intent(in) :: port
    integer :: st
    character(len=:), allocatable :: body
    call http_request(port, 'GET', '/json/version', st, body)
    cdp_version_ok = (st == 200)
  end function cdp_version_ok

  ! new tab -> (id, websocket url); ok false on failure
  subroutine cdp_new_tab(port, url, id, wsurl, ok)
    integer, intent(in) :: port
    character(len=*), intent(in) :: url
    character(len=:), allocatable, intent(out) :: id, wsurl
    logical, intent(out) :: ok
    integer :: st, root
    character(len=:), allocatable :: body
    ok = .false.; id = ''; wsurl = ''
    call http_request(port, 'PUT', '/json/new?'//url, st, body)
    if (st /= 200) return
    root = jparse(body)
    id = jstr(jget(root, 'id')); wsurl = jstr(jget(root, 'webSocketDebuggerUrl'))
    ok = (len(id) > 0 .and. len(wsurl) > 0)
  end subroutine cdp_new_tab

  ! A page in its own browser window (Target.createTarget newWindow).  A tab behind another tab of the same
  ! window is hidden and stops rendering (no requestAnimationFrame, no IntersectionObserver), so a page that
  ! loads older content when scrolled to the top never loads it there; a separate window keeps rendering.
  subroutine cdp_new_window(port, url, id, wsurl, ok)
    integer, intent(in) :: port
    character(len=*), intent(in) :: url
    character(len=:), allocatable, intent(out) :: id, wsurl
    logical, intent(out) :: ok
    type(cdp) :: bc
    integer :: st, root, r, k, e
    character(len=:), allocatable :: body
    ok = .false.; id = ''; wsurl = ''
    call http_request(port, 'GET', '/json/version', st, body)
    if (st /= 200) return
    root = jparse(body)
    if (.not. cdp_connect(bc, jstr(jget(root, 'webSocketDebuggerUrl')), 10000)) return
    r = cdp_call(bc, 'Target.createTarget', '{"url":'//jquote(url)//',"newWindow":true}')
    if (r > 0) then
       if (jis_str(jget(jget(r, 'result'), 'targetId'))) id = jstr(jget(jget(r, 'result'), 'targetId'))
    end if
    call cdp_disconnect(bc)
    if (len(id) == 0) return
    do k = 1, 40
       call http_request(port, 'GET', '/json/list', st, body)
       if (st == 200) then
          e = jfirst(jparse(body))
          do while (e > 0)
             if (jequal_str(jget(e, 'id'), id)) then
                wsurl = jstr(jget(e, 'webSocketDebuggerUrl')); ok = (len(wsurl) > 0); return
             end if
             e = jnext(e)
          end do
       end if
       call sleep_ms(50)
    end do
  end subroutine cdp_new_window

  subroutine cdp_close_tab(port, id)
    integer, intent(in) :: port
    character(len=*), intent(in) :: id
    integer :: st
    character(len=:), allocatable :: body
    call http_request(port, 'GET', '/json/close/'//id, st, body)
  end subroutine cdp_close_tab

  logical function cdp_connect(c, wsurl, timeout_ms)
    type(cdp), intent(inout) :: c
    character(len=*), intent(in) :: wsurl
    integer, intent(in) :: timeout_ms
    cdp_connect = ws_open(c%ws, wsurl, timeout_ms)
  end function cdp_connect

  subroutine cdp_disconnect(c)
    type(cdp), intent(inout) :: c
    call ws_close(c%ws)
  end subroutine cdp_disconnect

  ! send one command, wait for its reply; returns root handle of the reply (0 on failure)
  integer function cdp_call(c, method, params_json) result(reply)
    type(cdp), intent(inout) :: c
    character(len=*), intent(in) :: method, params_json
    integer :: myid, root
    character(len=:), allocatable :: msg
    logical :: ok
    reply = 0
    myid = c%next_id; c%next_id = c%next_id + 1
    if (.not. ws_send_text(c%ws, '{"id":'//itoa(myid)//',"method":'//jquote(method)//',"params":'//params_json//'}')) return
    do
       msg = ws_recv_message(c%ws, ok)
       if (.not. ok) return
       if (index(msg, '"id":'//itoa(myid)) == 0) cycle     ! events and other replies
       root = jparse(msg)
       if (jint(jget(root, 'id')) == myid) then
          reply = root; return
       end if
    end do
  end function cdp_call

  ! Runtime.evaluate with returnByValue; value handle or 0, err set on exception/failure
  integer function cdp_eval(c, expr, await_promise, err) result(v)
    type(cdp), intent(inout) :: c
    character(len=*), intent(in) :: expr
    logical, intent(in) :: await_promise
    character(len=:), allocatable, intent(out) :: err
    integer :: r, res
    character(len=5) :: aw
    err = ''; v = 0
    aw = 'false'; if (await_promise) aw = 'true '
    r = cdp_call(c, 'Runtime.evaluate', '{"expression":'//jquote(expr)//',"awaitPromise":'//trim(aw)// &
                 ',"returnByValue":true,"userGesture":true}')
    if (r == 0) then
       err = 'no reply to Runtime.evaluate (connection closed or timed out)'; return
    end if
    if (jget(r, 'error') > 0) then
       err = 'CDP error: '//jstr(jget(jget(r, 'error'), 'message')); return
    end if
    res = jget(r, 'result')
    if (jget(res, 'exceptionDetails') > 0) then
       err = 'JavaScript exception: '//jstr(jget(jget(jget(res, 'exceptionDetails'), 'exception'), 'description'))
       if (len(err) <= 22) err = err // jstr(jget(jget(res, 'exceptionDetails'), 'text'))
       return
    end if
    v = jget(jget(res, 'result'), 'value')
  end function cdp_eval

  ! "(async (opts) => {...})(opts)" style call
  integer function cdp_run_function(c, fn_source, opts_json, err) result(v)
    type(cdp), intent(inout) :: c
    character(len=*), intent(in) :: fn_source, opts_json
    character(len=:), allocatable, intent(out) :: err
    v = cdp_eval(c, '('//fn_source//')('//opts_json//')', .true., err)
  end function cdp_run_function

end module fx_cdp
