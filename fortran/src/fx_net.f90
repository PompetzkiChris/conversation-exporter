! fx_net.f90 — TCP (Winsock), HTTP/1.1 to 127.0.0.1, SHA-1, base64, WebSocket client.
! Pure Fortran calling ws2_32.dll through ISO_C_BINDING (Windows x64 has one calling convention).
module fx_net
  use iso_c_binding
  use iso_fortran_env, only: int64, int32, int8
  use fx_util
  implicit none
  private
  public :: net_init, tcp_open, tcp_close, tcp_send, http_request, sha1_bytes, base64_encode, sha256_hex, base64_decode, &
            ws_conn, ws_open, ws_send_text, ws_recv_message, ws_close

  integer(c_intptr_t), parameter :: INVALID_SOCKET = -1_c_intptr_t

  type, bind(C) :: sockaddr_in
     integer(c_int16_t) :: sin_family
     integer(c_int16_t) :: sin_port
     integer(c_int32_t) :: sin_addr
     character(kind=c_char) :: sin_zero(8)
  end type sockaddr_in

  type :: ws_conn
     integer(c_intptr_t) :: sock = INVALID_SOCKET
     character(len=:), allocatable :: buf     ! received but unconsumed bytes
     integer :: bpos = 1
     logical :: open = .false.
  end type ws_conn

  interface
     integer(c_int) function WSAStartup(ver, data) bind(C, name='WSAStartup')
       import :: c_int16_t, c_int, c_ptr
       integer(c_int16_t), value :: ver
       type(c_ptr), value :: data
     end function
     integer(c_intptr_t) function c_socket(af, typ, proto) bind(C, name='socket')
       import :: c_int, c_intptr_t
       integer(c_int), value :: af, typ, proto
     end function
     integer(c_int) function c_connect(s, addr, alen) bind(C, name='connect')
       import :: c_intptr_t, c_int, sockaddr_in
       integer(c_intptr_t), value :: s
       type(sockaddr_in), intent(in) :: addr
       integer(c_int), value :: alen
     end function
     integer(c_int) function c_send(s, buf, n, flags) bind(C, name='send')
       import :: c_intptr_t, c_int, c_char
       integer(c_intptr_t), value :: s
       character(kind=c_char), intent(in) :: buf(*)
       integer(c_int), value :: n, flags
     end function
     integer(c_int) function c_recv(s, buf, n, flags) bind(C, name='recv')
       import :: c_intptr_t, c_int, c_char
       integer(c_intptr_t), value :: s
       character(kind=c_char), intent(out) :: buf(*)
       integer(c_int), value :: n, flags
     end function
     integer(c_int) function closesocket(s) bind(C, name='closesocket')
       import :: c_intptr_t, c_int
       integer(c_intptr_t), value :: s
     end function
     integer(c_int) function setsockopt(s, level, optname, optval, optlen) bind(C, name='setsockopt')
       import :: c_intptr_t, c_int, c_int32_t
       integer(c_intptr_t), value :: s
       integer(c_int), value :: level, optname
       integer(c_int32_t), intent(in) :: optval
       integer(c_int), value :: optlen
     end function
  end interface

  logical, save :: started = .false.

contains

  subroutine net_init()
    integer(c_int8_t), target, save :: wsadata(1024)
    integer(c_int) :: rc
    if (started) return
    rc = WSAStartup(int(z'0202', c_int16_t), c_loc(wsadata))
    started = (rc == 0)
  end subroutine net_init

  ! connect to 127.0.0.1:port; recv timeout in ms
  function tcp_open(port, timeout_ms) result(s)
    integer, intent(in) :: port, timeout_ms
    integer(c_intptr_t) :: s
    type(sockaddr_in) :: a
    integer(c_int) :: rc
    integer(c_int32_t) :: tmo
    call net_init()
    s = c_socket(2_c_int, 1_c_int, 6_c_int)          ! AF_INET, SOCK_STREAM, IPPROTO_TCP
    if (s == INVALID_SOCKET) return
    a%sin_family = 2_c_int16_t
    a%sin_port = transfer(achar(port/256)//achar(mod(port,256)), 0_c_int16_t)
    a%sin_addr = transfer(achar(127)//achar(0)//achar(0)//achar(1), 0_c_int32_t)
    a%sin_zero = c_null_char
    rc = c_connect(s, a, int(c_sizeof(a), c_int))
    if (rc /= 0) then
       rc = closesocket(s); s = INVALID_SOCKET; return
    end if
    tmo = int(timeout_ms, c_int32_t)
    rc = setsockopt(s, int(z'FFFF', c_int), int(z'1006', c_int), tmo, 4_c_int)   ! SOL_SOCKET, SO_RCVTIMEO
  end function tcp_open

  subroutine tcp_close(s)
    integer(c_intptr_t), intent(inout) :: s
    integer(c_int) :: rc
    if (s /= INVALID_SOCKET) rc = closesocket(s)
    s = INVALID_SOCKET
  end subroutine tcp_close

  logical function tcp_send(s, data)
    integer(c_intptr_t), intent(in) :: s
    character(len=*), intent(in) :: data
    integer :: off, k
    integer(c_int) :: rc
    tcp_send = .false.
    off = 1
    do while (off <= len(data))
       k = min(len(data) - off + 1, 1048576)
       rc = c_send(s, data(off:off+k-1), int(k, c_int), 0_c_int)
       if (rc <= 0) return
       off = off + rc
    end do
    tcp_send = .true.
  end function tcp_send

  ! read up to 65536 bytes; returns '' and ok=.false. on close/timeout
  function tcp_recv_some(s, ok) result(r)
    integer(c_intptr_t), intent(in) :: s
    logical, intent(out) :: ok
    character(len=:), allocatable :: r
    character(len=65536) :: chunk
    integer(c_int) :: rc
    rc = c_recv(s, chunk, 65536_c_int, 0_c_int)
    ok = (rc > 0)
    if (rc > 0) then
       r = chunk(1:rc)
    else
       r = ''
    end if
  end function tcp_recv_some

  ! HTTP/1.1 request to 127.0.0.1:port, read until the server closes
  subroutine http_request(port, method, path, status, body)
    integer, intent(in) :: port
    character(len=*), intent(in) :: method, path
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: body
    integer(c_intptr_t) :: s
    type(strbuf) :: acc
    character(len=:), allocatable :: all, chunk
    logical :: ok
    integer :: h, ios, clen
    status = 0; body = ''
    s = tcp_open(port, 30000)
    if (s == INVALID_SOCKET) return
    if (.not. tcp_send(s, method//' '//path//' HTTP/1.1'//achar(13)//achar(10)// &
         'Host: 127.0.0.1:'//itoa(port)//achar(13)//achar(10)// &
         'Content-Length: 0'//achar(13)//achar(10)// &
         'Connection: close'//achar(13)//achar(10)//achar(13)//achar(10))) then
       call tcp_close(s); return
    end if
    ! Chrome's DevTools HTTP server keeps the connection open: stop once Content-Length bytes arrived
    clen = -1
    do
       chunk = tcp_recv_some(s, ok)
       if (.not. ok) exit
       call sb_add(acc, chunk)
       all = sb_str(acc)
       h = index(all, achar(13)//achar(10)//achar(13)//achar(10))
       if (h > 0) then
          if (clen < 0) clen = content_length(all(1:h))
          if (clen >= 0 .and. len(all) - (h + 3) >= clen) exit
       end if
    end do
    call tcp_close(s)
    all = sb_str(acc)
    if (len(all) >= 12) read(all(10:12), *, iostat=ios) status
    h = index(all, achar(13)//achar(10)//achar(13)//achar(10))
    if (h > 0) body = all(h+4:)
  contains
    integer function content_length(headers)
      character(len=*), intent(in) :: headers
      integer :: k, e, ios2
      character(len=:), allocatable :: low
      integer :: i2, ch
      low = headers
      do i2 = 1, len(low)
         ch = iachar(low(i2:i2))
         if (ch >= 65 .and. ch <= 90) low(i2:i2) = achar(ch + 32)
      end do
      content_length = -1
      k = index(low, 'content-length:')
      if (k == 0) return
      e = index(low(k:), achar(13))
      read(headers(k+15:k+e-2), *, iostat=ios2) content_length
      if (ios2 /= 0) content_length = -1
    end function content_length
  end subroutine http_request

  ! ---------------------------------------------------------------- SHA-1 / base64

  function sha1_bytes(msg) result(dig)
    character(len=*), intent(in) :: msg
    character(len=20) :: dig
    integer(int64), parameter :: M32 = int(z'FFFFFFFF', int64)
    integer(int64) :: h(0:4), w(0:79), a, b, c, d, e, f, k, t
    integer(int64) :: bitlen
    character(len=:), allocatable :: m
    integer :: nblocks, blk, i, j, padlen
    bitlen = int(len(msg), int64) * 8_int64
    padlen = mod(55 - mod(len(msg), 64) + 64, 64)
    m = msg // achar(128) // repeat(achar(0), padlen)
    do i = 7, 0, -1
       m = m // achar(int(iand(ishft(bitlen, -8*i), 255_int64)))
    end do
    h = [int(z'67452301',int64), int(z'EFCDAB89',int64), int(z'98BADCFE',int64), int(z'10325476',int64), int(z'C3D2E1F0',int64)]
    nblocks = len(m) / 64
    do blk = 0, nblocks - 1
       do i = 0, 15
          j = blk*64 + i*4
          w(i) = ior(ior(ishft(byte(m, j+1), 24), ishft(byte(m, j+2), 16)), ior(ishft(byte(m, j+3), 8), byte(m, j+4)))
       end do
       do i = 16, 79
          w(i) = rotl(ieor(ieor(w(i-3), w(i-8)), ieor(w(i-14), w(i-16))), 1)
       end do
       a = h(0); b = h(1); c = h(2); d = h(3); e = h(4)
       do i = 0, 79
          if (i < 20) then
             f = ior(iand(b, c), iand(iand(not(b), M32), d)); k = int(z'5A827999', int64)
          else if (i < 40) then
             f = ieor(ieor(b, c), d); k = int(z'6ED9EBA1', int64)
          else if (i < 60) then
             f = ior(ior(iand(b, c), iand(b, d)), iand(c, d)); k = int(z'8F1BBCDC', int64)
          else
             f = ieor(ieor(b, c), d); k = int(z'CA62C1D6', int64)
          end if
          t = iand(rotl(a, 5) + f + e + k + w(i), M32)
          e = d; d = c; c = rotl(b, 30); b = a; a = t
       end do
       h(0) = iand(h(0) + a, M32); h(1) = iand(h(1) + b, M32); h(2) = iand(h(2) + c, M32)
       h(3) = iand(h(3) + d, M32); h(4) = iand(h(4) + e, M32)
    end do
    do i = 0, 4
       do j = 0, 3
          dig(i*4+j+1:i*4+j+1) = achar(int(iand(ishft(h(i), -8*(3-j)), 255_int64)))
       end do
    end do
  contains
    integer(int64) function byte(s, pos)
      character(len=*), intent(in) :: s
      integer, intent(in) :: pos
      byte = int(iachar(s(pos:pos)), int64)
      if (byte < 0) byte = byte + 256
    end function byte
    integer(int64) function rotl(x, n)
      integer(int64), intent(in) :: x
      integer, intent(in) :: n
      rotl = iand(ior(ishft(x, n), ishft(x, n - 32)), M32)
    end function rotl
  end function sha1_bytes

  function base64_encode(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    character(len=64), parameter :: tbl = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    integer :: i, n, v, b1, b2, b3
    type(strbuf) :: o
    n = len(s)
    do i = 1, n, 3
       b1 = ub(i); b2 = 0; b3 = 0
       if (i+1 <= n) b2 = ub(i+1)
       if (i+2 <= n) b3 = ub(i+2)
       v = b1*65536 + b2*256 + b3
       call sb_add(o, tbl(v/262144+1:v/262144+1) // tbl(mod(v/4096,64)+1:mod(v/4096,64)+1))
       if (i+1 <= n) then
          call sb_add(o, tbl(mod(v/64,64)+1:mod(v/64,64)+1))
       else
          call sb_add(o, '=')
       end if
       if (i+2 <= n) then
          call sb_add(o, tbl(mod(v,64)+1:mod(v,64)+1))
       else
          call sb_add(o, '=')
       end if
    end do
    r = sb_str(o)
  contains
    integer function ub(pos)
      integer, intent(in) :: pos
      ub = iachar(s(pos:pos))
      if (ub < 0) ub = ub + 256
    end function ub
  end function base64_encode

  ! ---------------------------------------------------------------- SHA-256 / base64 decode

  function sha256_hex(msg) result(hex)
    character(len=*), intent(in) :: msg
    character(len=64) :: hex
    integer(int64), parameter :: M32 = int(z'FFFFFFFF', int64)
    integer(int64), parameter :: K(0:63) = [ &
      int(z'428a2f98',int64), int(z'71374491',int64), int(z'b5c0fbcf',int64), int(z'e9b5dba5',int64), &
      int(z'3956c25b',int64), int(z'59f111f1',int64), int(z'923f82a4',int64), int(z'ab1c5ed5',int64), &
      int(z'd807aa98',int64), int(z'12835b01',int64), int(z'243185be',int64), int(z'550c7dc3',int64), &
      int(z'72be5d74',int64), int(z'80deb1fe',int64), int(z'9bdc06a7',int64), int(z'c19bf174',int64), &
      int(z'e49b69c1',int64), int(z'efbe4786',int64), int(z'0fc19dc6',int64), int(z'240ca1cc',int64), &
      int(z'2de92c6f',int64), int(z'4a7484aa',int64), int(z'5cb0a9dc',int64), int(z'76f988da',int64), &
      int(z'983e5152',int64), int(z'a831c66d',int64), int(z'b00327c8',int64), int(z'bf597fc7',int64), &
      int(z'c6e00bf3',int64), int(z'd5a79147',int64), int(z'06ca6351',int64), int(z'14292967',int64), &
      int(z'27b70a85',int64), int(z'2e1b2138',int64), int(z'4d2c6dfc',int64), int(z'53380d13',int64), &
      int(z'650a7354',int64), int(z'766a0abb',int64), int(z'81c2c92e',int64), int(z'92722c85',int64), &
      int(z'a2bfe8a1',int64), int(z'a81a664b',int64), int(z'c24b8b70',int64), int(z'c76c51a3',int64), &
      int(z'd192e819',int64), int(z'd6990624',int64), int(z'f40e3585',int64), int(z'106aa070',int64), &
      int(z'19a4c116',int64), int(z'1e376c08',int64), int(z'2748774c',int64), int(z'34b0bcb5',int64), &
      int(z'391c0cb3',int64), int(z'4ed8aa4a',int64), int(z'5b9cca4f',int64), int(z'682e6ff3',int64), &
      int(z'748f82ee',int64), int(z'78a5636f',int64), int(z'84c87814',int64), int(z'8cc70208',int64), &
      int(z'90befffa',int64), int(z'a4506ceb',int64), int(z'bef9a3f7',int64), int(z'c67178f2',int64) ]
    integer(int64) :: h(0:7), w(0:63), a, b, c, d, e, f, g, hh, t1, t2
    integer(int64) :: bitlen
    character(len=:), allocatable :: m
    character(len=16), parameter :: hx = '0123456789abcdef'
    integer :: nblocks, blk, i, j, padlen, byte
    bitlen = int(len(msg), int64) * 8_int64
    padlen = mod(55 - mod(len(msg), 64) + 64, 64)
    allocate(character(len=len(msg) + 1 + padlen + 8) :: m)
    m(1:len(msg)) = msg
    m(len(msg)+1:len(msg)+1) = achar(128)
    if (padlen > 0) m(len(msg)+2:len(msg)+1+padlen) = repeat(achar(0), padlen)
    do i = 7, 0, -1
       m(len(m)-i:len(m)-i) = achar(int(iand(ishft(bitlen, -8*i), 255_int64)))
    end do
    h = [int(z'6a09e667',int64), int(z'bb67ae85',int64), int(z'3c6ef372',int64), int(z'a54ff53a',int64), &
         int(z'510e527f',int64), int(z'9b05688c',int64), int(z'1f83d9ab',int64), int(z'5be0cd19',int64)]
    nblocks = len(m) / 64
    do blk = 0, nblocks - 1
       do i = 0, 15
          j = blk*64 + i*4
          w(i) = ior(ior(ishft(ub(j+1), 24), ishft(ub(j+2), 16)), ior(ishft(ub(j+3), 8), ub(j+4)))
       end do
       do i = 16, 63
          t1 = ieor(rotr(w(i-2), 17), ieor(rotr(w(i-2), 19), ishft(w(i-2), -10)))
          t2 = ieor(rotr(w(i-15), 7), ieor(rotr(w(i-15), 18), ishft(w(i-15), -3)))
          w(i) = iand(t1 + w(i-7) + t2 + w(i-16), M32)
       end do
       a = h(0); b = h(1); c = h(2); d = h(3); e = h(4); f = h(5); g = h(6); hh = h(7)
       do i = 0, 63
          t1 = iand(hh + ieor(rotr(e, 6), ieor(rotr(e, 11), rotr(e, 25))) + ior(iand(e, f), iand(iand(not(e), M32), g)) &
                    + K(i) + w(i), M32)
          t2 = iand(ieor(rotr(a, 2), ieor(rotr(a, 13), rotr(a, 22))) + ior(ior(iand(a, b), iand(a, c)), iand(b, c)), M32)
          hh = g; g = f; f = e; e = iand(d + t1, M32); d = c; c = b; b = a; a = iand(t1 + t2, M32)
       end do
       h(0) = iand(h(0)+a, M32); h(1) = iand(h(1)+b, M32); h(2) = iand(h(2)+c, M32); h(3) = iand(h(3)+d, M32)
       h(4) = iand(h(4)+e, M32); h(5) = iand(h(5)+f, M32); h(6) = iand(h(6)+g, M32); h(7) = iand(h(7)+hh, M32)
    end do
    do i = 0, 7
       do j = 0, 3
          byte = int(iand(ishft(h(i), -8*(3-j)), 255_int64))
          hex(i*8+j*2+1:i*8+j*2+1) = hx(byte/16+1:byte/16+1)
          hex(i*8+j*2+2:i*8+j*2+2) = hx(mod(byte,16)+1:mod(byte,16)+1)
       end do
    end do
  contains
    integer(int64) function ub(pos)
      integer, intent(in) :: pos
      ub = int(iachar(m(pos:pos)), int64)
      if (ub < 0) ub = ub + 256
    end function ub
    ! 32-bit right rotation (left shift of the complement, masked)
    integer(int64) function rotr(x, n)
      integer(int64), intent(in) :: x
      integer, intent(in) :: n
      rotr = iand(ior(ishft(x, -n), ishft(x, 32 - n)), M32)
    end function rotr
  end function sha256_hex

  function base64_decode(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: i, n, v, bits, c, o
    allocate(character(len=(len(s)*3)/4 + 3) :: r)
    v = 0; bits = 0; o = 0
    do i = 1, len(s)
       c = iachar(s(i:i))
       if (c >= 65 .and. c <= 90) then
          n = c - 65
       else if (c >= 97 .and. c <= 122) then
          n = c - 71
       else if (c >= 48 .and. c <= 57) then
          n = c + 4
       else if (c == 43 .or. c == 45) then
          n = 62
       else if (c == 47 .or. c == 95) then
          n = 63
       else
          cycle
       end if
       v = iand(v * 64 + n, 16777215)
       bits = bits + 6
       if (bits >= 8) then
          bits = bits - 8
          o = o + 1
          r(o:o) = achar(iand(ishft(v, -bits), 255))
       end if
    end do
    r = r(1:o)
  end function base64_decode

  ! ---------------------------------------------------------------- WebSocket

  ! ws://127.0.0.1:PORT/PATH
  logical function ws_open(c, url, timeout_ms)
    type(ws_conn), intent(inout) :: c
    character(len=*), intent(in) :: url
    integer, intent(in) :: timeout_ms
    character(len=:), allocatable :: rest, hostport, path, key, resp, chunk, expect
    integer :: k, port, ios
    real :: rnd
    character(len=16) :: raw
    logical :: ok
    ws_open = .false.
    if (.not. starts_with(url, 'ws://')) return
    rest = url(6:)
    k = index(rest, '/')
    hostport = rest(1:k-1); path = rest(k:)
    k = index(hostport, ':')
    read(hostport(k+1:), *, iostat=ios) port
    if (ios /= 0) return
    c%sock = tcp_open(port, timeout_ms)
    if (c%sock == INVALID_SOCKET) return
    do k = 1, 16
       call random_number(rnd); raw(k:k) = achar(int(rnd*255.0))
    end do
    key = base64_encode(raw)
    if (.not. tcp_send(c%sock, 'GET '//path//' HTTP/1.1'//crlf()//'Host: '//hostport//crlf()// &
         'Upgrade: websocket'//crlf()//'Connection: Upgrade'//crlf()//'Sec-WebSocket-Key: '//key//crlf()// &
         'Sec-WebSocket-Version: 13'//crlf()//crlf())) return
    resp = ''
    do while (index(resp, crlf()//crlf()) == 0)
       chunk = tcp_recv_some(c%sock, ok)
       if (.not. ok) return
       resp = resp // chunk
    end do
    if (index(resp, ' 101 ') == 0) return
    expect = base64_encode(sha1_bytes(key//'258EAFA5-E914-47DA-95CA-C5AB0DC85B11'))
    if (index(resp, expect) == 0) return
    k = index(resp, crlf()//crlf())
    c%buf = resp(k+4:)
    c%bpos = 1
    c%open = .true.
    ws_open = .true.
  end function ws_open

  function crlf() result(r)
    character(len=2) :: r
    r = achar(13)//achar(10)
  end function crlf

  logical function ws_send_text(c, text)
    type(ws_conn), intent(inout) :: c
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: frame, masked
    character(len=4) :: mask
    integer :: n, i, k
    real :: rnd
    integer(int64) :: n64
    n = len(text)
    do k = 1, 4
       call random_number(rnd); mask(k:k) = achar(int(rnd*255.0))
    end do
    if (n < 126) then
       frame = achar(129) // achar(128 + n)
    else if (n < 65536) then
       frame = achar(129) // achar(254) // achar(n/256) // achar(mod(n,256))
    else
       n64 = int(n, int64)
       frame = achar(129) // achar(255)
       do i = 7, 0, -1
          frame = frame // achar(int(iand(ishft(n64, -8*i), 255_int64)))
       end do
    end if
    allocate(character(len=n) :: masked)
    do i = 1, n
       masked(i:i) = achar(ieor(ub1(text(i:i)), ub1(mask(mod(i-1,4)+1:mod(i-1,4)+1))))
    end do
    ws_send_text = tcp_send(c%sock, frame // mask // masked)
  end function ws_send_text

  integer function ub1(ch)
    character, intent(in) :: ch
    ub1 = iachar(ch)
    if (ub1 < 0) ub1 = ub1 + 256
  end function ub1

  ! ensure at least n unread bytes are buffered
  logical function fill(c, n)
    type(ws_conn), intent(inout) :: c
    integer, intent(in) :: n
    character(len=:), allocatable :: chunk
    logical :: ok
    fill = .true.
    if (c%bpos > 1048576) then
       c%buf = c%buf(c%bpos:); c%bpos = 1
    end if
    do while (len(c%buf) - c%bpos + 1 < n)
       chunk = tcp_recv_some(c%sock, ok)
       if (.not. ok) then
          fill = .false.; return
       end if
       c%buf = c%buf // chunk
    end do
  end function fill

  ! next complete text message (continuations joined, pings answered); ok=.false. on close/timeout
  function ws_recv_message(c, ok) result(msg)
    type(ws_conn), intent(inout) :: c
    logical, intent(out) :: ok
    character(len=:), allocatable :: msg
    type(strbuf) :: acc
    integer :: b0, b1, op, hdr, i
    integer(int64) :: plen
    logical :: fin
    ok = .false.; msg = ''
    do
       if (.not. fill(c, 2)) return
       b0 = ub1(c%buf(c%bpos:c%bpos)); b1 = ub1(c%buf(c%bpos+1:c%bpos+1))
       fin = btest(b0, 7); op = iand(b0, 15)
       plen = int(iand(b1, 127), int64); hdr = 2
       if (plen == 126) then
          if (.not. fill(c, 4)) return
          plen = int(ub1(c%buf(c%bpos+2:c%bpos+2))*256 + ub1(c%buf(c%bpos+3:c%bpos+3)), int64); hdr = 4
       else if (plen == 127) then
          if (.not. fill(c, 10)) return
          plen = 0
          do i = 2, 9
             plen = plen*256_int64 + int(ub1(c%buf(c%bpos+i:c%bpos+i)), int64)
          end do
          hdr = 10
       end if
       if (.not. fill(c, hdr + int(plen))) return
       select case (op)
       case (0, 1, 2)
          call sb_add(acc, c%buf(c%bpos+hdr:c%bpos+hdr+int(plen)-1))
          c%bpos = c%bpos + hdr + int(plen)
          if (fin) then
             msg = sb_str(acc); ok = .true.; return
          end if
       case (8)
          c%bpos = c%bpos + hdr + int(plen); c%open = .false.; return
       case (9)
          ok = tcp_send(c%sock, achar(138)//achar(128)//'abcd')   ! pong, empty masked payload
          c%bpos = c%bpos + hdr + int(plen)
       case default
          c%bpos = c%bpos + hdr + int(plen)
       end select
    end do
  end function ws_recv_message

  subroutine ws_close(c)
    type(ws_conn), intent(inout) :: c
    logical :: ok
    if (c%open) ok = tcp_send(c%sock, achar(136)//achar(128)//'abcd')
    call tcp_close(c%sock)
    c%open = .false.
  end subroutine ws_close

end module fx_net
