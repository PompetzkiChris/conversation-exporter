! fx_md.f90 — transcript.md writer, port of racket/md.rkt (itself a port of reference_transcript.py to_markdown).
! Used by all three lanes (Grok, Gemini, Qwen transcripts share the schema).
module fx_md
  use iso_fortran_env, only: int64
  use fx_util
  use fx_json
  use fx_grok, only: truthy, py_str
  implicit none
  private
  public :: transcript_markdown, pretty_json_string, pretty_json_value

contains

  integer function ub(ch)
    character, intent(in) :: ch
    ub = iachar(ch)
    if (ub < 0) ub = ub + 256
  end function ub

  ! number of code points
  integer function ncp(s)
    character(len=*), intent(in) :: s
    integer :: i, c
    ncp = 0
    do i = 1, len(s)
       c = ub(s(i:i))
       if (c < 128 .or. c >= 192) ncp = ncp + 1
    end do
  end function ncp

  ! byte index (1-based) where code point number k (0-based) starts; len+1 when k >= ncp
  integer function cp_byte(s, k)
    character(len=*), intent(in) :: s
    integer, intent(in) :: k
    integer :: i, c, n
    n = -1
    do i = 1, len(s)
       c = ub(s(i:i))
       if (c < 128 .or. c >= 192) then
          n = n + 1
          if (n == k) then
             cp_byte = i; return
          end if
       end if
    end do
    cp_byte = len(s) + 1
  end function cp_byte

  ! Python `a or b or c`: first truthy, else the last
  integer function py_or(a, b)
    integer, intent(in) :: a, b
    if (truthy(a)) then
       py_or = a
    else
       py_or = b
    end if
  end function py_or

  function one_line(p, limit) result(r)
    integer, intent(in) :: p, limit        ! limit <= 0: none
    character(len=:), allocatable :: r
    if (jis_str(p)) then
       r = jstr(p)
    else if (truthy(p)) then
       r = py_str(p)
    else
       r = ''
    end if
    r = replace_all(replace_all(r, achar(13), ''), achar(10), ' ')
    if (limit > 0) then
       if (ncp(r) > limit) r = r(1:cp_byte(r, limit)-1)
    end if
  end function one_line

  ! py-or over 3 values with "" as the final fallback, then py-str
  function str_or3(a, b, c) result(r)
    integer, intent(in) :: a, b, c
    character(len=:), allocatable :: r
    if (truthy(a)) then
       r = py_str(a)
    else if (truthy(b)) then
       r = py_str(b)
    else if (c /= 0 .and. truthy(c)) then
       r = py_str(c)
    else
       r = ''
    end if
  end function str_or3

  function text_with_citation_marks(text, cits) result(r)
    character(len=*), intent(in) :: text
    integer, intent(in) :: cits
    character(len=:), allocatable :: r
    type(strbuf) :: o
    integer :: n, ci, i, last, off
    if (jlen(cits) == 0) then
       r = text; return
    end if
    n = ncp(text)
    ci = jfirst(cits); i = 1; last = 0
    do while (ci > 0)
       off = 0
       if (jis_int(jget(ci, 'offset'))) off = int(jint(jget(ci, 'offset')))
       off = min(max(off, 0), n)
       if (off > last) call sb_add(o, text(cp_byte(text, last):cp_byte(text, off)-1))
       call sb_add(o, '['//itoa(i)//']')
       last = off; i = i + 1
       ci = jnext(ci)
    end do
    if (last < n) call sb_add(o, text(cp_byte(text, last):))
    r = sb_str(o)
  end function text_with_citation_marks

  function tool_label(k) result(r)
    character(len=*), intent(in) :: k
    character(len=:), allocatable :: r
    select case (k)
    case ('webSearch');           r = 'Searched web'
    case ('xSearch');             r = 'Searched X'
    case ('xUserSearch');         r = 'Searched X users'
    case ('browsePage');          r = 'Browsed'
    case ('conversationSearch');  r = 'Searching conversations'
    case ('viewImage');           r = 'View Image'
    case ('initTerminalSession'); r = 'Connected to computer'
    case ('chatroomSend');        r = 'Sent to All'
    case ('bash');                r = 'Ran command'
    case ('editFile');            r = 'Wrote file'
    case ('readFile');            r = 'Read file'
    case ('listDir');             r = 'Listed directory'
    case ('imageSearch');         r = 'Searched images'
    case ('mcp');                 r = 'Tool'
    case default;                 r = k
    end select
  end function tool_label

  logical function is_build_kind(k)
    character(len=*), intent(in) :: k
    is_build_kind = (k == 'bash' .or. k == 'editFile' .or. k == 'readFile' .or. k == 'listDir' .or. &
                     k == 'imageSearch' .or. k == 'mcp')
  end function is_build_kind

  ! args lookup: camelCase key, then snake_case key; "" when absent
  function arg_str(a, k1, k2) result(r)
    integer, intent(in) :: a
    character(len=*), intent(in) :: k1
    character(len=*), intent(in), optional :: k2
    character(len=:), allocatable :: r
    integer :: v
    r = ''
    v = jget(a, k1)
    if (jis_str(v)) then
       r = jstr(v); return
    else if (v > 0 .and. .not. jis_null(v)) then
       r = py_str(v); return
    end if
    if (present(k2)) then
       v = jget(a, k2)
       if (jis_str(v)) then
          r = jstr(v)
       else if (v > 0 .and. .not. jis_null(v)) then
          r = py_str(v)
       end if
    end if
  end function arg_str

  integer function max_backtick_run(s)
    character(len=*), intent(in) :: s
    integer :: i, run
    run = 0; max_backtick_run = 0
    do i = 1, len(s)
       if (s(i:i) == '`') then
          run = run + 1
          max_backtick_run = max(max_backtick_run, run)
       else
          run = 0
       end if
    end do
  end function max_backtick_run

  function path_language(p) result(r)
    character(len=*), intent(in) :: p
    character(len=:), allocatable :: r, ext
    integer :: i, c
    r = ''
    i = len(p)
    do while (i >= 1)
       c = iachar(p(i:i))
       if (.not. ((c >= 48 .and. c <= 57) .or. (c >= 65 .and. c <= 90) .or. (c >= 97 .and. c <= 122) .or. c == 95)) exit
       i = i - 1
    end do
    if (i < 1 .or. i == len(p)) return
    if (p(i:i) /= '.') return
    ext = p(i+1:)
    do c = 1, len(ext)
       if (ext(c:c) >= 'A' .and. ext(c:c) <= 'Z') ext(c:c) = achar(iachar(ext(c:c)) + 32)
    end do
    select case (ext)
    case ('py');                        r = 'python'
    case ('sh', 'bash');                r = 'bash'
    case ('js', 'mjs', 'cjs');          r = 'javascript'
    case ('ts');                        r = 'typescript'
    case ('tsx');                       r = 'tsx'
    case ('jsx');                       r = 'jsx'
    case ('json');                      r = 'json'
    case ('md');                        r = 'markdown'
    case ('html', 'htm');               r = 'html'
    case ('css');                       r = 'css'
    case ('c', 'h');                    r = 'c'
    case ('cpp', 'hpp');                r = 'cpp'
    case ('rkt');                       r = 'racket'
    case ('rs');                        r = 'rust'
    case ('go');                        r = 'go'
    case ('java');                      r = 'java'
    case ('rb');                        r = 'ruby'
    case ('php');                       r = 'php'
    case ('sql');                       r = 'sql'
    case ('yml', 'yaml');               r = 'yaml'
    case ('toml');                      r = 'toml'
    case ('xml', 'svg');                r = 'xml'
    case ('ini');                       r = 'ini'
    case ('csv');                       r = 'csv'
    case default;                       r = ''
    end select
  end function path_language

  subroutine ln(o, s)
    type(strbuf), intent(inout) :: o
    character(len=*), intent(in) :: s
    call sb_add(o, s); call sb_add(o, achar(10))
  end subroutine ln

  ! fenced block indented two spaces, blank line before and after; fence longer than any backtick run
  subroutine emit_fenced(o, lang, body)
    type(strbuf), intent(inout) :: o
    character(len=*), intent(in) :: lang, body
    character(len=:), allocatable :: b, fence
    integer :: a, z
    b = replace_all(body, achar(13), '')
    fence = repeat('`', max(3, max_backtick_run(b) + 1))
    call ln(o, '')
    call ln(o, '  '//fence//lang)
    a = 1
    do
       z = index(b(a:), achar(10))
       if (z == 0) then
          call line(b(a:)); exit
       end if
       call line(b(a:a+z-2))
       a = a + z
    end do
    call ln(o, '  '//fence)
    call ln(o, '')
  contains
    subroutine line(t)
      character(len=*), intent(in) :: t
      if (len(t) == 0) then
         call ln(o, '')
      else
         call ln(o, '  '//t)
      end if
    end subroutine line
  end subroutine emit_fenced

  ! display-only JSON pretty printer (floats allowed)
  recursive subroutine write_pretty(o, v, ind)
    type(strbuf), intent(inout) :: o
    integer, intent(in) :: v, ind
    integer, allocatable :: kids(:)
    integer :: n, i, j, t, c
    select case (jtype(v))
    case (J_OBJ)
       n = jlen(v)
       if (n == 0) then
          call sb_add(o, '{}'); return
       end if
       allocate(kids(n))
       c = jfirst(v); i = 0
       do while (c > 0)
          i = i + 1; kids(i) = c; c = jnext(c)
       end do
       do i = 2, n
          t = kids(i); j = i - 1
          do while (j >= 1)
             if (.not. lgt(jname(kids(j)), jname(t))) exit
             kids(j+1) = kids(j); j = j - 1
          end do
          kids(j+1) = t
       end do
       call sb_add(o, '{'//achar(10))
       do i = 1, n
          if (i > 1) call sb_add(o, ','//achar(10))
          call sb_add(o, repeat(' ', ind + 1)//jquote(jname(kids(i)))//': ')
          call write_pretty(o, kids(i), ind + 1)
       end do
       call sb_add(o, achar(10)//repeat(' ', ind)//'}')
    case (J_ARR)
       if (jlen(v) == 0) then
          call sb_add(o, '[]'); return
       end if
       call sb_add(o, '['//achar(10))
       c = jfirst(v); i = 0
       do while (c > 0)
          if (i > 0) call sb_add(o, ','//achar(10))
          call sb_add(o, repeat(' ', ind + 1))
          call write_pretty(o, c, ind + 1)
          i = i + 1; c = jnext(c)
       end do
       call sb_add(o, achar(10)//repeat(' ', ind)//']')
    case (J_STR)
       call sb_add(o, jquote(jstr(v)))
    case (J_TRUE)
       call sb_add(o, 'true')
    case (J_FALSE)
       call sb_add(o, 'false')
    case (J_NUM)
       call sb_add(o, racket_number(jraw_text(v)))
    case default
       call sb_add(o, 'null')
    end select
  end subroutine write_pretty

  ! Racket prints an exact integer as is and a flonum via number->string: "1.50" -> "1.5", "2" stays "2"
  function racket_number(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: k
    r = s
    if (scan(s, 'eE') > 0) return
    if (index(s, '.') == 0) return
    k = len(r)
    do while (k > index(r, '.') + 1)
       if (r(k:k) /= '0') exit
       k = k - 1
    end do
    r = r(1:k)
  end function racket_number

  function pretty_json_value(v) result(r)
    integer, intent(in) :: v
    character(len=:), allocatable :: r
    type(strbuf) :: o
    call write_pretty(o, v, 0)
    r = sb_str(o)
  end function pretty_json_value

  ! toolArgsJson is a JSON string: shown parsed when it parses as an object/array, raw otherwise
  function pretty_json_string(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: k, v
    r = s
    k = verify(s, ' '//achar(9)//achar(10)//achar(13))
    if (k == 0) return
    if (s(k:k) /= '{' .and. s(k:k) /= '[') return
    if (.not. json_complete(s(k:))) return
    v = jparse(s)
    if (jis_obj(v) .or. jis_arr(v)) r = pretty_json_value(v)
  end function pretty_json_string

  ! bracket balance outside strings, nothing but whitespace after the closing bracket
  logical function json_complete(s)
    character(len=*), intent(in) :: s
    integer :: i, depth
    logical :: instr, esc
    json_complete = .false.
    depth = 0; instr = .false.; esc = .false.
    do i = 1, len(s)
       if (instr) then
          if (esc) then
             esc = .false.
          else if (s(i:i) == '\') then
             esc = .true.
          else if (s(i:i) == '"') then
             instr = .false.
          end if
          cycle
       end if
       select case (s(i:i))
       case ('"'); instr = .true.
       case ('{', '['); depth = depth + 1
       case ('}', ']')
          depth = depth - 1
          if (depth < 0) return
          if (depth == 0) then
             json_complete = (verify(s(i+1:)//' ', ' '//achar(9)//achar(10)//achar(13)) == 0)
             return
          end if
       end select
    end do
  end function json_complete

  subroutine emit_image_surfaces(o, turn)
    type(strbuf), intent(inout) :: o
    integer, intent(in) :: turn
    character(len=16), parameter :: labels(3) = [character(len=16) :: 'Generated image', 'Image edit URI', 'Image attachment']
    character(len=18), parameter :: keys(3) = [character(len=18) :: 'generatedImageUrls', 'imageEditUris', 'imageAttachments']
    integer :: k, v, arr
    logical :: any
    any = .false.
    do k = 1, 3
       arr = jget(turn, trim(keys(k)))
       if (.not. jis_arr(arr)) cycle
       v = jfirst(arr)
       do while (v > 0)
          any = .true.
          if (jis_str(v)) then
             call ln(o, '- **'//trim(labels(k))//':** '//jstr(v))
          else
             call ln(o, '- **'//trim(labels(k))//':**')
             call emit_fenced(o, 'json', pretty_json_value(v))
          end if
          v = jnext(v)
       end do
    end do
    if (any) call ln(o, '')
  end subroutine emit_image_surfaces

  function count_suffix(items) result(r)
    integer, intent(in) :: items
    character(len=:), allocatable :: r
    if (jis_arr(items) .and. jlen(items) > 0) then
       r = '  ('//itoa(jlen(items))//' results)'
    else
       r = ''
    end if
  end function count_suffix

  integer function list_or0(p)
    integer, intent(in) :: p
    list_or0 = 0
    if (jis_arr(p)) list_or0 = p
  end function list_or0

  integer function hash_or0(p)
    integer, intent(in) :: p
    hash_or0 = 0
    if (jis_obj(p)) hash_or0 = p
  end function hash_or0

  subroutine emit_results(o, res)
    type(strbuf), intent(inout) :: o
    integer, intent(in) :: res
    integer :: it
    it = jfirst(list_or0(jget(res, 'items')))
    do while (it > 0)
       if (jequal_str(jget(res, 'kind'), 'web')) then
          call ln(o, '  - ['//one_line_or(jget(it, 'title'), jget(it, 'url'))//']('//py_str(jget(it, 'url'))//')')
       else if (jequal_str(jget(res, 'kind'), 'x')) then
          call ln(o, '  - @'//py_str(jget(it, 'username'))//' ('//py_str(jget(it, 'postId'))//'): '// &
                     one_line(jget(it, 'text'), 200))
       end if
       it = jnext(it)
    end do
  end subroutine emit_results

  ! one_line(a or b or "")
  function one_line_or(a, b) result(r)
    integer, intent(in) :: a, b
    character(len=:), allocatable :: r
    if (truthy(a)) then
       r = one_line(a, 0)
    else if (truthy(b)) then
       r = one_line(b, 0)
    else
       r = ''
    end if
  end function one_line_or

  subroutine emit_build_tool(o, label, k, a, res)
    type(strbuf), intent(inout) :: o
    character(len=*), intent(in) :: label, k
    integer, intent(in) :: a, res
    character(len=:), allocatable :: suffix, fp, olds, news, lang, cmd, det, aj
    integer :: v
    suffix = count_suffix(jget(res, 'items'))
    select case (k)
    case ('bash')
       call head(one_line_s(arg_str(a, 'description')))
       cmd = arg_str(a, 'command')
       if (len(cmd) > 0) call emit_fenced(o, 'bash', cmd)
    case ('editFile')
       fp = arg_str(a, 'filePath', 'file_path')
       olds = arg_str(a, 'oldString', 'old_string')
       news = arg_str(a, 'newString', 'new_string')
       lang = path_language(fp)
       call head(fp)
       if (len(olds) > 0) then
          call ln(o, '')
          call ln(o, '  Replaced:')
          call emit_fenced(o, lang, olds)
          call ln(o, '  With:')
       end if
       if (len(news) > 0 .or. len(olds) > 0) call emit_fenced(o, lang, news)
    case ('readFile')
       det = ''
       if (len(arg_str(a, 'fileType', 'file_type')) > 0) det = arg_str(a, 'fileType', 'file_type')
       v = jget(a, 'offset')
       if (jis_int(v)) det = join(det, 'offset '//i64toa(jint(v)))
       v = jget(a, 'limit')
       if (jis_int(v)) det = join(det, 'limit '//i64toa(jint(v)))
       if (len(det) > 0) then
          call head(arg_str(a, 'filePath', 'file_path')//'  ('//det//')')
       else
          call head(arg_str(a, 'filePath', 'file_path'))
       end if
    case ('listDir')
       call head(arg_str(a, 'targetDirectory', 'target_directory'))
    case ('imageSearch')
       call head(one_line_s(arg_str(a, 'imageDescription', 'image_description')))
       call emit_results(o, res)
    case default
       call head(arg_str(a, 'toolName', 'tool_name'))
       aj = arg_str(a, 'toolArgsJson', 'tool_args_json')
       if (len(aj) > 0) call emit_fenced(o, 'json', pretty_json_string(aj))
       call emit_results(o, res)
    end select
  contains
    subroutine head(s)
      character(len=*), intent(in) :: s
      call ln(o, '- **'//label//'** '//s//suffix)
    end subroutine head
    function join(a0, b0) result(r)
      character(len=*), intent(in) :: a0, b0
      character(len=:), allocatable :: r
      if (len(a0) == 0) then
         r = b0
      else
         r = a0//'; '//b0
      end if
    end function join
  end subroutine emit_build_tool

  function one_line_s(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    r = replace_all(replace_all(s, achar(13), ''), achar(10), ' ')
  end function one_line_s

  function transcript_markdown(t) result(md)
    integer, intent(in) :: t
    character(len=:), allocatable :: md
    type(strbuf) :: o
    integer :: c, turn, atts, a, th, rl, ev, args, res, items, cits, ci, n, s, webs, xs, w, p, qv, tr
    character(len=:), allocatable :: k, label, msg, q, ty
    c = jget(t, 'conversation')
    if (truthy(jget(c, 'title'))) then
       call ln(o, '# '//py_str(jget(c, 'title')))
    else if (truthy(jget(c, 'conversationId'))) then
       call ln(o, '# '//py_str(jget(c, 'conversationId')))
    else
       call ln(o, '# ')
    end if
    call ln(o, '')
    call ln(o, '- Conversation: '//py_str(jget(c, 'conversationId')))
    call ln(o, '- Source: '//py_str(jget(c, 'sourceUrl')))
    call ln(o, '- Created: '//py_str(jget(c, 'createTime')))
    call ln(o, '- Modified: '//py_str(jget(c, 'modifyTime')))
    call ln(o, '')
    turn = jfirst(list_or0(jget(t, 'turns')))
    do while (turn > 0)
       if (jequal_str(jget(turn, 'sender'), 'human')) then
          call ln(o, '## User  (turn '//py_str(jget(turn, 'index'))//', '//py_str(jget(turn, 'createTime'))//')')
          call ln(o, '')
          call emit_image_surfaces(o, turn)
          atts = list_or0(jget(turn, 'attachments'))
          a = jfirst(atts)
          do while (a > 0)
             call ln(o, '- Attachment: '//py_str(jget(a, 'fileName'))//' ('//py_str(jget(a, 'mimeType'))//', '// &
                        py_str(jget(a, 'sizeBytes'))//' bytes) '//py_str(jget(a, 'contentUrl')))
             a = jnext(a)
          end do
          if (jlen(atts) > 0) call ln(o, '')
          call ln(o, py_str(jget(turn, 'text')))
          call ln(o, '')
       else
          th = hash_or0(jget(turn, 'thinking'))
          call ln(o, '## Grok  (turn '//py_str(jget(turn, 'index'))//', '//py_str(jget(turn, 'createTime'))//', model '// &
                     py_str(jget(turn, 'model'))//')')
          call ln(o, '')
          call emit_image_surfaces(o, turn)
          call ln(o, '### Thoughts  ('//py_str(jget(th, 'durationMs'))//' ms)')
          call ln(o, '')
          rl = jfirst(list_or0(jget(th, 'rollouts')))
          do while (rl > 0)
             if (jequal_str(jget(rl, 'role'), 'Leader')) then
                call ln(o, '#### '//py_str(jget(rl, 'id'))//' (Leader)')
             else
                call ln(o, '#### '//py_str(jget(rl, 'id')))
             end if
             call ln(o, '')
             ev = jfirst(list_or0(jget(rl, 'events')))
             do while (ev > 0)
                ty = ''
                if (jis_str(jget(ev, 'type'))) ty = jstr(jget(ev, 'type'))
                if (ty == 'summary') then
                   call ln(o, '- _'//one_line(jget(ev, 'text'), 0)//'_')
                else if (ty == 'tool') then
                   args = hash_or0(jget(ev, 'args'))
                   if (jis_str(jget(ev, 'kind'))) then
                      k = jstr(jget(ev, 'kind')); label = tool_label(k)
                   else
                      k = ''; label = py_str(jget(ev, 'kind'))
                   end if
                   if (jis_str(jget(ev, 'kind')) .and. k == 'chatroomSend') then
                      call ln(o, '- **Sent to All:**')
                      call ln(o, '')
                      if (truthy(jget(args, 'message'))) then
                         msg = py_str(jget(args, 'message'))
                      else
                         msg = ''
                      end if
                      call emit_lines_indented(o, replace_all(msg, achar(13), ''))
                      call ln(o, '')
                   else if (jis_str(jget(ev, 'kind')) .and. is_build_kind(k)) then
                      call emit_build_tool(o, label, k, args, hash_or0(jget(ev, 'results')))
                   else
                      qv = jget(args, 'query')
                      if (qv > 0 .and. .not. jis_null(qv)) then
                         q = py_str(qv)
                      else
                         q = str_or3(jget(args, 'url'), jget(args, 'previewUrl'), jget(args, 'preview_url'))
                      end if
                      res = hash_or0(jget(ev, 'results'))
                      items = list_or0(jget(res, 'items'))
                      call ln(o, '- **'//label//'** '//q//count_suffix(items))
                      call emit_results(o, res)
                   end if
                else if (ty == 'tool_result') then
                   res = hash_or0(jget(ev, 'results'))
                   call ln(o, '- tool result '//py_str(jget(ev, 'toolCallId'))//': '//itoa(jlen(list_or0(jget(res, 'items'))))// &
                              ' items')
                else if (ty == 'text') then
                   call ln(o, '- ['//py_str(jget(ev, 'channel'))//'] '//one_line(jget(ev, 'text'), 200))
                else
                   call ln(o, '- '//py_str(jget(ev, 'type')))
                end if
                ev = jnext(ev)
             end do
             call ln(o, '')
             rl = jnext(rl)
          end do
          call ln(o, '### Reply')
          call ln(o, '')
          cits = list_or0(jget(turn, 'citations'))
          call ln(o, text_with_citation_marks(py_str(jget(turn, 'text')), cits))
          call ln(o, '')
          if (jlen(cits) > 0) then
             call ln(o, 'Citations:')
             call ln(o, '')
             ci = jfirst(cits); n = 0
             do while (ci > 0)
                n = n + 1
                if (truthy(jget(ci, 'url'))) then
                   q = py_str(jget(ci, 'url'))
                else
                   q = 'unresolved'
                end if
                call ln(o, '- ['//itoa(n)//'] '//q//' (citationId '//py_str(jget(ci, 'citationId'))//', card '// &
                           py_str(jget(ci, 'cardId'))//')')
                ci = jnext(ci)
             end do
             call ln(o, '')
          end if
          s = hash_or0(jget(turn, 'sources'))
          webs = list_or0(jget(s, 'webSearchResults')); xs = list_or0(jget(s, 'xposts'))
          tr = 0
          if (jis_int(jget(s, 'toolResultRows'))) tr = int(jint(jget(s, 'toolResultRows')))
          call ln(o, '### Sources  ('//itoa(jlen(webs))//' web, '//itoa(jlen(xs))//' X posts, '//itoa(tr)//' tool result rows)')
          call ln(o, '')
          w = jfirst(webs)
          do while (w > 0)
             call ln(o, '- ['//one_line_or(jget(w, 'title'), jget(w, 'url'))//']('//py_str(jget(w, 'url'))//')')
             w = jnext(w)
          end do
          p = jfirst(xs)
          do while (p > 0)
             call ln(o, '- X @'//py_str(jget(p, 'username'))//' '//py_str(jget(p, 'postId'))//': '//one_line(jget(p, 'text'), 200))
             p = jnext(p)
          end do
          call ln(o, '')
       end if
       turn = jnext(turn)
    end do
    md = sb_str(o)
  end function transcript_markdown

  ! Python str.split("\n"), each line prefixed with two spaces
  subroutine emit_lines_indented(o, s)
    type(strbuf), intent(inout) :: o
    character(len=*), intent(in) :: s
    integer :: a, z
    a = 1
    do
       z = index(s(a:), achar(10))
       if (z == 0) then
          call ln(o, '  '//s(a:)); exit
       end if
       call ln(o, '  '//s(a:a+z-2))
       a = a + z
    end do
  end subroutine emit_lines_indented

end module fx_md
