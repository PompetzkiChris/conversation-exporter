# build.ps1 — build the Fortran exporter with gfortran (WinLibs GCC).
#   powershell -NoProfile -File build.ps1            # everything
param([switch]$Clean)
$ErrorActionPreference = 'Stop'
# gfortran: already on PATH, or the WinLibs package that `winget install BrechtSanders.WinLibs.POSIX.UCRT` installs
if (-not (Get-Command gfortran -ErrorAction SilentlyContinue)) {
  $wl = Get-ChildItem (Join-Path $env:LOCALAPPDATA 'Microsoft\WinGet\Packages') -Directory -Filter 'BrechtSanders.WinLibs*' -ErrorAction SilentlyContinue |
        ForEach-Object { Join-Path $_.FullName 'mingw64\bin' } | Where-Object { Test-Path (Join-Path $_ 'gfortran.exe') } | Select-Object -First 1
  if (-not $wl) { throw 'gfortran not found: install it with  winget install BrechtSanders.WinLibs.POSIX.UCRT' }
  $env:PATH = "$wl;$env:PATH"
}
$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$js   = Join-Path $root 'vendor\json-fortran\src'
$src  = Join-Path $root 'src'
$b    = Join-Path $root 'build'
if ($Clean -and (Test-Path $b)) { Get-ChildItem $b -File | Remove-Item }
New-Item -ItemType Directory -Force $b | Out-Null

$flags = @('-O2', '-cpp', '-DINT64=1', '-ffree-line-length-none', "-J$b", "-I$b", "-I$js")

function Compile($file) {
  $obj = Join-Path $b ([IO.Path]::GetFileNameWithoutExtension($file) + '.o')
  & gfortran -c @flags $file -o $obj
  if ($LASTEXITCODE -ne 0) { throw "compile failed: $file" }
  return $obj
}

$objs = @()
foreach ($f in 'fx_util.f90','fx_json.f90','fx_net.f90','fx_cdp.f90','fx_gemini.f90','fx_qwen.f90','fx_grok.f90','fx_verify.f90','fx_md.f90','fx_html.f90','fx_behavior.f90') {
  $objs += Compile (Join-Path $src $f)
}

function Link($main, $exe) {
  $mo = Compile (Join-Path $src $main)
  & gfortran -O2 -static -o (Join-Path $b $exe) $mo @objs -lws2_32 -lshell32
  if ($LASTEXITCODE -ne 0) { throw "link failed: $exe" }
  $i = Get-Item (Join-Path $b $exe)
  "built $($i.FullName) ($($i.Length) bytes)"
}

# page scripts: taken verbatim from the Racket sources so both versions run the same JavaScript
$jsOut = Join-Path $b 'js'; New-Item -ItemType Directory -Force $jsOut | Out-Null
$map = @{ 'GEMINI-STATE-JS'='gemini-state.js'; 'GEMINI-FETCH-JS'='gemini-fetch.js'; 'GEMINI-DOM-JS'='gemini-dom.js';
          'QWEN-STATE-JS'='qwen-state.js'; 'QWEN-FETCH-JS'='qwen-fetch.js'; 'QWEN-DOM-JS'='qwen-dom.js' }
foreach ($rkt in 'gemini.rkt','qwen.rkt') {
  $lines = [IO.File]::ReadAllLines((Join-Path (Join-Path (Split-Path $root -Parent) 'racket') $rkt), [Text.Encoding]::UTF8)
  for ($i = 0; $i -lt $lines.Length; $i++) {
    if ($lines[$i] -match '^\(define ([A-Z-]+-JS) #<<JS\s*$' -and $map.ContainsKey($Matches[1])) {
      $name = $map[$Matches[1]]; $body = New-Object System.Collections.Generic.List[string]
      for ($j = $i + 1; $j -lt $lines.Length -and $lines[$j] -ne 'JS'; $j++) { $body.Add($lines[$j]) }
      [IO.File]::WriteAllText((Join-Path $jsOut $name), ($body -join "`n"), (New-Object Text.UTF8Encoding $false))
    }
  }
}
# Grok page scripts: hand-copied from grok-export.rkt (checked equal), extractor from shared/extract.js
Copy-Item (Join-Path $root 'js-src\*.js') $jsOut -Force
Copy-Item (Join-Path (Split-Path $root -Parent) 'shared\extract.js') $jsOut -Force
"page scripts: $((Get-ChildItem $jsOut).Name -join ', ')"

Link 'gemini_offline.f90' 'gemini_offline.exe'
Link 'qwen_offline.f90' 'qwen_offline.exe'
Link 'grok_offline.f90' 'grok_offline.exe'
Link 'verify_offline.f90' 'verify_offline.exe'
Link 'md_offline.f90' 'md_offline.exe'
Link 'html_offline.f90' 'html_offline.exe'
Link 'behavior_offline.f90' 'behavior_offline.exe'
Link 'ws_merge_offline.f90' 'ws_merge_offline.exe'
Link 'crypto_test.f90' 'crypto_test.exe'
Link 'fx_main.f90' 'exporter-f.exe'
# the window: no console, Win32 only (it runs exporter-f.exe for the export itself)
$go = Compile (Join-Path $src 'fx_gui.f90')
& gfortran -O2 -static -mwindows -o (Join-Path $b 'Exporter-Fortran.exe') $go (Join-Path $b 'fx_json.o') (Join-Path $b 'fx_util.o') -luser32 -lgdi32 -lshell32 -lkernel32
if ($LASTEXITCODE -ne 0) { throw 'link failed: Exporter-Fortran.exe' }
"built $((Get-Item (Join-Path $b 'Exporter-Fortran.exe')).FullName)"