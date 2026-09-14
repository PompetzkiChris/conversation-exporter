# Exporter — Herr Pompetzki und Signore Amodei (lol)

Saves complete conversations from **Grok** (grok.com), **Gemini** (gemini.google.com) and **Qwen**
(chat.qwen.ai) to your own disk, then checks the saved copy against what the page shows.

Everything runs on your machine, inside a Chrome window you sign in to yourself. No server, no
account with anyone else, no upload. The exporter reads the same data the chat page loads and
writes it to a folder.

## What you get for each conversation

| File | Contents |
|---|---|
| `transcript.json` | Every turn: your messages and attachments, the reply, model, timestamps. Grok: every agent's thoughts, every tool call (web / X / page / code), every search result, citations resolved to URLs, sources. Gemini and Qwen: thinking, web results, regenerated branches, blocked replies. |
| `transcript.md` | The same, readable. |
| `transcript.html` | The same as a self-contained page, attachments embedded. |
| `verification.json` | Check-by-check comparison of the saved text with the rendered page (turn count, user text, reply text, attachments, thought timers, agents, summaries, tool rows, citations, panel expansion, stability across two independent page reads, and consistency between Grok's two API formats). |
| `behavior-report.json` / `.md` | Pattern report over the transcript. |
| `attachments/` | Your uploaded files, byte-exact, size-checked against the API. |
| `screenshots/` | Full page, plus every expanded Thoughts and Sources panel (Grok). |
| `raw/` | The API payloads and page HTML exactly as served. |
| `manifest.json` | Every file with size and SHA-256, every API request, warnings, notes. |

Each run writes a new folder `<yyyymmdd-hhmmss>-<conversation id>` and never overwrites an older one.

## Two implementations, one output

The exporter exists twice, written independently against the same specification:

* **Racket** (`racket/`)
* **Fortran** (`fortran/`, gfortran, no third-party libraries: its own JSON, HTTP, WebSocket,
  SHA-1/SHA-256, base64 and Chrome DevTools client over Winsock)

On the same conversation both produce byte-identical `transcript.json`, `transcript.md`,
`transcript.html`, `verification.json` checks and behavior reports. When they disagree, one of them
has a bug; that is the reason there are two.

## Requirements

* Windows 10 or 11
* Google Chrome
* Racket 9.x (for the Racket version) or gfortran from WinLibs (for the Fortran version):
  `winget install Racket.Racket` / `winget install BrechtSanders.WinLibs.POSIX.UCRT`

## Build

```
cd racket && build.cmd && build-gui.cmd
```

```
powershell -NoProfile -File fortran\build.ps1
```

The Fortran build produces `fortran\build\Exporter-Fortran.exe` (the window) and
`fortran\build\exporter-f.exe` (command line).

## Use

1. Start the window app. The first export opens a dedicated Chrome profile
   (`%LOCALAPPDATA%\GrokExportClaude\profile`, remote debugging on port 9222). Sign in to
   grok.com, gemini.google.com and/or chat.qwen.ai in that window once.
2. Paste a link — `https://grok.com/c/…`, `https://grok.com/share/…`, a bare Grok conversation id,
   `https://gemini.google.com/app/…` or `https://chat.qwen.ai/c/…` — and press **Export**.
3. The finished export appears in the list; **Open folder** / **Open HTML** open it.

Command line:

```
exporter-f.exe <link> [--out DIR] [--port 9222] [--timeout SEC] [--skip-dom] [--keep-open] [--no-launch]
racket racket\grok-export.rkt <link> [same options]
```

Exit code 0: exported and every check passed. 2: exported, some check failed or a warning needs a
look (see `verification.json`). 1: nothing could be exported.

Exports go to `Documents\Exporter\exports` unless `--out` says otherwise.

## Notes

* The exporter only reads. It never sends messages, edits, deletes or shares anything.
* Grok sometimes serves a conversation with its X posts stripped for a while after a page load; the
  exporter detects this, re-fetches, and records every rejected attempt under `raw/`.
* A Qwen desktop app started with `--remote-debugging-port=9223` can be read as a fallback; that
  window is never navigated, scrolled or screenshotted.

## License

MIT — see `LICENSE`.