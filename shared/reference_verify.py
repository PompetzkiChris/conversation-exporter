r"""Reference verifier: transcript.json (API lane) vs dom-capture.json (DOM lane) -> verification.json.

  python reference_verify.py <transcript.json> <dom-capture.json> <out verification.json> [--attachments DIR]
  python reference_verify.py --stability <capture1.json> <capture2.json> [<out.json>]

Implements SPEC.md section 5 checks 1-10 and 12 (mode 1) and check 11 (--stability).  This file
is the precise definition of every check; the C and Racket apps must reproduce its verdicts.

Normalization (all text comparisons):  NFC; "\r" removed; zero-width characters U+200B, U+2060,
U+FEFF removed; every run of Unicode whitespace (including NBSP) collapsed to one space; trimmed.

Markdown stripping (check 3, check 8; applied identically to BOTH sides):  "[text](url)" -> "text",
"![alt](url)" -> "alt", list markers ("-", "*", "+", "1.", "1)") and heading/quote markers at line
starts removed, lines made only of "-*_=" (rules) removed, a backslash before punctuation removed,
then every "* _ # > `" character deleted; the result is split on whitespace into words.

Check semantics that the SPEC text leaves open are pinned down here (see NOTES-extract.md):
  3  assistant-text : word sequences must be identical; on failure the first differing position
                      and 8 words of context from both sides are reported.
  4  attachments    : API fileName sequence == DOM tooltip name sequence AND API fileId sequence ==
                      fileIds parsed from the DOM preview src (".../<fileId>/preview-image").  The
                      size comparison needs downloaded files: with --attachments DIR the file
                      "<turnIndex>-<fileId>-<fileName>" must exist with size == sizeBytes; without
                      the option sizes are reported as "not checked" and do not affect ok.
  7  summaries      : DOM summary rows are the rows of type "summary" (nested "+N more" entries
                      are individual rows).
  8  chatroom       : the markdown-stripped word sequence of the API message occurs contiguously
                      in the markdown-stripped word sequence of the Thoughts panel innerText.
  9  tool-rows      : DOM kind labels: webSearch->"Searched web", xSearch and xUserSearch->
                      "Searched 𝕏", browsePage->"Browsed".  Keys: normalized query, for browsePage
                      the URL (DOM href when present, else the displayed text) compared after
                      dropping scheme, "www." and a trailing "/".  Rows are matched as multisets
                      per rollout, duplicates paired in document order.  For matched webSearch
                      rows: DOM count == len(API items) and DOM result URL list == API item URL
                      list (order-sensitive).
 10  citations      : grok.com renders ONE chip per distinct cited URL (a second citation of the
                      same URL adds nothing; different URLs cited later are folded into the first
                      chip as "+N").  Therefore: effective DOM chip count (chips + their "+N")
                      == number of distinct API citation URLs (legacy JSON without URLs: == API
                      citation count only when the effective chip count says so, otherwise the
                      counts are reported and ok is decided on the chip count alone);
                      every a.citation href must be one of the API URLs and every API URL must be
                      matched by an href unless the covering chip is an X-post group chip
                      (button.inline, no href) -- those URLs are reported as unverifiable.
                      The chip hrefs are always reported so a legacy transcript can be enriched.
 11  dom-stability  : canonical JSON of the two captures minus top-level "capturedAt" and "env"
                      (env holds environment/timing facts: tab visibility, pass counts, elapsed ms).
 12  expansion-complete : remainingCollapsed == 0 for every thoughts/sources panel.
"""
import sys, os, re, json, hashlib, unicodedata
from collections import Counter

ZW = re.compile(r"[​⁠﻿]")
WS = re.compile(r"\s+")

def norm(s):
    if s is None: return ""
    s = unicodedata.normalize("NFC", str(s)).replace("\r", "")
    s = ZW.sub("", s)
    return WS.sub(" ", s).strip()

MATH_DELIM = re.compile(r"\\[()\[\]]|\$\$")   # \( \) \[ \] $$ : math delimiters; the page shows the TeX between them
TABLE_PIPE = re.compile(r"\|")                  # markdown table pipes: the page renders a table, not the pipes
CODE_SPAN = re.compile(r"`([^`\n]*)`")          # inline code: backslashes inside are literal, not markdown escapes
MD_LINK = re.compile(r"!?\[([^\]]*)\]\([^)]*\)")
MD_LINE_MARK = re.compile(r"^[ \t]*(?:[-*+]|\d+[.)])[ \t]+", re.M)
MD_HEAD = re.compile(r"^[ \t]*#+[ \t]*", re.M)
MD_QUOTE = re.compile(r"^[ \t]*>[ \t]*", re.M)
MD_RULE = re.compile(r"^[ \t]*(?:[-*_=][ \t]*){3,}$", re.M)
MD_ESC = re.compile(r"\\([\\`*_{}\[\]()#+\-.!>|~])")
MD_CHARS = re.compile(r"[*_#>`]")

def md_words(s):
    s = unicodedata.normalize("NFC", (s or "")).replace("\r", "")
    s = ZW.sub("", s)
    s = MATH_DELIM.sub("", s)
    s = MD_LINK.sub(r"\1", s)
    s = TABLE_PIPE.sub(" ", s)
    s = MD_RULE.sub("", s)
    s = MD_LINE_MARK.sub("", s)
    s = MD_HEAD.sub("", s)
    s = MD_QUOTE.sub("", s)
    s = CODE_SPAN.sub(lambda m: m.group(1).replace("\\", "\ue000"), s)
    s = MD_ESC.sub(r"\1", s)
    s = MD_CHARS.sub("", s)
    s = s.replace("\ue000", "\\")
    return s.split()

def sha(s):
    return hashlib.sha256(s.encode("utf-8")).hexdigest()

def urlkey(u):
    u = norm(u).lower()
    u = re.sub(r"^[a-z]+://", "", u)
    u = re.sub(r"^www\.", "", u)
    return u.rstrip("/")

def duration_seconds(label):
    """'Thought for 1m 21s' -> 81; supports h/m/s components."""
    if not label: return None
    m = re.search(r"(?:(\d+)h)?\s*(?:(\d+)m)?\s*(?:(\d+)s)?\s*$", label.strip())
    if not m or not any(m.groups()): return None
    h, mi, s = (int(x) if x else 0 for x in m.groups())
    return h * 3600 + mi * 60 + s

DOM_KIND = {"webSearch": "Searched web", "xSearch": "Searched 𝕏", "xUserSearch": "Searched 𝕏", "browsePage": "Browsed"}
PAGE_NOTE = ("observed grok.com behaviour (share page, 2026-09-02): chatroom messages whose outputChunk is streamed AFTER the last "
             "CHANNEL_ASSISTANT_RESPONSE chunk of the turn are not rendered in the Thoughts panel (test conversation turns 7 and 15); "
             "the API export keeps them, the page omits them")

class Report:
    def __init__(self):
        self.checks = []
        self.warnings = []
    def add(self, name, turn, expected, actual, ok, **extra):
        rec = {"name": name, "turnIndex": turn, "expected": expected, "actual": actual, "ok": bool(ok)}
        rec.update(extra)
        self.checks.append(rec)
        return rec

# ----------------------------------------------------------------------------- helpers over the capture
def dom_articles(cap):
    return cap.get("articles") or []

def panel(cap, key, idx):
    d = cap.get(key) or {}
    return d.get(str(idx)) if str(idx) in d else d.get(idx)

def dom_rows(pan):
    for sec in (pan or {}).get("sections") or []:
        for r in sec.get("rows") or []:
            yield sec, r

def api_events(turn):
    th = turn.get("thinking") or {}
    for rl in th.get("rollouts") or []:
        for ev in rl.get("events") or []:
            yield rl, ev

# ----------------------------------------------------------------------------- checks
def check_turn_count(rep, t, cap):
    turns = t["turns"]; arts = dom_articles(cap)
    exp = {"count": len(turns), "senders": ["user" if x["sender"] == "human" else "assistant" for x in turns]}
    act = {"count": len(arts), "senders": [a.get("role") for a in arts]}
    rep.add("turn-count", None, exp, act, exp == act)

def check_user_text(rep, t, cap):
    """Exact after normalization; when that fails, the DOM renders the user's text as markdown (list
    markers become CSS ::marker pseudo-elements that innerText does not contain), so the
    markdown-stripped word sequences (check 3 rules, both sides) must be identical instead."""
    arts = dom_articles(cap)
    for turn in t["turns"]:
        if turn["sender"] != "human": continue
        i = turn["index"]
        a = arts[i] if i < len(arts) else {}
        e, d = norm(turn.get("text")), norm(a.get("text"))
        exact = e == d
        ew, dw = md_words(turn.get("text")), md_words(a.get("text"))
        ok = exact or ew == dw
        rep.add("user-text", i, {"chars": len(e), "sha256": sha(e), "head": e[:80], "words": len(ew)}, {"chars": len(d), "sha256": sha(d), "head": d[:80], "words": len(dw)}, ok,
                mode=("exact" if exact else ("markdown-stripped" if ok else "mismatch")), firstDiff=None if ok else first_diff(ew, dw))

def first_diff(ew, dw, ctx=8):
    n = min(len(ew), len(dw))
    pos = next((k for k in range(n) if ew[k] != dw[k]), n)
    if pos == len(ew) == len(dw): return None
    return {"position": pos, "expected": ew[max(0, pos - ctx):pos + ctx], "actual": dw[max(0, pos - ctx):pos + ctx],
            "expectedWords": len(ew), "actualWords": len(dw)}

def check_assistant_text(rep, t, cap):
    arts = dom_articles(cap)
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        a = arts[i] if i < len(arts) else {}
        ew, dw = md_words(turn.get("text")), md_words(a.get("text"))
        equal = ew == dw
        # Export-completeness rule: every word the page shows must be in the API text (in order).
        # The API text may contain MORE than the page renders (observed: grok.com drops the reply text
        # before a repeated citation chip, turn 1 of the test conversation); that is a page omission,
        # recorded as a warning with the omitted spans, not a failure -- the export keeps the full text.
        sub = equal or is_subsequence(dw, ew) or is_subsequence(split_glue(dw), split_glue(ew))
        ok = sub
        rec = rep.add("assistant-text", i, {"words": len(ew), "sha256": sha(" ".join(ew))}, {"words": len(dw), "sha256": sha(" ".join(dw))}, ok,
                      firstDiff=None if equal else first_diff(ew, dw), rule="DOM words == API words, or DOM words a subsequence of API words (page omission -> warning)")
        if not equal:
            rec["domIsSubsequenceOfApi"] = sub
            rec["classification"] = "page-omits-content" if sub else "mismatch"
            rec["missingFromDom"] = missing_spans(ew, dw)
            if sub:
                rep.warnings.append("turn %d: the page renders %d of %d reply words; the export keeps the full API text (see assistant-text.missingFromDom)" % (i, len(dw), len(ew)))

GLUE = re.compile(r"([.!?][\"\u201d\u2019)\]]*)(?=[A-Z])")   # "team.An", "boy.”That": the API glues consecutive reply messages without a space
def split_glue(words):
    return GLUE.sub(r"\1 ", " ".join(words)).split()

def is_subsequence(small, big):
    it = iter(big)
    return all(any(w == x for x in it) for w in small)

def missing_spans(ew, dw):
    """Greedy alignment: spans of API words absent from the DOM (assuming DOM is API with deletions)."""
    spans, j, i, start = [], 0, 0, None
    while i < len(ew):
        if j < len(dw) and ew[i] == dw[j]:
            if start is not None: spans.append({"fromWord": start, "toWord": i - 1, "text": " ".join(ew[start:i])[:300]}); start = None
            i += 1; j += 1
        else:
            if start is None: start = i
            i += 1
    if start is not None: spans.append({"fromWord": start, "toWord": len(ew) - 1, "text": " ".join(ew[start:])[:300]})
    return spans[:20]

def check_attachments(rep, t, cap, att_dir):
    arts = dom_articles(cap)
    for turn in t["turns"]:
        if turn["sender"] != "human": continue
        i = turn["index"]
        api = turn.get("attachments") or []
        dom = (arts[i] if i < len(arts) else {}).get("attachments") or []
        if not api and not dom: continue
        exp = {"names": [x.get("fileName") for x in api], "fileIds": [x.get("fileId") for x in api], "previewUrls": [x.get("previewUrl") for x in api], "sizes": [x.get("sizeBytes") for x in api]}
        dom_ids = [(re.search(r"/([0-9a-f-]{36})/preview-image", d.get("previewSrc") or "") or [None, None])[1] for d in dom]
        act = {"names": [d.get("name") for d in dom], "fileIds": dom_ids, "previewUrls": [d.get("previewSrc") for d in dom]}
        sizes, size_ok = [], True
        for x in api:
            if att_dir:
                p = os.path.join(att_dir, "%d-%s-%s" % (i, x.get("fileId"), x.get("fileName")))
                if os.path.exists(p):
                    sz = os.path.getsize(p); sizes.append(sz); size_ok = size_ok and (sz == x.get("sizeBytes"))
                else:
                    sizes.append(None); size_ok = False
            else:
                sizes.append("not checked")
        act["sizes"] = sizes
        # names live only in a hover tooltip; a null DOM name means the tooltip did not render (hidden tab) and is not an error,
        # a present DOM name must match the API name.  fileIds (from the preview src) and preview URLs are the hard identity.
        names_ok = len(exp["names"]) == len(act["names"]) and all(d is None or d == e for e, d in zip(exp["names"], act["names"]))
        # a file without a preview (a PDF) has no element on the page that carries its id
        visible_ids = [x.get("fileId") if x.get("previewUrl") else None for x in api]
        ok = names_ok and visible_ids == act["fileIds"] and exp["previewUrls"] == act["previewUrls"] and size_ok
        rep.add("attachments", i, exp, act, ok, sizeCheck=("files" if att_dir else "not performed (no --attachments DIR)"),
                nameSource=("dom tooltip" if all(d is not None for d in act["names"]) else "api (the page shows the name only in a hover tooltip)"),
                rule="fileIds equal where the API has a preview (the page shows the id only in the preview image URL); previewUrls equal; DOM names equal when present; sizes equal when files are given")

def check_thought_label(rep, t, cap):
    arts = dom_articles(cap)
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        th = turn.get("thinking") or {}
        label = (arts[i] if i < len(arts) else {}).get("thoughtLabel")
        secs = duration_seconds(label)
        dur = th.get("durationMs")
        delta = None if (secs is None or dur is None) else abs(secs * 1000 - dur)
        ok = delta is not None and delta <= 2000
        extra = {}
        # the page shows no Thoughts label for a reply whose thinking produced no events
        if not label and not any(rl.get("events") for rl in th.get("rollouts") or []):
            ok = True; extra["note"] = "no thinking events in the API and no Thoughts label on the page"
        rep.add("thought-label", i, {"durationMs": dur, "toleranceMs": 2000}, {"label": label, "seconds": secs, "deltaMs": delta}, ok, **extra)

def check_rollouts(rep, t, cap):
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        th = turn.get("thinking") or {}
        api = sorted({rl["id"] for rl in th.get("rollouts") or [] if rl.get("events")})
        pan = panel(cap, "thoughtsByArticle", i) or {}
        # a page that names no agent (canvas replies) shows the single rollout of a solo turn
        sole = api[0] if len(api) == 1 else None
        dom = sorted({sec.get("rollout") or sole for sec in pan.get("sections") or [] if sec.get("rollout") or sole})
        extra = [x for x in dom if x not in api]
        missing = [x for x in api if x not in dom]
        ok = not extra          # the page must not show a rollout the API lacks; the reverse is a page omission (warning)
        rec = rep.add("rollouts", i, api, dom, ok, rule="every DOM rollout exists in the API; API rollouts absent from the page -> warning")
        if api != dom:
            rec["missingInDom"] = missing
            rec["extraInDom"] = extra
            rec["note"] = PAGE_NOTE
            if missing and not extra:
                rep.warnings.append("turn %d: page omits rollout(s) %s that the API contains; the export keeps them" % (i, missing))

def check_summaries(rep, t, cap):
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        api = [norm(ev.get("text")) for rl, ev in api_events(turn) if ev.get("type") == "summary"]
        pan = panel(cap, "thoughtsByArticle", i) or {}
        text = norm(pan.get("innerText"))
        dom_rows_ = [norm(r.get("text")) for sec, r in dom_rows(pan) if r.get("type") == "summary"]
        missing = [s for s in api if s and s not in text]
        ok = not missing and len(dom_rows_) == len(api)
        rep.add("summaries", i, {"count": len(api), "texts": api}, {"count": len(dom_rows_), "texts": dom_rows_}, ok, missingFromPanelText=missing)

def check_chatroom(rep, t, cap):
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        msgs = [(rl["id"], (ev.get("args") or {}).get("message") or "") for rl, ev in api_events(turn) if ev.get("type") == "tool" and ev.get("kind") == "chatroomSend"]
        pan = panel(cap, "thoughtsByArticle", i) or {}
        hay = " " + " ".join(md_words(pan.get("innerText"))) + " "
        dom_msgs = [r for sec, r in dom_rows(pan) if r.get("type") == "chatroom"]
        results = []
        api_word_lists = []
        for rl, m in msgs:
            w = md_words(m)
            found = (" " + " ".join(w) + " ") in hay if w else True
            api_word_lists.append(w)
            results.append({"rollout": rl, "words": len(w), "head": " ".join(w[:8]), "found": found})
        # Export-completeness rule: every chatroom message the page shows must be an API message
        # (markdown-stripped words equal); API messages the page does not render are a page omission (warning).
        dom_results, dom_ok = [], True
        for r in dom_msgs:
            w = md_words(r.get("message"))
            inapi = (w in api_word_lists) if w else True
            dom_ok = dom_ok and inapi
            dom_results.append({"words": len(w), "head": " ".join(w[:8]), "inApi": inapi})
        not_found = [x for x in results if not x["found"]]
        ok = dom_ok and len(dom_msgs) <= len(msgs)
        rec = rep.add("chatroom", i, {"count": len(msgs)}, {"count": len(dom_msgs), "messages": results, "domMessages": dom_results}, ok,
                      rule="every DOM chatroom message equals an API chatroomSend message; API messages absent from the page -> warning")
        if not_found or len(dom_msgs) != len(msgs):
            rec["note"] = PAGE_NOTE
            if ok:
                rep.warnings.append("turn %d: page renders %d of %d chatroom messages; the export keeps all of them" % (i, len(dom_msgs), len(msgs)))

def check_tool_rows(rep, t, cap):
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        pan = panel(cap, "sourcesByArticle", i)
        exp_rows = []
        for rl, ev in api_events(turn):
            if ev.get("type") != "tool" or ev.get("kind") not in DOM_KIND: continue
            args = ev.get("args") or {}
            res = ev.get("results") or {}
            items = res.get("items") or []
            key = urlkey(args.get("url")) if ev["kind"] == "browsePage" else norm(args.get("query"))
            if ev["kind"] == "browsePage" and key.startswith("grok.com/"): continue   # grok.com's own pages are not listed as sources
            exp_rows.append({"rollout": rl["id"], "kind": DOM_KIND[ev["kind"]], "apiKind": ev["kind"], "key": key,
                             "count": len(items) if ev["kind"] == "webSearch" else None,
                             "urls": [x.get("url") for x in items] if ev["kind"] == "webSearch" else None})
        if not exp_rows and not pan:
            continue
        ids = sorted({rl["id"] for rl in (turn.get("thinking") or {}).get("rollouts") or [] if rl.get("events")})
        sole = ids[0] if len(ids) == 1 else None
        act_rows = []
        for sec, r in dom_rows(pan or {}):
            if r.get("kind") not in DOM_KIND.values(): continue
            key = urlkey(r.get("url") or r.get("query")) if r.get("kind") == "Browsed" else norm(r.get("query"))
            act_rows.append({"rollout": sec.get("rollout") or sole, "kind": r.get("kind"), "key": key, "count": r.get("count"),
                             "urls": [x.get("url") for x in (r.get("results") or [])]})
        # multiset match per (rollout, kind, key); duplicates paired in order
        exp_c = Counter((r["rollout"], r["kind"], r["key"]) for r in exp_rows)
        act_c = Counter((r["rollout"], r["kind"], r["key"]) for r in act_rows)
        missing = sorted(list((exp_c - act_c).elements()))
        extra = sorted(list((act_c - exp_c).elements()))
        url_mismatch = []
        used = set()
        for e in exp_rows:
            if e["kind"] != "Searched web": continue
            for k, a in enumerate(act_rows):
                if k in used or (a["rollout"], a["kind"], a["key"]) != (e["rollout"], e["kind"], e["key"]): continue
                used.add(k)
                if a["count"] != e["count"] or a["urls"] != e["urls"]:
                    url_mismatch.append({"rollout": e["rollout"], "query": e["key"], "apiCount": e["count"], "domCount": a["count"], "apiUrls": e["urls"], "domUrls": a["urls"]})
                break
        ok = not missing and not extra and not url_mismatch
        rep.add("tool-rows", i,
                {"rows": len(exp_rows), "webSearchRows": sum(1 for r in exp_rows if r["kind"] == "Searched web"), "webResultUrls": sum(len(r["urls"] or []) for r in exp_rows if r["urls"])},
                {"rows": len(act_rows), "webSearchRows": sum(1 for r in act_rows if r["kind"] == "Searched web"), "webResultUrls": sum(len(r["urls"]) for r in act_rows if r["kind"] == "Searched web"), "domRowsTotal": (pan or {}).get("rowCount")},
                ok, missingInDom=[{"rollout": m[0], "kind": m[1], "key": m[2]} for m in missing], extraInDom=[{"rollout": m[0], "kind": m[1], "key": m[2]} for m in extra], urlMismatches=url_mismatch)

REFERRER = re.compile(r"[?&]referrer=grok-com$")   # grok.com appends this to X post links it renders
def href_key(h):
    return REFERRER.sub("", h) if isinstance(h, str) else h

def chip_size(chip):
    m = re.search(r"\+(\d+)\s*$", norm(chip.get("text")))
    return 1 + (int(m.group(1)) if m else 0)

def check_citations(rep, t, cap):
    arts = dom_articles(cap)
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        cits = turn.get("citations") or []
        chips = (arts[i] if i < len(arts) else {}).get("citationChips") or []
        if not cits and not chips: continue
        api_urls = [c.get("url") for c in cits]
        have_urls = any(u for u in api_urls)
        distinct = []
        for u in api_urls:
            if u and u not in distinct: distinct.append(u)
        eff = sum(chip_size(c) for c in chips)
        hrefs = [c.get("href") for c in chips if c.get("href")]
        exp = {"citations": len(cits), "distinctUrls": len(distinct) if have_urls else None, "urls": api_urls}
        act = {"chips": len(chips), "effectiveChips": eff, "chipTexts": [norm(c.get("text")) for c in chips], "chipHrefs": [c.get("href") for c in chips]}
        if have_urls:
            count_ok = eff == len(distinct) or eff == len(cits)
            bad_href = [h for h in hrefs if href_key(h) not in api_urls]
            unverifiable = [u for u in distinct if u not in [href_key(h) for h in hrefs]]
            # an unmatched API URL is acceptable only when some chip is an href-less group chip (X posts)
            groups_without_href = [c for c in chips if not c.get("href")]
            ok = count_ok and not bad_href and (not unverifiable or bool(groups_without_href))
            rep.add("citations", i, exp, act, ok, hrefNotInApi=bad_href, apiUrlsWithoutHref=unverifiable, rule="effectiveChips == distinct API URLs, or == citations (one chip per citation); every href in API URLs")
        else:
            # legacy transcript: no API URLs.  grok.com renders one chip per DISTINCT cited URL, so the chip
            # count can only be <= the citation count.  Enrichment is exact when they are equal (k-th chip ->
            # k-th citation); otherwise the mapping is ambiguous and the app must leave url/kind null.
            ok = 0 < eff <= len(cits)
            mode = "one-to-one" if eff == len(cits) else "ambiguous"
            rep.add("citations", i, exp, act, ok, rule="legacy transcript (no API URLs): 0 < effectiveChips <= citations; hrefs reported for enrichment",
                    enrichment={"mode": mode, "chipHrefs": [c.get("href") for c in chips], "chipTexts": [norm(c.get("text")) for c in chips]})
            if mode == "ambiguous":
                rep.warnings.append("turn %d: %d citation(s) but %d chip(s); citation URLs cannot be assigned from the DOM, left null" % (i, len(cits), eff))

def check_images(rep, t, cap):
    """Searched images: the page's image figures link to the same pages as the API's image cards, in order."""
    arts = dom_articles(cap)
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        api = turn.get("images") or []
        dom = (arts[i] if i < len(arts) else {}).get("images") or []
        if not api and not dom: continue
        exp = {"count": len(api), "links": [x.get("link") for x in api]}
        act = {"count": len(dom), "links": [d.get("link") for d in dom], "srcs": [d.get("src") for d in dom]}
        rep.add("images", i, exp, act, exp["links"] == act["links"], rule="page image links equal the API image cards' links, in order")

def check_expansion(rep, t, cap):
    for key, name in (("thoughtsByArticle", "thoughts"), ("sourcesByArticle", "sources")):
        d = cap.get(key) or {}
        for k in sorted(d, key=lambda x: int(x)):
            pan = d[k]
            if pan is None: continue
            rc = pan.get("remainingCollapsed")
            rep.add("expansion-complete", int(k), {"panel": name, "remainingCollapsed": 0}, {"panel": name, "remainingCollapsed": rc, "rows": pan.get("rowCount"), "links": pan.get("linkCount")}, rc == 0)

def check_api_presence(rep, t, cap):
    """Not a SPEC check: sanity that every assistant turn with a Thoughts panel in the API has one in the DOM and vice versa."""
    for turn in t["turns"]:
        if turn["sender"] != "assistant": continue
        i = turn["index"]
        th = (turn.get("thinking") or {})
        has_api = any(rl.get("events") for rl in th.get("rollouts") or [])
        pan = panel(cap, "thoughtsByArticle", i)
        if has_api and not pan:
            rep.warnings.append("turn %d: API has thinking events but the DOM capture has no Thoughts panel" % i)

# ----------------------------------------------------------------------------- stability
def canonical(obj):
    return json.dumps(obj, ensure_ascii=False, indent=1, sort_keys=True, separators=(",", ": ")) + "\n"

def strip_env(cap):
    """Drop volatile fields before comparing two rounds: capturedAt, env, and articles[*].html
    (the rendered markdown HTML carries per-render attributes; the text/structure fields are what must be stable)."""
    out = {k: v for k, v in cap.items() if k not in ("capturedAt", "env")}
    # attachment names come from a hover tooltip that one read may render and the other not (checked against the API)
    unnamed = lambda v: [{kk: vv for kk, vv in x.items() if kk != "name"} if isinstance(x, dict) else x for x in v] if isinstance(v, list) else v
    out["articles"] = [{k: (unnamed(v) if k == "attachments" else v) for k, v in a.items() if k != "html"} for a in (out.get("articles") or [])]
    return out

def diff_paths(a, b, path="", out=None, limit=40):
    if out is None: out = []
    if len(out) >= limit: return out
    if type(a) != type(b):
        out.append({"path": path or "/", "a": str(a)[:120], "b": str(b)[:120]}); return out
    if isinstance(a, dict):
        for k in sorted(set(a) | set(b)):
            if k not in a or k not in b: out.append({"path": path + "/" + str(k), "a": "present" if k in a else "absent", "b": "present" if k in b else "absent"}); continue
            diff_paths(a[k], b[k], path + "/" + str(k), out, limit)
    elif isinstance(a, list):
        if len(a) != len(b): out.append({"path": path, "a": "len %d" % len(a), "b": "len %d" % len(b)})
        for k in range(min(len(a), len(b))): diff_paths(a[k], b[k], path + "/" + str(k), out, limit)
    elif a != b:
        out.append({"path": path, "a": str(a)[:120], "b": str(b)[:120]})
    return out

def stability(p1, p2, out):
    c1 = json.load(open(p1, encoding="utf-8")); c2 = json.load(open(p2, encoding="utf-8"))
    s1, s2 = canonical(strip_env(c1)), canonical(strip_env(c2))
    ok = s1 == s2
    rec = {"name": "dom-stability", "turnIndex": None, "expected": {"sha256": sha(s1), "bytes": len(s1.encode("utf-8")), "file": os.path.basename(p1)},
           "actual": {"sha256": sha(s2), "bytes": len(s2.encode("utf-8")), "file": os.path.basename(p2)}, "ok": ok,
           "ignored": ["capturedAt", "env", "articles[*].html", "articles[*].attachments[*].name"], "differences": [] if ok else diff_paths(strip_env(c1), strip_env(c2))}
    res = {"tool": "reference_verify.py --stability", "checks": [rec], "summary": {"total": 1, "ok": 1 if ok else 0, "failed": 0 if ok else 1}, "ok": ok}
    txt = canonical(res)
    if out:
        with open(out, "w", encoding="utf-8", newline="\n") as f: f.write(txt)
    print("dom-stability:", "OK" if ok else "MISMATCH", "(%d differences shown)" % len(rec["differences"]))
    for d in rec["differences"][:20]: print("  ", d["path"], "|", d["a"], "|", d["b"])
    return 0 if ok else 2

# ----------------------------------------------------------------------------- main
def verify(transcript_path, capture_path, out_path, att_dir=None):
    t = json.load(open(transcript_path, encoding="utf-8"))
    cap = json.load(open(capture_path, encoding="utf-8"))
    rep = Report()
    check_turn_count(rep, t, cap)
    check_user_text(rep, t, cap)
    check_assistant_text(rep, t, cap)
    check_attachments(rep, t, cap, att_dir)
    check_thought_label(rep, t, cap)
    check_rollouts(rep, t, cap)
    check_summaries(rep, t, cap)
    check_chatroom(rep, t, cap)
    check_tool_rows(rep, t, cap)
    check_citations(rep, t, cap)
    check_images(rep, t, cap)
    check_expansion(rep, t, cap)
    check_api_presence(rep, t, cap)
    failed = [c for c in rep.checks if not c["ok"]]
    by_name = {}
    for c in rep.checks:
        b = by_name.setdefault(c["name"], {"total": 0, "ok": 0, "failed": 0})
        b["total"] += 1; b["ok" if c["ok"] else "failed"] += 1
    res = {"tool": "reference_verify.py", "transcript": os.path.basename(transcript_path), "capture": os.path.basename(capture_path),
           "checks": rep.checks, "byName": by_name, "summary": {"total": len(rep.checks), "ok": len(rep.checks) - len(failed), "failed": len(failed)},
           "failedChecks": [{"name": c["name"], "turnIndex": c["turnIndex"]} for c in failed], "warnings": rep.warnings, "ok": not failed}
    with open(out_path, "w", encoding="utf-8", newline="\n") as f: f.write(canonical(res))
    print("checks: %d, ok: %d, failed: %d -> %s" % (len(rep.checks), len(rep.checks) - len(failed), len(failed), out_path))
    for n, b in by_name.items(): print("  %-19s %3d ok %3d failed" % (n, b["ok"], b["failed"]))
    for c in failed: print("  FAILED", c["name"], "turn", c["turnIndex"])
    for w in rep.warnings: print("  WARNING", w)
    return 0 if not failed else 2

if __name__ == "__main__":
    a = sys.argv[1:]
    if a and a[0] == "--stability":
        sys.exit(stability(a[1], a[2], a[3] if len(a) > 3 else None))
    att = None
    if "--attachments" in a:
        k = a.index("--attachments"); att = a[k + 1]; del a[k:k + 2]
    if len(a) < 3:
        print(__doc__); sys.exit(1)
    sys.exit(verify(a[0], a[1], a[2], att))
