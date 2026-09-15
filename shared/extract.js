(async (opts) => {
  // Grok Heavy conversation DOM extractor (SPEC.md section 4).
  // Runs in page context via Runtime.evaluate({awaitPromise:true, returnByValue:true}).
  // Deterministic: the only timestamp is top-level capturedAt; timing/environment facts live under `env`.
  opts = opts || {};
  const settleMs = Number.isFinite(opts.settleMs) ? opts.settleMs : 700;
  const maxPasses = Number.isFinite(opts.maxPasses) ? opts.maxPasses : 25;
  const verbose = !!opts.verbose;
  const forceOpen = opts.forceOpen !== false;          // fast-forward height:0 boxes whose animation cannot run (hidden tab: no rAF)
  const only = opts.only || null;                      // {article:<index>, panel:"thoughts"|"sources"} -> expand one panel and keep it open
  const keepOpen = !!opts.keepOpen || !!only;
  const pauseForShot = !!opts.pauseForShot;            // hold every fully expanded panel open until the host has photographed it
  const shotTimeoutMs = Number.isFinite(opts.shotTimeoutMs) ? opts.shotTimeoutMs : 120000;
  const t0 = Date.now();
  const log = [];
  const say = (m) => { if (verbose) { log.push('[' + (Date.now() - t0) + 'ms] ' + m); try { window.__grokExtractLog = log; } catch (e) { /* ignore */ } } };
  // sleep: setTimeout in a visible tab; in a hidden tab Chrome throttles DOM timers (1/s, and 1/min after 5 min
  // "intensive throttling"), so yield through a MessageChannel (not a timer, not throttled) until the deadline.
  const yieldTask = () => new Promise((r) => { const ch = new MessageChannel(); ch.port1.onmessage = () => { ch.port1.close(); r(); }; ch.port2.postMessage(0); });
  // A hidden tab whose browser was started with throttling switched off (--disable-background-timer-throttling
  // --disable-renderer-backgrounding) keeps prompt timers; measure it instead of assuming.  The yield loop is only
  // used when timers are really throttled: it keeps the renderer's main thread busy, and same-site tabs share it.
  let timersPrompt = null;
  const measureTimers = () => new Promise((r) => { const s = performance.now(); setTimeout(() => r(performance.now() - s < 120), 20); });
  document.addEventListener('visibilitychange', () => { timersPrompt = null; });
  const sleep = async (ms) => {
    if (document.visibilityState === 'visible') return new Promise((r) => setTimeout(r, ms));
    if (timersPrompt === null) {
      const probe = await Promise.race([measureTimers(), new Promise((r) => { const ch = new MessageChannel(); const s = performance.now();
        const spin = () => { if (performance.now() - s > 150) r(false); else { ch.port1.onmessage = spin; ch.port2.postMessage(0); } }; spin(); })]);
      timersPrompt = !!probe;
    }
    if (timersPrompt) return new Promise((r) => setTimeout(r, ms));
    const end = performance.now() + ms;
    while (performance.now() < end) await yieldTask();
  };
  const norm = (s) => (s || '').replace(/ /g, ' ').replace(/\s+/g, ' ').trim();
  const firstLine = (s) => ((s || '').split(/\r?\n/).map((x) => x.trim()).find((x) => x) || '');
  const TOOL_KINDS = ['Searched web', 'Searched 𝕏', 'Browsed', 'View Image', 'Connected to computer', 'Searching conversations'];
  const HEADER_OPEN = /^(Show thinking|Expand)$/;
  const HEADER_SEL = 'button[class~="group/header"]';
  const AGENT_SEL = 'button[class~="group/agent"]';
  const ROW_SEL = '[class~="group/row"]';
  const SROW_SEL = '[class~="group/srow"]';
  const RESULT_SEL = '[class~="group/search-result"]';
  const BOX_SEL = 'div[style*="height"]';
  const q = (root, sel) => Array.from((root || document).querySelectorAll(sel));
  // the Thoughts / Sources panel; a build conversation also keeps its app preview pane (Preview, Files, Publish) in an
  // <aside>, which is not a panel and has no Close button
  const aside = () => Array.from(document.querySelectorAll('aside'))
    .find((a) => !a.querySelector('button[aria-label="Reload app preview"], button[aria-label="Mobile preview"]')) || null;
  const styleOf = (el) => (el.getAttribute('style') || '');
  const isH0 = (el) => /height:\s*0px/.test(styleOf(el));
  const hasContent = (el) => !!(el && el.firstElementChild && norm(el.textContent));
  const clickedOnce = new WeakSet();
  const forcedBoxes = new WeakSet();
  let forcedOpenTotal = 0;
  const env = { visibilityState: document.visibilityState, rafAlive: null, forcedOpen: 0, passes: {}, warnings: [] };
  env.rafAlive = await Promise.race([new Promise((r) => requestAnimationFrame(() => r(true))), sleep(400).then(() => false)]);

  // ------------------------------------------------------------------ settle helpers (evidence instead of fixed sleeps)
  // Signature of a subtree: how much is mounted and how tall it is. Cheaper than innerText, and it changes
  // whenever content mounts or a box opens -- exactly what the expansion loop is waiting for.
  const sigOf = (el) => { try { return el.getElementsByTagName('*').length + ':' + el.scrollHeight; } catch (e) { return 'x'; } };
  // Waits until the signature repeats minSame times in a row (the panel stopped changing), capped at capMs.
  async function waitStable(el, capMs, minSame) {
    const step = 60;
    const end = performance.now() + capMs;
    let last = null, same = 0;
    for (;;) {
      await sleep(step);
      const s = sigOf(el);
      if (s === last) { if (++same >= minSame) return true; } else { same = 0; last = s; }
      if (performance.now() >= end) return false;
    }
  }
  // Waits for a batch of clicks to land AND settle. If nothing changes at all it waits the full cap, which is
  // what the fixed sleep it replaces always did.
  async function waitAfterClicks(el, capMs) {
    const step = 60;
    const end = performance.now() + capMs;
    let last = sigOf(el), changed = false, same = 0;
    for (;;) {
      await sleep(step);
      const s = sigOf(el);
      if (s !== last) { changed = true; last = s; same = 0; }
      else if (changed && ++same >= 3) return true;
      if (performance.now() >= end) return changed;
    }
  }
  // Screenshot handshake (opts.pauseForShot): publish window.__grokShotReady = {article, panel, seq} while the panel
  // is open and fully expanded, wait for the host to answer window.__grokShotDone = seq, then carry on. With the
  // option off nothing is published and nothing is awaited, so the run is the one this file always did.
  let shotSeq = 0;
  async function shotPause(article, panel) {
    if (!pauseForShot) return false;
    const seq = ++shotSeq;
    try { window.__grokShotReady = { article: article, panel: panel, seq: seq }; }
    catch (e) { env.warnings.push('shot handshake: window.__grokShotReady is not writable'); return false; }
    const end = performance.now() + shotTimeoutMs;
    let done = false;
    while (performance.now() < end) {
      let d = null;
      try { d = window.__grokShotDone; } catch (e) { d = null; }
      if (d === seq) { done = true; break; }
      await sleep(50);
    }
    try { window.__grokShotReady = null; } catch (e) { /* ignore */ }
    if (done) env.shots = (env.shots || 0) + 1;
    else env.warnings.push('shot handshake for article ' + article + ' ' + panel + ' timed out after ' + shotTimeoutMs + ' ms');
    say('shot handshake ' + panel + ':' + article + ' ' + (done ? 'answered' : 'TIMED OUT'));
    return done;
  }

  // ------------------------------------------------------------------ helpers
  function leafSpans(root) { // spans without span children, excluding those inside a results box nested in root
    return q(root, 'span').filter((s) => {
      if (s.querySelector('span')) return false;
      for (let p = s.parentElement; p && p !== root; p = p.parentElement) { if (p.tagName === 'DIV' && /height/.test(styleOf(p))) return false; }
      return true;
    });
  }
  function ownText(el) { // text of the element's own text nodes only
    return norm(Array.from(el.childNodes).filter((n) => n.nodeType === 3).map((n) => n.nodeValue).join(' '));
  }
  function intOrNull(s) { s = norm(s); return /^\d+$/.test(s) ? parseInt(s, 10) : null; }
  function scrollableOf(el) {
    let p = el;
    while (p && p !== document.body) {
      const cs = getComputedStyle(p);
      if (p.scrollHeight > p.clientHeight + 4 && /(auto|scroll)/.test(cs.overflowY)) return p;
      p = p.parentElement;
    }
    return document.scrollingElement || document.documentElement;
  }
  function forceBoxes(root) { // fast-forward framer-motion boxes stuck at height:0 with mounted content
    if (!forceOpen) return 0;
    let n = 0;
    for (const d of q(root, 'div[style]')) {
      if (!isH0(d) || forcedBoxes.has(d) || !hasContent(d)) continue;
      d.style.height = 'auto';
      if (/opacity:\s*0(?![.\d])/.test(styleOf(d))) d.style.opacity = '1';
      forcedBoxes.add(d); n++;
    }
    forcedOpenTotal += n;
    return n;
  }
  function parseResults(box) {
    if (!box) return [];
    const web = q(box, RESULT_SEL);
    if (web.length) {
      return web.map((r) => {
        const links = q(r, 'a[href]');
        const title = links[0] ? norm(links[0].innerText) : norm(r.innerText);
        const url = links[0] ? links[0].getAttribute('href') : null;
        const domain = links[1] ? norm(links[1].innerText) : null;
        return { title, url, domain, text: null };
      });
    }
    const anchors = q(box, 'a[href]').filter((a) => !(a.parentElement && a.parentElement.closest('a')));
    return anchors.map((a) => {
      const head = a.querySelector('div');
      const p = a.querySelector('p');
      return { title: norm(head ? head.innerText : firstLine(a.innerText)), url: a.getAttribute('href'), domain: null, text: p ? (p.innerText || '').replace(/\r/g, '') : null };
    });
  }
  function parseToolHead(head) { // head: button/div holding kind span, query element (span or a) and count span; results boxes excluded
    const spans = leafSpans(head).filter((s) => norm(s.innerText));
    if (!spans.length) return { kind: null, query: null, count: null, url: null };
    const kindSpan = spans[0];
    const kind = norm(kindSpan.innerText);
    const wrap = kindSpan.parentElement;
    const qEl = Array.from(wrap.children).find((e) => e !== kindSpan) || null;
    const query = qEl ? (norm(qEl.innerText) || null) : null;
    let url = null;
    if (qEl) { const a = qEl.tagName === 'A' ? qEl : qEl.querySelector('a[href]'); if (a) url = a.getAttribute('href'); }
    let count = null;
    for (const s of spans) { if (wrap.contains(s)) continue; const c = intOrNull(s.innerText); if (c !== null) { count = c; break; } }
    return { kind, query, count, url };
  }

  // ------------------------------------------------------------------ 1. transcript
  async function scrollTranscript() {
    let arts = q(document, '[role=article]');
    if (!arts.length) return 0;
    const sc = scrollableOf(arts[0]);
    // A long conversation opens with only its latest turns (about 50); Grok prepends the next older batch each
    // time the transcript sits at the top.  Hold it there until the count stops growing and nothing is loading.
    const tOlder = Date.now();
    let before = arts.length, quiet = 0;
    while (quiet < 3 && Date.now() - tOlder < 300000) {
      sc.scrollTop = 0;
      sc.dispatchEvent(new WheelEvent('wheel', { deltaY: -2000, bubbles: true }));
      await sleep(500);
      const n = q(document, '[role=article]').length;
      const loading = q(sc, '[role=progressbar], .animate-spin').length > 0;
      if (n > before) { say('older turns loaded: ' + before + ' -> ' + n); before = n; quiet = 0; }
      else if (!loading) quiet++;
    }
    sc.scrollTop = 0; await sleep(200);
    let last = -1, stable = 0, lastTop = -1, stuck = 0;
    for (let guard = 0; guard < 400; guard++) {
      sc.scrollTop = sc.scrollTop + Math.max(200, sc.clientHeight * 0.85);
      await sleep(120);
      const n = q(document, '[role=article]').length;
      const atEnd = sc.scrollTop + sc.clientHeight >= sc.scrollHeight - 4;
      stuck = (sc.scrollTop === lastTop) ? stuck + 1 : 0;
      lastTop = sc.scrollTop;
      if (n === last && (atEnd || stuck >= 3)) { if (++stable >= 3) break; } else stable = 0;
      last = n;
    }
    sc.scrollTop = 0; await sleep(150);
    return q(document, '[role=article]').length;
  }
  // Panel innerText without the "Show more" / "Show less" clamp buttons: whether a block is clamped depends on
  // layout timing, so the label differs between two reads of the same panel while the content does not.
  function panelText(root) {
    const hid = q(root, 'button').filter((b) => /^Show (more|less)$/.test(norm(b.innerText))).map((b) => [b, b.style.display]);
    hid.forEach(([b]) => { b.style.display = 'none'; });
    const t = (root.innerText || '').replace(/\r/g, '');
    hid.forEach(([b, d]) => { b.style.display = d; if (!d) b.removeAttribute('style'); });
    return t;
  }
  // innerText with every rendered KaTeX formula read as its TeX source (the <annotation> KaTeX keeps), so the
  // page text lines up with the API text, which carries the TeX.  The swap is synchronous and fully undone.
  function textWithTex(root) {
    const shown = [];
    for (const k of q(root, '.katex')) {
      const ann = k.querySelector('annotation[encoding="application/x-tex"]');
      if (!ann) continue;
      const s = document.createElement('span');
      s.textContent = ann.textContent;
      k.parentNode.insertBefore(s, k);
      shown.push([k, s, k.style.display]);
      k.style.display = 'none';
    }
    // code blocks carry a header (language label, Collapse / Copy buttons) that is page chrome, not message text;
    // a searched image renders as a figure (source domain link, caption) where the API text has an image card
    const chrome = q(root, '[class~="group/code-header"], [data-testid="image-viewer"]').map((h) => [h, h.style.display]);
    chrome.forEach(([h]) => { h.style.display = 'none'; });
    const t = (root.innerText || '').replace(/\r/g, '');
    chrome.forEach(([h, d]) => { h.style.display = d; if (!d) h.removeAttribute('style'); });
    for (const [k, s, d] of shown) { s.remove(); k.style.display = d; if (!d) k.removeAttribute('style'); }
    return t;
  }
  function textWithoutChips(md) {
    const chips = q(md, 'a.citation, button.inline');
    const saved = chips.map((c) => c.style.display);
    chips.forEach((c) => { c.style.display = 'none'; });
    const t = textWithTex(md);
    chips.forEach((c, i) => { c.style.display = saved[i]; if (!saved[i]) c.removeAttribute('style'); });
    return t;
  }
  async function attachmentName(btn) { // the file name lives only in a Radix tooltip (aria-describedby -> element)
    const tip = () => { const id = btn.getAttribute('aria-describedby'); const el = id && document.getElementById(id); return el ? norm(el.textContent) : ''; };
    const ev = (t) => btn.dispatchEvent(new PointerEvent(t, { bubbles: true, pointerType: 'mouse', clientX: 10, clientY: 10 }));
    let name = null;
    // Both routes are armed at once and share one 3 s poll (they used to run in sequence: focus 0.8 s, then hover
    // 3 s = 3.8 s per chip). Keyboard focus opens the tooltip without the hover delay timer; the hover route's open
    // delay is a page timer, throttled in a hidden tab. Each route now has at least as long as it had before.
    try { btn.focus(); } catch (e) { /* ignore */ }
    try { ev('pointerenter'); ev('pointermove'); ev('mouseenter'); ev('mouseover'); } catch (e) { /* ignore */ }
    for (let i = 0; i < 30 && !name; i++) { await sleep(100); name = tip() || null; }
    try { ev('pointerleave'); ev('mouseleave'); ev('mouseout'); } catch (e) { /* ignore */ }
    try { btn.blur(); } catch (e) { /* ignore */ }
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', code: 'Escape', bubbles: true }));
    for (let i = 0; i < 10 && btn.getAttribute('aria-describedby'); i++) await sleep(100);
    return name;
  }
  // ------------------------------------------------------------------ canvas layout (agent / Build replies)
  // grok.com renders these replies as a "Worked for …" trigger (data-testid=canvas-trigger) that expands inline:
  // step rows (group/row, icon aria-label Thought / Open link / Explored workspace / Search), interim reply blocks
  // (data-canvas-assistant-response) and, outside the canvas, the final reply.  The API message is the interim
  // blocks and the final reply concatenated, so the page text is read the same way once the canvas is open.
  const canvasTrigger = (a) => a.querySelector('[data-testid="canvas-trigger"]');
  function canvasClosedToggles(box) { // one toggle per step row: each row has an icon toggle and a label toggle
    const out = [];
    for (const row of q(box, ROW_SEL)) {
      const bs = q(row, 'button[aria-expanded]').filter((b) => b.getAttribute('aria-hidden') !== 'true');
      if (!bs.length || bs.some((b) => b.getAttribute('aria-expanded') === 'true')) continue;
      out.push(bs[bs.length - 1]);
    }
    return out;
  }
  async function expandCanvas(a) {
    const trig = canvasTrigger(a);
    if (!trig) return null;
    if (trig.getAttribute('aria-expanded') !== 'true') {
      trig.click();
      for (let k = 0; k < 50 && trig.getAttribute('aria-expanded') !== 'true'; k++) await sleep(60);
    }
    const box = a.querySelector('.thinking-container') || trig.parentElement;
    await waitStable(box, Math.max(settleMs, 1000), 4);
    for (let pass = 0; pass < maxPasses; pass++) {
      const closed = canvasClosedToggles(box).filter((b) => !clickedOnce.has(b));
      if (!closed.length) break;
      for (const b of closed) { clickedOnce.add(b); b.click(); }
      await waitAfterClicks(box, settleMs);
      say('canvas pass ' + pass + ': clicked ' + closed.length);
    }
    forceBoxes(box);
    await waitStable(box, Math.max(settleMs, 1000), 4);
    return box;
  }
  function walkCanvas(box) {
    const rows = [];
    const visit = (el) => {
      if (el.matches(ROW_SEL)) {
        const icon = el.querySelector('[aria-label]');
        const label = icon ? icon.getAttribute('aria-label') : null;
        const spans = leafSpans(el).filter((s) => norm(s.innerText));
        const link = el.querySelector('a[href]');
        if (label === 'Search' || (spans.length && TOOL_KINDS.includes(norm(spans[0].innerText)))) {
          const kind = spans.length ? norm(spans[0].innerText) : null;
          const qEl = el.querySelector('.font-mono') || spans[spans.length - 1];
          rows.push({ type: 'tool', kind, query: qEl ? norm(qEl.innerText) : null, count: null, url: link ? link.getAttribute('href') : null, results: [] });
        } else if (link) {
          rows.push({ type: 'link', label: spans.length ? norm(spans[0].innerText) : null, url: link.getAttribute('href') });
        } else {
          rows.push({ type: 'step', icon: label, text: norm(el.innerText) });
        }
        return;
      }
      if (el.matches('[data-canvas-assistant-response]')) { rows.push({ type: 'message', text: textWithTex(el).trim() }); return; }
      if (el.matches('[data-testid="canvas-trigger"]')) return;
      if (el.tagName === 'SPAN' && el.classList.contains('block')) { const t = norm(el.innerText); if (t) rows.push({ type: 'step', icon: null, text: t }); return; }
      for (const c of Array.from(el.children)) visit(c);
    };
    visit(box);
    return [{ rollout: null, role: null, rows }];
  }
  // Sources as a Radix accordion (one item open at a time): open each item, wait for its results, read them.
  async function readAccordion(a) {
    const rows = [];
    for (const el of q(a, '[data-orientation="vertical"]')) {
      const head = el.matches('h3') ? null : el.querySelector(':scope > div > h3 > button[aria-controls], :scope > h3 > button[aria-controls]');
      if (head) {
        if (head.getAttribute('aria-expanded') !== 'true') { clickedOnce.add(head); head.click(); }
        // the region mounts its results within a frame or two of the click; stop as soon as they are there and
        // unchanged for two 30 ms samples (a region with no links settles the same way), cap 3 s
        let region = null, last = null, same = 0;
        const end = performance.now() + 3000;
        while (performance.now() < end) {
          await sleep(30);
          region = document.getElementById(head.getAttribute('aria-controls'));
          if (!region || region.hidden) continue;
          const s = sigOf(region);
          if (s === last && hasContent(region)) { if (++same >= 2) break; } else { same = 0; last = s; }
        }
        if (region) forceBoxes(region);
        say('accordion item read: ' + (head.innerText || '').slice(0, 40).replace(/\s+/g, ' '));
        const kindEl = head.querySelector('.text-xs');
        const qEl = head.querySelector('.font-semibold');
        const cEl = q(head, 'span').filter((s) => intOrNull(s.innerText) !== null).pop();
        rows.push({ kind: kindEl ? norm(kindEl.innerText) : null, query: qEl ? norm(qEl.innerText) : null,
                    count: cEl ? intOrNull(cEl.innerText) : null, url: null, results: parseResults(region), disabled: false });
      }
    }
    for (const r of q(a, 'div.flex.w-full > div.flex.flex-col > a[href]')) {
      const lab = r.parentElement.querySelector('.text-xs');
      rows.push({ kind: lab ? norm(lab.innerText) : null, query: null, count: null, url: r.getAttribute('href'), results: [], disabled: false });
    }
    return [{ rollout: null, role: null, rows }];
  }
  const isAccordion = (a) => !!a.querySelector('h3 > button[aria-controls]') && !a.querySelector(SROW_SEL);

  let articleEls = [];   // the elements collectArticles read; the page may mount more turns afterwards
  async function collectArticles() {
    const out = [];
    const arts = articleEls = q(document, '[role=article]');
    for (let i = 0; i < arts.length; i++) {
      const a = arts[i];
      const label = a.getAttribute('aria-label') || '';
      const testid = a.getAttribute('data-testid') || '';
      const role = (testid === 'user-message' || label === 'You') ? 'user' : ((testid === 'assistant-message' || label === 'Grok') ? 'assistant' : 'unknown');
      const canvas = canvasTrigger(a);
      if (canvas) { await expandCanvas(a); say('article ' + i + ': canvas expanded'); }
      const mds = q(a, '.response-content-markdown');
      const md = canvas ? (mds[mds.length - 1] || null) : a.querySelector('.response-content-markdown');
      const chipRoots = canvas ? mds : (md ? [md] : []);
      const chips = [].concat(...chipRoots.map((m) => q(m, 'a.citation, button.inline'))).map((c) => ({ text: norm(c.innerText), href: c.tagName === 'A' ? c.getAttribute('href') : null }));
      const attachments = [];
      for (const b of q(a.parentElement, 'button[aria-label="Open attachment"]')) {
        const img = b.querySelector('img');
        attachments.push({ name: await attachmentName(b), previewSrc: img ? img.getAttribute('src') : null });
      }
      const tb = a.querySelector('button[class~="group/notes"]');
      const sb = q(a, '[role=button][aria-label]').find((b) => /\bsources?$/i.test(b.getAttribute('aria-label')));
      // searched images shown in the reply (the API's image cards), in page order
      const images = [].concat(...(canvas ? mds : (md ? [md] : [])).map((m) => q(m, '[data-testid="image-viewer"]'))).map((v) => {
        const img = v.querySelector('img'), link = v.querySelector('a[href]');
        return { src: img ? img.getAttribute('src') : null, alt: img ? img.getAttribute('alt') : null, link: link ? link.getAttribute('href') : null };
      });
      out.push({
        index: i, role, ariaLabel: label,
        text: (canvas && mds.length > 1) ? mds.map((m) => textWithoutChips(m).trim()).join('') : (md ? textWithoutChips(md) : textWithTex(a)),
        html: md ? md.innerHTML : a.innerHTML,
        citationChips: chips, attachments,
        thoughtLabel: tb ? norm(tb.innerText) : (canvas ? norm(canvas.innerText) : null),
        sourcesLabel: sb ? sb.getAttribute('aria-label') : null,
        ...(images.length ? { images } : {}),
      });
    }
    return out;
  }

  // ------------------------------------------------------------------ 2. aside handling
  async function closeAside() {
    for (let i = 0; i < 40; i++) {
      const a = aside();
      if (!a) return true;
      const c = a.querySelector('button[aria-label="Close"]');
      if (c) c.click(); else env.warnings.push('aside without Close button');
      for (let j = 0; j < 15 && aside(); j++) await sleep(40);   // was one fixed 150 ms sleep
      if (!aside()) return true;
    }
    env.warnings.push('aside did not close');
    return false;
  }
  async function openPanel(btn, header) {
    await closeAside();
    btn.scrollIntoView({ block: 'center' });
    await sleep(60);
    btn.click();
    for (let i = 0; i < 100; i++) {
      await sleep(100);
      const a = aside();
      if (a && firstLine(a.innerText) === header) break;
    }
    const a = aside();
    if (!a || firstLine(a.innerText) !== header) { env.warnings.push('panel "' + header + '" did not open'); return null; }
    // Wait until the panel stops growing: four identical 60 ms samples prove what the two 700 ms innerText
    // samples proved, and stop as soon as it is true instead of always paying 1.4 s.
    const settled = await waitStable(a, Math.max(settleMs, 3000), 4);
    say('opened ' + header + (settled ? ' (settled)' : ' (settle cap reached)'));
    return a;
  }
  async function scrollAside(a) {
    const sc = [a, ...q(a, '*')].filter((e) => e.scrollHeight > e.clientHeight + 20 && /(auto|scroll)/.test(getComputedStyle(e).overflowY));
    for (const e of sc) e.scrollTop = e.scrollHeight;
    await sleep(120);
    for (const e of sc) e.scrollTop = 0;
  }
  function headerState(h) { return (h.innerText || '').split(/\r?\n/).map((x) => x.trim()).filter(Boolean).pop() || ''; }
  function thoughtsPending(a) { // {clickable: [...], pending: n}
    const clickable = [];
    let pending = 0;
    for (const h of q(a, HEADER_SEL)) if (HEADER_OPEN.test(headerState(h))) clickable.push(h);
    for (const b of q(a, 'button')) if (norm(b.innerText) === 'Show more') clickable.push(b);
    for (const b of q(a, 'button[aria-label="Toggle results"]')) {
      if (b.disabled) continue;
      const row = b.closest(ROW_SEL);
      if (row && q(row, BOX_SEL).some((d) => hasContent(d))) continue;   // results mounted (expanded or forced)
      if (clickedOnce.has(b)) { pending++; continue; }                      // clicked, still nothing mounted
      clickable.push(b);
    }
    for (const row of q(a, ROW_SEL)) {           // summary groups "+N more" with a collapsed empty box
      if (!/\+\d+ more/.test(row.innerText || '')) continue;
      const col = row.children[1] || row;
      const box = q(col, BOX_SEL).find((d) => isH0(d) && !forcedBoxes.has(d) && !hasContent(d));
      if (!box) continue;
      const b = col.querySelector('button');
      if (b && !clickedOnce.has(b)) clickable.push(b); else pending++;
    }
    return { clickable, pending };
  }
  function sourcesPending(a) {
    const clickable = [];
    let pending = 0;
    for (const row of q(a, SROW_SEL)) {
      const b = row.querySelector(':scope > button');
      if (!b || b.disabled) continue;
      if (q(row, ':scope > div').some((d) => hasContent(d))) continue;
      if (clickedOnce.has(b)) { pending++; continue; }
      clickable.push(b);
    }
    return { clickable, pending };
  }
  async function expand(a, pendingFn, key) {
    let passes = 0, clicks = 0, lastPending = 0;
    while (passes < maxPasses) {
      passes++;
      const { clickable, pending } = pendingFn(a);
      say(key + ' pass ' + passes + ': pending computed, ' + clickable.length + ' clickable');
      lastPending = pending;
      if (!clickable.length) {
        const f = forceBoxes(a);
        say(key + ' pass ' + passes + ': forced ' + f + ' boxes');
        // Nothing to click. Prove the panel has stopped changing before leaving it: a section that mounts late,
        // or a click whose content has not landed yet (pending > 0), would otherwise be walked away from.
        await waitStable(a, pending > 0 ? Math.max(settleMs, 2000) : Math.max(settleMs, 1000), 4);
        forceBoxes(a);
        const again = pendingFn(a);
        if (again.clickable.length) continue;
        lastPending = again.pending;
        if (again.pending > 0 && passes < maxPasses) continue;
        break;
      }
      for (const b of clickable) {
        if (b.getAttribute('aria-label') === 'More options' || /^(Collapse|Show less|Hide thinking|Close)$/.test(norm(b.innerText))) continue;
        clickedOnce.add(b); b.click(); clicks++;
      }
      say(key + ' pass ' + passes + ': clicked ' + clickable.length);
      await waitAfterClicks(a, settleMs);
      const f = forceBoxes(a);
      say(key + ' pass ' + passes + ': forced ' + f + ' boxes');
      await scrollAside(a);
      say(key + ' pass ' + passes + ': scrolled');
    }
    env.passes[key] = { passes, clicks };
    say(key + ': ' + passes + ' passes, ' + clicks + ' clicks, pending ' + lastPending);
    return lastPending;
  }
  function remainingCollapsedThoughts(a) {
    let n = 0;
    for (const h of q(a, HEADER_SEL)) if (HEADER_OPEN.test(headerState(h))) n++;
    for (const b of q(a, 'button')) if (norm(b.innerText) === 'Show more') n++;
    for (const b of q(a, 'button[aria-label="Toggle results"]')) { if (b.disabled) continue; const row = b.closest(ROW_SEL); if (!(row && q(row, BOX_SEL).some((d) => hasContent(d)))) n++; }
    for (const d of q(a, 'div[style]')) if (isH0(d) && hasContent(d)) n++;   // boxes still visually collapsed
    return n;
  }
  function remainingCollapsedSources(a) {
    let n = 0;
    for (const row of q(a, SROW_SEL)) { const b = row.querySelector(':scope > button'); if (!b || b.disabled) continue; if (!q(row, ':scope > div').some((d) => hasContent(d))) n++; }
    for (const d of q(a, 'div[style]')) if (isH0(d) && hasContent(d)) n++;
    return n;
  }

  // ------------------------------------------------------------------ 3. panel walkers
  function parseHeader(h) {
    const span = h.querySelector('span');
    if (!span) return { rollout: norm(h.innerText), role: null };
    const roleSpan = span.querySelector('span');
    const rollout = ownText(span) || norm(span.innerText);
    const role = roleSpan ? (norm(roleSpan.innerText) || null) : null;
    return { rollout, role };
  }
  function parseThoughtRow(row) {
    const rows = [];
    const cols = Array.from(row.children);
    const col = cols[1] || cols[0] || row;
    const head = col.querySelector('button') || col;
    const spans = leafSpans(head).filter((s) => norm(s.innerText));
    const first = spans.length ? norm(spans[0].innerText) : '';
    if (/^Sent to /.test(first) || /^Sent to /.test(norm(col.innerText))) { rows.push({ type: 'chatroom', label: first || norm(col.innerText), message: null }); return rows; }
    if (/^Used /.test(first)) { // connector (MCP) calls: "Used Github Search Repositories …" is a tool row, not a summary
      rows.push({ type: 'tool', kind: 'connector', query: first, count: null, url: null, results: [] });
      return rows;
    }
    // image searches, X thread reads and image views are tool calls (imageSearch, xThreadFetch, viewImage), not summaries
    const head1 = first || norm(col.innerText);
    if (/^(Searched images|Reading thread|Viewed image)\b/.test(head1)) {
      const t = parseToolHead(head);
      const box = q(col, BOX_SEL).find((d) => d.querySelector('a[href]')) || null;
      rows.push({ type: 'tool', kind: (head1.match(/^(Searched images|Reading thread|Viewed image)/) || [])[1], query: t.query, count: t.count, url: t.url, results: parseResults(box) });
      return rows;
    }
    if (first && TOOL_KINDS.includes(first)) {
      const t = parseToolHead(head);
      const box = q(col, BOX_SEL).find((d) => d.querySelector('a[href]')) || null;
      rows.push({ type: 'tool', kind: t.kind, query: t.query, count: t.count, url: t.url, results: parseResults(box) });
      return rows;
    }
    if (/\+\d+ more/.test(col.innerText || '')) { // grouped consecutive summaries
      const b = col.querySelector('button');
      const text = b ? norm(b.innerText) : first;
      if (text) rows.push({ type: 'summary', text });
      const box = q(col, BOX_SEL)[0];
      if (box) for (const s of q(box, 'span').filter((x) => !x.querySelector('span'))) { const t = norm(s.innerText); if (t) rows.push({ type: 'summary', text: t }); }
      return rows;
    }
    const iconLabel = (cols[0] && cols[0].querySelector('[aria-label]')) ? cols[0].querySelector('[aria-label]').getAttribute('aria-label') : null;
    const text = norm(col.innerText) || (iconLabel && iconLabel !== 'Toggle results' ? norm(iconLabel) : '');
    rows.push(text ? { type: 'summary', text } : { type: 'other', text: norm(row.innerText) });
    return rows;
  }
  function walkThoughts(a) {
    const sections = [];
    let cur = null;
    const section = () => { if (!cur) { cur = { rollout: null, role: null, rows: [] }; sections.push(cur); } return cur; };
    const visit = (el) => {
      if (el.matches(HEADER_SEL)) { const h = parseHeader(el); cur = { rollout: h.rollout, role: h.role, rows: [] }; sections.push(cur); return; }
      if (el.matches(ROW_SEL)) { for (const r of parseThoughtRow(el)) section().rows.push(r); return; }
      if (el.classList.contains('streamdown-chat-md')) {
        const msg = (el.innerText || '').replace(/\r/g, '').trim();
        const s = section();
        const last = s.rows[s.rows.length - 1];
        if (last && last.type === 'chatroom' && last.message === null) last.message = msg; else s.rows.push({ type: 'chatroom', label: null, message: msg });
        return;
      }
      for (const c of Array.from(el.children)) visit(c);
    };
    for (const c of Array.from(a.children)) visit(c);
    return sections;
  }
  function walkSources(a) {
    const sections = [];
    let cur = null;
    const section = () => { if (!cur) { cur = { rollout: null, role: null, rows: [] }; sections.push(cur); } return cur; };
    const visit = (el) => {
      if (el.matches(AGENT_SEL) || el.matches(HEADER_SEL)) { const h = parseHeader(el); cur = { rollout: h.rollout, role: h.role, rows: [] }; sections.push(cur); return; }
      if (el.matches(SROW_SEL)) {
        const b = el.querySelector(':scope > button');
        const t = b ? parseToolHead(b) : { kind: null, query: norm(el.innerText), count: null, url: null };
        const box = q(el, ':scope > div').find((d) => d.querySelector('a[href]')) || null;
        section().rows.push({ kind: t.kind, query: t.query, count: t.count, url: t.url, results: parseResults(box), disabled: !!(b && b.disabled) });
        return;
      }
      for (const c of Array.from(el.children)) visit(c);
    };
    for (const c of Array.from(a.children)) visit(c);
    return sections;
  }
  function countLinks(a) { return q(a, 'a[href]').length; }

  // ------------------------------------------------------------------ main
  const stats = { articles: 0, userArticles: 0, assistantArticles: 0, thoughtsPanels: 0, sourcesPanels: 0, thoughtRows: 0, sourceRows: 0, thoughtLinks: 0, sourceLinks: 0, remainingCollapsedTotal: 0 };
  await closeAside();
  stats.articles = await scrollTranscript();
  say('transcript scrolled: ' + stats.articles + ' articles');
  const articles = await collectArticles();
  say('articles collected');
  stats.articles = articles.length;
  stats.userArticles = articles.filter((x) => x.role === 'user').length;
  stats.assistantArticles = articles.filter((x) => x.role === 'assistant').length;
  const thoughtsByArticle = {}, sourcesByArticle = {};
  const arts = articleEls;
  for (let i = 0; i < arts.length; i++) {
    if (articles[i].role !== 'assistant') continue;
    if (only && only.article !== i) continue;
    const art = arts[i];
    const tb = art.querySelector('button[class~="group/notes"]');
    const sb = q(art, '[role=button][aria-label]').find((b) => /\bsources?$/i.test(b.getAttribute('aria-label')));
    const canvas = !tb && canvasTrigger(art);
    if ((!only || only.panel === 'thoughts') && canvas) {
      const box = await expandCanvas(art);
      box.scrollIntoView({ block: 'start' });
      await sleep(80);
      await shotPause(i, 'thoughts');
      const sections = walkCanvas(box);
      const remainingCollapsed = canvasClosedToggles(box).length;
      const rows = sections.reduce((n, s) => n + s.rows.length, 0);
      const links = countLinks(box);
      thoughtsByArticle[i] = { layout: 'canvas', sections, innerText: panelText(box), remainingCollapsed, rowCount: rows, linkCount: links };
      stats.thoughtsPanels++; stats.thoughtRows += rows; stats.thoughtLinks += links; stats.remainingCollapsedTotal += remainingCollapsed;
      say('canvas ' + i + ': rows ' + rows + ' links ' + links + ' remaining ' + remainingCollapsed);
    } else if (!only || only.panel === 'thoughts') {
      if (!tb) thoughtsByArticle[i] = null;
      else {
        const a = await openPanel(tb, 'Thoughts');
        if (!a) thoughtsByArticle[i] = { error: 'panel did not open', sections: [], innerText: '', remainingCollapsed: -1, rowCount: 0, linkCount: 0 };
        else {
          await expand(a, thoughtsPending, 'thoughts:' + i);
          forceBoxes(a);
          await scrollAside(a);
          await shotPause(i, 'thoughts');
          say('thoughts ' + i + ': walking');
          const sections = walkThoughts(a);
          say('thoughts ' + i + ': walked');
          const remainingCollapsed = remainingCollapsedThoughts(a);
          say('thoughts ' + i + ': remaining computed');
          const rows = sections.reduce((n, s) => n + s.rows.length, 0);
          const links = countLinks(a);
          thoughtsByArticle[i] = { sections, innerText: panelText(a), remainingCollapsed, rowCount: rows, linkCount: links };
          stats.thoughtsPanels++; stats.thoughtRows += rows; stats.thoughtLinks += links; stats.remainingCollapsedTotal += remainingCollapsed;
          say('thoughts ' + i + ': sections ' + sections.length + ' rows ' + rows + ' links ' + links + ' remaining ' + remainingCollapsed);
        }
      }
    }
    if (!only || only.panel === 'sources') {
      if (!sb) sourcesByArticle[i] = null;
      else {
        const a = await openPanel(sb, 'Sources');
        if (!a) sourcesByArticle[i] = { label: sb.getAttribute('aria-label'), error: 'panel did not open', sections: [], innerText: '', remainingCollapsed: -1, rowCount: 0, linkCount: 0 };
        else {
          const accordion = isAccordion(a);
          let sections;
          if (accordion) {
            sections = await readAccordion(a);
            await scrollAside(a);
            await shotPause(i, 'sources');
          } else {
            await expand(a, sourcesPending, 'sources:' + i);
            forceBoxes(a);
            await scrollAside(a);
            await shotPause(i, 'sources');
            sections = walkSources(a);
          }
          const remainingCollapsed = remainingCollapsedSources(a);
          const rows = sections.reduce((n, s) => n + s.rows.length, 0);
          const links = countLinks(a);
          sourcesByArticle[i] = { label: sb.getAttribute('aria-label'), sections, innerText: panelText(a), remainingCollapsed, rowCount: rows, linkCount: links };
          stats.sourcesPanels++; stats.sourceRows += rows; stats.sourceLinks += links; stats.remainingCollapsedTotal += remainingCollapsed;
          say('sources ' + i + ': sections ' + sections.length + ' rows ' + rows + ' links ' + links + ' remaining ' + remainingCollapsed);
        }
      }
    }
  }
  if (!keepOpen) await closeAside();
  env.forcedOpen = forcedOpenTotal;
  env.elapsedMs = Date.now() - t0;
  if (verbose) env.log = log;
  return { url: location.href, title: document.title, capturedAt: new Date().toISOString(), articles, thoughtsByArticle, sourcesByArticle, stats, env };
})
