(() => {
  // grok.com streams each message's steps (with tool results such as command output) over wss://grok.com/ws/mgw as
  // conversation.history.item events; the REST responses lack those results.  Keep those frames for the exporter.
  if (window.__fxWsHooked) return;
  window.__fxWsHooked = true;
  window.__fxMgw = { items: [], done: false };
  const Native = window.WebSocket;
  class Recording extends Native {
    constructor(url, protocols) {
      super(url, protocols);
      if (/\/ws\/mgw\//.test(String(url))) {
        this.addEventListener('message', (ev) => {
          if (typeof ev.data !== 'string') return;
          if (ev.data.indexOf('"conversation.history.item"') >= 0) window.__fxMgw.items.push(ev.data);
          else if (ev.data.indexOf('"conversation.history.done"') >= 0) window.__fxMgw.done = true;
        });
      }
    }
  }
  window.WebSocket = Recording;
})();