#!/usr/bin/env python3
"""Directory + 404 page for *.demo.data-ui.com (HAProxy backend).

HAProxy has no CGI; this tiny service is the equivalent. The HAProxy
map (/etc/haproxy/data-ui.map) is the router's source of truth, so it
is re-read on every request and the page can never go stale:

  - Host demo.data-ui.com        -> 200, list of live demos
  - any other *.demo.data-ui.com -> 404, "not found" + the same list
  - GET /health                  -> 200, for the HAProxy health check
"""
import html
import os
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

MAP_PATH = "/etc/haproxy/data-ui.map"
ZONE = ".demo.data-ui.com"
APEX = "demo.data-ui.com"
# Never listed, though HAProxy still routes them (infra, not demo).
EXCLUDE = {"chat.demo.data-ui.com"}
BIND_HOST, BIND_PORT = "127.0.0.1", int(os.environ.get("PORT", "8484"))

# Sinister Code logo (164x133 jpeg, ~4 KB), embedded so the page
# is self-contained: no request to sinistercode.com at render time.
# Re-embed with: base64 -w0 <logo.jpg>  (regenerate the chunks)
LOGO = (
    "data:image/jpeg;base64,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"
)

STYLE = (
    "body{margin:0;min-height:100vh;display:grid;place-items:center;"
    "background:#f1f3f6;color:#202124;font:18px/1.65 system-ui,sans-serif}"
    "main{max-width:38rem;padding:2.5rem 3rem;background:#ffffff;"
    "border:1px solid #dadce0;border-radius:12px;"
    "box-shadow:0 2px 12px rgba(60,64,67,.15)}"
    "h1{margin:0 0 .3rem;font-size:1.9rem;font-weight:600;color:#1f2328;text-align:center}"
    "h1 img{height:1.6em;vertical-align:-.32em;margin-right:.3em}"
    "h2{margin:0 0 1rem;font-size:1.4rem;font-weight:600;color:#3c4043;text-align:center}"
    "a{color:#0b57d0;text-decoration:none}a:hover{text-decoration:underline}"
    "code{color:#5f6368;font-size:.95em}"
    "sup{line-height:0}"
    "ul{margin:.5rem 0 1.25rem;padding-left:1.2rem}"
    "li{margin:.4rem 0}"
    ".foot{margin-top:1.5rem;color:#5f6368;font-size:.8em;line-height:1.7}"
)


MASTHEAD = ("<h1><img src='" + LOGO + "' alt=''>Sinister Code"
            "<sup>&#167;</sup></h1>")


def load_demo_hosts():
    """Hosts mapped under the demo zone, sorted, deduplicated."""
    hosts = set()
    try:
        with open(MAP_PATH, encoding="utf-8") as map_file:
            for line in map_file:
                parts = line.split()
                if len(parts) == 2 and parts[0].endswith(ZONE) \
                        and parts[0] not in EXCLUDE:
                    hosts.add(parts[0])
    except OSError:
        pass  # unreadable map -> empty list, page still renders
    return sorted(hosts)


def render_page(host, found):
    demos = load_demo_hosts()
    items = "\n".join(
        '<li><a href="https://{0}/">{1}</a></li>'.format(
            html.escape(demo, quote=True),
            html.escape(demo[: -len(ZONE)], quote=True),
        )
        for demo in demos
    ) or "<li><em>none running</em></li>"
    if found:
        head = (MASTHEAD +
                "<h2>Data UI<sup>&#8224;</sup> demos</h2>"
                "<p>Live demo sites on <code>*" + ZONE + "</code>:</p>")
        status = 200
        cache = "max-age=60"
    else:
        head = (MASTHEAD +
                "<h2>404: Data UI<sup>&#8224;</sup> demo not found</h2>"
                "<p><b>" + html.escape(host or "this host", quote=True)
                + "</b> is not a Data UI demo site. "
                "It may have been renamed or retired.</p>"
                "<p>Demos currently running:</p>")
        status = 404
        cache = "no-store"
    body = (
        "<!doctype html><html><head><meta charset='utf-8'>"
        "<meta name='viewport' content='width=device-width,initial-scale=1'>"
        "<title>Sinister Code | Data UI Demos</title><style>" + STYLE + "</style></head>"
        "<body><main>" + head + "<ul>" + items + "</ul>"
        "<div class='foot'>"
        "<sup>&#8224;</sup> For more information about Data UI demos, "
        "visit <a href='https://data-ui.com/'>data-ui.com</a>.<br>"
        "<sup>§</sup> Maintained by "
        "<a href='https://sinistercode.com/public/donnie'>Donnie Cameron</a> "
        "at <a href='https://sinistercode.com/'>Sinister Code</a>."
        "</div></main></body></html>"
    )
    return status, cache, body.encode("utf-8")


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def _respond(self, include_body):
        if self.path == "/health":
            status, cache, body = 200, "no-store", b"ok\n"
        else:
            host = (self.headers.get("Host") or "").split(":")[0]
            host = host.strip(".").lower()
            status, cache, body = render_page(host, host == APEX)
        self.send_response(status)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", cache)
        self.end_headers()
        if include_body:
            self.wfile.write(body)

    def do_GET(self):
        self._respond(include_body=True)

    def do_HEAD(self):
        self._respond(include_body=False)

    def log_message(self, fmt, *args):
        pass  # stay quiet; systemd journal covers lifecycle


if __name__ == "__main__":
    server = ThreadingHTTPServer((BIND_HOST, BIND_PORT), Handler)
    print(f"demo-directory listening on {BIND_HOST}:{BIND_PORT}", flush=True)
    server.serve_forever()
