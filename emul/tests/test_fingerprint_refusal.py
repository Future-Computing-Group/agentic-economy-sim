"""a run refuses to start unless every container resolves the host backend.

A published container backend answers on the same name and port as the host
daemon, so a run that skips this check can silently measure the wrong engine.
The sentinel tag is the discriminator and it is configurable, not a constant.
"""
import json
import threading
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer

import _ctx
import load_gen

MODEL = "mistral:7b-instruct-q4_K_M"
SENTINEL = "command-r7b"
HOST_TAGS = [MODEL, "command-r7b:latest", "qwen2.5:7b"]
VM_TAGS = [MODEL]


def stub_server(tags, tier="edge", netem=15):
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            if self.path == "/fingerprint":
                body = {"tier": tier, "ollama_base": "http://stub:11434",
                        "models": tags, "netem_delay_ms": netem, "tier_cap": 3,
                        "container": "stub-%s" % tier, "error": None}
            elif self.path == "/api/tags":
                body = {"models": [{"name": t} for t in tags]}
            else:
                self.send_response(404); self.end_headers(); return
            raw = json.dumps(body).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(raw)))
            self.end_headers()
            self.wfile.write(raw)

        def log_message(self, *a):
            pass

    srv = HTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv, "http://127.0.0.1:%d" % srv.server_address[1]


class TestFingerprintRefusal(unittest.TestCase):
    def setUp(self):
        self.servers = []

    def tearDown(self):
        for s in self.servers:
            s.shutdown()
            s.server_close()

    def _url(self, tags, tier="edge"):
        srv, url = stub_server(tags, tier)
        self.servers.append(srv)
        return url

    def test_all_host_backends_accepted(self):
        good = self._url(HOST_TAGS)
        urls = {t: self._url(HOST_TAGS, t) for t in ("device", "edge", "cloud")}
        block = load_gen.check_fingerprint(urls, good, MODEL, SENTINEL)
        self.assertEqual(sorted(block["tiers"]), ["cloud", "device", "edge"])
        self.assertTrue(block["sentinel_ok"])

    def test_container_backend_without_sentinel_refused(self):
        host = self._url(HOST_TAGS)
        urls = {"device": self._url(HOST_TAGS, "device"),
                "edge": self._url(VM_TAGS, "edge"),
                "cloud": self._url(HOST_TAGS, "cloud")}
        with self.assertRaises(SystemExit) as cm:
            load_gen.check_fingerprint(urls, host, MODEL, SENTINEL)
        self.assertIn("edge", str(cm.exception))
        self.assertIn(SENTINEL, str(cm.exception))

    def test_missing_model_refused(self):
        host = self._url(HOST_TAGS)
        urls = {t: self._url(HOST_TAGS, t) for t in ("device", "edge", "cloud")}
        with self.assertRaises(SystemExit) as cm:
            load_gen.check_fingerprint(urls, host, "no-such-model:1b", SENTINEL)
        self.assertIn("no-such-model:1b", str(cm.exception))

    def test_sentinel_is_configurable(self):
        host = self._url(HOST_TAGS)
        urls = {t: self._url(HOST_TAGS, t) for t in ("device", "edge", "cloud")}
        block = load_gen.check_fingerprint(urls, host, MODEL, "qwen2.5")
        self.assertTrue(block["sentinel_ok"])


if __name__ == "__main__":
    unittest.main()
