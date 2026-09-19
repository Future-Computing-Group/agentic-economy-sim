"""The round barrier, and what a stage record keeps when the server answers 502.

Both are checked against stub tier servers: no container, no backend, no model.
The barrier is the other measurement the testbed exists for, and a straggler is
the only thing that can break it.
"""
import json
import tempfile
import threading
import time
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import _ctx
import load_gen

STRAGGLER = ("s1", "aggregate", 1.0)   # task, stage, extra seconds


def tier_server(tier, netem, fail=False):
    class Handler(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"

        def _send(self, code, obj):
            raw = json.dumps(obj).encode()
            self.send_response(code)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(raw)))
            self.end_headers()
            self.wfile.write(raw)

        def do_GET(self):
            self._send(200, {"tier": tier, "ollama_base": "http://stub:11434", "models": [],
                             "model": "stub", "tier_cap": 3, "netem_delay_ms": netem,
                             "netem_jitter_ms": 1, "container": "stub-%s" % tier,
                             "error": None})

        def do_POST(self):
            req = json.loads(self.rfile.read(int(self.headers.get("Content-Length", "0"))))
            if fail:
                # Exactly what the real server answers when its backend blows up.
                return self._send(502, {"tier": tier, "wait_ms": 412.5, "service_ms": 90210.0,
                                        "tokens": 0, "text": "", "netem_delay_ms": netem,
                                        "container": "stub-%s" % tier,
                                        "error": "URLError: <urlopen error timed out>"})
            extra = STRAGGLER[2] if (req["task_id"], req["stage"]) == STRAGGLER[:2] else 0.0
            time.sleep(float(req.get("sleep_ms", 0)) / 1000.0 + extra)
            self._send(200, {"tier": tier, "wait_ms": 0.0,
                             "service_ms": float(req.get("sleep_ms", 0)) + extra * 1000.0,
                             "tokens": 7, "text": "sub one\nsub two\n", "netem_delay_ms": netem,
                             "container": "stub-%s" % tier, "error": None})

        def log_message(self, *a):
            pass

    srv = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv, "http://127.0.0.1:%d" % srv.server_address[1]


class TestRoundBarrier(unittest.TestCase):
    def setUp(self):
        self.servers = []
        self.profile = Path(tempfile.mkdtemp()) / "profile.json"
        self.profile.write_text(json.dumps({"tiers": {t: {"mean_latency_ms": 50.0}
                                                      for t in ("device", "edge", "cloud")}}))

    def tearDown(self):
        for s in self.servers:
            s.shutdown()
            s.server_close()

    def _stack(self, fail_tier=None):
        urls = {}
        for tier, netem in (("device", 5), ("edge", 15), ("cloud", 50)):
            srv, url = tier_server(tier, netem, fail=(tier == fail_tier))
            self.servers.append(srv)
            urls[tier] = url
        return urls

    def _run(self, urls, out):
        load_gen.main(["--replay-dir", str(_ctx.FIXTURES), "--load", "smoke",
                       "--out-dir", str(out), "--transport", "sleep",
                       "--offered-scale", "1.0", "--max-rounds", "3", "--run-id", "barrier",
                       "--profile", str(self.profile), "--device-url", urls["device"],
                       "--edge-url", urls["edge"], "--cloud-url", urls["cloud"]])
        return [json.loads(l) for l in (out / "stage_records.jsonl").read_text().splitlines()]

    def test_no_round_starts_before_the_previous_one_finishes(self):
        out = Path(tempfile.mkdtemp())
        stages = self._run(self._stack(), out)
        done, sent = {}, {}
        for s in stages:
            end = s["t_send"] + (s["wait_ms"] + s["service_ms"]) / 1000.0
            done[s["round"]] = max(done.get(s["round"], 0.0), end)
            sent[s["round"]] = min(sent.get(s["round"], 1e18), s["t_send"])
        self.assertEqual(sorted(done), [1, 2, 3])
        for r in sorted(done)[:-1]:
            self.assertLess(done[r], sent[r + 1],
                            "round %d starts before round %d finishes" % (r + 1, r))

    def test_run_meta_records_the_subsample(self):
        """The seed and the replayed task ids, so the simulator side can match them."""
        out = Path(tempfile.mkdtemp())
        self._run(self._stack(), out)
        meta = json.loads((out / "run_meta.json").read_text())
        self.assertEqual(meta["subsample_seed"], 1)
        replayed = meta["replayed_task_ids"]["smoke"]
        self.assertEqual(sorted(replayed), ["1", "2", "3"])
        self.assertEqual(sorted(replayed["1"]), ["s1", "s2"])
        self.assertEqual(replayed["3"], ["s6"])

    def test_the_straggler_actually_straggled(self):
        """Without a slow task the barrier assertion would be vacuous."""
        out = Path(tempfile.mkdtemp())
        stages = self._run(self._stack(), out)
        slow = [s for s in stages if s["task_id"] == STRAGGLER[0] and s["stage"] == STRAGGLER[1]]
        self.assertTrue(slow and slow[0]["service_ms"] > 900, slow)


class TestErrorResponsePreservesSplit(unittest.TestCase):
    def test_502_keeps_wait_and_service(self):
        out = Path(tempfile.mkdtemp())
        t = TestRoundBarrier("test_the_straggler_actually_straggled")
        t.setUp()
        try:
            stages = t._run(t._stack(fail_tier="edge"), out)
        finally:
            t.tearDown()
        edge = [s for s in stages if s["tier"] == "edge"]
        self.assertTrue(edge)
        for s in edge:
            self.assertEqual(s["wait_ms"], 412.5)
            self.assertEqual(s["service_ms"], 90210.0)
            self.assertIn("URLError", s["error"])
            self.assertEqual(s["netem_delay_ms"], 15)


if __name__ == "__main__":
    unittest.main()
