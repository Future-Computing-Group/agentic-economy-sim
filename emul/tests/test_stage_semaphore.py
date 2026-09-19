"""The stage server's admission semaphore, and what it does with a hostile body.

The wait/service split is the measurement the testbed exists for, so it gets a
standing test that needs no container: the server runs as a local process on the
sleep transport, which calls no backend.
"""
import json
import os
import socket
import subprocess
import sys
import time
import unittest
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor

import _ctx


def free_port():
    s = socket.socket()
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


class StageServer:
    """stage_server.py as a subprocess, with an unroutable backend."""

    def __init__(self, cap, tier="edge"):
        self.port = free_port()
        env = dict(os.environ, TIER=tier, TIER_CAP=str(cap), NETEM_DELAY_MS="15",
                   NETEM_JITTER_MS="3", PORT=str(self.port),
                   OLLAMA_BASE="http://127.0.0.1:1")
        self.proc = subprocess.Popen([sys.executable, str(_ctx.EMUL / "stage_server.py")],
                                     env=env, stdout=subprocess.DEVNULL,
                                     stderr=subprocess.DEVNULL)
        for _ in range(100):
            try:
                socket.create_connection(("127.0.0.1", self.port), 0.2).close()
                return
            except OSError:
                time.sleep(0.05)
        raise RuntimeError("stage server did not come up")

    def post(self, body, raw=False):
        data = body if raw else json.dumps(body).encode()
        req = urllib.request.Request("http://127.0.0.1:%d/stage" % self.port, data=data,
                                     headers={"Content-Type": "application/json"})
        try:
            with urllib.request.urlopen(req, timeout=30) as r:
                return r.status, json.load(r)
        except urllib.error.HTTPError as e:
            return e.code, json.load(e)

    def stage(self, sleep_ms, stage="plan"):
        return self.post({"task_id": "t", "round": 1, "stage": stage,
                          "prompt": "p", "transport": "sleep", "sleep_ms": sleep_ms})

    def stop(self):
        self.proc.terminate()
        self.proc.wait(timeout=10)


class TestSemaphore(unittest.TestCase):
    def test_cap_three_blocks_the_excess(self):
        srv = StageServer(cap=3)
        try:
            t0 = time.time()
            with ThreadPoolExecutor(max_workers=6) as pool:
                out = [f.result() for f in [pool.submit(srv.stage, 300) for _ in range(6)]]
            wall_ms = (time.time() - t0) * 1000.0
        finally:
            srv.stop()
        waits = sorted(r["wait_ms"] for _, r in out)
        self.assertTrue(all(s == 200 for s, _ in out))
        self.assertTrue(all(w < 100 for w in waits[:3]), waits)
        self.assertTrue(all(w > 200 for w in waits[3:]), waits)
        for _, r in out:
            self.assertGreater(r["service_ms"], 290)   # service is the backend, not the wait
            self.assertLess(r["service_ms"], 450)
        self.assertGreater(wall_ms, 560)               # two batches of 300 ms

    def test_cap_one_serialises(self):
        srv = StageServer(cap=1)
        try:
            with ThreadPoolExecutor(max_workers=3) as pool:
                out = [f.result() for f in [pool.submit(srv.stage, 200) for _ in range(3)]]
        finally:
            srv.stop()
        waits = sorted(r["wait_ms"] for _, r in out)
        self.assertLess(waits[0], 100, waits)
        self.assertGreater(waits[1], 150, waits)
        self.assertGreater(waits[2], 350, waits)       # the wait accumulates linearly

    def test_cap_five_does_not_block(self):
        srv = StageServer(cap=5)
        try:
            with ThreadPoolExecutor(max_workers=5) as pool:
                out = [f.result() for f in [pool.submit(srv.stage, 150) for _ in range(5)]]
        finally:
            srv.stop()
        self.assertTrue(all(r["wait_ms"] < 100 for _, r in out))


class TestMalformedBody(unittest.TestCase):
    """A bad request gets a diagnosis, not a dropped connection."""

    @classmethod
    def setUpClass(cls):
        cls.srv = StageServer(cap=2)

    @classmethod
    def tearDownClass(cls):
        cls.srv.stop()

    def test_unparseable_body(self):
        status, body = self.srv.post(b"this is not json", raw=True)
        self.assertEqual(status, 400)
        self.assertIn("error", body)

    def test_json_array_body(self):
        status, body = self.srv.post([1, 2, 3])
        self.assertEqual(status, 400)
        self.assertIn("error", body)

    def test_json_string_body(self):
        status, body = self.srv.post("hello")
        self.assertEqual(status, 400)
        self.assertIn("error", body)

    def test_unknown_stage_name(self):
        status, body = self.srv.stage(10, stage="nonesuch")
        self.assertEqual(status, 400)
        self.assertIn("nonesuch", body["error"])

    def test_known_stages_accepted(self):
        for stage in ("plan", "tool_0", "tool_1", "aggregate"):
            status, _ = self.srv.stage(5, stage=stage)
            self.assertEqual(status, 200, stage)

    def test_server_survives_and_still_serves(self):
        self.srv.post(b"{{{", raw=True)
        status, body = self.srv.stage(5)
        self.assertEqual(status, 200)
        self.assertIsNone(body["error"])

    def test_container_id_is_reported(self):
        _, body = self.srv.stage(5)
        self.assertTrue(body["container"])
        self.assertIsInstance(body["container"], str)


if __name__ == "__main__":
    unittest.main()
