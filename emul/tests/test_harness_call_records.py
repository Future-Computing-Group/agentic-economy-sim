"""The released agent harness must persist its per-call stage records.

It already measures per-call latency and tokens and then throws them away,
keeping only per-tier means. The emulation aligns its own stage records against
that ordered timeline, so the records have to survive to the output file. A stub
backend stands in for the model: no model is called here.
"""
import contextlib
import importlib.util
import io
import json
import sys
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

import _ctx

PLAN = "first sub-question\nsecond sub-question\n"


class StubBackend(BaseHTTPRequestHandler):
    def do_POST(self):
        self.rfile.read(int(self.headers.get("Content-Length", "0")))
        raw = json.dumps({"response": PLAN, "prompt_eval_count": 5,
                          "eval_count": 9}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)

    def log_message(self, *a):
        pass


class TestCallRecords(unittest.TestCase):
    def setUp(self):
        self.srv = HTTPServer(("127.0.0.1", 0), StubBackend)
        threading.Thread(target=self.srv.serve_forever, daemon=True).start()
        self.url = "http://127.0.0.1:%d/api/generate" % self.srv.server_address[1]

    def tearDown(self):
        self.srv.shutdown()

    def run_harness(self, n=2):
        path = _ctx.REPO / "agentic" / "run_agent_workload.py"
        spec = importlib.util.spec_from_file_location("harness_records", path)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        mod.OLLAMA = self.url
        out = Path(tempfile.mkdtemp()) / "profile.json"
        argv = sys.argv
        sys.argv = ["run_agent_workload.py", "--model", "stub", "--n", str(n), "--out", str(out)]
        try:
            with contextlib.redirect_stdout(io.StringIO()):
                mod.main()
        finally:
            sys.argv = argv
        return json.loads(out.read_text())

    def test_per_call_records_present(self):
        profile = self.run_harness(n=2)
        calls = profile["calls"]
        self.assertEqual(len(calls), 8)  # 4 stages per task, 2 tasks
        for c in calls:
            self.assertEqual(sorted(c), ["latency_ms", "t_start", "tokens"])
            self.assertEqual(c["tokens"], 14)
            self.assertGreater(c["latency_ms"], 0)
        self.assertEqual([c["t_start"] for c in calls],
                         sorted(c["t_start"] for c in calls))

    def test_per_run_stages_present(self):
        profile = self.run_harness(n=2)
        self.assertEqual(len(profile["runs"]), 2)
        self.assertEqual(sorted(profile["runs"][0]), ["aggregate", "plan", "tool_0", "tool_1"])
        self.assertEqual(profile["runs"][0]["plan"]["tier"], "device")

    def test_existing_keys_untouched(self):
        profile = self.run_harness(n=2)
        self.assertEqual(sorted(profile["tiers"]), ["cloud", "device", "edge"])
        self.assertEqual(profile["tiers"]["edge"]["n_stage_calls"], 4)
        self.assertIn("demand_weight", profile["tiers"]["cloud"])
        self.assertEqual(profile["n_tasks"], 2)
        self.assertIn("structure", profile)


if __name__ == "__main__":
    unittest.main()
