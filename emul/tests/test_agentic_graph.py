"""The agent harness emits the dependency graph it actually executed.

The graph is derived from the parent stage ids each call records as it is made,
so the nodes, the edges and the leaves are a property of the run rather than of
a list typed beside it. Two patterns share the edge-tier stage `tool1`, which is
what lets the union of the two recordings carry a crossing leaf-block family.

Two fingerprints keep the instrument honest, both fail-closed on the netem
template of `emul/load_gen.py`: a response served by another model aborts the
run, and a task whose recorded stage set is not its pattern's declared set is
dropped with a counted reason.
"""
import importlib.util
import json
import sys
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

import _ctx

PLAN = "first sub-question\nsecond sub-question\n"


def load_harness():
    path = _ctx.REPO / "agentic" / "run_agent_workload.py"
    spec = importlib.util.spec_from_file_location("agent_graph_%d" % id(path), path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def make_backend(served_model=None):
    class Stub(BaseHTTPRequestHandler):
        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers.get("Content-Length", "0"))))
            raw = json.dumps({"model": served_model or body["model"],
                              "response": PLAN,
                              "prompt_eval_count": 5, "eval_count": 9}).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(raw)))
            self.end_headers()
            self.wfile.write(raw)

        def log_message(self, *a):
            pass
    return Stub


class HarnessCase(unittest.TestCase):
    served_model = None

    def setUp(self):
        self.srv = HTTPServer(("127.0.0.1", 0), make_backend(self.served_model))
        threading.Thread(target=self.srv.serve_forever, daemon=True).start()
        self.host = "http://127.0.0.1:%d" % self.srv.server_address[1]
        self.harness = load_harness()

    def tearDown(self):
        self.srv.shutdown()

    def run_harness(self, pattern="a", n=2, out=None):
        out = out or Path(tempfile.mkdtemp()) / "profile.json"
        argv = sys.argv
        sys.argv = ["run_agent_workload.py", "--model", "stub", "--n", str(n),
                    "--pattern", pattern, "--host", self.host, "--out", str(out)]
        try:
            self.harness.main()
        finally:
            sys.argv = argv
        return json.loads(Path(out).read_text())


class TestRecordedGraph(HarnessCase):
    def test_pattern_a_graph_is_derived_from_the_recorded_parents(self):
        graph = self.run_harness("a")["graph"]
        self.assertEqual([n["id"] for n in graph["nodes"]],
                         ["plan", "tool0", "tool1", "aggregate"])
        self.assertEqual([n["tier"] for n in graph["nodes"]],
                         ["device", "edge", "edge", "cloud"])
        self.assertEqual([(e["from"], e["to"]) for e in graph["edges"]],
                         [("plan", "tool0"), ("plan", "tool1"),
                          ("tool0", "aggregate"), ("tool1", "aggregate")])
        self.assertEqual(graph["leaves"], ["aggregate"])

    def test_pattern_b_shares_tool1_and_has_two_leaves(self):
        graph = self.run_harness("b")["graph"]
        self.assertEqual([n["id"] for n in graph["nodes"]],
                         ["plan_b", "retrieve", "tool1", "summary", "citations"])
        self.assertEqual([n["tier"] for n in graph["nodes"]],
                         ["device", "edge", "edge", "cloud", "cloud"])
        self.assertEqual([(e["from"], e["to"]) for e in graph["edges"]],
                         [("plan_b", "retrieve"), ("plan_b", "tool1"),
                          ("retrieve", "summary"), ("tool1", "summary"),
                          ("retrieve", "citations")])
        self.assertEqual(graph["leaves"], ["summary", "citations"])

    def test_the_union_of_the_two_recordings_has_three_leaves(self):
        runs = []
        for pattern in ("a", "b"):
            runs += self.run_harness(pattern, n=1)["runs"]
        graph = self.harness.graph_block(runs)
        self.assertEqual(len(graph["nodes"]), 8)
        self.assertEqual(graph["leaves"], ["aggregate", "summary", "citations"])
        # tool1 reaches {aggregate, summary} and retrieve reaches
        # {summary, citations}: the pair that crosses, from the recording.
        self.assertIn({"from": "plan", "to": "tool1"}, graph["edges"])
        self.assertIn({"from": "plan_b", "to": "tool1"}, graph["edges"])
        self.assertIn({"from": "tool1", "to": "summary"}, graph["edges"])

    def test_call_records_carry_the_stage_the_parents_and_the_model(self):
        calls = self.run_harness("a", n=1)["calls"]
        self.assertEqual(sorted(calls[0]),
                         ["latency_ms", "model", "parents", "stage", "t_start",
                          "tier", "tokens"])
        self.assertEqual([c["stage"] for c in calls],
                         ["plan", "tool0", "tool1", "aggregate"])
        self.assertEqual(calls[3]["parents"], ["tool0", "tool1"])
        self.assertEqual(calls[0]["parents"], [])
        self.assertEqual(calls[0]["model"], "stub")

    def test_the_profile_names_the_host_and_the_pattern(self):
        profile = self.run_harness("b", n=1)
        self.assertEqual(profile["pattern"], "b")
        self.assertEqual(profile["host"], self.host)
        self.assertEqual(len(profile["response_digest"]), 64)


class TestStageAggregation(HarnessCase):
    def test_tokens_sum_within_a_task_per_stage(self):
        def stage(tier, tokens, latency_ms=10.0):
            return {"tier": tier, "tokens": tokens, "latency_ms": latency_ms,
                    "parents": []}
        runs = [{"plan": stage("device", 80), "tool0": stage("edge", 70)},
                {"plan": stage("device", 40), "tool0": stage("edge", 10)}]
        agg = self.harness.aggregate_stages(runs)
        self.assertEqual(agg["plan"]["mean_tokens_per_task"], 60)
        self.assertEqual(agg["tool0"]["mean_tokens_per_task"], 40)
        self.assertEqual(agg["tool0"]["n_stage_calls"], 2)
        self.assertEqual(agg["plan"]["demand_weight"], 1.5)
        self.assertEqual(agg["tool0"]["demand_weight"], 1.0)

    def test_the_recorded_profile_carries_a_stage_block_beside_the_tier_block(self):
        profile = self.run_harness("a", n=2)
        self.assertEqual(sorted(profile["aggregate_stages"]),
                         ["aggregate", "plan", "tool0", "tool1"])
        self.assertEqual(sorted(profile["tiers"]), ["cloud", "device", "edge"])


class TestGraphFingerprint(HarnessCase):
    def bad_pattern(self, drop):
        runner, stages = self.harness.PATTERNS["a"]

        def wrong(model, question, endpoint):
            out = runner(model, question, endpoint)
            if drop:
                out.pop("tool1")
            return out
        self.harness.PATTERNS["a"] = (wrong, stages)

    def test_a_task_whose_stage_set_is_wrong_is_dropped_with_a_reason(self):
        runs = [{"plan": {}, "tool0": {}, "tool1": {}, "aggregate": {}},
                {"plan": {}, "tool0": {}, "aggregate": {}}]
        kept, dropped = self.harness.keep_runs(runs, ("plan", "tool0", "tool1", "aggregate"))
        self.assertEqual(len(kept), 1)
        self.assertEqual(sum(dropped.values()), 1)

    def test_a_short_run_writes_no_profile(self):
        self.bad_pattern(drop=True)
        out = Path(tempfile.mkdtemp()) / "profile.json"
        with self.assertRaises(SystemExit) as e:
            self.run_harness("a", n=2, out=out)
        self.assertIn("graph fingerprint refusal", str(e.exception))
        self.assertFalse(out.exists())


class TestModelFingerprint(HarnessCase):
    served_model = "command-r7b:latest"

    def test_a_response_from_another_model_aborts_and_writes_nothing(self):
        out = Path(tempfile.mkdtemp()) / "profile.json"
        with self.assertRaises(SystemExit) as e:
            self.run_harness("a", n=1, out=out)
        self.assertIn("model fingerprint refusal", str(e.exception))
        self.assertIn("command-r7b:latest", str(e.exception))
        self.assertFalse(out.exists())


class TestRederive(HarnessCase):
    def test_rederive_rebuilds_the_graph_from_the_recorded_parents(self):
        first = Path(tempfile.mkdtemp()) / "profile.json"
        self.run_harness("b", n=2, out=first)
        second = Path(tempfile.mkdtemp()) / "again.json"
        argv = sys.argv
        sys.argv = ["run_agent_workload.py", "--rederive-from", str(first),
                    "--out", str(second)]
        try:
            self.harness.main()
        finally:
            sys.argv = argv
        a, b = json.loads(first.read_text()), json.loads(second.read_text())
        self.assertEqual(a["graph"], b["graph"])
        self.assertEqual(a["tiers"], b["tiers"])
        self.assertEqual(a["aggregate_stages"], b["aggregate_stages"])


if __name__ == "__main__":
    unittest.main()
