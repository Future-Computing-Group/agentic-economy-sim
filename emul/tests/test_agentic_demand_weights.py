"""Per-tier demand weights are PER TASK, not per stage call.

The simulator's demand weight is the load one task puts on one tier: it builds
one task bundle per tier and charges that tier the weight once per task. The
agent visits the edge tier twice in every task, because its two tool calls are
parallel branches of the same task, so a mean over stage calls understates edge
demand by the branching factor. Tokens are therefore summed within a task and
averaged over tasks. Latency stays per call: base latency is a per-stage
quantity and the parallel branches overlap in time rather than adding.
"""
import importlib.util
import json
import unittest

import _ctx

_spec = importlib.util.spec_from_file_location(
    "agent_workload", _ctx.REPO / "agentic" / "run_agent_workload.py")
harness = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(harness)


def stage(tier, tokens, latency_ms=10.0):
    return {"tier": tier, "tokens": tokens, "latency_ms": latency_ms}


def task(plan, tool_0, tool_1, aggregate):
    return {"plan": stage("device", plan),
            "tool_0": stage("edge", tool_0),
            "tool_1": stage("edge", tool_1),
            "aggregate": stage("cloud", aggregate)}


ONE_TASK = [task(plan=80, tool_0=70, tool_1=74, aggregate=160)]


class TestPerTaskAggregation(unittest.TestCase):
    def test_parallel_calls_on_one_tier_sum_within_a_task(self):
        tiers = harness.aggregate_tiers(ONE_TASK)
        self.assertEqual(tiers["edge"]["mean_tokens_per_task"], 144)
        self.assertEqual(tiers["device"]["mean_tokens_per_task"], 80)
        self.assertEqual(tiers["cloud"]["mean_tokens_per_task"], 160)
        # The per-call fields are unchanged by the per-task total beside them.
        self.assertEqual(tiers["edge"]["n_stage_calls"], 2)
        self.assertEqual(tiers["edge"]["mean_tokens"], 72)

    def test_demand_weights_normalise_to_the_smallest_tier(self):
        weights = {t: v["demand_weight"]
                   for t, v in harness.aggregate_tiers(ONE_TASK).items()}
        self.assertEqual(min(weights.values()), 1.0)
        self.assertEqual(weights["device"], 1.0)
        self.assertEqual(weights["edge"], 1.8)     # 144 / 80
        self.assertEqual(weights["cloud"], 2.0)    # 160 / 80

    def test_the_mean_runs_over_tasks_not_over_calls(self):
        two = ONE_TASK + [task(plan=80, tool_0=10, tool_1=6, aggregate=160)]
        tiers = harness.aggregate_tiers(two)
        self.assertEqual(tiers["edge"]["mean_tokens_per_task"], 80)   # (144+16)/2
        self.assertEqual(tiers["edge"]["n_stage_calls"], 4)
        self.assertEqual(tiers["edge"]["demand_weight"], 1.0)         # now the smallest

    def test_rederive_recovers_per_task_totals_from_per_tier_aggregates(self):
        old = {"model": "m", "n_tasks": 5, "structure": "series-parallel",
               "tiers": {
                   "device": {"mean_tokens": 80.2, "mean_latency_ms": 1354.25,
                              "n_stage_calls": 5, "demand_weight": 1.11},
                   "edge": {"mean_tokens": 72.4, "mean_latency_ms": 1016.29,
                            "n_stage_calls": 10, "demand_weight": 1.0},
                   "cloud": {"mean_tokens": 163.2, "mean_latency_ms": 979.99,
                             "n_stage_calls": 5, "demand_weight": 2.25}}}
        new = harness.rederive(old)
        self.assertEqual(new["tiers"]["edge"]["mean_tokens_per_task"], 144.8)
        self.assertEqual(new["tiers"]["device"]["mean_tokens_per_task"], 80.2)
        self.assertEqual(new["tiers"]["cloud"]["mean_tokens_per_task"], 163.2)
        self.assertEqual(new["tiers"]["edge"]["demand_weight"], 1.81)
        # Everything but the tier block travels through untouched, and the
        # per-call fields inside it do too.
        self.assertEqual(new["model"], "m")
        self.assertEqual(new["n_tasks"], 5)
        self.assertEqual(new["structure"], "series-parallel")
        self.assertEqual(new["tiers"]["edge"]["mean_latency_ms"], 1016.29)
        self.assertEqual(new["tiers"]["edge"]["n_stage_calls"], 10)

    def test_the_shipped_profiles_are_recomputable_from_their_own_records(self):
        """A shipped weight is what the shipped records produce.

        The profiles are regenerable measurements, so the pin is the identity
        between a profile's aggregates and its own per-stage records rather
        than a fixed numeral a re-recording would have to be edited around.
        A hand-edited weight, or a graph typed beside the records instead of
        derived from them, fails here.
        """
        for name in ("agentic_profile.json", "agentic_profile_b.json"):
            profile = json.loads((_ctx.REPO / "agentic" / name).read_text())
            again = harness.rederive(profile)
            for block in ("tiers", "aggregate_stages", "graph"):
                self.assertEqual(again[block], profile[block], name)
            weights = {t: b["demand_weight"] for t, b in profile["tiers"].items()}
            self.assertEqual(sorted(weights), ["cloud", "device", "edge"], name)
            self.assertEqual(min(weights.values()), 1.0, name)

    def test_the_two_shipped_profiles_are_the_two_patterns(self):
        want = {"agentic_profile.json": ("a", ["aggregate"]),
                "agentic_profile_b.json": ("b", ["summary", "citations"])}
        for name, (pattern, leaves) in want.items():
            profile = json.loads((_ctx.REPO / "agentic" / name).read_text())
            self.assertEqual(profile["pattern"], pattern)
            self.assertEqual(profile["graph"]["leaves"], leaves)
            self.assertEqual(profile["model"], "mistral:7b-instruct-q4_K_M")


if __name__ == "__main__":
    unittest.main()
