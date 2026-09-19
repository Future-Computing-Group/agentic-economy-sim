"""Wait versus service, per tier, out of the stage records.

The split is the measurement the testbed exists for, so it is a module with a
test rather than a script that lived next to one run's output. The fixture is
that run's own records, trimmed: the values are measured, the identifiers and
the wall-clock stamps are not.
"""
import unittest

import _ctx
import decompose

T_KILL = 50.0


class TestDecompose(unittest.TestCase):
    def setUp(self):
        self.recs = decompose.load_records(_ctx.FIXTURES / "stage_records_rung2.jsonl")
        self.pre, self.post = decompose.split_at(self.recs, T_KILL)

    def test_the_fixture_splits_at_the_kill(self):
        self.assertEqual(len(self.recs), 16)
        self.assertEqual(len(self.pre), 12)
        self.assertEqual(len(self.post), 4)

    def test_per_tier_medians_and_p95(self):
        by_tier = decompose.per_tier(self.pre)
        self.assertEqual(sorted(by_tier), ["cloud", "device", "edge"])
        for tier, n, w_med, w_p95, s_med, s_p95 in [
            ("device", 4, 0.0075, 0.016, 5293.9265, 6806.590),
            ("edge",   4, 0.0065, 0.024, 3519.0970, 4349.903),
            ("cloud",  4, 0.0080, 0.019, 4317.2450, 4801.894),
        ]:
            row = by_tier[tier]
            self.assertEqual(row["n"], n, tier)
            self.assertAlmostEqual(row["wait_med"], w_med, places=6, msg=tier)
            self.assertAlmostEqual(row["wait_p95"], w_p95, places=6, msg=tier)
            self.assertAlmostEqual(row["service_med"], s_med, places=3, msg=tier)
            self.assertAlmostEqual(row["service_p95"], s_p95, places=3, msg=tier)
            # Where the contention binds: at our admission gate, or inside the
            # one backend all three tiers share.
            self.assertAlmostEqual(row["wait_fraction"], w_med / (w_med + s_med),
                                   places=9, msg=tier)

    def test_a_failed_call_carries_no_wait_or_service_and_reaches_no_statistic(self):
        # The post-kill cloud records have wait_ms and service_ms of None: the
        # call never got a reply to measure. Read as zeros they would halve the
        # tier's median service time and report the outage as a speed-up.
        errored = [r for r in self.post if r["error"]]
        self.assertTrue(errored and all(r["service_ms"] is None for r in errored))
        whole_run = decompose.per_tier(self.recs)["cloud"]
        self.assertEqual(whole_run["n"], 4)
        self.assertAlmostEqual(whole_run["service_med"], 4317.245, places=3)

    def test_outcomes_name_the_tier_that_went_away(self):
        out = decompose.outcomes(self.post)
        self.assertEqual((out["cloud"]["ok"], out["cloud"]["errored"]), (0, 2))
        self.assertEqual((out["device"]["ok"], out["device"]["errored"]), (1, 0))
        self.assertEqual((out["edge"]["ok"], out["edge"]["errored"]), (1, 0))
        self.assertEqual(out["cloud"]["errors"], ["URLError"])
        self.assertEqual(out["device"]["errors"], [])

    def test_per_load_splits_the_contrast_the_statistics_rest_on(self):
        by_load = decompose.per_load(self.pre)
        self.assertEqual(sorted(by_load), ["high", "medium"])
        self.assertEqual(by_load["medium"]["n"], 6)
        self.assertEqual(by_load["high"]["n"], 6)

    def test_per_load_reports_whatever_loads_the_run_carried(self):
        # The run picks its own two load levels; a fixed list of names silently
        # drops the ones it does not know, and a load missing from the
        # decomposition reads as a load that produced no calls.
        recs = [dict(r, load="low") if r["load"] == "medium" else r for r in self.pre]
        by_load = decompose.per_load(recs)
        self.assertEqual(sorted(by_load), ["high", "low"])
        self.assertEqual(by_load["low"]["n"], 6)
        self.assertEqual(by_load["high"]["n"], 6)

    def test_a_tier_with_no_records_reports_no_statistic(self):
        row = decompose.per_tier([])["device"]
        self.assertEqual(row["n"], 0)
        self.assertIsNone(row["service_med"])
        self.assertIsNone(row["wait_fraction"])

    def test_a_run_with_no_kill_is_all_pre(self):
        pre, post = decompose.split_at(self.recs, None)
        self.assertEqual((len(pre), len(post)), (16, 0))


if __name__ == "__main__":
    unittest.main()
