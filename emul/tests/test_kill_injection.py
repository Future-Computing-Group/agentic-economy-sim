"""The mid-run tier loss, and the per-round counters that make it visible.

The failure overlay pins `restart: "no"`, so the kill is permanent; what has to
be right here is that it happens once, at the declared round, before that round
is issued, and that the run records it. The kill itself is stubbed: this rung
tests the generator's decision, not Docker's.
"""
import json
import tempfile
import unittest
from pathlib import Path

import _ctx
import load_gen
import test_round_barrier


class KillHarness(unittest.TestCase):
    """A three-tier stub stack under the sleep transport, as rung 0 uses."""

    def setUp(self):
        self.helper = test_round_barrier.TestRoundBarrier("test_the_straggler_actually_straggled")
        self.helper.setUp()
        self.urls = self.helper._stack()
        self.calls = []
        self._real_kill = load_gen.kill_service
        load_gen.kill_service = lambda cfg, rnd: self.calls.append(
            (cfg.kill_service, rnd["round"])) or {"service": cfg.kill_service,
                                                  "round": rnd["round"], "rc": 0}

    def tearDown(self):
        load_gen.kill_service = self._real_kill
        self.helper.tearDown()

    def _run(self, *extra):
        out = Path(tempfile.mkdtemp())
        load_gen.main(["--replay-dir", str(_ctx.FIXTURES), "--load", "smoke",
                       "--out-dir", str(out), "--transport", "sleep",
                       "--offered-scale", "1.0", "--max-rounds", "3",
                       "--run-id", "kill", "--profile", str(self.helper.profile),
                       "--device-url", self.urls["device"], "--edge-url", self.urls["edge"],
                       "--cloud-url", self.urls["cloud"], *extra])
        return json.loads((out / "run_meta.json").read_text())

    def test_kill_fires_once_at_the_declared_round(self):
        meta = self._run("--t-kill", "2")
        self.assertEqual(self.calls, [("cloud", 2)])
        self.assertEqual(meta["t_kill"]["round"], 2)
        self.assertEqual(meta["t_kill"]["service"], "cloud")

    def test_the_kill_precedes_the_round_it_declares(self):
        """Killed after round 2 ran, the overlay would measure nothing in round 2."""
        order = []
        load_gen.kill_service = lambda cfg, rnd: order.append("kill@%d" % rnd["round"]) or {}
        real_round = load_gen.run_round
        load_gen.run_round = lambda cfg, sink, load, rnd: (
            order.append("round@%d" % rnd["round"]) or real_round(cfg, sink, load, rnd))
        try:
            self._run("--t-kill", "2")
        finally:
            load_gen.run_round = real_round
        self.assertEqual(order, ["round@1", "kill@2", "round@2", "round@3"])

    def test_no_kill_without_the_flag(self):
        meta = self._run()
        self.assertEqual(self.calls, [])
        self.assertIsNone(meta["t_kill"])

    def test_per_round_counters_are_recorded(self):
        meta = self._run()
        rounds = meta["rounds"]
        self.assertEqual([r["round"] for r in rounds], [1, 2, 3])
        self.assertEqual([r["issued"] for r in rounds], [2, 2, 1])
        self.assertEqual([r["completed"] for r in rounds], [2, 2, 1])
        self.assertEqual([r["dropped"] for r in rounds], [0, 0, 0])
        for r in rounds:
            self.assertEqual(r["load"], "smoke")
            self.assertGreater(r["wall_s"], 0.0)

    def test_a_failing_tier_shows_up_as_dropped(self):
        """The counter has to move when a tier stops answering, or it says nothing."""
        self.helper.tearDown()
        self.helper.setUp()
        self.urls = self.helper._stack(fail_tier="cloud")
        meta = self._run()
        self.assertEqual([r["completed"] for r in meta["rounds"]], [0, 0, 0])
        self.assertEqual([r["dropped"] for r in meta["rounds"]], [2, 2, 1])


if __name__ == "__main__":
    unittest.main()
