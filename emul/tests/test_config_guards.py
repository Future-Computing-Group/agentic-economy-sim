"""The testbed must emulate the environment it claims to, and say so out loud.

Three configuration guards: the qdisc a container actually installed, the tier
delays against the environment file the simulator exported, and a restart policy
that must not resurrect a container mid-measurement.
"""
import json
import re
import tempfile
import unittest
from pathlib import Path

import _ctx
import load_gen
from test_fingerprint_refusal import HOST_TAGS, MODEL, SENTINEL, stub_server

BASE_MS = {"device": 5, "edge": 15, "cloud": 50}
# What the simulator's environment carries as base_ms: the profile's MEASURED
# per-tier inference latency. The testbed runs that same inference for real, so
# these must never be what the netem delay is asserted against.
PROFILE_MS = {"device": 1354.258, "edge": 1016.295, "cloud": 979.994}


class TestQdiscDelay(unittest.TestCase):
    """What the smoke asserts per container, at the source rather than by timing."""

    def test_reads_the_configured_delay(self):
        line = "qdisc netem 8003: root refcnt 11 limit 1000 delay 5ms  1ms seed 148114"
        self.assertEqual(load_gen.qdisc_delay_ms(line), 5.0)

    def test_distinguishes_one_millisecond_from_five(self):
        line = "qdisc netem 8016: root refcnt 11 limit 1000 delay 1ms seed 4711"
        self.assertEqual(load_gen.qdisc_delay_ms(line), 1.0)

    def test_zero_delay_qdisc_has_no_delay_token(self):
        line = "qdisc netem 8013: root refcnt 11 limit 1000 seed 998877"
        self.assertIsNone(load_gen.qdisc_delay_ms(line))

    def test_no_netem_at_all(self):
        self.assertIsNone(load_gen.qdisc_delay_ms("qdisc noqueue 0: root refcnt 2"))

    def test_units_are_read_not_assumed(self):
        self.assertEqual(load_gen.qdisc_delay_ms("... delay 1s ..."), 1000.0)
        self.assertEqual(load_gen.qdisc_delay_ms("... delay 100us ..."), 0.1)
        self.assertEqual(load_gen.qdisc_delay_ms("... delay 2.5ms ..."), 2.5)


class TestNetemAgainstAssumedNetwork(unittest.TestCase):
    """netem carries the cross-tier NETWORK delay, and is asserted against it.

    The simulator's base_ms is the measured per-tier inference latency. The
    testbed executes that inference on the real model, so real inference already
    supplies the base_ms term; netem must carry only the network component the
    testbed cannot otherwise produce. Asserting the qdisc against base_ms made
    the container emulate the inference time a second time, on the wire.
    """

    def _tiers(self, delays):
        return {t: {"tier": t, "netem_delay_ms": d, "tier_cap": 3} for t, d in delays.items()}

    def test_the_default_assumption_is_the_cross_tier_rtt(self):
        self.assertEqual(load_gen.NETWORK_DELAY_MS, {"device": 5.0, "edge": 15.0, "cloud": 50.0})

    def test_matching_passes(self):
        self.assertTrue(load_gen.check_netem(self._tiers(BASE_MS), BASE_MS))

    def test_a_container_emulating_the_profile_latency_is_refused(self):
        # The regression this assertion exists for: under the old expected
        # source these delays were the passing case.
        with self.assertRaises(SystemExit) as cm:
            load_gen.check_netem(self._tiers(PROFILE_MS), load_gen.NETWORK_DELAY_MS)
        msg = str(cm.exception)
        self.assertIn("device", msg)
        self.assertIn("1354", msg)

    def test_mismatch_refuses_naming_tier_and_both_values(self):
        with self.assertRaises(SystemExit) as cm:
            load_gen.check_netem(self._tiers({"device": 5, "edge": 15, "cloud": 900}), BASE_MS)
        msg = str(cm.exception)
        self.assertIn("cloud", msg)
        self.assertIn("900", msg)
        self.assertIn("50", msg)

    def test_absent_tier_refuses(self):
        with self.assertRaises(SystemExit):
            load_gen.check_netem(self._tiers({"device": 5, "edge": 15}), BASE_MS)


class TestDelaySpec(unittest.TestCase):
    """The assumed delays are a declared run parameter, not a buried constant."""

    def test_parses_a_full_spec(self):
        self.assertEqual(load_gen.parse_delays("device=5,edge=15,cloud=50"), BASE_MS)

    def test_accepts_fractional_values(self):
        self.assertEqual(load_gen.parse_delays("device=2.5,edge=15,cloud=50")["device"], 2.5)

    def test_a_missing_tier_is_refused_rather_than_defaulted(self):
        with self.assertRaises(SystemExit) as cm:
            load_gen.parse_delays("device=5,edge=15")
        self.assertIn("cloud", str(cm.exception))

    def test_an_unknown_tier_is_refused(self):
        with self.assertRaises(SystemExit) as cm:
            load_gen.parse_delays("device=5,edge=15,cloud=50,fog=7")
        self.assertIn("fog", str(cm.exception))

    def test_a_malformed_spec_is_refused(self):
        with self.assertRaises(SystemExit):
            load_gen.parse_delays("device:5")


class TestRefusalIsWired(unittest.TestCase):
    """The guard has to run before the first stage call, like the fingerprint."""

    def setUp(self):
        self.servers = []

    def tearDown(self):
        for s in self.servers:
            s.shutdown()
            s.server_close()

    def _url(self, tier, netem):
        srv, url = stub_server(HOST_TAGS, tier, netem)
        self.servers.append(srv)
        return url

    def test_run_refuses_when_netem_is_not_the_assumed_network_delay(self):
        host = self._url("host", 0)
        out = Path(tempfile.mkdtemp())
        with self.assertRaises(SystemExit) as cm:
            load_gen.main(["--replay-dir", str(_ctx.FIXTURES), "--load", "smoke",
                           "--out-dir", str(out), "--transport", "ollama",
                           "--model", MODEL, "--sentinel-tag", SENTINEL,
                           "--host-ollama", host, "--max-rounds", "1",
                           "--device-url", self._url("device", 900),
                           "--edge-url", self._url("edge", 1000),
                           "--cloud-url", self._url("cloud", 1100)])
        self.assertIn("netem", str(cm.exception))
        self.assertFalse((out / "stage_records.jsonl").exists())


class TestRestartPolicy(unittest.TestCase):
    """A container that dies mid-run must stay dead, not come back with a fresh queue."""

    def _restart_values(self, name):
        text = (_ctx.EMUL / name).read_text()
        return re.findall(r"^\s*restart:\s*(\S+)", text, flags=re.M)

    def test_base_stack_declares_no_restart_policy(self):
        self.assertEqual(self._restart_values("docker-compose.yaml"), [])

    def test_failure_overlay_pins_no(self):
        values = self._restart_values("docker-compose.failure.yaml")
        self.assertEqual(len(values), 3)
        self.assertTrue(all(v == '"no"' for v in values), values)


if __name__ == "__main__":
    unittest.main()
