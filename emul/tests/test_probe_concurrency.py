"""What the concurrency probe reads out of its own measurements.

c* and the semaphore caps are fixed from the probe before any calibration
statistic is computed and are never adjusted afterwards, so the arithmetic that
turns per-k latencies into both has to be a tested function rather than a line
in a run script.
"""
import unittest

import _ctx
import probe_concurrency as probe

# The smoke probe's own per-call medians, in ms. The backend serialises: at
# k = 2 a call already costs half again as much as at k = 1.
MEASURED = {"1": {"k": 1, "median_ms": 552.2201061248779},
            "2": {"k": 2, "median_ms": 815.8094882965088},
            "4": {"k": 4, "median_ms": 1209.679365158081},
            "8": {"k": 8, "median_ms": 2184.216856956482}}

CAPACITIES = {"device": 200, "edge": 300, "cloud": 500}


class TestSummarise(unittest.TestCase):
    def test_a_serialising_backend_is_comfortable_at_one_call(self):
        out = probe.summarise(MEASURED)
        self.assertEqual(out["c_star"], 1)
        self.assertAlmostEqual(out["ratio_to_k1"][2], 1.4773266660305615)
        self.assertAlmostEqual(out["ratio_to_k1"][8], 3.9553374329013433)

    def test_keys_survive_a_json_round_trip(self):
        # probe.json comes back with string keys; the summary must not depend
        # on which side of the file it is read from.
        as_ints = {int(k): v for k, v in MEASURED.items()}
        self.assertEqual(probe.summarise(as_ints), probe.summarise(MEASURED))

    def test_a_backend_that_does_not_degrade_is_comfortable_at_the_top_level(self):
        flat = {k: {"k": k, "median_ms": 500.0} for k in (1, 2, 4, 8)}
        self.assertEqual(probe.summarise(flat)["c_star"], 8)

    def test_c_star_is_the_largest_k_still_inside_the_tolerance(self):
        levels = {1: {"median_ms": 500.0}, 2: {"median_ms": 520.0},
                  4: {"median_ms": 560.0}, 8: {"median_ms": 900.0}}
        self.assertEqual(probe.summarise(levels)["c_star"], 4)
        # A stricter tolerance is a different reading of the same measurement.
        self.assertEqual(probe.summarise(levels, tolerance=0.05)["c_star"], 2)

    def test_an_empty_probe_is_not_a_concurrency_of_one_by_accident(self):
        with self.assertRaises(ValueError):
            probe.summarise({})


class TestCaps(unittest.TestCase):
    def test_caps_are_the_simulators_capacity_proportions(self):
        caps = probe.caps_for(1, CAPACITIES)
        self.assertEqual(caps, {"device": 2, "edge": 3, "cloud": 5})
        # The proportions, not the levels, are what the simulator fixes.
        self.assertAlmostEqual(caps["edge"] / caps["device"],
                               CAPACITIES["edge"] / CAPACITIES["device"])
        self.assertAlmostEqual(caps["cloud"] / caps["device"],
                               CAPACITIES["cloud"] / CAPACITIES["device"])

    def test_caps_sum_to_roughly_c_star_once_c_star_is_large_enough(self):
        self.assertEqual(sum(probe.caps_for(10, CAPACITIES).values()), 10)
        self.assertEqual(probe.caps_for(20, CAPACITIES),
                         {"device": 4, "edge": 6, "cloud": 10})

    def test_caps_never_fall_below_the_smallest_integer_realisation(self):
        # c* = 1 cannot be realised in those proportions, so the caps sit above
        # the backend's comfortable concurrency and the binding queue is inside
        # the backend. That is a measurement to report, not a reason to round
        # a tier's cap down to zero.
        for c_star in (0, 1, 4):
            self.assertEqual(probe.caps_for(c_star, CAPACITIES),
                             {"device": 2, "edge": 3, "cloud": 5})

    def test_capacities_that_arrive_as_floats_still_reduce(self):
        caps = probe.caps_for(1, {"device": 200.0, "edge": 300.0, "cloud": 500.0})
        self.assertEqual(caps, {"device": 2, "edge": 3, "cloud": 5})


if __name__ == "__main__":
    unittest.main()
