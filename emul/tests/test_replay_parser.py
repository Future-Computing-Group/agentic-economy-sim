"""the replay parser reads the simulator's export and nothing else."""
import json
import tempfile
import unittest
from pathlib import Path

import _ctx
import replay


class TestAllocParser(unittest.TestCase):
    def test_rounds_in_order_admitted_only(self):
        rounds = replay.load_rounds(_ctx.FIXTURES / "alloc_fixture.csv")
        self.assertEqual([r["round"] for r in rounds], [1, 2])
        self.assertEqual([len(r["tasks"]) for r in rounds], [3, 1])
        self.assertEqual([t["task_id"] for t in rounds[0]["tasks"]], ["t1", "t2", "t3"])
        self.assertEqual([t["task_id"] for t in rounds[1]["tasks"]], ["t5"])

    def test_typed_columns(self):
        task = replay.load_rounds(_ctx.FIXTURES / "alloc_fixture.csv")[0]["tasks"][0]
        self.assertEqual(task["deadline_ms"], 500)
        self.assertIsInstance(task["deadline_ms"], int)
        self.assertAlmostEqual(task["value_base"], 12.5)
        self.assertEqual(task["agent_id"], "a1")

    def test_k_after_offered_scale(self):
        rounds = replay.load_rounds(_ctx.FIXTURES / "alloc_fixture.csv")
        a = [len(r["tasks"]) for r in rounds]
        self.assertEqual(replay.k_for(a[0], 1.0), 3)
        self.assertEqual(replay.k_for(a[0], 0.5), 2)   # round(1.5)
        self.assertEqual(replay.k_for(a[1], 0.5), 1)   # clamped up from round(0.5)
        self.assertEqual(replay.k_for(0, 1.0), 0)      # an empty round runs nothing

    def test_missing_column_raises(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "alloc_bad.csv"
            p.write_text("round,task_id,agent_id,value_base,admitted\n1,t1,a1,1.0,TRUE\n")
            with self.assertRaises(replay.ReplayError) as cm:
                replay.load_rounds(p)
            self.assertIn("deadline_ms", str(cm.exception))

    def test_non_monotone_round_raises(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "alloc_bad.csv"
            rows = _ctx.FIXTURES.joinpath("alloc_fixture.csv").read_text().splitlines()
            p.write_text("\n".join(rows + ["1,t7,a1,500,1.0,TRUE"]) + "\n")
            with self.assertRaises(replay.ReplayError) as cm:
                replay.load_rounds(p)
            self.assertIn("round", str(cm.exception))


class TestEnvParser(unittest.TestCase):
    def test_loads_constants(self):
        env = replay.load_env(_ctx.FIXTURES / "env_fixture.json")
        self.assertEqual(env["base_ms"]["cloud"], 50)
        self.assertEqual(env["capacities"]["edge"], 300)
        self.assertEqual(env["deadlines"], [500, 750, 1000])

    def test_missing_key_raises(self):
        with tempfile.TemporaryDirectory() as d:
            env = json.loads(_ctx.FIXTURES.joinpath("env_fixture.json").read_text())
            del env["base_ms"]
            p = Path(d) / "env_bad.json"
            p.write_text(json.dumps(env))
            with self.assertRaises(replay.ReplayError) as cm:
                replay.load_env(p)
            self.assertIn("base_ms", str(cm.exception))


class TestSchedule(unittest.TestCase):
    def test_single_load_is_file_order(self):
        rounds = replay.load_rounds(_ctx.FIXTURES / "alloc_fixture.csv")
        sched = replay.interleave({"medium": rounds}, 10)
        self.assertEqual([(ld, r["round"]) for ld, r in sched], [("medium", 1), ("medium", 2)])

    def test_loads_alternate_in_blocks(self):
        med = [{"round": i, "tasks": []} for i in range(1, 6)]
        high = [{"round": i, "tasks": []} for i in range(1, 4)]
        sched = replay.interleave({"medium": med, "high": high}, 2)
        self.assertEqual([(ld, r["round"]) for ld, r in sched],
                         [("medium", 1), ("medium", 2), ("high", 1), ("high", 2),
                          ("medium", 3), ("medium", 4), ("high", 3), ("medium", 5)])

class TestAdmittedEncoding(unittest.TestCase):
    """The market decided; a row we cannot read is an error, never a silent drop."""

    def _write(self, tmp, admitted_field):
        p = Path(tmp) / "alloc_x.csv"
        p.write_text("round,task_id,agent_id,deadline_ms,value_base,admitted\n"
                     "1,t1,a1,500,1.0,%s\n" % admitted_field)
        return p

    def test_true_and_false_spellings_accepted(self):
        with tempfile.TemporaryDirectory() as d:
            for yes in ("TRUE", "true", "T", "1", "yes", "True"):
                self.assertEqual(len(replay.load_rounds(self._write(d, yes))[0]["tasks"]), 1, yes)
            for no in ("FALSE", "false", "F", "0", "no", "False"):
                self.assertEqual(len(replay.load_rounds(self._write(d, no))[0]["tasks"]), 0, no)

    def test_unreadable_value_raises_naming_file_row_and_value(self):
        with tempfile.TemporaryDirectory() as d:
            for bad in ("MAYBE", "NA", "", "NULL"):
                with self.assertRaises(replay.ReplayError) as cm:
                    replay.load_rounds(self._write(d, bad))
                msg = str(cm.exception)
                self.assertIn("alloc_x.csv", msg)
                self.assertIn("row 2", msg)
                self.assertIn(repr(bad), msg)

    def test_short_row_missing_the_field_raises(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "alloc_short.csv"
            p.write_text("round,task_id,agent_id,deadline_ms,value_base,admitted\n1,t1,a1,500,1.0\n")
            with self.assertRaises(replay.ReplayError) as cm:
                replay.load_rounds(p)
            self.assertIn("row 2", str(cm.exception))


class TestEnvTypes(unittest.TestCase):
    """base_ms is the field the testbed is forbidden to assume; it must be numbers."""

    def _env(self, tmp, **override):
        env = json.loads(_ctx.FIXTURES.joinpath("env_fixture.json").read_text())
        env.update(override)
        p = Path(tmp) / "env_x.json"
        p.write_text(json.dumps(env))
        return p

    def test_string_valued_base_ms_raises(self):
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaises(replay.ReplayError) as cm:
                replay.load_env(self._env(d, base_ms={"device": "5", "edge": 15, "cloud": 50}))
            self.assertIn("base_ms", str(cm.exception))
            self.assertIn("device", str(cm.exception))

    def test_missing_tier_raises(self):
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaises(replay.ReplayError) as cm:
                replay.load_env(self._env(d, capacities={"device": 200, "edge": 300}))
            self.assertIn("capacities", str(cm.exception))
            self.assertIn("cloud", str(cm.exception))

    def test_null_block_raises(self):
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaises(replay.ReplayError) as cm:
                replay.load_env(self._env(d, capacities=None))
            self.assertIn("capacities", str(cm.exception))

    def test_bool_is_not_a_number(self):
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaises(replay.ReplayError):
                replay.load_env(self._env(d, base_ms={"device": True, "edge": 15, "cloud": 50}))


if __name__ == "__main__":
    unittest.main()
