"""Which admitted tasks a round replays, and which question each instance asks.

Both were prefix choices: the allocation export arrives sorted by descending
surplus, so a prefix of the admitted list is the top-surplus tasks and not a
sample of the round, and a question bound to the instance index means a run with
k < 5 never asks the last of the five measured questions.
"""
import unittest

import _ctx
import load_gen

ROUND_TASKS = [{"task_id": "t%d" % i, "agent_id": "a1", "deadline_ms": d, "value_base": v}
               for i, (d, v) in enumerate(
                   [(1000, 9.0), (1000, 8.0), (750, 7.0), (750, 6.0), (500, 5.0), (500, 4.0)])]


class TestSelectTasks(unittest.TestCase):
    def test_deterministic(self):
        a = load_gen.select_tasks(ROUND_TASKS, 3, 7, "medium", 4)
        b = load_gen.select_tasks(ROUND_TASKS, 3, 7, "medium", 4)
        self.assertEqual([tid for _, tid in a], [tid for _, tid in b])

    def test_subsample_is_a_sample_not_a_prefix(self):
        """Over rounds, every admitted task can be drawn, including the last."""
        drawn = set()
        for rnd in range(1, 21):
            drawn.update(tid for _, tid in load_gen.select_tasks(ROUND_TASKS, 3, 1, "medium", rnd))
        self.assertEqual(drawn, {t["task_id"] for t in ROUND_TASKS})

    def test_deadline_mix_is_not_skewed_to_the_head(self):
        """A prefix would replay only the loosest deadlines; a sample does not."""
        seen = set()
        for rnd in range(1, 21):
            seen.update(t["deadline_ms"] for t, _ in load_gen.select_tasks(ROUND_TASKS, 3, 1, "m", rnd))
        self.assertEqual(seen, {500, 750, 1000})

    def test_subset_size_and_uniqueness(self):
        chosen = load_gen.select_tasks(ROUND_TASKS, 4, 3, "high", 2)
        self.assertEqual(len(chosen), 4)
        self.assertEqual(len({tid for _, tid in chosen}), 4)
        self.assertTrue({tid for _, tid in chosen} <= {t["task_id"] for t in ROUND_TASKS})

    def test_oversample_covers_every_task_then_replicates(self):
        chosen = load_gen.select_tasks(ROUND_TASKS, 8, 3, "high", 2)
        self.assertEqual(len(chosen), 8)
        self.assertEqual(len({tid for _, tid in chosen}), 8)
        self.assertEqual({tid for _, tid in chosen[:6]}, {t["task_id"] for t in ROUND_TASKS})
        self.assertTrue(all("#" in tid for _, tid in chosen[6:]))

    def test_empty_and_zero(self):
        self.assertEqual(load_gen.select_tasks([], 3, 1, "m", 1), [])
        self.assertEqual(load_gen.select_tasks(ROUND_TASKS, 0, 1, "m", 1), [])

    def test_seed_changes_the_draw(self):
        a = {tid for _, tid in load_gen.select_tasks(ROUND_TASKS, 3, 1, "m", 1)}
        b = {tid for _, tid in load_gen.select_tasks(ROUND_TASKS, 3, 99, "m", 1)}
        self.assertNotEqual(a, b)


class TestQuestionChoice(unittest.TestCase):
    def test_all_questions_cycle_across_rounds_at_k_one(self):
        asked = {load_gen.question_for(rnd, 0) for rnd in range(1, 6)}
        self.assertEqual(asked, set(load_gen.QUESTIONS))

    def test_instances_within_a_round_differ(self):
        asked = [load_gen.question_for(3, i) for i in range(3)]
        self.assertEqual(len(set(asked)), 3)

    def test_every_question_is_one_of_the_measured_five(self):
        for rnd in range(1, 11):
            for i in range(4):
                self.assertIn(load_gen.question_for(rnd, i), load_gen.QUESTIONS)


if __name__ == "__main__":
    unittest.main()
