"""Rung 0: the emulated agent sends byte-identical prompts to the released harness.

The load generator duplicates the harness's inline f-strings (it cannot import
them: they are built inside run_one). This test drives the harness's own
run_one with a stub transport, captures the four prompts it emits, and asserts
the generator reproduces them byte for byte. An edit to either side fails here.
"""
import importlib.util
import unittest

import _ctx
import load_gen

QUESTION = "What is the capital of France, and what is its approximate population?"
PLAN_TEXT = "- population of France's capital\nname of France's capital\n"
ANSWERS = ["answer to sub one", "answer to sub two"]


def harness_module():
    path = _ctx.REPO / "agentic" / "run_agent_workload.py"
    spec = importlib.util.spec_from_file_location("harness_under_test", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def harness_prompts(plan_text):
    """Run the harness's run_one against a stub transport; return its prompts."""
    mod = harness_module()
    seen = []

    def stub_call(model, prompt, timeout=120):
        seen.append(prompt)
        if len(seen) == 1:
            return 1.0, 7, plan_text
        return 1.0, 7, ANSWERS[min(len(seen) - 2, len(ANSWERS) - 1)]

    mod.call = stub_call
    mod.run_one("stub-model", QUESTION)
    return seen


class TestPromptsIdentical(unittest.TestCase):
    def _generator_prompts(self, plan_text):
        subs = load_gen.parse_subs(plan_text, QUESTION)
        prompts = [load_gen.plan_prompt(QUESTION)]
        prompts += [load_gen.tool_prompt(s) for s in subs]
        answers = [ANSWERS[min(i, len(ANSWERS) - 1)] for i in range(len(subs))]
        prompts.append(load_gen.aggregate_prompt(QUESTION, answers))
        return prompts

    def test_four_prompts_byte_identical(self):
        expected = harness_prompts(PLAN_TEXT)
        self.assertEqual(len(expected), 4)
        self.assertEqual(self._generator_prompts(PLAN_TEXT), expected)

    def test_pad_path_identical(self):
        """Planner returns one line: both sides pad with the question itself."""
        one_line = "only one sub-question\n"
        expected = harness_prompts(one_line)
        self.assertEqual(len(expected), 4)
        self.assertEqual(self._generator_prompts(one_line), expected)

    def test_question_list_identical(self):
        self.assertEqual(load_gen.QUESTIONS, harness_module().TASKS)

    def test_pad_detected(self):
        self.assertTrue(load_gen.padded("only one sub-question\n"))
        self.assertFalse(load_gen.padded(PLAN_TEXT))


if __name__ == "__main__":
    unittest.main()
