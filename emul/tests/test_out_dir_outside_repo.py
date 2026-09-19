"""run data never lands inside the code repository."""
import tempfile
import unittest
from pathlib import Path

import _ctx
import load_gen


class TestOutDirGuard(unittest.TestCase):
    def test_repo_root_refused(self):
        with self.assertRaises(SystemExit):
            load_gen.resolve_out_dir(str(_ctx.REPO))

    def test_path_inside_repo_refused(self):
        with self.assertRaises(SystemExit) as cm:
            load_gen.resolve_out_dir(str(_ctx.EMUL / "runs"))
        self.assertIn("repository", str(cm.exception))

    def test_relative_path_inside_repo_refused(self):
        import os
        cwd = os.getcwd()
        os.chdir(_ctx.EMUL)
        try:
            with self.assertRaises(SystemExit):
                load_gen.resolve_out_dir("out")
        finally:
            os.chdir(cwd)

    def test_outside_repo_allowed(self):
        with tempfile.TemporaryDirectory() as d:
            self.assertEqual(load_gen.resolve_out_dir(d), Path(d).resolve())

    def test_out_dir_is_required_with_no_default(self):
        parser = load_gen.build_parser()
        with self.assertRaises(SystemExit):
            parser.parse_args(["--replay-dir", str(_ctx.FIXTURES)])
        action = {a.dest: a for a in parser._actions}["out_dir"]
        self.assertIsNone(action.default)


if __name__ == "__main__":
    unittest.main()
