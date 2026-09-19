"""Shared test context: put the emulation package dir on sys.path."""
import pathlib
import sys

EMUL = pathlib.Path(__file__).resolve().parents[1]
REPO = EMUL.parent
FIXTURES = pathlib.Path(__file__).resolve().parent / "fixtures"
if str(EMUL) not in sys.path:
    sys.path.insert(0, str(EMUL))
