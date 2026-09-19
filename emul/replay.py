#!/usr/bin/env python3
"""Reader for the simulator's replay export: alloc_<load>.csv and env_<load>.json.

The market clears in the simulator; this module only reads what it decided.
Nothing here re-derives an environment constant, and nothing here admits or
rejects a task.
"""
import csv
import json
from pathlib import Path

ALLOC_COLUMNS = ("round", "task_id", "agent_id", "deadline_ms", "value_base", "admitted")
ENV_KEYS = ("graph_type", "load_level", "capacities", "base_ms", "deadlines")
TIER_KEYS = ("device", "edge", "cloud")
NUMERIC_BLOCKS = ("base_ms", "capacities")
TRUE = {"true", "t", "1", "yes", "y"}
FALSE = {"false", "f", "0", "no", "n"}


class ReplayError(ValueError):
    """The export is not what the simulator is supposed to write."""


def _bool(value, name, row):
    """Read an exported logical, or refuse.

    Anything outside the two sets is an error rather than a false: R renders a
    missing logical as NA, and reading that as "not admitted" would shrink the
    replayed set without a word, which is exactly the failure this module exists
    to make impossible.
    """
    text = "" if value is None else str(value).strip().lower()
    if text in TRUE:
        return True
    if text in FALSE:
        return False
    raise ReplayError("%s row %d: admitted value %r is neither true nor false" % (name, row, value))


def _numeric_block(env, key, name):
    block = env.get(key)
    if not isinstance(block, dict):
        raise ReplayError("%s: %s must be an object keyed by tier, got %r" % (name, key, block))
    for tier in TIER_KEYS:
        value = block.get(tier)
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise ReplayError("%s: %s[%s] must be a number, got %r" % (name, key, tier, value))


def load_rounds(path):
    """Return [{"round": int, "tasks": [admitted task dicts]}] in file order.

    Rows for a round must be contiguous and rounds must not go backwards; both
    would mean the export was concatenated or reordered, which silently changes
    which tasks are replayed together.
    """
    path = Path(path)
    rounds, seen = [], None
    with path.open(newline="") as fh:
        reader = csv.DictReader(fh)
        missing = [c for c in ALLOC_COLUMNS if c not in (reader.fieldnames or [])]
        if missing:
            raise ReplayError("%s is missing column(s): %s" % (path.name, ", ".join(missing)))
        for row in reader:
            rnd = int(row["round"])
            if seen is None or rnd != seen:
                if seen is not None and rnd <= seen:
                    raise ReplayError("%s: round %d follows round %d; rounds must increase"
                                      % (path.name, rnd, seen))
                rounds.append({"round": rnd, "tasks": []})
                seen = rnd
            if _bool(row.get("admitted"), path.name, reader.line_num):
                rounds[-1]["tasks"].append({
                    "task_id": row["task_id"],
                    "agent_id": row["agent_id"],
                    "deadline_ms": int(float(row["deadline_ms"])),
                    "value_base": float(row["value_base"]),
                })
    return rounds


def load_env(path):
    """Return the environment constants the testbed must not re-derive."""
    path = Path(path)
    env = json.loads(path.read_text())
    missing = [k for k in ENV_KEYS if k not in env]
    if missing:
        raise ReplayError("%s is missing key(s): %s" % (path.name, ", ".join(missing)))
    for key in NUMERIC_BLOCKS:
        _numeric_block(env, key, path.name)
    return env


def k_for(n_admitted, offered_scale):
    """Concurrent agent instances for a round of n_admitted tasks."""
    if n_admitted <= 0:
        return 0
    return max(1, round(n_admitted * offered_scale))


def interleave(rounds_by_load, block):
    """[(load, round)] with the loads alternating in blocks of `block` rounds.

    Alternating blocks rather than one load after the other: a laptop that warms
    up or picks up background work over a session would otherwise be free to
    masquerade as a load effect.
    """
    queues = {load: list(rounds) for load, rounds in rounds_by_load.items()}
    order = list(rounds_by_load)
    size = max(1, int(block))
    schedule, i = [], 0
    while any(queues.values()):
        load = order[i % len(order)]
        take, queues[load] = queues[load][:size], queues[load][size:]
        schedule.extend((load, rnd) for rnd in take)
        i += 1
    return schedule
