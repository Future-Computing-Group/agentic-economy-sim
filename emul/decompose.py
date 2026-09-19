#!/usr/bin/env python3
"""Wait versus service, per tier, from a run's stage records.

wait_ms is time blocked on the tier's admission semaphore; service_ms is the
backend call itself. The split says where the contention actually binds: at our
admission gate, which is the analogue of the simulator's load-dependent
queueing term, or inside the one inference backend all three tiers share, which
the simulator's load-independent per-tier offset has no room for.

A failed call measures neither: it has no reply to time. Those records carry
None and are counted as outcomes rather than folded into a median, because a
None read as a zero reports an outage as a speed-up.

Usage: python3 decompose.py RUN_DIR
"""
import json
import statistics
import sys
from pathlib import Path

TIERS = ("device", "edge", "cloud")
# The order the simulator declares its load levels in. It orders the report; it
# does not decide which loads the report covers, because a run picks its own two.
LOAD_ORDER = ("low", "medium", "high")


def load_records(path):
    """Stage records, one JSON object per line."""
    text = Path(path).read_text()
    return [json.loads(line) for line in text.splitlines() if line.strip()]


def split_at(records, t_kill):
    """(before, after) the kill. A run with no kill is all before."""
    if t_kill is None:
        return list(records), []
    return ([r for r in records if r["t_send"] < t_kill],
            [r for r in records if r["t_send"] >= t_kill])


def _p95(values):
    """Nearest-rank p95, which is what a handful of records per tier supports."""
    if not values:
        return None
    ordered = sorted(values)
    return ordered[min(len(ordered) - 1, round(0.95 * (len(ordered) - 1)))]


def _summary(records):
    measured = [r for r in records if r.get("error") is None
                and r.get("wait_ms") is not None and r.get("service_ms") is not None]
    if not measured:
        return {"n": 0, "wait_med": None, "wait_p95": None,
                "service_med": None, "service_p95": None, "wait_fraction": None,
                "tokens_med": None}
    wait = [r["wait_ms"] for r in measured]
    service = [r["service_ms"] for r in measured]
    tokens = [r["tokens"] for r in measured if r.get("tokens") is not None]
    w_med, s_med = statistics.median(wait), statistics.median(service)
    return {"n": len(measured), "wait_med": w_med, "wait_p95": _p95(wait),
            "service_med": s_med, "service_p95": _p95(service),
            "wait_fraction": w_med / (w_med + s_med) if (w_med + s_med) else 0.0,
            "tokens_med": statistics.median(tokens) if tokens else None}


def per_tier(records, tiers=TIERS):
    """Per-tier wait and service medians and p95, over the measured calls."""
    return {t: _summary([r for r in records if r["tier"] == t]) for t in tiers}


def per_load(records):
    """The same summary per load, for whatever loads the run actually carried.

    The loads come from the records rather than from a fixed list: a run
    replays two of the simulator's levels and which two is its own choice, so a
    hard-coded pair drops the levels it does not name and invents the ones it
    does. LOAD_ORDER only orders the result.
    """
    seen = {r.get("load") for r in records if r.get("load") is not None}
    ordered = [g for g in LOAD_ORDER if g in seen]
    ordered += sorted(seen - set(ordered))
    return {g: _summary([r for r in records if r.get("load") == g]) for g in ordered}


def outcomes(records, tiers=TIERS):
    """Per tier: calls that answered, calls that failed, and how they failed."""
    out = {}
    for tier in tiers:
        rows = [r for r in records if r["tier"] == tier]
        errs = sorted({str(r["error"]).split(":")[0] for r in rows if r.get("error")})
        out[tier] = {"ok": sum(1 for r in rows if not r.get("error")),
                     "errored": sum(1 for r in rows if r.get("error")),
                     "errors": errs}
    return out


def _fmt(x, places=1):
    return "-" if x is None else "%.*f" % (places, x)


def main(argv=None):
    argv = list(sys.argv[1:] if argv is None else argv)
    if len(argv) != 1:
        print(__doc__.strip().splitlines()[-1], file=sys.stderr)
        return 2
    run_dir = Path(argv[0])
    records = load_records(run_dir / "stage_records.jsonl")
    meta = json.loads((run_dir / "run_meta.json").read_text())
    t_kill = (meta.get("t_kill") or {}).get("t")
    caps = meta.get("tier_caps") or {}
    pre, post = split_at(records, t_kill)

    print("stage records: %d (%d before the kill, %d after)"
          % (len(records), len(pre), len(post)))
    print("\nPre-kill stage calls, per tier (the clean measurement):")
    print("%-7s %-4s %5s %10s %10s %10s %10s %10s"
          % ("tier", "cap", "n", "wait med", "wait p95", "svc med", "svc p95", "wait frac"))
    for tier, row in per_tier(pre).items():
        print("%-7s %-4s %5d %10s %10s %10s %10s %10s"
              % (tier, caps.get(tier, "-"), row["n"], _fmt(row["wait_med"]),
                 _fmt(row["wait_p95"]), _fmt(row["service_med"]),
                 _fmt(row["service_p95"]), _fmt(row["wait_fraction"], 3)))

    print("\nPer load, pre-kill (the load contrast the primary statistics rest on):")
    for load, row in per_load(pre).items():
        print("  %-7s n=%d  wait med %s ms  service med %s ms  tokens med %s"
              % (load, row["n"], _fmt(row["wait_med"]), _fmt(row["service_med"]),
                 _fmt(row["tokens_med"], 0)))

    if post:
        print("\nPost-kill stage calls, by tier and outcome:")
        for tier, row in outcomes(post).items():
            print("  %-7s ok %d, errored %d%s"
                  % (tier, row["ok"], row["errored"],
                     "  (%s)" % ", ".join(row["errors"]) if row["errors"] else ""))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
