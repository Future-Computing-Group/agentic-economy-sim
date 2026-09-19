#!/usr/bin/env python3
"""What concurrency the shared inference backend serves comfortably, and the
semaphore caps that follow from it.

k simultaneous /api/generate calls at k = 1, 2, 4, 8, one measured plan prompt
each. c* is the largest k whose median per-call latency stays within a
tolerance of the k = 1 median. The tier caps are then the simulator's own
capacity proportions, at the smallest integer realisation whose sum reaches c*.

c* and the caps are fixed from this probe BEFORE any calibration statistic is
computed and are never adjusted afterwards, which is why the arithmetic lives
here with a test rather than in the run script that consumed it.

Usage: python3 probe_concurrency.py OUT.json [--env env_<load>.json]
"""
import argparse
import json
import math
import statistics
import time
import urllib.request
from concurrent.futures import ThreadPoolExecutor

BASE = "http://127.0.0.1:11434"
MODEL = "mistral:7b-instruct-q4_K_M"
LEVELS = (1, 2, 4, 8)
CAPACITIES = {"device": 200, "edge": 300, "cloud": 500}
QUESTION = "What is the capital of France, and what is its approximate population?"
PROMPT = ("You are a planner. For the question: '%s', "
          "list exactly two short sub-questions to look up, one per line." % QUESTION)


def summarise(levels, tolerance=0.2):
    """Per-k latency ratios against k = 1, and c*.

    `levels` maps k (int or the string a JSON round trip leaves behind) to a
    dict carrying median_ms.
    """
    medians = {int(k): float(v["median_ms"]) for k, v in levels.items()}
    if not medians:
        raise ValueError("no probe levels: c* is a measurement, not a default")
    base = medians[min(medians)]
    ratio = {k: medians[k] / base for k in sorted(medians)}
    return {"ratio_to_k1": ratio,
            "c_star": max(k for k in ratio if ratio[k] <= 1 + tolerance)}


def caps_for(c_star, capacities=CAPACITIES):
    """Tier semaphore caps in the simulator's capacity proportions.

    The smallest integer realisation of those proportions is the floor. When
    c* falls below its sum the caps sit above the backend's comfortable
    concurrency, which is a property of the run to report (the binding queue is
    then inside the backend, and the wait/service split measures it) and not a
    reason to round a tier down to nothing.
    """
    units = {t: max(1, int(round(float(v)))) for t, v in capacities.items()}
    step = math.gcd(*units.values())
    smallest = {t: v // step for t, v in units.items()}
    scale = max(1, round(float(c_star) / sum(smallest.values())))
    return {t: v * scale for t, v in smallest.items()}


def one_call(_, base=BASE, model=MODEL, prompt=PROMPT):
    body = json.dumps({"model": model, "prompt": prompt, "stream": False}).encode()
    req = urllib.request.Request(base + "/api/generate", data=body,
                                 headers={"Content-Type": "application/json"})
    t0 = time.time()
    with urllib.request.urlopen(req, timeout=300) as r:
        resp = json.load(r)
    return {"latency_ms": (time.time() - t0) * 1000.0,
            "tokens": int(resp.get("prompt_eval_count", 0)) + int(resp.get("eval_count", 0))}


def probe(base=BASE, model=MODEL, ks=LEVELS):
    """Measure per-call latency at each concurrency level, warm model first."""
    one_call(0, base, model)
    levels = {}
    for k in ks:
        with ThreadPoolExecutor(max_workers=k) as pool:
            t0 = time.time()
            calls = list(pool.map(lambda i: one_call(i, base, model), range(k)))
        wall_ms = (time.time() - t0) * 1000.0
        lat = [c["latency_ms"] for c in calls]
        levels[k] = {"k": k, "n": k, "median_ms": statistics.median(lat),
                     "min_ms": min(lat), "max_ms": max(lat), "wall_ms": wall_ms,
                     "throughput_calls_per_s": k / (wall_ms / 1000.0)}
        print("k=%d  median %.0f ms  min %.0f  max %.0f  wall %.0f ms"
              % (k, levels[k]["median_ms"], min(lat), max(lat), wall_ms), flush=True)
    return levels


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("out", help="where to write the probe record")
    ap.add_argument("--env", default=None,
                    help="env_<load>.json, for the tier capacities the caps mirror")
    ap.add_argument("--base", default=BASE)
    ap.add_argument("--model", default=MODEL)
    ap.add_argument("--tolerance", type=float, default=0.2)
    cfg = ap.parse_args(argv)

    capacities = CAPACITIES
    if cfg.env:
        capacities = json.loads(open(cfg.env).read())["capacities"]

    levels = probe(cfg.base, cfg.model)
    out = {"backend": cfg.base, "model": cfg.model, "levels": levels,
           "tolerance": cfg.tolerance, "capacities": capacities}
    out.update(summarise(levels, cfg.tolerance))
    out["tier_caps"] = caps_for(out["c_star"], capacities)
    print("c* = %d (largest k within %d%% of the k=1 median %.0f ms); caps %s"
          % (out["c_star"], round(100 * cfg.tolerance),
             levels[min(levels)]["median_ms"], out["tier_caps"]))
    with open(cfg.out, "w") as fh:
        json.dump(out, fh, indent=2, default=str)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
