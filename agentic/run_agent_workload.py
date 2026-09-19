#!/usr/bin/env python3
"""
Exp.9: instrument a REAL multi-step LLM tool-use agent and emit the
service-dependency DAG + per-stage resource/latency profile it actually
produces. This is the bridge that makes the evaluation agentic rather than a
synthetic relabel: the DAG structure and per-tier demand weights fed to the
simulator come from a measured agent execution, not hand-drawn numbers.

The agent workflow is a canonical tool-using pattern:
    plan (device)  ->  {tool_a, tool_b}  (edge, parallel)  ->  aggregate (cloud)
i.e. a series-parallel DAG. Each stage is a real LLM call against a local
Ollama model; we record wall-clock latency and token counts (prompt + eval),
which become the per-stage compute demand.

Output: agentic/agentic_profile.json — consumed by the R simulator
(build_dependency_graph("agentic")) so the existing allocation experiments
(exp4 stability, exp7a DSIC) run on a measured agentic workload.

Usage:  python3 run_agent_workload.py --model mistral:7b-instruct-q4_K_M --n 5
No GPU/cluster: a handful of local calls. Deterministic structure; the numbers
are whatever the real model produces.
"""
import argparse, json, time, statistics, sys, urllib.request

OLLAMA = "http://localhost:11434/api/generate"
CALLS = []  # per-call stage records, kept for the emulation timeline

TASKS = [
    "What is the capital of France, and what is its approximate population?",
    "Summarise the cause of ocean tides in two sentences.",
    "List two prime numbers between 20 and 30 and explain why they are prime.",
    "What is the boiling point of water at sea level in C and F?",
    "Name a renewable energy source and one advantage it has.",
]

def call(model, prompt, timeout=120):
    body = json.dumps({"model": model, "prompt": prompt, "stream": False}).encode()
    req = urllib.request.Request(OLLAMA, data=body,
                                 headers={"Content-Type": "application/json"})
    t0 = time.time()
    with urllib.request.urlopen(req, timeout=timeout) as r:
        resp = json.load(r)
    dt_ms = (time.time() - t0) * 1000.0
    toks = int(resp.get("prompt_eval_count", 0)) + int(resp.get("eval_count", 0))
    CALLS.append({"t_start": t0, "latency_ms": dt_ms, "tokens": toks})
    return dt_ms, toks, resp.get("response", "")

def run_one(model, question):
    """One agentic task = plan -> 2 parallel tools -> aggregate (SP DAG)."""
    stages = {}
    # plan (device-tier: light)
    dt, tk, plan = call(model, f"You are a planner. For the question: '{question}', "
                                "list exactly two short sub-questions to look up, one per line.")
    stages["plan"] = {"latency_ms": dt, "tokens": tk, "tier": "device"}
    subs = [s.strip("- ").strip() for s in plan.splitlines() if s.strip()][:2]
    while len(subs) < 2:
        subs.append(question)
    # tools (edge-tier: parallel branches)
    for i, sub in enumerate(subs):
        dt, tk, ans = call(model, f"Answer concisely: {sub}")
        stages[f"tool_{i}"] = {"latency_ms": dt, "tokens": tk, "tier": "edge"}
        subs[i] = ans
    # aggregate (cloud-tier: heavier synthesis)
    dt, tk, _ = call(model, "Synthesise a final answer to '" + question +
                     "' from these notes:\n" + "\n".join(subs))
    stages["aggregate"] = {"latency_ms": dt, "tokens": tk, "tier": "cloud"}
    return stages

def with_demand_weights(agg):
    """Normalise per-task tier demand to weights whose smallest tier is 1."""
    base = min(t["mean_tokens_per_task"] for t in agg.values())
    return {name: {**t, "demand_weight": round(t["mean_tokens_per_task"] / base, 2)}
            for name, t in agg.items()}

def aggregate_tiers(runs):
    """Per-tier stage statistics of a set of task runs.

    Demand weight is a PER-TASK quantity: the simulator charges a tier its
    weight once per task, and a task visits the edge tier twice because its two
    tool calls are parallel branches of the same task. Tokens are therefore
    summed within a task before being averaged over tasks; a mean over stage
    calls would understate edge demand by the branching factor. Latency stays
    per call, because base latency is a per-stage quantity and parallel branches
    overlap in time rather than adding.
    """
    agg = {}
    for tier in ["device", "edge", "cloud"]:
        per_task, toks, lats = [], [], []
        for r in runs:
            stages = [st for st in r.values() if st["tier"] == tier]
            per_task.append(sum(st["tokens"] for st in stages))
            toks += [st["tokens"] for st in stages]
            lats += [st["latency_ms"] for st in stages]
        agg[tier] = {"mean_tokens": statistics.mean(toks),
                     "mean_tokens_per_task": statistics.mean(per_task),
                     "mean_latency_ms": statistics.mean(lats),
                     "n_stage_calls": len(toks)}
    return with_demand_weights(agg)

def rederive(profile):
    """Recompute an existing profile's tier block, leaving every other field.

    A profile that kept its per-stage records is re-aggregated from them. One
    that kept only per-tier means is recovered exactly anyway: the mean of the
    per-task totals is the tier's total token count over the number of tasks,
    and that total is a sum of integer token counts, so rounding it removes the
    float error in mean_tokens * n_stage_calls and nothing else.
    """
    if "runs" in profile:
        return {**profile, "tiers": aggregate_tiers(profile["runs"])}
    agg = {}
    for name, t in profile["tiers"].items():
        total = round(t["mean_tokens"] * t["n_stage_calls"])
        agg[name] = {k: v for k, v in t.items() if k != "demand_weight"}
        agg[name]["mean_tokens_per_task"] = total / profile["n_tasks"]
    return {**profile, "tiers": with_demand_weights(agg)}

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", default="mistral:7b-instruct-q4_K_M")
    ap.add_argument("--n", type=int, default=5)
    ap.add_argument("--out", default="agentic/agentic_profile.json")
    ap.add_argument("--rederive-from", metavar="JSON",
                    help="recompute the tier block of an existing profile "
                         "instead of calling the model")
    a = ap.parse_args()

    if a.rederive_from:
        with open(a.rederive_from) as f:
            profile = rederive(json.load(f))
    else:
        runs = []
        for q in TASKS[:a.n]:
            try:
                runs.append(run_one(a.model, q))
            except Exception as e:
                print(f"warn: task failed: {e}", file=sys.stderr)
        if not runs:
            print("no successful runs", file=sys.stderr); sys.exit(1)

        profile = {
            "model": a.model, "n_tasks": len(runs),
            "runs": runs, "calls": CALLS,
            "structure": "series-parallel (plan -> 2 parallel tools -> aggregate)",
            "tiers": aggregate_tiers(runs),
        }
    with open(a.out, "w") as f:
        json.dump(profile, f, indent=2)
    print(json.dumps(profile, indent=2))

if __name__ == "__main__":
    main()
