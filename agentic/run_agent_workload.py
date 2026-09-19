#!/usr/bin/env python3
"""Exp.9: instrument a REAL multi-step LLM tool-use agent and emit the
service-dependency DAG + per-stage resource/latency profile it actually
produces. This is the bridge that makes the evaluation agentic rather than a
synthetic relabel: the DAG structure and per-stage demand weights fed to the
simulator come from a measured agent execution, not hand-drawn numbers.

Two tool-using patterns are recorded, selected by --pattern:

    a:  plan (device)   -> {tool0, tool1} (edge, parallel) -> aggregate (cloud)
    b:  plan_b (device) -> {retrieve, tool1} (edge)        -> summary (cloud)
                           retrieve                        -> citations (cloud)

Pattern b reuses pattern a's edge-tier stage `tool1`, so the union of the two
recordings has three leaves whose blocks cross: `tool1` reaches {aggregate,
summary} and `retrieve` reaches {summary, citations}, neither nested in the
other. Whether that crossing appears is decided by the recording, never by this
docstring: the graph block is derived from the parent stage ids each call
records as it is made.

Each stage is a real LLM call against an Ollama daemon; we record wall-clock
latency and token counts (prompt + eval), which become the per-stage compute
demand. Two fingerprints fail closed rather than reporting a measurement from
an instrument that was not what it claimed to be: a response served by another
model aborts the run, and a task whose recorded stage set is not its pattern's
declared set is dropped with a counted reason.

Output: agentic/agentic_profile.json — consumed by the R simulator
(build_dependency_graph("agentic")) so the existing allocation experiments run
on a measured agentic workload.

Usage:  python3 run_agent_workload.py --model mistral:7b-instruct-q4_K_M --n 5
No GPU/cluster: a handful of local calls. Deterministic structure; the numbers
are whatever the real model produces.
"""
import argparse, hashlib, json, time, statistics, sys, urllib.request

DEFAULT_HOST = "http://localhost:11434"
CALLS = []       # per-call stage records, kept for the emulation timeline
DIGEST = hashlib.sha256()

TASKS = [
    "What is the capital of France, and what is its approximate population?",
    "Summarise the cause of ocean tides in two sentences.",
    "List two prime numbers between 20 and 30 and explain why they are prime.",
    "What is the boiling point of water at sea level in C and F?",
    "Name a renewable energy source and one advantage it has.",
]


def endpoint_of(host):
    """Generate endpoint of an Ollama daemon named as host, host:port or a URL."""
    if "://" not in host:
        host = "http://" + host
    return host.rstrip("/") + "/api/generate"


def call(model, prompt, stage, tier, parents, endpoint, timeout=120):
    """One stage call, recorded with the stage ids whose output it consumed.

    The served model is checked against the requested one on every response:
    two daemons answer the same API on different ports, so nothing but the
    response itself can say which engine produced a number.
    """
    body = json.dumps({"model": model, "prompt": prompt, "stream": False}).encode()
    req = urllib.request.Request(endpoint, data=body,
                                 headers={"Content-Type": "application/json"})
    t0 = time.time()
    with urllib.request.urlopen(req, timeout=timeout) as r:
        resp = json.load(r)
    dt_ms = (time.time() - t0) * 1000.0
    served = resp.get("model")
    if served != model:
        raise SystemExit(
            "model fingerprint refusal:\n"
            "  stage %s at %s was served by %r against the requested %r\n"
            "  no profile written; point --host at the daemon holding the "
            "declared model" % (stage, endpoint, served, model))
    text = resp.get("response", "")
    DIGEST.update(text.encode())
    toks = int(resp.get("prompt_eval_count", 0)) + int(resp.get("eval_count", 0))
    CALLS.append({"stage": stage, "parents": list(parents), "tier": tier,
                  "t_start": t0, "latency_ms": dt_ms, "tokens": toks,
                  "model": served})
    return dt_ms, toks, text


def record(stages, name, tier, parents, model, prompt, endpoint):
    """Make one call and file it under its stage name with its parents."""
    dt, tk, text = call(model, prompt, name, tier, parents, endpoint)
    stages[name] = {"latency_ms": dt, "tokens": tk, "tier": tier,
                    "parents": list(parents)}
    return text


def run_one_a(model, question, endpoint):
    """Pattern a: plan -> two parallel tool calls -> aggregate."""
    st = {}
    plan = record(st, "plan", "device", [], model,
                  "You are a planner. For the question: '%s', list exactly two "
                  "short sub-questions to look up, one per line." % question,
                  endpoint)
    subs = [s.strip("- ").strip() for s in plan.splitlines() if s.strip()][:2]
    while len(subs) < 2:
        subs.append(question)
    for i, sub in enumerate(subs):
        subs[i] = record(st, "tool%d" % i, "edge", ["plan"], model,
                         "Answer concisely: %s" % sub, endpoint)
    record(st, "aggregate", "cloud", ["tool0", "tool1"], model,
           "Synthesise a final answer to '" + question + "' from these notes:\n"
           + "\n".join(subs), endpoint)
    return st


def run_one_b(model, question, endpoint):
    """Pattern b: plan -> retrieval beside the shared tool call -> summary, citations.

    `tool1` is pattern a's stage, called with pattern a's prompt shape, so the
    two recordings name one node and not two.
    """
    st = {}
    plan = record(st, "plan_b", "device", [], model,
                  "You are a planner. For the question: '%s', write a short "
                  "retrieval query on the first line and a short tool request "
                  "on the second line." % question, endpoint)
    lines = [s.strip("- ").strip() for s in plan.splitlines() if s.strip()][:2]
    while len(lines) < 2:
        lines.append(question)
    facts = record(st, "retrieve", "edge", ["plan_b"], model,
                   "Retrieve the background facts for: %s" % lines[0], endpoint)
    answer = record(st, "tool1", "edge", ["plan_b"], model,
                    "Answer concisely: %s" % lines[1], endpoint)
    record(st, "summary", "cloud", ["retrieve", "tool1"], model,
           "Synthesise a final answer to '" + question + "' from these notes:\n"
           + facts + "\n" + answer, endpoint)
    record(st, "citations", "cloud", ["retrieve"], model,
           "List two short source references that support these facts:\n" + facts,
           endpoint)
    return st


PATTERNS = {
    "a": (run_one_a, ("plan", "tool0", "tool1", "aggregate")),
    "b": (run_one_b, ("plan_b", "retrieve", "tool1", "summary", "citations")),
}
STRUCTURE = {
    "a": "plan -> 2 parallel tool calls -> aggregate",
    "b": "plan -> retrieval beside the shared tool call -> summary, citations",
}


def keep_runs(runs, declared):
    """Drop tasks whose recorded stage set is not the pattern's declared set.

    A model that emits one sub-question rather than two, or a tool stage that
    returns nothing, produces a task on a different graph from the one the
    profile claims; aggregating it would average two structures into one.

    @return (kept runs, reason -> count of dropped tasks)
    """
    kept, dropped = [], {}
    for r in runs:
        if set(r) == set(declared):
            kept.append(r)
        else:
            why = "stage set {%s} against the declared {%s}" % (
                ",".join(sorted(r)), ",".join(sorted(declared)))
            dropped[why] = dropped.get(why, 0) + 1
    return kept, dropped


def graph_block(runs):
    """The executed dependency graph, derived from the recorded parents.

    Nodes and edges are taken from the call sequence the runs actually made, so
    a pattern that did not execute as written cannot be papered over by a list
    typed beside it. Leaves are the nodes no edge leaves, which is the sink
    set the leaf blocks are taken over. Several patterns' runs concatenated
    give their union graph.
    """
    nodes, edges = {}, []
    for r in runs:
        for name, st in r.items():
            nodes.setdefault(name, st["tier"])
            for p in st["parents"]:
                if (p, name) not in edges:
                    edges.append((p, name))
    sources = {f for f, _ in edges}
    return {"nodes": [{"id": v, "tier": t} for v, t in nodes.items()],
            "edges": [{"from": f, "to": t} for f, t in edges],
            "leaves": [v for v in nodes if v not in sources]}


def with_demand_weights(agg):
    """Normalise per-task demand to weights whose smallest entry is 1."""
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


def aggregate_stages(runs):
    """Per-stage statistics, the node-level counterpart of aggregate_tiers.

    Same convention: tokens summed within a task and averaged over the tasks
    that reached the stage, latency kept per call. The weights are what a node
    demands of one task, which is the vector a leaf token's recipe multiplies.
    """
    agg = {}
    for name in dict.fromkeys(n for r in runs for n in r):
        stages = [r[name] for r in runs if name in r]
        agg[name] = {
            "tier": stages[0]["tier"],
            "mean_tokens": statistics.mean(st["tokens"] for st in stages),
            "mean_tokens_per_task": statistics.mean(st["tokens"] for st in stages),
            "mean_latency_ms": statistics.mean(st["latency_ms"] for st in stages),
            "n_stage_calls": len(stages)}
    return with_demand_weights(agg)


def rederive(profile):
    """Recompute an existing profile's aggregates, leaving every other field.

    A profile that kept its per-stage records is re-aggregated from them, graph
    block included. One that kept only per-tier means is recovered exactly
    anyway: the mean of the per-task totals is the tier's total token count over
    the number of tasks, and that total is a sum of integer token counts, so
    rounding it removes the float error in mean_tokens * n_stage_calls and
    nothing else. Such a profile predates the per-call parents and therefore
    gains no graph block, which is what keeps the fallback in the R reader
    honest rather than silently half-measured.
    """
    if "runs" in profile:
        out = {**profile, "tiers": aggregate_tiers(profile["runs"])}
        if all("parents" in st for r in profile["runs"] for st in r.values()):
            out["graph"] = graph_block(profile["runs"])
            out["aggregate_stages"] = aggregate_stages(profile["runs"])
        return out
    agg = {}
    for name, t in profile["tiers"].items():
        total = round(t["mean_tokens"] * t["n_stage_calls"])
        agg[name] = {k: v for k, v in t.items() if k != "demand_weight"}
        agg[name]["mean_tokens_per_task"] = total / profile["n_tasks"]
    return {**profile, "tiers": with_demand_weights(agg)}


def main():
    global CALLS, DIGEST
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", default="mistral:7b-instruct-q4_K_M")
    ap.add_argument("--n", type=int, default=5)
    ap.add_argument("--pattern", choices=sorted(PATTERNS), default="a")
    ap.add_argument("--host", default=DEFAULT_HOST,
                    help="Ollama daemon to record against. Not hardcoded, so a "
                         "container daemon answering the same API on another "
                         "port cannot stand in for the host one")
    ap.add_argument("--out", default="agentic/agentic_profile.json")
    ap.add_argument("--rederive-from", metavar="JSON",
                    help="recompute the aggregates of an existing profile "
                         "instead of calling the model")
    a = ap.parse_args()

    if a.rederive_from:
        with open(a.rederive_from) as f:
            profile = rederive(json.load(f))
    else:
        CALLS, DIGEST = [], hashlib.sha256()
        run_one, declared = PATTERNS[a.pattern]
        endpoint = endpoint_of(a.host)
        runs = []
        for q in TASKS[:a.n]:
            try:
                runs.append(run_one(a.model, q, endpoint))
            except Exception as e:
                print("warn: task failed: %s" % e, file=sys.stderr)
        runs, dropped = keep_runs(runs, declared)
        if len(runs) < a.n:
            bad = ["%d task(s) recorded %s" % (n, why) for why, n in dropped.items()]
            bad.append("%d of %d requested tasks survived" % (len(runs), a.n))
            raise SystemExit("graph fingerprint refusal:\n  " + "\n  ".join(bad)
                             + "\n  no profile written")

        profile = {
            "model": a.model, "host": a.host, "pattern": a.pattern,
            "n_tasks": len(runs), "runs": runs, "calls": CALLS,
            "structure": STRUCTURE[a.pattern],
            "response_digest": DIGEST.hexdigest(),
            "graph": graph_block(runs),
            "tiers": aggregate_tiers(runs),
            "aggregate_stages": aggregate_stages(runs),
        }
    with open(a.out, "w") as f:
        json.dump(profile, f, indent=2)
    g = profile.get("graph", {})
    print("wrote %s: %d tasks, %d nodes, leaves %s" %
          (a.out, profile["n_tasks"], len(g.get("nodes", [])),
           ", ".join(g.get("leaves", []))))


if __name__ == "__main__":
    main()
