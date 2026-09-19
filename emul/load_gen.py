#!/usr/bin/env python3
"""Replay the market's admitted tasks on the tier containers and record what happens.

The simulator decides, the testbed executes, neither re-decides: rounds and
tasks come from the exported allocation file, and this generator runs the same
series-parallel agent call graph the released harness runs (plan on device, two
parallel tools on edge, aggregate on cloud), one POST /stage per call.

Run data never lands in this repository: --out-dir is required, has no default,
and is refused if it resolves inside the git tree.

Usage:
  python3 load_gen.py --replay-dir DIR --load medium --load high \
      --out-dir /path/outside/the/repo/<run-id> --offered-scale 0.1
"""
import argparse
import csv
import json
import platform
import random
import re
import subprocess
import threading
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import replay

HERE = Path(__file__).resolve().parent
TIERS = ("device", "edge", "cloud")

# The emulated CROSS-TIER NETWORK delay per tier, in ms, and the only thing
# netem carries.
#
# The simulator's env base_ms is the profile's measured per-tier INFERENCE
# latency. The testbed executes that same inference, on the same model, for
# real: real inference already supplies the base_ms term. Emulating base_ms on
# the wire as well made every stage pay its tier's inference time a second time,
# which is what put the testbed's medians several times above the simulator's.
# What the testbed cannot otherwise produce is the network hop between tiers, so
# that is what the qdisc emulates and what check_netem asserts against. These
# are assumed values, not measurements of any real path, and they are recorded
# in run_meta.json as such.
NETWORK_DELAY_MS = {"device": 5.0, "edge": 15.0, "cloud": 50.0}
DELAY_UNITS = {"us": 0.001, "ms": 1.0, "s": 1000.0}
STAGE_FIELDS = ("run_id", "round", "task_id", "stage", "tier", "t_send", "wait_ms",
                "service_ms", "netem_delay_ms", "tokens", "container", "error", "load")
TASK_FIELDS = ("run_id", "round", "task_id", "deadline_ms", "e2e_ms", "completed",
               "met_deadline", "error", "load")

# Duplicated from the released agent harness, which builds them as inline
# f-strings inside run_one and cannot export them. test_prompts_identical.py
# drives the harness's own run_one and asserts byte identity, so an edit to
# either side fails there rather than drifting silently.
QUESTIONS = [
    "What is the capital of France, and what is its approximate population?",
    "Summarise the cause of ocean tides in two sentences.",
    "List two prime numbers between 20 and 30 and explain why they are prime.",
    "What is the boiling point of water at sea level in C and F?",
    "Name a renewable energy source and one advantage it has.",
]


def plan_prompt(question):
    return (f"You are a planner. For the question: '{question}', "
            "list exactly two short sub-questions to look up, one per line.")


def tool_prompt(sub):
    return f"Answer concisely: {sub}"


def aggregate_prompt(question, subs):
    return ("Synthesise a final answer to '" + question +
            "' from these notes:\n" + "\n".join(subs))


def parse_subs(plan_text, question):
    subs = [s.strip("- ").strip() for s in plan_text.splitlines() if s.strip()][:2]
    while len(subs) < 2:
        subs.append(question)
    return subs


def question_for(rnd, i):
    """The question instance i of round rnd asks.

    Offset by the round so that all five measured questions cycle: bound to the
    instance index alone, a run with k below five would only ever exercise a
    prefix of the token mix the profile was measured over.
    """
    return QUESTIONS[(rnd + i) % len(QUESTIONS)]


def select_tasks(tasks, k, seed, load, rnd):
    """Deterministically sample k of a round's admitted tasks.

    A prefix is not a sample: the allocation arrives sorted by descending
    surplus, so taking the head replays the highest-surplus tasks, whose
    deadlines are systematically looser than the round's. The seed is recorded
    in the run metadata so the simulator side can be restricted to the same
    tasks rather than only to the same rounds.
    """
    if k <= 0 or not tasks:
        return []
    rng = random.Random("%s:%s:%s" % (seed, load, rnd))
    chosen = [(t, t["task_id"]) for t in rng.sample(tasks, min(k, len(tasks)))]
    for i in range(len(tasks), k):          # k above the admitted count: replicas
        task = rng.choice(tasks)
        chosen.append((task, "%s#%d" % (task["task_id"], i)))
    return chosen


def padded(plan_text):
    """True when the planner returned fewer than two lines and the pad fired."""
    return len([s for s in plan_text.splitlines() if s.strip()]) < 2


def _get_json(url, timeout=10):
    with urllib.request.urlopen(url, timeout=timeout) as r:
        return json.load(r)


def _post_json(url, payload, timeout):
    raw = json.dumps(payload).encode()
    req = urllib.request.Request(url, data=raw, headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return json.load(r)
    except urllib.error.HTTPError as exc:
        # The server's answer to a failed stage carries its diagnosis and the
        # wait/service split it measured before failing. A 5xx must not discard
        # the one record where that attribution matters most.
        try:
            return json.load(exc)
        except ValueError:
            raise exc


def _tag_present(tags, want):
    """Exact match on a qualified tag, family match on an unqualified one."""
    if ":" in want:
        return want in tags
    return any(t.split(":")[0] == want for t in tags)


def probe_containers(tier_urls, timeout=10):
    return {tier: _get_json(url.rstrip("/") + "/fingerprint", timeout)
            for tier, url in sorted(tier_urls.items())}


def check_fingerprint(tier_urls, host_ollama, model, sentinel):
    """Refuse the run unless every container resolves the same backend as the host.

    A backend published from a container answers on the same name and port as a
    host daemon, so "is it up" is not the question; "is it the same store" is.
    The sentinel is a tag that only the host store holds, and it is a parameter
    because store contents change.
    """
    host_tags = sorted(m["name"] for m in
                       _get_json(host_ollama.rstrip("/") + "/api/tags").get("models", []))
    tiers = probe_containers(tier_urls)
    bad = []
    if not _tag_present(host_tags, sentinel):
        bad.append("host %s does not hold the sentinel tag %s; pick a sentinel that is "
                   "actually host-only" % (host_ollama, sentinel))
    for tier, fp in tiers.items():
        tags = fp.get("models") or []
        if fp.get("error"):
            bad.append("%s: backend %s unreachable: %s" % (tier, fp.get("ollama_base"), fp["error"]))
        if not _tag_present(tags, model):
            bad.append("%s: model %s absent from %s" % (tier, model, fp.get("ollama_base")))
        if not _tag_present(tags, sentinel):
            bad.append("%s: host-only sentinel %s absent from %s; this container is talking to "
                       "a different backend than the host" % (tier, sentinel, fp.get("ollama_base")))
    block = {"host": {"ollama_base": host_ollama, "models": host_tags},
             "tiers": tiers, "model": model, "sentinel": sentinel,
             "sentinel_ok": not bad, "enforced": True}
    if bad:
        raise SystemExit("fingerprint refusal:\n  " + "\n  ".join(bad))
    return block


def qdisc_delay_ms(line):
    """The delay a `tc qdisc show` line actually carries, in ms, or None.

    tc renders large values in seconds and small ones in microseconds, so the
    unit is read rather than assumed.
    """
    m = re.search(r"\bdelay\s+([0-9.]+)\s*(us|ms|s)\b", line)
    return float(m.group(1)) * DELAY_UNITS[m.group(2)] if m else None


def parse_delays(text):
    """Read "device=5,edge=15,cloud=50" into {tier: ms}, or refuse.

    Every tier must be named. A tier left out would otherwise silently inherit
    a default, and the whole point of the parameter is that the assumed network
    delays are declared by the run rather than buried in a constant.
    """
    out = {}
    for part in [p for p in str(text).split(",") if p.strip()]:
        if "=" not in part:
            raise SystemExit("bad --network-delay-ms %r: expected tier=ms, got %r"
                             % (text, part))
        tier, _, value = part.partition("=")
        tier = tier.strip()
        if tier not in TIERS:
            raise SystemExit("bad --network-delay-ms %r: unknown tier %r; expected %s"
                             % (text, tier, ", ".join(TIERS)))
        try:
            out[tier] = float(value)
        except ValueError:
            raise SystemExit("bad --network-delay-ms %r: %r is not a number" % (text, value))
    missing = [t for t in TIERS if t not in out]
    if missing:
        raise SystemExit("bad --network-delay-ms %r: no delay for %s"
                         % (text, ", ".join(missing)))
    return out


def check_netem(tiers, network_ms):
    """Refuse the run unless each container emulates the assumed network delay.

    The delays are compose environment overrides, and this is what keeps them
    the run's declared cross-tier delays rather than defaults that happen to
    agree with them today. The expected value is the network assumption, never
    the environment's base_ms: base_ms is inference time and the testbed runs
    the inference for real.
    """
    bad = []
    for tier in TIERS:
        configured = (tiers.get(tier) or {}).get("netem_delay_ms")
        declared = (network_ms or {}).get(tier)
        if configured is None or declared is None or float(configured) != float(declared):
            bad.append("%s: container emulates %s ms against the assumed cross-tier "
                       "delay %s ms" % (tier, configured, declared))
    if bad:
        raise SystemExit("netem refusal:\n  " + "\n  ".join(bad) +
                         "\n  bring the stack up with DEVICE_DELAY_MS / EDGE_DELAY_MS / "
                         "CLOUD_DELAY_MS set to the assumed cross-tier delays")
    return True


def _git_root(start):
    for p in [start, *start.parents]:
        if (p / ".git").exists():
            return p
    return None


def resolve_out_dir(path):
    """Run data lives outside the code repository, with no in-repo default."""
    out = Path(path).expanduser().resolve()
    root = _git_root(HERE)
    if root is not None and (out == root or root in out.parents):
        raise SystemExit("refusing --out-dir %s: it is inside the code repository at %s; "
                         "run data belongs outside the repository" % (out, root))
    return out


class Sink:
    """Thread-safe writer for the two record streams."""

    def __init__(self, out_dir, run_id):
        self.run_id = run_id
        self._lock = threading.Lock()
        self._stages = (out_dir / "stage_records.jsonl").open("w")
        self._tasks_fh = (out_dir / "task_records.csv").open("w", newline="")
        self._tasks = csv.DictWriter(self._tasks_fh, fieldnames=TASK_FIELDS)
        self._tasks.writeheader()
        self.n_stages = 0
        self.n_tasks = 0

    def stage(self, rec):
        with self._lock:
            self._stages.write(json.dumps(rec) + "\n")
            self.n_stages += 1

    def task(self, rec):
        with self._lock:
            self._tasks.writerow(rec)
            self.n_tasks += 1

    def close(self):
        self._stages.close()
        self._tasks_fh.close()


def stage_call(cfg, sink, load, rnd, task_id, stage, tier, prompt):
    url = cfg.tier_urls[tier].rstrip("/") + "/stage"
    payload = {"task_id": task_id, "round": rnd, "stage": stage, "prompt": prompt,
               "transport": cfg.transport, "sleep_ms": cfg.sleep_ms[tier]}
    t_send = time.time()
    try:
        resp = _post_json(url, payload, cfg.timeout)
        err = resp.get("error")
    except Exception as exc:
        resp, err = {}, "%s: %s" % (type(exc).__name__, exc)
    sink.stage({"run_id": sink.run_id, "round": rnd, "task_id": task_id, "stage": stage,
                "tier": tier, "t_send": t_send, "wait_ms": resp.get("wait_ms"),
                "service_ms": resp.get("service_ms"),
                "netem_delay_ms": resp.get("netem_delay_ms"), "tokens": resp.get("tokens"),
                "container": resp.get("container") or tier, "error": err, "load": load})
    return err, resp.get("text", "")


def run_task(cfg, sink, load, rnd, task_id, deadline_ms, question):
    """One replayed task: plan -> two parallel tools -> aggregate, over real hops."""
    t0 = time.time()
    err, plan_text = stage_call(cfg, sink, load, rnd, task_id, "plan", "device",
                                plan_prompt(question))
    pad = padded(plan_text)
    if err is None:
        subs = parse_subs(plan_text, question)
        with ThreadPoolExecutor(max_workers=len(subs)) as pool:
            futures = [pool.submit(stage_call, cfg, sink, load, rnd, task_id,
                                   "tool_%d" % i, "edge", tool_prompt(sub))
                       for i, sub in enumerate(subs)]
            results = [f.result() for f in futures]
        err = next((e for e, _ in results if e is not None), None)
        if err is None:
            err, _ = stage_call(cfg, sink, load, rnd, task_id, "aggregate", "cloud",
                                aggregate_prompt(question, [t for _, t in results]))
    e2e_ms = (time.time() - t0) * 1000.0
    completed, met = err is None, err is None and e2e_ms <= deadline_ms
    sink.task({"run_id": sink.run_id, "round": rnd, "task_id": task_id,
               "deadline_ms": deadline_ms, "e2e_ms": round(e2e_ms, 3),
               "completed": completed, "met_deadline": met,
               "error": err, "load": load})
    return {"pad": pad, "completed": completed, "met_deadline": met}


def run_round(cfg, sink, load, rnd):
    """One round: k simultaneous agent instances, then a barrier before the next.

    Returns the round's own counters. They are what a run in progress can be
    read by -- a tier lost mid-run shows up here as completions going to zero
    while tasks keep being issued -- and post-hoc reconstruction from the record
    files cannot show it as it happens.
    """
    tasks = rnd["tasks"]
    k = replay.k_for(len(tasks), cfg.offered_scale)
    chosen = select_tasks(tasks, k, cfg.subsample_seed, load, rnd["round"])
    counts = {"load": load, "round": rnd["round"], "k": k, "admitted": len(tasks),
              "issued": len(chosen), "completed": 0, "dropped": 0, "missed_deadline": 0,
              "pads": 0, "wall_s": 0.0, "task_ids": [tid for _, tid in chosen]}
    if not chosen:
        return counts
    started = time.time()
    with ThreadPoolExecutor(max_workers=len(chosen)) as pool:
        futures = [pool.submit(run_task, cfg, sink, load, rnd["round"], task_id,
                               task["deadline_ms"], question_for(rnd["round"], i))
                   for i, (task, task_id) in enumerate(chosen)]
        outcomes = [f.result() for f in futures]
    # The pool's context manager joins every instance: that join is the round barrier.
    counts.update(
        completed=sum(1 for o in outcomes if o["completed"]),
        dropped=sum(1 for o in outcomes if not o["completed"]),
        missed_deadline=sum(1 for o in outcomes if o["completed"] and not o["met_deadline"]),
        pads=sum(1 for o in outcomes if o["pad"]),
        wall_s=round(time.time() - started, 3))
    return counts


def kill_service(cfg, rnd):
    """Permanent loss of one tier, at the declared round.

    The failure overlay is passed alongside the base file so the kill lands on a
    stack whose restart policy is pinned to "no": a tier that came back with a
    fresh semaphore, and nothing in the records to show it had gone, would make
    the rest of the run unreadable.
    """
    cmd = ["docker", "compose", "-f", str(HERE / "docker-compose.yaml"),
           "-f", str(HERE / "docker-compose.failure.yaml"), "kill", cfg.kill_service]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    return {"service": cfg.kill_service, "round": rnd["round"], "t": time.time(),
            "rc": proc.returncode, "cmd": " ".join(cmd),
            "stderr": (proc.stderr or "").strip() or None}


def _sh(cmd):
    """Best-effort provenance; a missing tool is recorded as absent, not fatal."""
    try:
        return subprocess.run(cmd, capture_output=True, text=True, timeout=20).stdout.strip() or None
    except Exception:
        return None


def build_parser():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--replay-dir", required=True,
                    help="directory holding alloc_<load>.csv and env_<load>.json")
    ap.add_argument("--load", action="append", default=None,
                    help="load level to replay; repeatable (default: medium)")
    ap.add_argument("--out-dir", required=True, default=None,
                    help="run-data directory; must resolve outside this repository")
    ap.add_argument("--offered-scale", type=float, default=1.0)
    ap.add_argument("--network-delay-ms",
                    default=",".join("%s=%g" % (t, NETWORK_DELAY_MS[t]) for t in TIERS),
                    help="assumed cross-tier network delay per tier; netem carries this "
                         "and nothing else, because real inference supplies base_ms")
    ap.add_argument("--subsample-seed", type=int, default=1,
                    help="seed for the per-round subsample; recorded in the run metadata")
    ap.add_argument("--transport", choices=("ollama", "sleep"), default="ollama")
    ap.add_argument("--t-kill", type=int, default=0,
                    help="kill the failure tier before the first round at or past this "
                         "round index; 0 runs without a failure")
    ap.add_argument("--kill-service", default="cloud",
                    help="compose service the failure overlay takes down")
    ap.add_argument("--block-rounds", type=int, default=10,
                    help="alternate loads every N rounds")
    ap.add_argument("--max-rounds", type=int, default=0, help="0 replays every round")
    ap.add_argument("--model", default="mistral:7b-instruct-q4_K_M")
    ap.add_argument("--sentinel-tag", default="command-r7b",
                    help="tag held only by the host model store")
    ap.add_argument("--host-ollama", default="http://127.0.0.1:11434")
    ap.add_argument("--device-url", default="http://127.0.0.1:8101")
    ap.add_argument("--edge-url", default="http://127.0.0.1:8102")
    ap.add_argument("--cloud-url", default="http://127.0.0.1:8103")
    ap.add_argument("--profile", default=str(HERE.parent / "agentic" / "agentic_profile.json"))
    ap.add_argument("--timeout", type=float, default=180.0)
    ap.add_argument("--run-id", default=None)
    ap.add_argument("--image-ref", default="rtse-emul-tier")
    return ap


def main(argv=None):
    cfg = build_parser().parse_args(argv)
    out_dir = resolve_out_dir(cfg.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    cfg.run_id = cfg.run_id or time.strftime("%H%M%S", time.gmtime())
    cfg.tier_urls = {"device": cfg.device_url, "edge": cfg.edge_url, "cloud": cfg.cloud_url}
    loads = cfg.load or ["medium"]

    profile = json.loads(Path(cfg.profile).read_text())
    cfg.sleep_ms = {t: profile["tiers"][t]["mean_latency_ms"] for t in TIERS}

    replay_dir = Path(cfg.replay_dir)
    envs = {ld: replay.load_env(replay_dir / ("env_%s.json" % ld)) for ld in loads}
    rounds = {ld: replay.load_rounds(replay_dir / ("alloc_%s.csv" % ld)) for ld in loads}
    if cfg.max_rounds > 0:
        rounds = {ld: rs[:cfg.max_rounds] for ld, rs in rounds.items()}

    network_ms = parse_delays(cfg.network_delay_ms)
    if cfg.transport == "ollama":
        fingerprint = check_fingerprint(cfg.tier_urls, cfg.host_ollama, cfg.model,
                                        cfg.sentinel_tag)
        check_netem(fingerprint["tiers"], network_ms)
    else:
        # No model is called under the sleep transport, so there is no measurement
        # for a wrong backend to corrupt; the containers are still probed and the
        # resolved backends recorded, only the refusal is not armed.
        fingerprint = {"enforced": False, "reason": "sleep transport calls no backend",
                       "tiers": probe_containers(cfg.tier_urls)}

    sink = Sink(out_dir, cfg.run_id)
    started = time.time()
    k_seen = {ld: [] for ld in loads}
    replayed = {ld: {} for ld in loads}
    pads = 0
    killed, per_round = None, []
    for load, rnd in replay.interleave(rounds, cfg.block_rounds):
        # Before the round, not after it: killed afterwards, the declared round
        # would be the last clean one rather than the first degraded one.
        if cfg.t_kill and killed is None and rnd["round"] >= cfg.t_kill:
            killed = kill_service(cfg, rnd)
            print("killed %s before round %d: rc=%s"
                  % (cfg.kill_service, rnd["round"], killed.get("rc")), flush=True)
        counts = run_round(cfg, sink, load, rnd)
        k_seen[load].append(counts["k"])
        replayed[load][rnd["round"]] = counts.pop("task_ids")
        pads += counts["pads"]
        per_round.append(counts)
        print("round %s/%d  k=%d  issued=%d  completed=%d  dropped=%d  missed=%d  "
              "wall=%.1fs  (tasks=%d stages=%d)"
              % (load, rnd["round"], counts["k"], counts["issued"], counts["completed"],
                 counts["dropped"], counts["missed_deadline"], counts["wall_s"],
                 sink.n_tasks, sink.n_stages), flush=True)
    sink.close()

    caps = {t: fp.get("tier_cap") for t, fp in (fingerprint.get("tiers") or {}).items()}
    netem = {t: fp.get("netem_delay_ms") for t, fp in (fingerprint.get("tiers") or {}).items()}
    meta = {
        "run_id": cfg.run_id,
        "transport": cfg.transport,
        "offered_scale": cfg.offered_scale,
        "subsample_seed": cfg.subsample_seed,
        "replayed_task_ids": replayed,
        "block_rounds": cfg.block_rounds,
        "loads": {ld: {"rounds": len(rounds[ld]),
                       "k_min": min(k_seen[ld], default=0),
                       "k_max": max(k_seen[ld], default=0),
                       "k_total": sum(k_seen[ld])} for ld in loads},
        "fingerprint": fingerprint,
        "tier_caps": caps,
        "cap_proportions": ([round(caps[t] / min(v for v in caps.values() if v), 3)
                             for t in TIERS if caps.get(t)]
                            if caps and all(caps.get(t) for t in TIERS) else None),
        "netem_delay_ms": netem,
        # Assumed, not measured: these are the cross-tier network delays the run
        # declares, and they are not a measurement of any real network path.
        "assumed_network_delay_ms": network_ms,
        "env": envs,
        "netem_matches_assumed_network_ms": all(
            netem.get(t) == network_ms.get(t) for t in TIERS),
        "pad_count": pads,
        "pad_rate": round(pads / sink.n_tasks, 4) if sink.n_tasks else None,
        # The sleep transport returns no plan text, so the pad always fires and
        # the rate says nothing about the workload.
        "pad_rate_meaningful": cfg.transport == "ollama",
        "n_tasks": sink.n_tasks,
        "n_stage_records": sink.n_stages,
        "rounds": per_round,
        "t_kill": killed,
        "image_id": _sh(["docker", "image", "inspect", cfg.image_ref, "--format", "{{.Id}}"]),
        "emul_git_sha": _sh(["git", "-C", str(HERE), "rev-parse", "HEAD"]),
        "host": {"platform": platform.platform(), "machine": platform.machine(),
                 "python": platform.python_version()},
        "wall_s": round(time.time() - started, 3),
    }
    (out_dir / "run_meta.json").write_text(json.dumps(meta, indent=2) + "\n")
    print("wrote %d stage records and %d task records to %s"
          % (sink.n_stages, sink.n_tasks, out_dir), flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
