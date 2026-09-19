#!/bin/bash
# Rung 1: container smoke. No model is called; the sleep transport stands in for
# the backend so this rung tests the substrate only -- that the qdisc is really
# installed on this image, that the delay is really on the wire between
# containers, and that a replayed round produces well-formed records behind a
# barrier.
#
# Usage: EMUL_RUNS_DIR=/path/outside/the/repo/runs/emulation ./smoke_rung1.sh
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(git -C "$HERE" rev-parse --show-toplevel)"
COMPOSE=(docker compose -f "$HERE/docker-compose.yaml")
RUN_ID="${RUN_ID:-rung1-$(date -u +%H%M%S)}"
OUT_ROOT="${EMUL_RUNS_DIR:-$(dirname "$REPO")/runs/emulation}"
OUT_DIR="$OUT_ROOT/$(date -u +%Y-%m-%d)-$RUN_ID"
# The RTT check is evidence that the emulated delay is on the path between
# containers. It is not a per-tier assay: its resolution is coarser than a single
# tier's delay, so what pins each tier's value is the qdisc assertion above it,
# which compares the installed qdisc against the delay that container reports.
# Widen RTT_SLACK_MS on a loaded machine.
REPLAY_DIR="$HERE/tests/fixtures"
ENV_FILE="$REPLAY_DIR/env_smoke.json"
RTT_LOW_MARGIN_MS="${RTT_LOW_MARGIN_MS:-3}"
RTT_HIGH_FRAC="${RTT_HIGH_FRAC:-1.6}"
RTT_SLACK_MS="${RTT_SLACK_MS:-20}"
# The backend route carries no emulated delay: base_ms is a cross-tier term and
# the container's hop to the shared inference backend is not one. Widen on a
# loaded machine, but never above a tier delay, or the check stops discriminating.
RTT_HOST_MAX_MS="${RTT_HOST_MAX_MS:-4}"
HOST_PROBE_PORT="${HOST_PROBE_PORT:-11434}"
PASS=0; FAIL=0

ok()   { echo "ok   - $1"; PASS=$((PASS + 1)); }
bad()  { echo "FAIL - $1"; FAIL=$((FAIL + 1)); }
check(){ if [ "$1" = "0" ]; then ok "$2"; else bad "$2 :: $3"; fi; }

teardown() { echo "--- tearing down"; "${COMPOSE[@]}" down --remove-orphans >/dev/null 2>&1; }
trap teardown EXIT

echo "--- docker"
docker info --format 'server {{.ServerVersion}} on {{.OperatingSystem}}' || exit 1

echo "--- build"
"${COMPOSE[@]}" build --quiet || exit 1

echo "--- up"
"${COMPOSE[@]}" up -d || exit 1

echo "--- wait for the stage servers"
python3 - <<'PY' || { echo "stage servers did not come up"; docker compose -f "$HERE/docker-compose.yaml" logs --tail 30; exit 1; }
import json, sys, time, urllib.request
for port in (8101, 8102, 8103):
    for _ in range(60):
        try:
            with urllib.request.urlopen("http://127.0.0.1:%d/fingerprint" % port, timeout=2) as r:
                fp = json.load(r)
            print("  %s up: netem %sms/%sms cap %s" % (fp["tier"], fp["netem_delay_ms"],
                                                       fp["netem_jitter_ms"], fp["tier_cap"]))
            break
        except Exception:
            time.sleep(1)
    else:
        sys.exit("port %d never answered" % port)
PY

echo "--- the installed qdisc against the delay each container reports"
port_for() { case "$1" in device) echo 8101;; edge) echo 8102;; cloud) echo 8103;; esac; }
for tier in device edge cloud; do
  out="$("${COMPOSE[@]}" exec -T "$tier" tc qdisc show dev eth0 2>&1)"
  printf '  %-7s %s\n' "$tier" "$out"
  "${COMPOSE[@]}" exec -T "$tier" tc filter show dev eth0 2>&1 |
    grep -E 'flowid|match' | sed "s/^/    $tier filter: /"
  python3 - "$HERE" "http://127.0.0.1:$(port_for "$tier")" "$tier" "$out" <<'QD'
import json, sys, urllib.request
here, url, tier, line = sys.argv[1:5]
sys.path.insert(0, here)
import load_gen
declared = float(load_gen.NETWORK_DELAY_MS[tier])
configured = float(json.load(urllib.request.urlopen(url + "/fingerprint", timeout=5))["netem_delay_ms"])
installed = load_gen.qdisc_delay_ms(line)
print("    installed %s ms, configured %s ms, assumed cross-tier delay %s ms"
      % (installed, configured, declared))
sys.exit(0 if installed is not None and abs(installed - configured) < 0.05
         and configured == declared else 1)
QD
  check $? "$tier: installed qdisc is the assumed cross-tier delay" "installed, configured and declared delays disagree"
done

echo "--- container-to-container RTT against the configured delay"
rtt() { # from to expected_ms
  local measured
  measured="$("${COMPOSE[@]}" exec -T "$1" python -u -c "
import socket, statistics, time
r = []
for _ in range(12):
    t = time.time()
    s = socket.create_connection(('$2', 8000), 5)
    r.append((time.time() - t) * 1000.0)
    s.close()
print('%.2f' % statistics.median(r))
" 2>&1 | tr -d '\r')"
  python3 - "$1" "$2" "$3" "$measured" "$RTT_LOW_MARGIN_MS" "$RTT_HIGH_FRAC" "$RTT_SLACK_MS" <<'PY'
import sys
frm, to, exp, meas, lo_m, hi_f, slack = sys.argv[1:]
try:
    meas = float(meas)
except ValueError:
    print("unparseable RTT: %s" % meas); sys.exit(1)
exp, lo_m, hi_f, slack = float(exp), float(lo_m), float(hi_f), float(slack)
lo, hi = exp - lo_m, exp * hi_f + slack
print("  %s->%s median RTT %.2f ms (configured %.0f, accept %.1f..%.1f)" % (frm, to, meas, exp, lo, hi))
sys.exit(0 if lo <= meas <= hi else 1)
PY
}
rtt device edge 20; check $? "device->edge RTT tracks netem (5+15 ms)" "out of tolerance"
rtt device cloud 55; check $? "device->cloud RTT tracks netem (5+50 ms)" "out of tolerance"
rtt edge cloud 65; check $? "edge->cloud RTT tracks netem (15+50 ms)" "out of tolerance"

echo "--- container-to-host RTT: the backend route carries no emulated delay"
# The same TCP connect, to the host instead of to a sibling container. A refused
# connection has made the round trip just as a completed one has, so this does
# not depend on the backend being up; a connection that hangs is a defect and
# the timeout is left to surface as one.
rtt_host() { # tier tier_delay_ms
  local measured
  measured="$("${COMPOSE[@]}" exec -T "$1" python -u -c "
import socket, statistics, time
r = []
for _ in range(12):
    t = time.time()
    try:
        socket.create_connection(('host.docker.internal', $HOST_PROBE_PORT), 5).close()
    except ConnectionRefusedError:
        pass
    r.append((time.time() - t) * 1000.0)
print('%.2f' % statistics.median(r))
" 2>&1 | tr -d '\r')"
  python3 - "$1" "$2" "$measured" "$RTT_HOST_MAX_MS" <<'PY'
import sys
tier, delay, meas, cap = sys.argv[1:]
try:
    meas = float(meas)
except ValueError:
    print("  unparseable host RTT from %s: %s" % (tier, meas)); sys.exit(1)
delay, cap = float(delay), float(cap)
print("  %s->host median RTT %.2f ms (tier delay %.0f ms, accept <= %.1f)" % (tier, meas, delay, cap))
sys.exit(0 if meas <= cap else 1)
PY
}
rtt_host device 5; check $? "device->host RTT is unshaped" "the backend route is carrying the tier delay"
rtt_host edge 15; check $? "edge->host RTT is unshaped" "the backend route is carrying the tier delay"
rtt_host cloud 50; check $? "cloud->host RTT is unshaped" "the backend route is carrying the tier delay"

echo "--- three replayed rounds, sleep transport"
mkdir -p "$OUT_DIR"
python3 "$HERE/load_gen.py" --replay-dir "$REPLAY_DIR" --load smoke \
  --out-dir "$OUT_DIR" --transport sleep --offered-scale 1.0 --max-rounds 3 \
  --run-id "$RUN_ID" 2>&1 | sed 's/^/  /'
check "${PIPESTATUS[0]}" "load generator completed three rounds" "generator failed"

echo "--- records"
python3 - "$OUT_DIR" "$HERE/../agentic/agentic_profile.json" "$ENV_FILE" <<'PY'
import csv, json, sys
from pathlib import Path

out, profile_path, env_path = Path(sys.argv[1]), Path(sys.argv[2]), Path(sys.argv[3])
profile = json.loads(profile_path.read_text())
NETEM = {t: float(v) for t, v in json.loads(env_path.read_text())["base_ms"].items()}
fails = []

stages = [json.loads(l) for l in (out / "stage_records.jsonl").read_text().splitlines() if l.strip()]
tasks = list(csv.DictReader((out / "task_records.csv").open()))
meta = json.loads((out / "run_meta.json").read_text())

FIELDS = {"run_id", "round", "task_id", "stage", "tier", "t_send", "wait_ms", "service_ms",
          "netem_delay_ms", "tokens", "container", "error", "load"}

if len(tasks) != 5:
    fails.append("expected 5 replayed tasks (2+2+1 admitted), got %d" % len(tasks))
if len(stages) != 4 * len(tasks):
    fails.append("expected 4 stage records per task, got %d for %d tasks" % (len(stages), len(tasks)))
for s in stages:
    if set(s) != FIELDS:
        fails.append("stage record fields %s" % sorted(set(s) ^ FIELDS)); break
    if s["error"] is not None:
        fails.append("stage error: %s" % s["error"]); break
    if s["netem_delay_ms"] != NETEM[s["tier"]]:
        fails.append("%s reports netem %s" % (s["tier"], s["netem_delay_ms"])); break
    expect = profile["tiers"][s["tier"]]["mean_latency_ms"]
    if not 0.9 * expect <= s["service_ms"] <= 1.3 * expect + 50:
        fails.append("%s service_ms %.0f not the profile's %.0f" % (s["tier"], s["service_ms"], expect)); break
if not all(t["completed"] == "True" for t in tasks):
    fails.append("not every task completed")
if {t["round"] for t in tasks} != {"1", "2", "3"}:
    fails.append("rounds replayed: %s" % sorted({t["round"] for t in tasks}))
if meta["n_tasks"] != len(tasks) or meta["n_stage_records"] != len(stages):
    fails.append("run_meta counts disagree with the record files")
if meta["fingerprint"]["enforced"] is not False:
    fails.append("sleep transport should record the fingerprint as not enforced")

# The barrier: no stage of round r+1 is sent before the last stage of round r.
by_round = {}
for s in stages:
    by_round.setdefault(s["round"], []).append(s["t_send"])
for r in sorted(by_round)[:-1]:
    if max(by_round[r]) >= min(by_round[r + 1]):
        fails.append("round %d overlaps round %d" % (r, r + 1))

print("  %d stage records, %d task records, rounds %s"
      % (len(stages), len(tasks), sorted(by_round)))
print("  per-tier median service_ms: " + ", ".join(
    "%s %.0f" % (t, sorted(s["service_ms"] for s in stages if s["tier"] == t)[len(
        [s for s in stages if s["tier"] == t]) // 2]) for t in ("device", "edge", "cloud")))
for f in fails:
    print("  defect: " + f)
sys.exit(1 if fails else 0)
PY
check $? "records well formed and the round barrier held" "see defects above"

echo "smoke_rung1.sh: $PASS passed, $FAIL failed  (run data: $OUT_DIR)"
[ "$FAIL" -eq 0 ]
