#!/bin/bash
# Rung 0: what the entrypoint installs, and what it refuses to come up without.
#
# Two properties. The container must not come up emulating nothing, which is
# the defect this file was written for. And the delay must land on the hop the
# model has a term for -- the cross-tier hop -- and nowhere else: a root netem
# also shapes the container's egress to the shared inference backend, so every
# stage call pays the tier delay a second time on a path the simulator does not
# model at all. The tc command lines are asserted here because they are the
# whole of that decision; the container-level evidence is in smoke_rung1.sh.
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ENTRYPOINT="$HERE/../entrypoint.sh"
PASS=0
FAIL=0

stub_dir() {
  local d
  d="$(mktemp -d)"
  cat > "$d/tc" <<'STUB'
#!/bin/sh
printf '%s\n' "$*" >> "${TC_LOG:-/dev/null}"
case "$1 $2" in
  "qdisc add")   [ "${TC_ADD_FAIL:-0}" = "1" ] && exit 2 ;;
  "qdisc show")  printf '%s\n' "$TC_SHOW_OUTPUT" ;;
  "filter show") printf '%s\n' "${TC_FILTER_OUTPUT-filter parent 1: protocol ip pref 2 u32 chain 0 fh 800::800 flowid 1:3}" ;;
esac
exit 0
STUB
  cat > "$d/ip" <<'STUB'
#!/bin/sh
printf '%s\n' "${IP_ROUTE_OUTPUT-172.18.0.0/16 proto kernel scope link src 172.18.0.3}"
STUB
  cat > "$d/getent" <<'STUB'
#!/bin/sh
printf '%s\n' "${GETENT_OUTPUT-192.168.65.254 STREAM host.docker.internal}"
STUB
  cat > "$d/python" <<'STUB'
#!/bin/sh
echo "REACHED_EXEC $*"
STUB
  chmod +x "$d/tc" "$d/ip" "$d/getent" "$d/python"
  printf '%s' "$d"
}

ok()  { echo "ok   - $1"; PASS=$((PASS + 1)); }
bad() { echo "FAIL - $1 :: $2"; FAIL=$((FAIL + 1)); }

check() { # name expected_status expected_pattern out status
  local name="$1" want_status="$2" want_pat="$3" out="$4" status="$5"
  if [ "$status" = "$want_status" ] && printf '%s' "$out" | grep -q "$want_pat"; then
    ok "$name"
  else
    bad "$name" "status=$status want=$want_status, output: $out"
  fi
}

logged() { # name pattern
  if grep -qF -- "$2" "$TC_LOG"; then ok "$1"; else bad "$1" "no tc line matching: $2"; fi
}

not_logged() { # name pattern
  if grep -qF -- "$2" "$TC_LOG"; then bad "$1" "tc line present but must not be: $2"; else ok "$1"; fi
}

D="$(stub_dir)"
export TIER=edge NETEM_DELAY_MS=15 NETEM_JITTER_MS=3
export OLLAMA_BASE="http://host.docker.internal:11434"
export TC_LOG="$D/tc.log"

# --- the command lines the wrapper generates ---------------------------------
: > "$TC_LOG"
OUT="$(PATH="$D:$PATH" TC_SHOW_OUTPUT="qdisc netem 30: parent 1:3 limit 1000 delay 15ms 3ms" bash "$ENTRYPOINT" 2>&1)"; ST=$?
check "a well-formed stack reaches exec" 0 "REACHED_EXEC" "$OUT" "$ST"

not_logged "the delay is not installed on the root qdisc" "root netem"
logged "root is a prio qdisc whose priomap keeps every TOS class out of the delayed band" \
  "qdisc add dev eth0 root handle 1: prio bands 3 priomap 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"
logged "netem carries the tier's delay and jitter on band 1:3" \
  "qdisc add dev eth0 parent 1:3 handle 30: netem delay 15ms 3ms"
logged "the container subnet, where the other tiers are, is filtered into the delayed band" \
  "u32 match ip dst 172.18.0.0/16 flowid 1:3"
logged "the shared backend is filtered into an undelayed band" \
  "u32 match ip dst 192.168.65.254/32 flowid 1:2"

back_prio="$(grep 'flowid 1:2' "$TC_LOG" | sed -n 's/.*prio \([0-9][0-9]*\).*/\1/p' | head -1)"
sub_prio="$(grep 'flowid 1:3' "$TC_LOG" | sed -n 's/.*prio \([0-9][0-9]*\).*/\1/p' | head -1)"
if [ -n "$back_prio" ] && [ -n "$sub_prio" ] && [ "$back_prio" -lt "$sub_prio" ]; then
  ok "the backend exemption outranks the subnet filter (prio $back_prio before $sub_prio)"
else
  bad "the backend exemption outranks the subnet filter" "backend prio '$back_prio', subnet prio '$sub_prio'"
fi

# The backend can resolve inside the container subnet (the bridge gateway is on
# it), and then the subnet filter would shape it. The exemption is what makes
# that case safe, so it is asserted on that case and not only on the easy one.
: > "$TC_LOG"
OUT="$(PATH="$D:$PATH" GETENT_OUTPUT="172.18.0.1 STREAM host.docker.internal" \
  TC_SHOW_OUTPUT="qdisc netem 30: parent 1:3 limit 1000 delay 15ms 3ms" bash "$ENTRYPOINT" 2>&1)"; ST=$?
check "a backend on the container subnet still reaches exec" 0 "REACHED_EXEC" "$OUT" "$ST"
logged "a backend on the container subnet is exempted by address" \
  "u32 match ip dst 172.18.0.1/32 flowid 1:2"

# --- the refusals ------------------------------------------------------------
: > "$TC_LOG"
OUT="$(PATH="$D:$PATH" TC_SHOW_OUTPUT="qdisc noqueue 0: root refcnt 2" bash "$ENTRYPOINT" 2>&1)"; ST=$?
check "no netem qdisc: exits 3 naming the tier" 3 "netem NOT applied on edge" "$OUT" "$ST"

: > "$TC_LOG"
OUT="$(PATH="$D:$PATH" TC_SHOW_OUTPUT="qdisc netem 30: parent 1:3 limit 1000 delay 15ms 3ms" \
  TC_FILTER_OUTPUT="" bash "$ENTRYPOINT" 2>&1)"; ST=$?
check "netem installed but nothing filtered into it: exits 3 naming the tier" 3 \
  "nothing is filtered into the delayed band on edge" "$OUT" "$ST"

: > "$TC_LOG"
OUT="$(PATH="$D:$PATH" IP_ROUTE_OUTPUT="" TC_SHOW_OUTPUT="qdisc netem 30: parent 1:3 delay 15ms 3ms" \
  bash "$ENTRYPOINT" 2>&1)"; ST=$?
check "no connected subnet to filter on: exits 4 naming the tier" 4 \
  "no connected subnet on eth0 for edge" "$OUT" "$ST"

: > "$TC_LOG"
OUT="$(PATH="$D:$PATH" TC_ADD_FAIL=1 TC_SHOW_OUTPUT="qdisc noqueue 0: root refcnt 2" bash "$ENTRYPOINT" 2>&1)"; ST=$?
if [ "$ST" != "0" ]; then ok "failing tc add is not swallowed"; else bad "failing tc add is not swallowed" "exit 0"; fi

rm -rf "$D"
echo "test_entrypoint.sh: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
