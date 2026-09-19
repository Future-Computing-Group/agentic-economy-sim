#!/bin/bash
# Put the tier's emulated network delay on the cross-tier hop, then hand over to
# the stage server.
#
# The delay is the simulator's per-tier base_ms, and the model maps that term to
# the hop between tiers. A root netem also shapes this container's egress to the
# shared inference backend on the host, so every stage call would pay the tier
# delay a second time on a path the simulator has no term for, and the testbed's
# end-to-end medians would carry it once per stage. A prio qdisc with a filter
# puts the delay on the container subnet, where the other tiers are, and leaves
# the backend route alone. The del-then-add ordering and the u32 filter copy the
# WAN setup script this repository's sibling deployment uses; the pinned priomap
# and the two guards have no counterpart there.
#
# Two guards, either of which keeps the container down. The netem qdisc must be
# installed: a testbed that emulates nothing is worse than no testbed. And
# something must be filtered into it: netem parked in a band no packet reaches
# emulates nothing just as thoroughly, and looks installed while doing it.
set -euo pipefail
IFACE="${NETEM_IFACE:-eth0}"

# The connected subnet, read off the kernel route rather than configured: every
# other tier container is on it and nothing else is, and a compose network that
# moves needs no edit here.
SUBNET="$(ip -o route show dev "$IFACE" scope link 2>/dev/null | awk 'NR==1 {print $1}')" || true
[ -n "$SUBNET" ] || { echo "no connected subnet on $IFACE for $TIER" >&2; exit 4; }

# The shared backend. host.docker.internal can resolve onto the container subnet
# (the bridge gateway sits on it), so it is exempted by address at a higher
# priority than the subnet filter rather than assumed to fall outside it.
BACKEND_HOST="$(printf '%s' "${OLLAMA_BASE:-}" | sed -e 's#^[a-z][a-z]*://##' -e 's#[:/].*$##')"
BACKEND_IP="$(getent ahostsv4 "${BACKEND_HOST:-host.docker.internal}" 2>/dev/null | awk 'NR==1 {print $1}')" || true

tc qdisc del dev "$IFACE" root 2>/dev/null || true
# priomap all ones: unfiltered traffic lands in band 1:2 whatever its TOS bits,
# so the only packets that can reach the delayed band are the filtered ones. The
# default priomap routes three TOS classes into band 3 and would shape backend
# traffic that happened to carry them.
tc qdisc add dev "$IFACE" root handle 1: prio bands 3 priomap 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
tc qdisc add dev "$IFACE" parent 1:3 handle 30: netem delay "${NETEM_DELAY_MS}ms" "${NETEM_JITTER_MS}ms"
[ -n "$BACKEND_IP" ] && tc filter add dev "$IFACE" parent 1:0 protocol ip prio 1 u32 \
  match ip dst "$BACKEND_IP/32" flowid 1:2
tc filter add dev "$IFACE" parent 1:0 protocol ip prio 2 u32 match ip dst "$SUBNET" flowid 1:3

tc qdisc show dev "$IFACE" | grep -q netem || { echo "netem NOT applied on $TIER" >&2; exit 3; }
tc filter show dev "$IFACE" | grep -q "flowid 1:3" ||
  { echo "nothing is filtered into the delayed band on $TIER" >&2; exit 3; }
exec python -u /app/stage_server.py
