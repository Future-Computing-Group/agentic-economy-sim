#!/usr/bin/env python3
"""One tier's stage server: an admission semaphore in front of a shared backend.

POST /stage      {task_id, round, stage, prompt, transport, sleep_ms}
                 -> {wait_ms, service_ms, tokens, text, ...}
GET  /fingerprint -> the backend this container actually resolves, and its tags.

wait_ms is time blocked on the tier's semaphore, service_ms is the backend call
itself. That split is the measurement the testbed exists for: it is the direct
analogue of the simulator's load-independent per-tier offset and its
load-dependent queueing term, and it is what shows whether the contention we
think we are imposing is in fact the contention that binds.

Stdlib only, same http/urllib pair the released agent harness already uses.
"""
import json
import os
import socket
import threading
import time
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

TIER = os.environ.get("TIER", "device")
TIER_CAP = int(os.environ.get("TIER_CAP", "2"))
NETEM_DELAY_MS = float(os.environ.get("NETEM_DELAY_MS", "0"))
NETEM_JITTER_MS = float(os.environ.get("NETEM_JITTER_MS", "0"))
OLLAMA_BASE = os.environ.get("OLLAMA_BASE", "http://host.docker.internal:11434").rstrip("/")
MODEL = os.environ.get("MODEL", "mistral:7b-instruct-q4_K_M")
PORT = int(os.environ.get("PORT", "8000"))
TIMEOUT = float(os.environ.get("STAGE_TIMEOUT_S", "180"))

STAGES = ("plan", "tool_0", "tool_1", "aggregate")
CONTAINER = socket.gethostname()

SEM = threading.BoundedSemaphore(TIER_CAP)


def generate(prompt):
    """Same request body and token accounting as the released agent harness."""
    body = json.dumps({"model": MODEL, "prompt": prompt, "stream": False}).encode()
    req = urllib.request.Request(OLLAMA_BASE + "/api/generate", data=body,
                                 headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
        resp = json.load(r)
    toks = int(resp.get("prompt_eval_count", 0)) + int(resp.get("eval_count", 0))
    return toks, resp.get("response", "")


def model_tags():
    with urllib.request.urlopen(OLLAMA_BASE + "/api/tags", timeout=10) as r:
        return sorted(m["name"] for m in json.load(r).get("models", []))


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def _send(self, code, obj):
        raw = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)

    def do_GET(self):
        if self.path != "/fingerprint":
            return self._send(404, {"error": "no such endpoint"})
        try:
            tags, err = model_tags(), None
        except Exception as exc:
            tags, err = [], "%s: %s" % (type(exc).__name__, exc)
        self._send(200, {"tier": TIER, "ollama_base": OLLAMA_BASE, "models": tags,
                         "model": MODEL, "tier_cap": TIER_CAP, "container": CONTAINER,
                         "netem_delay_ms": NETEM_DELAY_MS, "netem_jitter_ms": NETEM_JITTER_MS,
                         "error": err})

    def do_POST(self):
        if self.path != "/stage":
            return self._send(404, {"error": "no such endpoint"})
        try:
            n = int(self.headers.get("Content-Length") or 0)
        except ValueError:
            n = 0
        raw = self.rfile.read(n)          # always drained, so keep-alive survives a bad body
        try:
            req = json.loads(raw or b"{}")
            if not isinstance(req, dict):
                raise ValueError("body must be a JSON object")
            if req.get("stage") not in STAGES:
                raise ValueError("unknown stage %r; expected one of %s"
                                 % (req.get("stage"), ", ".join(STAGES)))
            if req.get("transport", "ollama") == "sleep":
                float(req.get("sleep_ms", 0.0))
        except Exception as exc:
            return self._send(400, {"tier": TIER, "container": CONTAINER,
                                    "error": "%s: %s" % (type(exc).__name__, exc)})
        t_queued = time.time()
        with SEM:
            wait_ms = (time.time() - t_queued) * 1000.0
            t_service = time.time()
            try:
                if req.get("transport", "ollama") == "sleep":
                    time.sleep(float(req.get("sleep_ms", 0.0)) / 1000.0)
                    tokens, text = 0, ""
                else:
                    tokens, text = generate(req.get("prompt", ""))
                err = None
            except Exception as exc:
                tokens, text, err = 0, "", "%s: %s" % (type(exc).__name__, exc)
            service_ms = (time.time() - t_service) * 1000.0
        self._send(200 if err is None else 502, {
            "tier": TIER, "task_id": req.get("task_id"), "round": req.get("round"),
            "stage": req.get("stage"), "container": CONTAINER, "wait_ms": round(wait_ms, 3),
            "service_ms": round(service_ms, 3), "tokens": tokens, "text": text,
            "netem_delay_ms": NETEM_DELAY_MS, "error": err})

    def log_message(self, *args):
        pass


if __name__ == "__main__":
    print("stage server: tier=%s cap=%d netem=%sms/%sms backend=%s"
          % (TIER, TIER_CAP, NETEM_DELAY_MS, NETEM_JITTER_MS, OLLAMA_BASE), flush=True)
    ThreadingHTTPServer(("0.0.0.0", PORT), Handler).serve_forever()
