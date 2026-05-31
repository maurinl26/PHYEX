#!/usr/bin/env bash
#
# Drive a RunPods GPU pod from a GitHub-hosted runner: create -> wait -> SSH in,
# build + test the OpenACC bindings -> always tear the pod down.
#
# Env (set by .github/workflows/python-gpu-runpod.yml):
#   RUNPOD_API_KEY  RunPods API key (secret)
#   GPU_TYPE        e.g. "NVIDIA GeForce RTX 4090"
#   GPU_ARCH        NVHPC -gpu arch, e.g. cc89
#   IMAGE           NVHPC container image
#   SHA, REPO       commit + "owner/name" to test
#
set -euo pipefail

: "${RUNPOD_API_KEY:?RUNPOD_API_KEY is required}"
GPU_TYPE="${GPU_TYPE:-NVIDIA GeForce RTX 4090}"
GPU_ARCH="${GPU_ARCH:-cc89}"
IMAGE="${IMAGE:-nvcr.io/nvidia/nvhpc:24.5-devel-cuda_multi-ubuntu22.04}"
SHA="${SHA:-master}"
REPO="${REPO:?REPO is required}"

runpodctl config --apiKey "$RUNPOD_API_KEY" >/dev/null

POD_ID=""
cleanup() {
  if [[ -n "$POD_ID" ]]; then
    echo "==> Removing pod $POD_ID"
    runpodctl remove pod "$POD_ID" || echo "WARN: pod $POD_ID may still be running — check the RunPods console"
  fi
}
trap cleanup EXIT

echo "==> Creating $GPU_TYPE pod from $IMAGE"
CREATE_OUT="$(runpodctl create pod \
  --name "phyex-gpu-${SHA:0:8}" \
  --imageName "$IMAGE" \
  --gpuType "$GPU_TYPE" \
  --gpuCount 1 \
  --containerDiskSize 40 \
  --ports '22/tcp' \
  --secureCloud \
  --args 'bash -c "sleep infinity"')"
echo "$CREATE_OUT"
POD_ID="$(echo "$CREATE_OUT" | grep -oE '"[a-z0-9]{12,}"' | head -1 | tr -d '"')"
[[ -n "$POD_ID" ]] || { echo "ERROR: could not parse pod id"; exit 1; }
echo "POD_ID=$POD_ID"

echo "==> Waiting for SSH to come up"
SSH_HOST=""; SSH_PORT=""
for i in $(seq 1 40); do
  INFO="$(runpodctl get pod "$POD_ID" --allfields 2>/dev/null || true)"
  SSH_HOST="$(echo "$INFO" | grep -oE '[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+' | head -1 || true)"
  SSH_PORT="$(echo "$INFO" | grep -oE '22->[0-9]+' | grep -oE '[0-9]+$' | head -1 || true)"
  if [[ -n "$SSH_HOST" && -n "$SSH_PORT" ]]; then break; fi
  sleep 15
done
[[ -n "$SSH_HOST" && -n "$SSH_PORT" ]] || { echo "ERROR: SSH endpoint not ready"; exit 1; }
echo "==> SSH at $SSH_HOST:$SSH_PORT"

SSH="ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -p $SSH_PORT root@$SSH_HOST"
for i in $(seq 1 20); do $SSH true 2>/dev/null && break || sleep 10; done

echo "==> Building and testing on the pod"
$SSH bash -s <<EOF
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq && apt-get install -y -qq git python3 python3-pip cmake ninja-build >/dev/null
git clone --depth 1 https://github.com/${REPO}.git phyex && cd phyex
git fetch --depth 1 origin ${SHA} && git checkout ${SHA}
export GPU_ARCH=${GPU_ARCH}
bash ci/runpod_gpu_test.sh
EOF

echo "==> GPU job finished OK"
