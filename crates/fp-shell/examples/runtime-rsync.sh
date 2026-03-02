#!/usr/bin/env bash
set -euo pipefail

SSH_CONTROL_PATH="${TMPDIR:-/tmp}/fp-shell-%r@%h:%p"

ssh_cmd() {
  ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

scp_cmd() {
  scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

rsync_cmd() {
  rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "$@"
}

run_remote() {
  local host="$1"
  local cmd="$2"
  ssh_cmd "$host" "$cmd"
}

rsync_cmd -az --delete --checksum -- './dist/' 'web-1:/srv/fp-service/dist/'
run_remote 'web-1' 'sudo systemctl restart fp-service'
