#!/usr/bin/env bash
set -euo pipefail

__fp_last_changed=0

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

run_remote 'web-1' 'uname -a'
run_remote 'web-2' 'uname -a'
run_remote 'web-3' 'uname -a'
run_remote 'web-1' 'sudo systemctl status fp-service'
run_remote 'web-2' 'sudo systemctl status fp-service'
run_remote 'web-3' 'sudo systemctl status fp-service'
