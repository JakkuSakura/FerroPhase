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

sync_and_restart() {
    local host="$1"
    local service="$2"
    rsync_cmd -az --delete -- './dist/' '$1:/srv/fp-service/dist/'
    run_remote '$1' 'sudo systemctl restart $2'
}
sync_and_restart 'web-1' 'fp-service'
sync_and_restart 'web-2' 'fp-service'
