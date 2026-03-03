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

restart_service() {
    local host="$1"
    local service="$2"
    echo restarting service=$2 on host=$1
    run_remote "$1" "sudo systemctl restart $2"
}
for host in 'web-1' 'web-2'; do
    echo loop host=$host
    run_remote "$host" 'hostname'
    restart_service "$host" 'fp-service'
done
