#!/usr/bin/env bash
set -xeuo pipefail

__fp_last_changed=0

declare -A FP_HOST_TRANSPORT=()
declare -A FP_SSH_ADDRESS=()
declare -A FP_SSH_USER=()
declare -A FP_SSH_PORT=()
declare -A FP_DOCKER_CONTAINER=()
declare -A FP_DOCKER_USER=()
declare -A FP_K8S_POD=()
declare -A FP_K8S_NAMESPACE=()
declare -A FP_K8S_CONTAINER=()
declare -A FP_K8S_CONTEXT=()
declare -A FP_WINRM_ADDRESS=()
declare -A FP_WINRM_USER=()
declare -A FP_WINRM_PASSWORD=()
declare -A FP_WINRM_PORT=()
declare -A FP_WINRM_SCHEME=()
declare -A FP_CHROOT_DIRECTORY=()


SSH_CONTROL_PATH="${TMPDIR:-/tmp}/fp-shell-%r@%h:%p"

std::ops::server::shell_local 'echo local hello' 'localhost' '' '' '' '' 'false' '' 'false' ''
std::ops::server::shell_local 'echo ssh hello' 'localhost' '' '' '' '' 'false' '' 'false' ''
std::ops::server::shell_local 'echo docker hello' 'localhost' '' '' '' '' 'false' '' 'false' ''
std::ops::server::shell_local 'echo kubectl hello' 'localhost' '' '' '' '' 'false' '' 'false' ''
std::ops::server::shell_local 'Write-Host winrm hello' 'localhost' '' '' '' '' 'false' '' 'false' ''
