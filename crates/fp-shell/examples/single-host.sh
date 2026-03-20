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

FP_HOST_TRANSPORT['web-2']='ssh'
FP_SSH_ADDRESS['web-2']='10.0.0.12'
FP_WINRM_ADDRESS['web-2']='10.0.0.12'
FP_SSH_USER['web-2']='deploy'
FP_DOCKER_USER['web-2']='deploy'
FP_WINRM_USER['web-2']='deploy'
FP_HOST_TRANSPORT['web-1']='ssh'
FP_SSH_ADDRESS['web-1']='10.0.0.11'
FP_WINRM_ADDRESS['web-1']='10.0.0.11'
FP_SSH_USER['web-1']='deploy'
FP_DOCKER_USER['web-1']='deploy'
FP_WINRM_USER['web-1']='deploy'

SSH_CONTROL_PATH="${TMPDIR:-/tmp}/fp-shell-%r@%h:%p"

host_address() {
    local host="$1"
    runtime_host_address "${host}"
}

host_user() {
    local host="$1"
    runtime_host_user "${host}"
}

host_port() {
    local host="$1"
    runtime_host_port "${host}"
}

ssh_target() {
    local host="$1"
    local user="$(host_user "${host}")"
    local address="$(host_address "${host}")"
    if [[ "${user}" != '' ]]; then
        printf '%s\n' "${user}@${address}"
    else
        printf '%s\n' "${address}"
    fi
}

run_ssh_host() {
    local host="$1"
    local cmd="$2"
    local target="$(ssh_target "${host}")"
    local port="$(host_port "${host}")"
    if [[ "${port}" != '' ]]; then
        ssh_port "${port}" "${target}" "${cmd}"
    else
        ssh "${target}" "${cmd}"
    fi
}

copy_ssh_host() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local target="$(ssh_target "${host}")"
    local remote="${target}:${dest}"
    local port="$(host_port "${host}")"
    if [[ "${port}" != '' ]]; then
        scp_port "${port}" "${src}" "${remote}"
    else
        scp "${src}" "${remote}"
    fi
}

command_with_options() {
    local command="$1"
    local cwd="$2"
    local sudo="$3"
    if [[ "${cwd}" != '' ]]; then
        if [[ "${sudo}" == 'true' ]]; then
            printf '%s\n' "sudo cd ${cwd} && ${command}"
        else
            printf '%s\n' "cd ${cwd} && ${command}"
        fi
    else
        if [[ "${sudo}" == 'true' ]]; then
            printf '%s\n' "sudo ${command}"
        else
            printf '%s\n' "${command}"
        fi
    fi
}

process_ok() {
    local command="$1"
    ok "${command}"
}

should_apply() {
    local only_if="$1"
    local unless="$2"
    local creates="$3"
    local removes="$4"
    if [[ "${only_if}" != '' ]]; then
        if true; then
        fi
    fi
    if [[ "${unless}" != '' ]]; then
        if process_ok "${unless}"; then
        fi
    fi
    if [[ "${creates}" != '' ]]; then
        if true; then
        fi
    fi
    if [[ "${removes}" != '' ]]; then
        if true; then
        fi
    fi
    printf '%s\n' 'true'
}

shell_run_ssh() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    runtime_set_changed 'false'
    if should_apply "${only_if}" "${unless}" "${creates}" "${removes}"; then
        run_ssh_host "${host}" "${command}"
        runtime_set_changed 'true'
    fi
}

shell_copy_ssh() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    runtime_set_changed 'false'
    if should_apply "${only_if}" "${unless}" "${creates}" "${removes}"; then
        copy_ssh_host "${host}" "${src}" "${dest}"
        runtime_set_changed 'true'
    fi
}

shell_ssh() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(command_with_options "${command}" "${cwd}" "${sudo}")"
    shell_run_ssh "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

copy_ssh() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    shell_copy_ssh "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

address() {
    local host="$1"
    runtime_host_address "${host}"
}

user() {
    local host="$1"
    runtime_host_user "${host}"
}

port() {
    local host="$1"
    runtime_host_port "${host}"
}

ok() {
    local command="$1"
    shell_status "${command}"
}

shell_ssh 'sudo systemctl restart fp-service' 'web-1' '' '' '' '' '' ''
copy_ssh './config/prod.env' '/etc/fp-service/.env' 'web-1' '' '' '' ''
