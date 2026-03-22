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

__fp_std_ops_server_shell_local_() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(__fp_std_shell_backend_command_with_options_ "${command}" "${cwd}" "${sudo}")"
    __fp_std_shell_backend_shell_run_local_ "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

__fp_std_shell_backend_run_local_host_() {
    local cmd="$1"
    invoke_expression "${cmd}"
}

__fp_std_shell_backend_command_with_options_() {
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

__fp_std_shell_backend_process_ok_() {
    local command="$1"
    __fp_std_shell_process_process_ok_ "${command}"
}

__fp_std_shell_backend_should_apply_() {
    local only_if="$1"
    local unless="$2"
    local creates="$3"
    local removes="$4"
    if [[ "${only_if}" != '' ]]; then
        if true; then
        fi
    fi
    if [[ "${unless}" != '' ]]; then
        if __fp_std_shell_backend_process_ok_ "${unless}"; then
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

__fp_std_shell_backend_shell_run_local_() {
    local _host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    runtime_set_changed 'false'
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_local_host_ "${command}"
        runtime_set_changed 'true'
    fi
}

__fp_std_shell_process_process_raw_() {
    local text="$1"
    printf '%s\n' "${text}"
}

__fp_std_shell_process_process_pipe_() {
    local lhs="$1"
    local rhs="$2"
    printf '%s\n' "${lhs} | ${rhs}"
}

__fp_std_shell_process_process_stdout_to_() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} > ${path}"
}

__fp_std_shell_process_process_run_() {
    local command="$1"
    __fp_std_ops_server_shell_local_ "${command}" 'localhost' '' '' '' '' '' ''
}

__fp_std_shell_process_process_ok_() {
    local command="$1"
    shell_status "${command}"
}

local pipeline="$(__fp_std_shell_process_process_pipe_ "$(__fp_std_shell_process_process_raw_ 'printf deploy')" "$(__fp_std_shell_process_process_stdout_to_ "$(__fp_std_shell_process_process_raw_ 'cat')" '/tmp/fp-shell.log')")"
__fp_std_shell_process_process_run_ "${pipeline}"
