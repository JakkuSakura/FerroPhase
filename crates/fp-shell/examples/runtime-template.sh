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

__fp_std_ops_files_template_local_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_std_shell_backend_shell_template_local_ "${hosts}" "${src}" "${dest}" "${vars}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

__fp_std_shell_backend_copy_local_host_() {
    local src="$1"
    local dest="$2"
    copy_item "${src}" "${dest}"
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

__fp_std_shell_backend_shell_template_local_() {
    local _host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    runtime_set_changed 'false'
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        local tmp="$(runtime_temp_path)"
        render_template "${src}" "${tmp}" "${vars}"
        __fp_std_shell_backend_copy_local_host_ "${tmp}" "${dest}"
        remove_file "${tmp}"
        runtime_set_changed 'true'
    fi
}

__fp_std_shell_process_process_ok_() {
    local command="$1"
    shell_status "${command}"
}

__fp_std_ops_files_template_local_ './templates/fp-service.conf.tpl' '/etc/fp-service/fp-service.conf' 'localhost' '' '' '' '' ''
