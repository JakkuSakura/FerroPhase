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


SSH_CONTROL_PATH="${TMPDIR:-/tmp}/fp-shell-%r@%h:%p"

copy_local_host() {
    local src="$1"
    local dest="$2"
    copy_item "${src}" "${dest}"
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

shell_template_local() {
    local _host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    runtime_set_changed 'false'
    if should_apply "${only_if}" "${unless}" "${creates}" "${removes}"; then
        local tmp="$(runtime_temp_path)"
        render_template "${src}" "${tmp}" "${vars}"
        copy_local_host "${tmp}" "${dest}"
        remove_file "${tmp}"
        runtime_set_changed 'true'
    fi
}

template_local() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    shell_template_local "${hosts}" "${src}" "${dest}" "${vars}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

ok() {
    local command="$1"
    shell_status "${command}"
}

template_local './templates/fp-service.conf.tpl' '/etc/fp-service/fp-service.conf' 'localhost' '' '' '' '' ''
