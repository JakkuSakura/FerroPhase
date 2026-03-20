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

FP_HOST_TRANSPORT['web-1']='ssh'
FP_SSH_ADDRESS['web-1']='10.0.0.11'
FP_WINRM_ADDRESS['web-1']='10.0.0.11'
FP_SSH_USER['web-1']='deploy'
FP_DOCKER_USER['web-1']='deploy'
FP_WINRM_USER['web-1']='deploy'
FP_HOST_TRANSPORT['web-2']='ssh'
FP_SSH_ADDRESS['web-2']='10.0.0.12'
FP_WINRM_ADDRESS['web-2']='10.0.0.12'
FP_SSH_USER['web-2']='deploy'
FP_DOCKER_USER['web-2']='deploy'
FP_WINRM_USER['web-2']='deploy'

SSH_CONTROL_PATH="${TMPDIR:-/tmp}/fp-shell-%r@%h:%p"

host_transport() {
    local host="$1"
    runtime_host_transport "${host}"
}

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

host_container() {
    local host="$1"
    runtime_host_container "${host}"
}

host_pod() {
    local host="$1"
    runtime_host_pod "${host}"
}

host_namespace() {
    local host="$1"
    runtime_host_namespace "${host}"
}

host_context() {
    local host="$1"
    runtime_host_context "${host}"
}

run_local_host() {
    local cmd="$1"
    invoke_expression "${cmd}"
}

run_host() {
    local host="$1"
    local cmd="$2"
    local transport="$(host_transport "${host}")"
    case "${transport}" in
        local)
            run_local_host "${cmd}"
            ;;
        ssh)
            run_ssh_host "${host}" "${cmd}"
            ;;
        docker)
            run_docker_host "${host}" "${cmd}"
            ;;
        kubectl)
            run_kubectl_host "${host}" "${cmd}"
            ;;
        winrm)
            winrm_run "${host}" "${cmd}"
            ;;
        *)
            runtime_fail "unsupported transport: ${transport}"
            ;;
    esac
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

run_docker_host() {
    local host="$1"
    local cmd="$2"
    local container="$(host_container "${host}")"
    local user="$(host_user "${host}")"
    if [[ "${user}" != '' ]]; then
        docker_exec_user "${user}" "${container}" 'sh' '-lc' "${cmd}"
    else
        docker_exec "${container}" 'sh' '-lc' "${cmd}"
    fi
}

run_kubectl_host() {
    local host="$1"
    local cmd="$2"
    local context="$(host_context "${host}")"
    local namespace="$(host_namespace "${host}")"
    local container="$(host_container "${host}")"
    local pod="$(host_pod "${host}")"
    case "${context}" in
        )
            case "${namespace}" in
                )
                    case "${container}" in
                        )
                            kubectl_exec "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl_exec_container "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                    esac
                    ;;
                *)
                    case "${container}" in
                        )
                            kubectl_namespace_exec "${namespace}" 'exec' "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl_namespace_exec_container "${namespace}" 'exec' '-c' "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                    esac
                    ;;
            esac
            ;;
        *)
            case "${namespace}" in
                )
                    case "${container}" in
                        )
                            kubectl_context_exec "${context}" 'exec' "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl_context_exec_container "${context}" 'exec' '-c' "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                    esac
                    ;;
                *)
                    case "${container}" in
                        )
                            kubectl_context_namespace_exec "${context}" '-n' "${namespace}" 'exec' "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl_context_namespace_exec_container "${context}" '-n' "${namespace}" 'exec' '-c' "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                    esac
                    ;;
            esac
            ;;
    esac
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

shell_run() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    runtime_set_changed 'false'
    if should_apply "${only_if}" "${unless}" "${creates}" "${removes}"; then
        run_host "${host}" "${command}"
        runtime_set_changed 'true'
    fi
}

shell_run_local() {
    local _host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    runtime_set_changed 'false'
    if should_apply "${only_if}" "${unless}" "${creates}" "${removes}"; then
        run_local_host "${command}"
        runtime_set_changed 'true'
    fi
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

shell() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(command_with_options "${command}" "${cwd}" "${sudo}")"
    shell_run "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

shell_local() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(command_with_options "${command}" "${cwd}" "${sudo}")"
    shell_run_local "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
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

restart() {
    local name="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    shell "systemctl restart ${name}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

transport() {
    local host="$1"
    runtime_host_transport "${host}"
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

shell_local 'echo local pre-check' 'localhost' '' '' '' '' '' ''
restart 'fp-service' 'web-1' '' '' '' '' ''
shell_ssh 'sudo journalctl -u fp-service -n 10' 'web-1' '' '' '' '' '' ''
shell_ssh 'sudo journalctl -u fp-service -n 10' 'web-2' '' '' '' '' '' ''
shell_local 'echo local post-check' 'localhost' '' '' '' '' '' ''
