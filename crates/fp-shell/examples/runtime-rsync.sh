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

__fp_std_ops_files_rsync_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local archive="$4"
    local compress="$5"
    local delete="$6"
    local checksum="$7"
    local only_if="$8"
    local unless="$9"
    local creates="$10"
    local removes="$11"
    local flags="$(__fp_std_shell_backend_rsync_flag_string_ "${archive}" "${compress}" "${delete}" "${checksum}")"
    __fp_std_shell_backend_shell_rsync_ "${hosts}" "${flags}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

__fp_std_ops_server_shell_() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(__fp_std_shell_backend_command_with_options_ "${command}" "${cwd}" "${sudo}")"
    __fp_std_shell_backend_shell_run_ "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed 
}

__fp_std_shell_backend_host_transport_() {
    local host="$1"
}

__fp_std_shell_backend_host_address_() {
    local host="$1"
}

__fp_std_shell_backend_host_user_() {
    local host="$1"
}

__fp_std_shell_backend_host_port_() {
    local host="$1"
}

__fp_std_shell_backend_host_container_() {
    local host="$1"
}

__fp_std_shell_backend_host_pod_() {
    local host="$1"
}

__fp_std_shell_backend_host_namespace_() {
    local host="$1"
}

__fp_std_shell_backend_host_context_() {
    local host="$1"
}

__fp_std_shell_backend_host_chroot_directory_() {
    local host="$1"
}

__fp_std_shell_backend_run_local_host_() {
    local cmd="$1"
    invoke_expression "${cmd}"
}

__fp_std_shell_backend_run_host_() {
    local host="$1"
    local cmd="$2"
    local transport="$(__fp_std_shell_backend_host_transport_ "${host}")"
    case "${transport}" in
        local)
            __fp_std_shell_backend_run_local_host_ "${cmd}"
            ;;
        ssh)
            __fp_std_shell_backend_run_ssh_host_ "${host}" "${cmd}"
            ;;
        docker)
            __fp_std_shell_backend_run_docker_host_ "${host}" "${cmd}"
            ;;
        kubectl)
            __fp_std_shell_backend_run_kubectl_host_ "${host}" "${cmd}"
            ;;
        winrm)
            winrm_run "${host}" "${cmd}"
            ;;
        chroot)
            __fp_std_shell_backend_run_chroot_host_ "${host}" "${cmd}"
            ;;
        *)
            runtime_fail "unsupported transport: ${transport}"
            ;;
    esac
}

__fp_std_shell_backend_rsync_host_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local transport="$(__fp_std_shell_backend_host_transport_ "${host}")"
    case "${transport}" in
        local)
            rsync_cli "${flags}" "${src}" "${dest}"
            ;;
        chroot)
            __fp_std_shell_backend_rsync_chroot_host_ "${host}" "${flags}" "${src}" "${dest}"
            ;;
        *)
            __fp_std_shell_backend_rsync_remote_host_ "${host}" "${flags}" "${src}" "${dest}"
            ;;
    esac
}

__fp_std_shell_backend_ssh_target_() {
    local host="$1"
    local user="$(__fp_std_shell_backend_host_user_ "${host}")"
    local address="$(__fp_std_shell_backend_host_address_ "${host}")"
    if [[ "${user}" != '' ]]; then
        printf '%s\n' "${user}@${address}"
    else
        printf '%s\n' "${address}"
    fi
}

__fp_std_shell_backend_run_ssh_host_() {
    local host="$1"
    local cmd="$2"
    local target="$(__fp_std_shell_backend_ssh_target_ "${host}")"
    local port="$(__fp_std_shell_backend_host_port_ "${host}")"
    if [[ "${port}" != '' ]]; then
        ssh_port "${port}" "${target}" "${cmd}"
    else
        ssh "${target}" "${cmd}"
    fi
}

__fp_std_shell_backend_run_docker_host_() {
    local host="$1"
    local cmd="$2"
    local container="$(__fp_std_shell_backend_host_container_ "${host}")"
    local user="$(__fp_std_shell_backend_host_user_ "${host}")"
    if [[ "${user}" != '' ]]; then
        docker_exec_user "${user}" "${container}" 'sh' '-lc' "${cmd}"
    else
        docker_exec "${container}" 'sh' '-lc' "${cmd}"
    fi
}

__fp_std_shell_backend_run_kubectl_host_() {
    local host="$1"
    local cmd="$2"
    local context="$(__fp_std_shell_backend_host_context_ "${host}")"
    local namespace="$(__fp_std_shell_backend_host_namespace_ "${host}")"
    local container="$(__fp_std_shell_backend_host_container_ "${host}")"
    local pod="$(__fp_std_shell_backend_host_pod_ "${host}")"
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

__fp_std_shell_backend_run_chroot_host_() {
    local host="$1"
    local cmd="$2"
    chroot_exec "$(__fp_std_shell_backend_host_chroot_directory_ "${host}")" 'sh' '-lc' "${cmd}"
}

__fp_std_shell_backend_chroot_path_() {
    local host="$1"
    local path="$2"
    printf '%s\n' "$(__fp_std_shell_backend_host_chroot_directory_ "${host}")${path}"
}

__fp_std_shell_backend_rsync_remote_target_() {
    local host="$1"
    local address="$(__fp_std_shell_backend_host_address_ "${host}")"
    if [[ "${address}" == '' ]]; then
        runtime_fail "host is not rsync-reachable: missing address for ${host}"
        printf '%s\n' ''
    else
        local user="$(__fp_std_shell_backend_host_user_ "${host}")"
        if [[ "${user}" != '' ]]; then
            printf '%s\n' "${user}@${address}"
        else
            printf '%s\n' "${address}"
        fi
    fi
}

__fp_std_shell_backend_rsync_remote_host_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local remote="$(__fp_std_shell_backend_rsync_remote_target_ "${host}"):${dest}"
    local port="$(__fp_std_shell_backend_host_port_ "${host}")"
    if [[ "${port}" != '' ]]; then
        rsync_cli_shell "ssh -p ${port}" "${flags}" "${src}" "${remote}"
    else
        rsync_cli "${flags}" "${src}" "${remote}"
    fi
}

__fp_std_shell_backend_rsync_chroot_host_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local target="$(__fp_std_shell_backend_chroot_path_ "${host}" "${dest}")"
    rsync_cli "${flags}" "${src}" "${target}"
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

__fp_std_shell_backend_rsync_flag_string_() {
    local archive="$1"
    local compress="$2"
    local delete="$3"
    local checksum="$4"
    if [[ "${archive}" == 'true' ]]; then
        if [[ "${compress}" == 'true' ]]; then
            __fp_std_shell_backend_rsync_flag_string_suffix_ '-az' "${delete}" "${checksum}"
        else
            __fp_std_shell_backend_rsync_flag_string_suffix_ '-a' "${delete}" "${checksum}"
        fi
    else
        if [[ "${compress}" == 'true' ]]; then
            __fp_std_shell_backend_rsync_flag_string_suffix_ '-z' "${delete}" "${checksum}"
        else
            __fp_std_shell_backend_rsync_flag_string_suffix_ '' "${delete}" "${checksum}"
        fi
    fi
}

__fp_std_shell_backend_rsync_flag_string_suffix_() {
    local base="$1"
    local delete="$2"
    local checksum="$3"
    if [[ "${delete}" == 'true' ]]; then
        if [[ "${checksum}" == 'true' ]]; then
            if [[ "${base}" != '' ]]; then
                printf '%s\n' "${base} --delete --checksum"
            else
                printf '%s\n' '--delete --checksum'
            fi
        else
            if [[ "${base}" != '' ]]; then
                printf '%s\n' "${base} --delete"
            else
                printf '%s\n' '--delete'
            fi
        fi
    else
        if [[ "${checksum}" == 'true' ]]; then
            if [[ "${base}" != '' ]]; then
                printf '%s\n' "${base} --checksum"
            else
                printf '%s\n' '--checksum'
            fi
        else
            printf '%s\n' "${base}"
        fi
    fi
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

__fp_std_shell_backend_shell_run_() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    runtime_set_changed 'false'
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_host_ "${host}" "${command}"
        runtime_set_changed 'true'
    fi
}

__fp_std_shell_backend_shell_rsync_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    runtime_set_changed 'false'
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_rsync_host_ "${host}" "${flags}" "${src}" "${dest}"
        runtime_set_changed 'true'
    fi
}

__fp_std_shell_process_process_ok_() {
    local command="$1"
    shell_status "${command}"
}

local host='web-1'
__fp_std_ops_files_rsync_ './dist/' '/srv/fp-service/dist/' "${host}" 'true' 'true' 'true' 'true' '' '' '' ''
__fp_std_ops_server_shell_ 'sudo systemctl restart fp-service' "${host}" '' '' '' '' '' ''
