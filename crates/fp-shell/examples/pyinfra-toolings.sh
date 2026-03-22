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

__fp_std_facts_dir_exists_() {
    local path="$1"
    dir_exists_native "${path}"
}

__fp_std_facts_files_is_directory_() {
    local path="$1"
    __fp_std_facts_dir_exists_ "${path}"
}

__fp_std_facts_flatpak_packages_() {
    __fp_std_shell_process_process_output_ 'flatpak list --columns=application'
}

__fp_std_facts_gem_packages_() {
    __fp_std_shell_process_process_output_ 'gem list --local'
}

__fp_std_facts_server_which_() {
    local command="$1"
    __fp_std_shell_process_process_output_ "command -v ${command}"
}

__fp_std_ops_flatpak_packages_() {
    local packages="$1"
    local hosts="$2"
    local remote="$3"
    local present="$4"
    local sudo="$5"
    if [[ "${present}" == 'true' ]]; then
        if [[ "${remote}" == '' ]]; then
            __fp_std_ops_server_shell_ "flatpak install --noninteractive ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "flatpak install --noninteractive ${remote} ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "flatpak uninstall --noninteractive ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_git_pull_() {
    local path="$1"
    local hosts="$2"
    local rebase="$3"
    local sudo="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    if [[ "${rebase}" == 'true' ]]; then
        __fp_std_ops_server_shell_ 'git pull --rebase' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" "${path}"
    else
        __fp_std_ops_server_shell_ 'git pull' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" "${path}"
    fi
}

__fp_std_ops_mysql_user_() {
    local user="$1"
    local hosts="$2"
    local present="$3"
    local user_hostname="$4"
    local password="$5"
    local privileges="$6"
    local mysql_user="$7"
    local mysql_password="$8"
    local mysql_host="$9"
    local mysql_port="$10"
    local sudo="$11"
    local auth="$(__fp_std_ops_mysql_mysql_auth_args_ '' "${mysql_user}" "${mysql_password}" "${mysql_host}" "${mysql_port}")"
    if [[ "${present}" == 'true' ]]; then
        __fp_std_ops_server_shell_ "mysql ${auth} -Be \"CREATE USER IF NOT EXISTS '${user}'@'${user_hostname}' IDENTIFIED BY '${password}'\"" "${hosts}" '' '' '' '' "${sudo}" ''
        if [[ "${privileges}" != '' ]]; then
            __fp_std_ops_server_shell_ "mysql ${auth} -Be \"GRANT ${privileges} ON *.* TO '${user}'@'${user_hostname}'\"" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
        printf '%s\n' 'true'
    else
        __fp_std_ops_server_shell_ "mysql ${auth} -Be \"DROP USER '${user}'@'${user_hostname}'\"" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_mysql_mysql_auth_args_() {
    local database="$1"
    local user="$2"
    local password="$3"
    local host="$4"
    local port="$5"
    if [[ "${database}" == '' ]]; then
        if [[ "${user}" == '' ]]; then
            if [[ "${password}" == '' ]]; then
                if [[ "${host}" == '' ]]; then
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' ''
                    else
                        printf '%s\n' "-P${port}"
                    fi
                else
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' "-h${host}"
                    else
                        printf '%s\n' "-h${host} -P${port}"
                    fi
                fi
            else
                if [[ "${host}" == '' ]]; then
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' "-p\\\"${password}\\\""
                    else
                        printf '%s\n' "-p\\\"${password}\\\" -P${port}"
                    fi
                else
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' "-p\\\"${password}\\\" -h${host}"
                    else
                        printf '%s\n' "-p\\\"${password}\\\" -h${host} -P${port}"
                    fi
                fi
            fi
        else
            if [[ "${password}" == '' ]]; then
                if [[ "${host}" == '' ]]; then
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' "-u\\\"${user}\\\""
                    else
                        printf '%s\n' "-u\\\"${user}\\\" -P${port}"
                    fi
                else
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' "-u\\\"${user}\\\" -h${host}"
                    else
                        printf '%s\n' "-u\\\"${user}\\\" -h${host} -P${port}"
                    fi
                fi
            else
                if [[ "${host}" == '' ]]; then
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' "-u\\\"${user}\\\" -p\\\"${password}\\\""
                    else
                        printf '%s\n' "-u\\\"${user}\\\" -p\\\"${password}\\\" -P${port}"
                    fi
                else
                    if [[ "${port}" == '' ]]; then
                        printf '%s\n' "-u\\\"${user}\\\" -p\\\"${password}\\\" -h${host}"
                    else
                        printf '%s\n' "-u\\\"${user}\\\" -p\\\"${password}\\\" -h${host} -P${port}"
                    fi
                fi
            fi
        fi
    else
        local base="$(__fp_std_ops_mysql_mysql_auth_args_ '' "${user}" "${password}" "${host}" "${port}")"
        if [[ "${base}" == '' ]]; then
            printf '%s\n' "${database}"
        else
            printf '%s\n' "${database} ${base}"
        fi
    fi
}

__fp_std_ops_pipx_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local extra_args="$5"
    local sudo="$6"
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            if [[ "${extra_args}" == '' ]]; then
                __fp_std_ops_server_shell_ "pipx upgrade ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            else
                __fp_std_ops_server_shell_ "pipx upgrade ${extra_args} ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        else
            if [[ "${extra_args}" == '' ]]; then
                __fp_std_ops_server_shell_ "pipx install ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            else
                __fp_std_ops_server_shell_ "pipx install ${extra_args} ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        fi
    else
        __fp_std_ops_server_shell_ "pipx uninstall ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_pipx_ensure_path_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'pipx ensurepath' "${hosts}" '' '' '' '' "${sudo}" ''
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

__fp_std_ops_server_utils_wait_() {
    local port="$1"
    local hosts="$2"
    local interval="$3"
    local sudo="$4"
    if [[ "${interval}" == '' ]]; then
        __fp_std_ops_server_shell_ "while ! (netstat -an | grep LISTEN | grep -e '\\\\.${port}' -e ':${port}'); do echo waiting for port ${port}; sleep 1; done" "${hosts}" '' '' '' '' "${sudo}" ''
    else
        __fp_std_ops_server_shell_ "while ! (netstat -an | grep LISTEN | grep -e '\\\\.${port}' -e ':${port}'); do echo waiting for port ${port}; sleep ${interval}; done" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_snap_packages_() {
    local packages="$1"
    local hosts="$2"
    local channel="$3"
    local classic="$4"
    local present="$5"
    local latest="$6"
    local sudo="$7"
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "snap refresh ${packages} --channel=${channel}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            if [[ "${classic}" == 'true' ]]; then
                __fp_std_ops_server_shell_ "snap install --classic ${packages} --channel=${channel}" "${hosts}" '' '' '' '' "${sudo}" ''
            else
                __fp_std_ops_server_shell_ "snap install ${packages} --channel=${channel}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        fi
    else
        __fp_std_ops_server_shell_ "snap remove ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_systemd_service_() {
    local service="$1"
    local hosts="$2"
    local running="$3"
    local restarted="$4"
    local reloaded="$5"
    local enabled="$6"
    local daemon_reload="$7"
    local sudo="$8"
    local only_if="$9"
    local unless="$10"
    local creates="$11"
    local removes="$12"
    local service="$(__fp_std_ops_systemd_normalize_service_ "${service}")"
    if [[ "${daemon_reload}" == 'true' ]]; then
        __fp_std_ops_server_shell_ 'systemctl daemon-reload' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
    fi
    if [[ "${restarted}" == 'true' ]]; then
        __fp_std_ops_server_shell_ "systemctl restart ${service}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
    else
        if [[ "${reloaded}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "systemctl reload ${service}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
        else
            if [[ "${running}" == 'true' ]]; then
                __fp_std_ops_server_shell_ "systemctl start ${service}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
            else
                __fp_std_ops_server_shell_ "systemctl stop ${service}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
            fi
        fi
    fi
    if [[ "${enabled}" == 'true' ]]; then
        __fp_std_ops_server_shell_ "systemctl enable ${service}" "${hosts}" '' '' '' '' "${sudo}" ''
    else
        __fp_std_ops_server_shell_ "systemctl disable ${service}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
    printf '%s\n' 'true'
}

__fp_std_ops_systemd_normalize_service_() {
    local service="$1"
    if __fp_std_shell_process_process_ok_ "printf '%s' ${service} | grep -Eq '\\.(service|socket|device|mount|automount|swap|target|path|timer|slice|scope)$'"; then
        printf '%s\n' "${service}"
    else
        printf '%s\n' "${service}.service"
    fi
}

__fp_std_ops_zypper_update_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'zypper update -y' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_zypper_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local update="$5"
    local clean="$6"
    local sudo="$7"
    if [[ "${clean}" == 'true' ]]; then
        __fp_std_ops_server_shell_ 'zypper clean --all' "${hosts}" '' '' '' '' "${sudo}" ''
    fi
    if [[ "${update}" == 'true' ]]; then
        __fp_std_ops_zypper_update_ "${hosts}" "${sudo}"
    fi
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "zypper update -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "zypper --non-interactive install -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "zypper --non-interactive remove -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_shell_backend_host_transport_() {
    local host="$1"
}

__fp_std_shell_backend_host_address_() {
    local host="$1"
}

__fp_std_shell_backend_host_user_() {
    local host="$1"
    __fp_std_ops_mysql_user_ "${host}" 'localhost' 'true' '' '' '' '' '' '' '' ''
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

__fp_std_shell_process_process_ok_() {
    local command="$1"
    shell_status "${command}"
}

__fp_std_shell_process_process_output_() {
    local command="$1"
    shell_output "${command}"
}

if [[ "$(__fp_std_facts_server_which_ 'git')" != '' ]]; then
    __fp_std_ops_server_shell_local_ 'echo git present' 'localhost' '' '' '' '' 'false' ''
fi
if [[ "$(__fp_std_facts_server_which_ 'pipx')" != '' ]]; then
    __fp_std_ops_pipx_ensure_path_ 'localhost' ''
    __fp_std_ops_pipx_packages_ 'ruff' 'localhost' 'true' 'true' '' ''
fi
if __fp_std_facts_files_is_directory_ '/srv'; then
    __fp_std_ops_server_utils_wait_ '22' 'localhost' '' ''
fi
local gems="$(__fp_std_facts_gem_packages_)"
__fp_std_ops_server_shell_local_ "${gems}" 'localhost' '' '' '' '' 'false' ''
local flatpaks="$(__fp_std_facts_flatpak_packages_)"
__fp_std_ops_server_shell_local_ "${flatpaks}" 'localhost' '' '' '' '' 'false' ''
__fp_std_ops_git_pull_ '/srv/fp-service' 'web-1' 'true' '' '' '' '' ''
__fp_std_ops_flatpak_packages_ 'org.videolan.VLC' 'web-1' 'flathub' 'true' ''
__fp_std_ops_snap_packages_ 'lxd' 'web-1' '4.0/stable' '' 'true' 'true' ''
__fp_std_ops_zypper_packages_ 'vim' 'web-1' 'true' '' '' 'true' 'true'
__fp_std_ops_systemd_service_ 'fp-service' 'web-1' 'true' 'true' '' 'true' '' '' '' '' '' ''
