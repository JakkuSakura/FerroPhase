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

FP_HOST_TRANSPORT['ssh-web']='ssh'
FP_SSH_ADDRESS['ssh-web']='10.0.0.11'
FP_WINRM_ADDRESS['ssh-web']='10.0.0.11'
FP_SSH_USER['ssh-web']='deploy'
FP_DOCKER_USER['ssh-web']='deploy'
FP_WINRM_USER['ssh-web']='deploy'
FP_SSH_PORT['ssh-web']='22'
FP_WINRM_PORT['ssh-web']='22'
FP_HOST_TRANSPORT['docker-app']='docker'
FP_SSH_USER['docker-app']='root'
FP_DOCKER_USER['docker-app']='root'
FP_WINRM_USER['docker-app']='root'
FP_DOCKER_CONTAINER['docker-app']='app'
FP_K8S_CONTAINER['docker-app']='app'
FP_HOST_TRANSPORT['windows-admin']='winrm'
FP_SSH_ADDRESS['windows-admin']='10.0.0.21'
FP_WINRM_ADDRESS['windows-admin']='10.0.0.21'
FP_SSH_USER['windows-admin']='Administrator'
FP_DOCKER_USER['windows-admin']='Administrator'
FP_WINRM_USER['windows-admin']='Administrator'
FP_SSH_PORT['windows-admin']='5985'
FP_WINRM_PORT['windows-admin']='5985'
FP_WINRM_PASSWORD['windows-admin']='change-me'
FP_WINRM_SCHEME['windows-admin']='http'
FP_HOST_TRANSPORT['localhost']='local'
FP_HOST_TRANSPORT['k8s-api']='kubectl'
FP_DOCKER_CONTAINER['k8s-api']='api'
FP_K8S_CONTAINER['k8s-api']='api'
FP_K8S_POD['k8s-api']='api-7f9f6'
FP_K8S_NAMESPACE['k8s-api']='prod'
FP_K8S_CONTEXT['k8s-api']='prod-cluster'

SSH_CONTROL_PATH="${TMPDIR:-/tmp}/fp-shell-%r@%h:%p"

fp_validate_runtime() {
  command -v 'bash' >/dev/null 2>&1 || { echo "missing required command: bash" >&2; exit 1; }
  command -v 'command' >/dev/null 2>&1 || { echo "missing required command: command" >&2; exit 1; }
  command -v 'docker' >/dev/null 2>&1 || { echo "missing required command: docker" >&2; exit 1; }
  command -v 'envsubst' >/dev/null 2>&1 || { echo "missing required command: envsubst" >&2; exit 1; }
  command -v 'kubectl' >/dev/null 2>&1 || { echo "missing required command: kubectl" >&2; exit 1; }
  command -v 'mktemp' >/dev/null 2>&1 || { echo "missing required command: mktemp" >&2; exit 1; }
  command -v 'pwsh' >/dev/null 2>&1 || { echo "missing required command: pwsh" >&2; exit 1; }
  command -v 'rm' >/dev/null 2>&1 || { echo "missing required command: rm" >&2; exit 1; }
  command -v 'rsync' >/dev/null 2>&1 || { echo "missing required command: rsync" >&2; exit 1; }
  command -v 'scp' >/dev/null 2>&1 || { echo "missing required command: scp" >&2; exit 1; }
  command -v 'ssh' >/dev/null 2>&1 || { echo "missing required command: ssh" >&2; exit 1; }
  command -v 'test' >/dev/null 2>&1 || { echo "missing required command: test" >&2; exit 1; }
}

fp_validate_runtime


ssh_cmd() {
  ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

scp_cmd() {
  scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath="$SSH_CONTROL_PATH" -- "$@"
}

rsync_cmd() {
  rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "$@"
}

winrm_pwsh() {
  local host="$1"
  local mode="$2"
  local command="${3:-}"
  local source="${4:-}"
  local destination="${5:-}"
  local address="${FP_WINRM_ADDRESS[$host]}"
  local user="${FP_WINRM_USER[$host]}"
  local password="${FP_WINRM_PASSWORD[$host]:-}"
  local scheme="${FP_WINRM_SCHEME[$host]:-http}"
  local port="${FP_WINRM_PORT[$host]:-}"

  if [[ -z "$password" ]]; then
    echo "winrm password is required for non-interactive bash target: $host" >&2
    return 1
  fi

  FP_WINRM_ADDRESS="$address" \
  FP_WINRM_USER="$user" \
  FP_WINRM_PASSWORD="$password" \
  FP_WINRM_SCHEME="$scheme" \
  FP_WINRM_PORT="$port" \
  FP_WINRM_MODE="$mode" \
  FP_WINRM_COMMAND="$command" \
  FP_WINRM_SOURCE="$source" \
  FP_WINRM_DESTINATION="$destination" \
  pwsh -NoProfile -NonInteractive -Command '
$ErrorActionPreference = "Stop"
$sessionArgs = @{
    ComputerName = $env:FP_WINRM_ADDRESS
}
if ($env:FP_WINRM_PORT) {
    $sessionArgs.Port = [int]$env:FP_WINRM_PORT
}
$scheme = if ([string]::IsNullOrWhiteSpace($env:FP_WINRM_SCHEME)) {
    "http"
} else {
    $env:FP_WINRM_SCHEME.ToLowerInvariant()
}
switch ($scheme) {
    "http" {}
    "https" { $sessionArgs.UseSSL = $true }
    default { throw "unsupported winrm scheme: $($env:FP_WINRM_SCHEME)" }
}
$securePassword = ConvertTo-SecureString $env:FP_WINRM_PASSWORD -AsPlainText -Force
$credential = New-Object System.Management.Automation.PSCredential($env:FP_WINRM_USER, $securePassword)
$session = New-PSSession -Credential $credential @sessionArgs
try {
    switch ($env:FP_WINRM_MODE) {
        "run" {
            Invoke-Command -Session $session -ScriptBlock ([scriptblock]::Create($env:FP_WINRM_COMMAND))
        }
        "copy" {
            $remoteDestination = $env:FP_WINRM_DESTINATION
            $remoteDirectory = [System.IO.Path]::GetDirectoryName($remoteDestination)
            if ($remoteDirectory) {
                Invoke-Command -Session $session -ScriptBlock {
                    param([string]$Directory)
                    [System.IO.Directory]::CreateDirectory($Directory) | Out-Null
                } -ArgumentList $remoteDirectory
            }
            Copy-Item -ToSession $session -Path $env:FP_WINRM_SOURCE -Destination $remoteDestination -Force
        }
        default {
            throw "unsupported winrm mode: $($env:FP_WINRM_MODE)"
        }
    }
}
finally {
    if ($null -ne $session) {
        Remove-PSSession -Session $session
    }
}
'
}
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

copy_local_host() {
    local src="$1"
    local dest="$2"
    copy_item "${src}" "${dest}"
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
            winrm_pwsh "${host}" run "${cmd}"
            ;;
        *)
            echo "unsupported transport: ${transport}" >&2; return 1
            ;;
    esac
}

copy_host() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local transport="$(host_transport "${host}")"
    case "${transport}" in
        local)
            copy_local_host "${src}" "${dest}"
            ;;
        ssh)
            copy_ssh_host "${host}" "${src}" "${dest}"
            ;;
        docker)
            copy_docker_host "${host}" "${src}" "${dest}"
            ;;
        kubectl)
            copy_kubectl_host "${host}" "${src}" "${dest}"
            ;;
        winrm)
            winrm_pwsh "${host}" copy "${src}" "${dest}"
            ;;
        *)
            echo "unsupported transport for copy: ${transport}" >&2; return 1
            ;;
    esac
}

template_host() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local tmp="$(mktemp)"
    eval '${vars} envsubst < ${src} > ${tmp}'
    copy_host "${host}" "${tmp}" "${dest}"
    rm -f "${tmp}"
}

rsync_host() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local transport="$(host_transport "${host}")"
    case "${transport}" in
        local)
            rsync "${flags}" "${src}" "${dest}" '' '' '' '' '' '' '' ''
            ;;
        *)
            rsync_remote_host "${host}" "${flags}" "${src}" "${dest}"
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
        ssh -p "${port}" "${target}" "${cmd}"
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
        scp -P "${port}" "${src}" "${remote}"
    else
        scp "${src}" "${remote}"
    fi
}

run_docker_host() {
    local host="$1"
    local cmd="$2"
    local container="$(host_container "${host}")"
    local user="$(host_user "${host}")"
    if [[ "${user}" != '' ]]; then
        docker exec --user "${user}" "${container}" 'sh' '-lc' "${cmd}"
    else
        docker exec "${container}" 'sh' '-lc' "${cmd}"
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
                            kubectl exec "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl exec -c "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                    esac
                    ;;
                *)
                    case "${container}" in
                        )
                            kubectl -n "${namespace}" 'exec' "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl -n "${namespace}" 'exec' '-c' "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
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
                            kubectl --context "${context}" 'exec' "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl --context "${context}" 'exec' '-c' "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                    esac
                    ;;
                *)
                    case "${container}" in
                        )
                            kubectl --context "${context}" '-n' "${namespace}" 'exec' "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                        *)
                            kubectl --context "${context}" '-n' "${namespace}" 'exec' '-c' "${container}" "${pod}" '--' 'sh' '-lc' "${cmd}"
                            ;;
                    esac
                    ;;
            esac
            ;;
    esac
}

copy_docker_host() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local container="$(host_container "${host}")"
    docker cp "${src}" "${container}:${dest}"
}

copy_kubectl_host() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local context="$(host_context "${host}")"
    local namespace="$(host_namespace "${host}")"
    local remote="$(host_pod "${host}"):${dest}"
    case "${context}" in
        )
            case "${namespace}" in
                )
                    kubectl cp "${src}" "${remote}"
                    ;;
                *)
                    kubectl -n "${namespace}" 'cp' "${src}" "${remote}"
                    ;;
            esac
            ;;
        *)
            case "${namespace}" in
                )
                    kubectl --context "${context}" 'cp' "${src}" "${remote}"
                    ;;
                *)
                    kubectl --context "${context}" '-n' "${namespace}" 'cp' "${src}" "${remote}"
                    ;;
            esac
            ;;
    esac
}

rsync_remote_target() {
    local host="$1"
    local address="$(host_address "${host}")"
    if [[ "${address}" == '' ]]; then
        echo "host is not rsync-reachable: missing address for ${host}" >&2; return 1
        printf '%s\n' ''
    else
        local user="$(host_user "${host}")"
        if [[ "${user}" != '' ]]; then
            printf '%s\n' "${user}@${address}"
        else
            printf '%s\n' "${address}"
        fi
    fi
}

rsync_remote_host() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local remote="$(rsync_remote_target "${host}"):${dest}"
    local port="$(host_port "${host}")"
    if [[ "${port}" != '' ]]; then
        rsync -e "ssh -p ${port}" "${flags}" "${src}" "${remote}"
    else
        rsync "${flags}" "${src}" "${remote}" '' '' '' '' '' '' '' ''
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

rsync_flag_string() {
    local archive="$1"
    local compress="$2"
    local delete="$3"
    local checksum="$4"
    if [[ "${archive}" == 'true' ]]; then
        if [[ "${compress}" == 'true' ]]; then
            rsync_flag_string_suffix '-az' "${delete}" "${checksum}"
        else
            rsync_flag_string_suffix '-a' "${delete}" "${checksum}"
        fi
    else
        if [[ "${compress}" == 'true' ]]; then
            rsync_flag_string_suffix '-z' "${delete}" "${checksum}"
        else
            rsync_flag_string_suffix '' "${delete}" "${checksum}"
        fi
    fi
}

rsync_flag_string_suffix() {
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

shell_run() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_last_changed=0
    if [[ "${only_if}" != '' ]]; then
        if process_ok "${only_if}"; then
            shell_run_after_only_if "${host}" "${command}" "${unless}" "${creates}" "${removes}"
        fi
    else
        shell_run_after_only_if "${host}" "${command}" "${unless}" "${creates}" "${removes}"
    fi
}

shell_run_after_only_if() {
    local host="$1"
    local command="$2"
    local unless="$3"
    local creates="$4"
    local removes="$5"
    if [[ "${unless}" != '' ]]; then
        if true; then
            shell_run_after_unless "${host}" "${command}" "${creates}" "${removes}"
        fi
    else
        shell_run_after_unless "${host}" "${command}" "${creates}" "${removes}"
    fi
}

shell_run_after_unless() {
    local host="$1"
    local command="$2"
    local creates="$3"
    local removes="$4"
    if [[ "${creates}" != '' ]]; then
        if process_ok "test ! -e ${creates}"; then
            shell_run_after_creates "${host}" "${command}" "${removes}"
        fi
    else
        shell_run_after_creates "${host}" "${command}" "${removes}"
    fi
}

shell_run_after_creates() {
    local host="$1"
    local command="$2"
    local removes="$3"
    if [[ "${removes}" != '' ]]; then
        if process_ok "test -e ${removes}"; then
            run_host "${host}" "${command}"
            __fp_last_changed=1
        fi
    else
        run_host "${host}" "${command}"
        __fp_last_changed=1
    fi
}

shell_copy() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if [[ "${only_if}" != '' ]]; then
        if process_ok "${only_if}"; then
            shell_copy_after_only_if "${host}" "${src}" "${dest}" "${unless}" "${creates}" "${removes}"
        fi
    else
        shell_copy_after_only_if "${host}" "${src}" "${dest}" "${unless}" "${creates}" "${removes}"
    fi
}

shell_copy_after_only_if() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    if [[ "${unless}" != '' ]]; then
        if true; then
            shell_copy_after_unless "${host}" "${src}" "${dest}" "${creates}" "${removes}"
        fi
    else
        shell_copy_after_unless "${host}" "${src}" "${dest}" "${creates}" "${removes}"
    fi
}

shell_copy_after_unless() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local creates="$4"
    local removes="$5"
    if [[ "${creates}" != '' ]]; then
        if process_ok "test ! -e ${creates}"; then
            shell_copy_after_creates "${host}" "${src}" "${dest}" "${removes}"
        fi
    else
        shell_copy_after_creates "${host}" "${src}" "${dest}" "${removes}"
    fi
}

shell_copy_after_creates() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local removes="$4"
    if [[ "${removes}" != '' ]]; then
        if process_ok "test -e ${removes}"; then
            copy_host "${host}" "${src}" "${dest}"
            __fp_last_changed=1
        fi
    else
        copy_host "${host}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

shell_template() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if [[ "${only_if}" != '' ]]; then
        if process_ok "${only_if}"; then
            shell_template_after_only_if "${host}" "${src}" "${dest}" "${vars}" "${unless}" "${creates}" "${removes}"
        fi
    else
        shell_template_after_only_if "${host}" "${src}" "${dest}" "${vars}" "${unless}" "${creates}" "${removes}"
    fi
}

shell_template_after_only_if() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    if [[ "${unless}" != '' ]]; then
        if true; then
            shell_template_after_unless "${host}" "${src}" "${dest}" "${vars}" "${creates}" "${removes}"
        fi
    else
        shell_template_after_unless "${host}" "${src}" "${dest}" "${vars}" "${creates}" "${removes}"
    fi
}

shell_template_after_unless() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local creates="$5"
    local removes="$6"
    if [[ "${creates}" != '' ]]; then
        if process_ok "test ! -e ${creates}"; then
            shell_template_after_creates "${host}" "${src}" "${dest}" "${vars}" "${removes}"
        fi
    else
        shell_template_after_creates "${host}" "${src}" "${dest}" "${vars}" "${removes}"
    fi
}

shell_template_after_creates() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local removes="$5"
    if [[ "${removes}" != '' ]]; then
        if process_ok "test -e ${removes}"; then
            template_host "${host}" "${src}" "${dest}" "${vars}"
            __fp_last_changed=1
        fi
    else
        template_host "${host}" "${src}" "${dest}" "${vars}"
        __fp_last_changed=1
    fi
}

shell_rsync() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if [[ "${only_if}" != '' ]]; then
        if process_ok "${only_if}"; then
            shell_rsync_after_only_if "${host}" "${flags}" "${src}" "${dest}" "${unless}" "${creates}" "${removes}"
        fi
    else
        shell_rsync_after_only_if "${host}" "${flags}" "${src}" "${dest}" "${unless}" "${creates}" "${removes}"
    fi
}

shell_rsync_after_only_if() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    if [[ "${unless}" != '' ]]; then
        if true; then
            shell_rsync_after_unless "${host}" "${flags}" "${src}" "${dest}" "${creates}" "${removes}"
        fi
    else
        shell_rsync_after_unless "${host}" "${flags}" "${src}" "${dest}" "${creates}" "${removes}"
    fi
}

shell_rsync_after_unless() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local creates="$5"
    local removes="$6"
    if [[ "${creates}" != '' ]]; then
        if process_ok "test ! -e ${creates}"; then
            shell_rsync_after_creates "${host}" "${flags}" "${src}" "${dest}" "${removes}"
        fi
    else
        shell_rsync_after_creates "${host}" "${flags}" "${src}" "${dest}" "${removes}"
    fi
}

shell_rsync_after_creates() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local removes="$5"
    if [[ "${removes}" != '' ]]; then
        if process_ok "test -e ${removes}"; then
            rsync_host "${host}" "${flags}" "${src}" "${dest}"
            __fp_last_changed=1
        fi
    else
        rsync_host "${host}" "${flags}" "${src}" "${dest}"
        __fp_last_changed=1
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

copy() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    shell_copy "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

template() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    shell_template "${hosts}" "${src}" "${dest}" "${vars}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

rsync() {
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
    local flags="$(rsync_flag_string "${archive}" "${compress}" "${delete}" "${checksum}")"
    shell_rsync "${hosts}" "${flags}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
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

has_command() {
    local command="$1"
    command -v "${command}"
}

file_exists() {
    local path="$1"
    test -f "${path}"
}

dir_exists() {
    local path="$1"
    test -d "${path}"
}

path_exists() {
    local path="$1"
    test -e "${path}"
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

has_rsync() {
    has_command 'rsync'
}

has_ssh() {
    has_command 'ssh'
}

has_docker() {
    has_command 'docker'
}

has_kubectl() {
    has_command 'kubectl'
}

has_pwsh() {
    has_command 'pwsh'
}

host_supports_rsync() {
    local host="$1"
    case "$(transport "${host}")" in
        ssh)
            ;;
        docker)
            ;;
        kubectl)
            ;;
        winrm)
            ;;
        local)
            has_rsync 
            ;;
        *)
            printf '%s\n' 'false'
            ;;
    esac
}

raw() {
    local text="$1"
    printf '%s\n' "${text}"
}

pipe() {
    local lhs="$1"
    local rhs="$2"
    printf '%s\n' "${lhs} | ${rhs}"
}

stdout_to() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} > ${path}"
}

stdout_append() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} >> ${path}"
}

stderr_to() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} 2> ${path}"
}

stderr_append() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} 2>> ${path}"
}

run() {
    local command="$1"
    shell "${command}" '' '' '' '' '' '' ''
}

ok() {
    local command="$1"
    bash -lc "${command}"
}

shell 'echo local hello' '' '' '' '' '' '' ''
shell 'echo ssh hello' 'ssh-web' '' '' '' '' '' ''
shell 'echo docker hello' 'docker-app' '' '' '' '' '' ''
shell 'echo kubectl hello' 'k8s-api' '' '' '' '' '' ''
shell 'Write-Host winrm hello' 'windows-admin' '' '' '' '' '' ''
