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

shell_run() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    runtime_set_changed 'false'
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
            runtime_set_changed 'true'
        fi
    else
        run_host "${host}" "${command}"
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

shell 'uname -a' 'web-1' '' '' '' '' '' ''
shell 'sudo systemctl status fp-service' 'web-1' '' '' '' '' '' ''
shell 'uname -a' 'web-2' '' '' '' '' '' ''
shell 'sudo systemctl status fp-service' 'web-2' '' '' '' '' '' ''
shell 'uname -a' 'web-3' '' '' '' '' '' ''
shell 'sudo systemctl status fp-service' 'web-3' '' '' '' '' '' ''
