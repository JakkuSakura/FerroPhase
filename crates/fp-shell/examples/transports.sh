#!/usr/bin/env bash
set -euo pipefail

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

FP_HOST_TRANSPORT['k8s-api']='kubectl'
FP_K8S_POD['k8s-api']='api-7f9f6'
FP_K8S_NAMESPACE['k8s-api']='prod'
FP_K8S_CONTAINER['k8s-api']='api'
FP_K8S_CONTEXT['k8s-api']='prod-cluster'
FP_HOST_TRANSPORT['ssh-web']='ssh'
FP_SSH_ADDRESS['ssh-web']='10.0.0.11'
FP_SSH_USER['ssh-web']='deploy'
FP_SSH_PORT['ssh-web']='22'
FP_HOST_TRANSPORT['windows-admin']='winrm'
FP_WINRM_ADDRESS['windows-admin']='10.0.0.21'
FP_WINRM_USER['windows-admin']='Administrator'
FP_WINRM_PASSWORD['windows-admin']='change-me'
FP_WINRM_PORT['windows-admin']='5985'
FP_WINRM_SCHEME['windows-admin']='http'
FP_HOST_TRANSPORT['localhost']='local'
FP_HOST_TRANSPORT['docker-app']='docker'
FP_DOCKER_CONTAINER['docker-app']='app'
FP_DOCKER_USER['docker-app']='root'

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
run_host() {
    local host="$1"
    local cmd="$2"
    local transport="${FP_HOST_TRANSPORT["${host}"]:-ssh}"
    if [[ "${transport}" == 'local' ]]; then
        bash -lc "${cmd}"
    else
        if [[ "${transport}" == 'ssh' ]]; then
            __fp_host="${host}"
            __fp_address="${FP_SSH_ADDRESS[$__fp_host]:-$__fp_host}"
            __fp_user="${FP_SSH_USER[$__fp_host]:-}"
            __fp_port="${FP_SSH_PORT[$__fp_host]:-}"
            __fp_target="$__fp_address"
            if [[ -n "$__fp_user" ]]; then
                __fp_target="$__fp_user@$__fp_target"
            fi
            if [[ -n "$__fp_port" ]]; then
                ssh_cmd -p "$__fp_port" "$__fp_target" "${cmd}"
            else
                ssh_cmd "$__fp_target" "${cmd}"
            fi
        else
            if [[ "${transport}" == 'docker' ]]; then
                __fp_host="${host}"
                __fp_container="${FP_DOCKER_CONTAINER[$__fp_host]}"
                __fp_user="${FP_DOCKER_USER[$__fp_host]:-}"
                if [[ -n "$__fp_user" ]]; then
                    docker exec --user "$__fp_user" "$__fp_container" sh -lc "${cmd}"
                else
                    docker exec "$__fp_container" sh -lc "${cmd}"
                fi
            else
                if [[ "${transport}" == 'kubectl' ]]; then
                    __fp_host="${host}"
                    __fp_kubectl_args=()
                    __fp_context="${FP_K8S_CONTEXT[$__fp_host]:-}"
                    __fp_namespace="${FP_K8S_NAMESPACE[$__fp_host]:-}"
                    __fp_container="${FP_K8S_CONTAINER[$__fp_host]:-}"
                    __fp_pod="${FP_K8S_POD[$__fp_host]}"
                    [[ -n "$__fp_context" ]] && __fp_kubectl_args+=(--context "$__fp_context")
                    [[ -n "$__fp_namespace" ]] && __fp_kubectl_args+=(-n "$__fp_namespace")
                    __fp_kubectl_args+=(exec)
                    [[ -n "$__fp_container" ]] && __fp_kubectl_args+=(-c "$__fp_container")
                    __fp_kubectl_args+=("$__fp_pod" -- sh -lc "${cmd}")
                    kubectl "${__fp_kubectl_args[@]}"
                else
                    if [[ "${transport}" == 'winrm' ]]; then
                        winrm_pwsh "${host}" run "${cmd}"
                    else
                        echo "unsupported transport: ${transport}" >&2
                        return 1
                    fi
                fi
            fi
        fi
    fi
}

copy_host() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local transport="${FP_HOST_TRANSPORT["${host}"]:-ssh}"
    if [[ "${transport}" == 'local' ]]; then
        cp -- "${src}" "${dest}"
    else
        if [[ "${transport}" == 'ssh' ]]; then
            __fp_host="${host}"
            __fp_address="${FP_SSH_ADDRESS[$__fp_host]:-$__fp_host}"
            __fp_user="${FP_SSH_USER[$__fp_host]:-}"
            __fp_port="${FP_SSH_PORT[$__fp_host]:-}"
            __fp_target="$__fp_address"
            if [[ -n "$__fp_user" ]]; then
                __fp_target="$__fp_user@$__fp_target"
            fi
            __fp_remote_path="${dest}"
            if [[ -n "$__fp_port" ]]; then
                scp_cmd -P "$__fp_port" "${src}" "$__fp_target:$__fp_remote_path"
            else
                scp_cmd "${src}" "$__fp_target:$__fp_remote_path"
            fi
        else
            if [[ "${transport}" == 'docker' ]]; then
                __fp_host="${host}"
                __fp_container="${FP_DOCKER_CONTAINER[$__fp_host]}"
                __fp_remote_path="${dest}"
                docker cp "${src}" "$__fp_container:$__fp_remote_path"
            else
                if [[ "${transport}" == 'kubectl' ]]; then
                    __fp_host="${host}"
                    __fp_kubectl_args=()
                    __fp_context="${FP_K8S_CONTEXT[$__fp_host]:-}"
                    __fp_namespace="${FP_K8S_NAMESPACE[$__fp_host]:-}"
                    __fp_pod="${FP_K8S_POD[$__fp_host]}"
                    __fp_remote_path="${dest}"
                    [[ -n "$__fp_context" ]] && __fp_kubectl_args+=(--context "$__fp_context")
                    [[ -n "$__fp_namespace" ]] && __fp_kubectl_args+=(-n "$__fp_namespace")
                    kubectl cp "${__fp_kubectl_args[@]}" "${src}" "$__fp_pod:$__fp_remote_path"
                else
                    if [[ "${transport}" == 'winrm' ]]; then
                        winrm_pwsh "${host}" copy "${src}" "${dest}"
                    else
                        echo "unsupported transport for copy: ${transport}" >&2
                        return 1
                    fi
                fi
            fi
        fi
    fi
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
    local transport="${FP_HOST_TRANSPORT["${host}"]:-ssh}"
    if [[ "${transport}" == 'ssh' ]]; then
        __fp_host="${host}"
        __fp_address="${FP_SSH_ADDRESS[$__fp_host]:-$__fp_host}"
        __fp_user="${FP_SSH_USER[$__fp_host]:-}"
        __fp_port="${FP_SSH_PORT[$__fp_host]:-}"
        __fp_target="$__fp_address"
        if [[ -n "$__fp_user" ]]; then
            __fp_target="$__fp_user@$__fp_target"
        fi
        __fp_remote_path="${dest}"
        rsync_cmd ${flags} -- "${src}" "$__fp_target:$__fp_remote_path"
    else
        echo "rsync is only supported for ssh in bash target, got: ${transport}" >&2
        return 1
    fi
}

__fp_changed=0
echo local hello
__fp_changed=1
__fp_last_changed="$__fp_changed"
__fp_changed=0
run_host 'ssh-web' 'echo ssh hello'
__fp_changed=1
__fp_last_changed="$__fp_changed"
__fp_changed=0
run_host 'docker-app' 'echo docker hello'
__fp_changed=1
__fp_last_changed="$__fp_changed"
__fp_changed=0
run_host 'k8s-api' 'echo kubectl hello'
__fp_changed=1
__fp_last_changed="$__fp_changed"
__fp_changed=0
run_host 'windows-admin' 'Write-Host winrm hello'
__fp_changed=1
__fp_last_changed="$__fp_changed"
