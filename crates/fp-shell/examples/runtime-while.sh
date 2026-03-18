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
FP_SSH_USER['web-2']='deploy'
FP_HOST_TRANSPORT['web-1']='ssh'
FP_SSH_ADDRESS['web-1']='10.0.0.11'
FP_SSH_USER['web-1']='deploy'

SSH_CONTROL_PATH="${TMPDIR:-/tmp}/fp-shell-%r@%h:%p"

fp_validate_runtime() {
  command -v 'bash' >/dev/null 2>&1 || { echo "missing required command: bash" >&2; exit 1; }
  command -v 'docker' >/dev/null 2>&1 || { echo "missing required command: docker" >&2; exit 1; }
  command -v 'kubectl' >/dev/null 2>&1 || { echo "missing required command: kubectl" >&2; exit 1; }
  command -v 'pwsh' >/dev/null 2>&1 || { echo "missing required command: pwsh" >&2; exit 1; }
  command -v 'ssh' >/dev/null 2>&1 || { echo "missing required command: ssh" >&2; exit 1; }
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

run_local_host() {
    local cmd="$1"
    bash -lc "${cmd}"
}

copy_local_host() {
    local src="$1"
    local dest="$2"
    cp -- "${src}" "${dest}"
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
            __fp_host="${host}"; __fp_address="${FP_SSH_ADDRESS[$__fp_host]:-$__fp_host}"; __fp_user="${FP_SSH_USER[$__fp_host]:-}"; __fp_port="${FP_SSH_PORT[$__fp_host]:-}"; __fp_target="$__fp_address"; if [[ -n "$__fp_user" ]]; then __fp_target="$__fp_user@$__fp_target"; fi; if [[ -n "$__fp_port" ]]; then ssh_cmd -p "$__fp_port" "$__fp_target" "${cmd}"; else ssh_cmd "$__fp_target" "${cmd}"; fi
            ;;
        docker)
            __fp_host="${host}"; __fp_container="${FP_DOCKER_CONTAINER[$__fp_host]}"; __fp_user="${FP_DOCKER_USER[$__fp_host]:-}"; if [[ -n "$__fp_user" ]]; then docker exec --user "$__fp_user" "$__fp_container" sh -lc "${cmd}"; else docker exec "$__fp_container" sh -lc "${cmd}"; fi
            ;;
        kubectl)
            __fp_host="${host}"; __fp_kubectl_args=(); __fp_context="${FP_K8S_CONTEXT[$__fp_host]:-}"; __fp_namespace="${FP_K8S_NAMESPACE[$__fp_host]:-}"; __fp_container="${FP_K8S_CONTAINER[$__fp_host]:-}"; __fp_pod="${FP_K8S_POD[$__fp_host]}"; [[ -n "$__fp_context" ]] && __fp_kubectl_args+=(--context "$__fp_context"); [[ -n "$__fp_namespace" ]] && __fp_kubectl_args+=(-n "$__fp_namespace"); __fp_kubectl_args+=(exec); [[ -n "$__fp_container" ]] && __fp_kubectl_args+=(-c "$__fp_container"); __fp_kubectl_args+=("$__fp_pod" -- sh -lc "${cmd}"); kubectl "${__fp_kubectl_args[@]}"
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
            __fp_host="${host}"; __fp_address="${FP_SSH_ADDRESS[$__fp_host]:-$__fp_host}"; __fp_user="${FP_SSH_USER[$__fp_host]:-}"; __fp_port="${FP_SSH_PORT[$__fp_host]:-}"; __fp_target="$__fp_address"; if [[ -n "$__fp_user" ]]; then __fp_target="$__fp_user@$__fp_target"; fi; __fp_remote_path="${dest}"; if [[ -n "$__fp_port" ]]; then scp_cmd -P "$__fp_port" "${src}" "$__fp_target:$__fp_remote_path"; else scp_cmd "${src}" "$__fp_target:$__fp_remote_path"; fi
            ;;
        docker)
            __fp_host="${host}"; __fp_container="${FP_DOCKER_CONTAINER[$__fp_host]}"; __fp_remote_path="${dest}"; docker cp "${src}" "$__fp_container:$__fp_remote_path"
            ;;
        kubectl)
            __fp_host="${host}"; __fp_kubectl_args=(); __fp_context="${FP_K8S_CONTEXT[$__fp_host]:-}"; __fp_namespace="${FP_K8S_NAMESPACE[$__fp_host]:-}"; __fp_pod="${FP_K8S_POD[$__fp_host]}"; __fp_remote_path="${dest}"; [[ -n "$__fp_context" ]] && __fp_kubectl_args+=(--context "$__fp_context"); [[ -n "$__fp_namespace" ]] && __fp_kubectl_args+=(-n "$__fp_namespace"); kubectl cp "${__fp_kubectl_args[@]}" "${src}" "$__fp_pod:$__fp_remote_path"
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
        ssh)
            __fp_host="${host}"; __fp_address="${FP_SSH_ADDRESS[$__fp_host]:-$__fp_host}"; __fp_user="${FP_SSH_USER[$__fp_host]:-}"; __fp_port="${FP_SSH_PORT[$__fp_host]:-}"; __fp_target="$__fp_address"; if [[ -n "$__fp_user" ]]; then __fp_target="$__fp_user@$__fp_target"; fi; __fp_remote_path="${dest}"; rsync_cmd ${flags} -- "${src}" "$__fp_target:$__fp_remote_path"
            ;;
        *)
            echo "rsync is only supported for ssh in shell target, got: ${transport}" >&2; return 1
            ;;
    esac
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
        if ${only_if}; then
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
        if test ! -e ${creates}; then
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
        if test -e ${removes}; then
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
        if ${only_if}; then
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
        if test ! -e ${creates}; then
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
        if test -e ${removes}; then
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
        if ${only_if}; then
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
        if test ! -e ${creates}; then
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
        if test -e ${removes}; then
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
        if ${only_if}; then
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
        if test ! -e ${creates}; then
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
        if test -e ${removes}; then
            rsync_host "${host}" "${flags}" "${src}" "${dest}"
            __fp_last_changed=1
        fi
    else
        rsync_host "${host}" "${flags}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

while test -f /tmp/fp-rollout.lock; do
    shell_run 'localhost' "sleep 1" '' '' '' ''
done
shell_run 'web-1' "sudo systemctl restart fp-service" '' '' '' ''
shell_run 'web-2' "sudo systemctl restart fp-service" '' '' '' ''
