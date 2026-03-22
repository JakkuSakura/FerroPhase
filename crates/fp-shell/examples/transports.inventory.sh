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

fp_validate_runtime() {
  command -v 'bash' >/dev/null 2>&1 || { echo "missing required command: bash" >&2; exit 1; }
  command -v 'chroot' >/dev/null 2>&1 || { echo "missing required command: chroot" >&2; exit 1; }
  command -v 'command' >/dev/null 2>&1 || { echo "missing required command: command" >&2; exit 1; }
  command -v 'date' >/dev/null 2>&1 || { echo "missing required command: date" >&2; exit 1; }
  command -v 'docker' >/dev/null 2>&1 || { echo "missing required command: docker" >&2; exit 1; }
  command -v 'envsubst' >/dev/null 2>&1 || { echo "missing required command: envsubst" >&2; exit 1; }
  command -v 'kubectl' >/dev/null 2>&1 || { echo "missing required command: kubectl" >&2; exit 1; }
  command -v 'mktemp' >/dev/null 2>&1 || { echo "missing required command: mktemp" >&2; exit 1; }
  command -v 'printenv' >/dev/null 2>&1 || { echo "missing required command: printenv" >&2; exit 1; }
  command -v 'pwsh' >/dev/null 2>&1 || { echo "missing required command: pwsh" >&2; exit 1; }
  command -v 'rm' >/dev/null 2>&1 || { echo "missing required command: rm" >&2; exit 1; }
  command -v 'rsync' >/dev/null 2>&1 || { echo "missing required command: rsync" >&2; exit 1; }
  command -v 'scp' >/dev/null 2>&1 || { echo "missing required command: scp" >&2; exit 1; }
  command -v 'ssh' >/dev/null 2>&1 || { echo "missing required command: ssh" >&2; exit 1; }
  command -v 'test' >/dev/null 2>&1 || { echo "missing required command: test" >&2; exit 1; }
  command -v 'uname' >/dev/null 2>&1 || { echo "missing required command: uname" >&2; exit 1; }
  command -v 'whoami' >/dev/null 2>&1 || { echo "missing required command: whoami" >&2; exit 1; }
}

fp_validate_runtime

__fp_std_facts_has_command_() {
    local command="$1"
    command -v "${command}"
}

__fp_std_facts_file_exists_() {
    local path="$1"
    test -f "${path}"
}

__fp_std_facts_dir_exists_() {
    local path="$1"
    test -d "${path}"
}

__fp_std_facts_path_exists_() {
    local path="$1"
    test -e "${path}"
}

__fp_std_facts_host_transport_() {
    local host="$1"
}

__fp_std_facts_host_address_() {
    local host="$1"
}

__fp_std_facts_host_user_() {
    local host="$1"
}

__fp_std_facts_host_port_() {
    local host="$1"
}

__fp_std_facts_apk_packages_() {
    __fp_std_shell_process_process_output_ 'apk list --installed'
}

__fp_std_facts_apt_sources_() {
    __fp_std_shell_process_process_output_ 'cat /etc/apt/sources.list /etc/apt/sources.list.d/*.list 2>/dev/null'
}

__fp_std_facts_apt_keys_() {
    __fp_std_shell_process_process_output_ 'apt-key list --with-colons 2>/dev/null || true'
}

__fp_std_facts_apt_simulate_() {
    local command="$1"
    __fp_std_shell_process_process_output_ "LC_ALL=C DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' ${command} --dry-run"
}

__fp_std_facts_brew_version_() {
    __fp_std_shell_process_process_output_ 'brew --version'
}

__fp_std_facts_brew_packages_() {
    __fp_std_shell_process_process_output_ 'brew list --versions'
}

__fp_std_facts_brew_casks_() {
    __fp_std_shell_process_process_output_ 'if brew --version | grep -q -e '"'"'Homebrew\ +(1\.|2\.[0-5]).*'"'"' 1>/dev/null; then brew cask list --versions; else brew list --cask --versions; fi'
}

__fp_std_facts_brew_taps_() {
    __fp_std_shell_process_process_output_ 'brew tap'
}

__fp_std_facts_cargo_packages_() {
    __fp_std_shell_process_process_output_ 'cargo install --list'
}

__fp_std_facts_choco_packages_() {
    __fp_std_shell_process_process_output_ 'choco list'
}

__fp_std_facts_choco_version_() {
    __fp_std_shell_process_process_output_ 'choco --version'
}

__fp_std_facts_dnf_repositories_() {
    __fp_std_shell_process_process_output_ 'cat /etc/dnf.conf /etc/dnf.repos.d/*.repo /etc/yum.repos.d/*.repo 2>/dev/null'
}

__fp_std_facts_docker_container_running_() {
    local name="$1"
    __fp_std_shell_process_process_ok_ "docker inspect --format '{{.State.Running}}' ${name} | grep -q true"
}

__fp_std_facts_docker_image_present_() {
    local name="$1"
    __fp_std_shell_process_process_ok_ "docker image inspect ${name}"
}

__fp_std_facts_docker_container_id_() {
    local name="$1"
    __fp_std_shell_process_process_output_ "docker inspect --format '{{.Id}}' ${name}"
}

__fp_std_facts_files_exists_() {
    local path="$1"
    __fp_std_facts_path_exists_ "${path}"
}

__fp_std_facts_files_is_file_() {
    local path="$1"
    __fp_std_facts_file_exists_ "${path}"
}

__fp_std_facts_files_is_directory_() {
    local path="$1"
    __fp_std_facts_dir_exists_ "${path}"
}

__fp_std_facts_files_is_link_() {
    local path="$1"
    test -L "${path}"
}

__fp_std_facts_files_read_file_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "cat ${path}"
}

__fp_std_facts_files_sha1_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "sha1sum ${path} | awk '{print $1}'"
}

__fp_std_facts_files_sha256_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "sha256sum ${path} | awk '{print $1}'"
}

__fp_std_facts_files_md5_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "md5sum ${path} | awk '{print $1}'"
}

__fp_std_facts_files_mode_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "stat -c %a ${path}"
}

__fp_std_facts_files_owner_user_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "stat -c %U ${path}"
}

__fp_std_facts_files_owner_group_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "stat -c %G ${path}"
}

__fp_std_facts_files_size_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "stat -c %s ${path}"
}

__fp_std_facts_git_head_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "git -C ${path} rev-parse HEAD"
}

__fp_std_facts_git_branch_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "git -C ${path} rev-parse --abbrev-ref HEAD"
}

__fp_std_facts_git_origin_url_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "git -C ${path} remote get-url origin"
}

__fp_std_facts_git_working_tree_status_() {
    local path="$1"
    __fp_std_shell_process_process_output_ "git -C ${path} status --short"
}

__fp_std_facts_npm_packages_() {
    local directory="$1"
    if [[ "${directory}" == '' ]]; then
        __fp_std_shell_process_process_output_ 'npm list -g --depth=0'
    else
        __fp_std_shell_process_process_output_ "! test -d ${directory} || (cd ${directory} && npm list -g --depth=0)"
    fi
}

__fp_std_facts_pacman_packages_() {
    __fp_std_shell_process_process_output_ 'pacman -Q'
}

__fp_std_facts_pacman_expand_package_() {
    local package="$1"
    __fp_std_shell_process_process_output_ "pacman -S --print-format '%n' ${package} || true"
}

__fp_std_facts_pip_packages_() {
    local pip="$1"
    if [[ "${pip}" == '' ]]; then
        __fp_std_shell_process_process_output_ 'pip freeze --all'
    else
        __fp_std_shell_process_process_output_ "${pip} freeze --all"
    fi
}

__fp_std_facts_pip_packages3_() {
    __fp_std_shell_process_process_output_ 'pip3 freeze --all'
}

__fp_std_facts_server_current_user_() {
    whoami
}

__fp_std_facts_server_home_() {
    __fp_std_facts_server_user_home_ ''
}

__fp_std_facts_server_user_home_() {
    local user="$1"
    if [[ "${user}" == '' ]]; then
        bash -lc 'echo $HOME'
    else
        bash -lc "echo ~${user}"
    fi
}

__fp_std_facts_server_path_() {
    printenv PATH
}

__fp_std_facts_server_hostname_() {
    uname -n
}

__fp_std_facts_server_kernel_() {
    uname -s
}

__fp_std_facts_server_kernel_version_() {
    uname -r
}

__fp_std_facts_server_arch_() {
    uname -m
}

__fp_std_facts_server_date_() {
    date +%Y-%m-%dT%H:%M:%S%z
}

__fp_std_facts_server_command_() {
    local command="$1"
    __fp_std_shell_process_process_output_ "${command}"
}

__fp_std_facts_server_which_() {
    local command="$1"
    __fp_std_shell_process_process_output_ "command -v ${command}"
}

__fp_std_facts_systemd_is_active_() {
    local service="$1"
    __fp_std_shell_process_process_ok_ "systemctl is-active --quiet $(__fp_std_facts_systemd_normalize_service_ "${service}")"
}

__fp_std_facts_systemd_is_enabled_() {
    local service="$1"
    __fp_std_shell_process_process_ok_ "systemctl is-enabled --quiet $(__fp_std_facts_systemd_normalize_service_ "${service}")"
}

__fp_std_facts_systemd_service_status_() {
    local service="$1"
    __fp_std_shell_process_process_output_ "systemctl status $(__fp_std_facts_systemd_normalize_service_ "${service}")"
}

__fp_std_facts_systemd_show_property_() {
    local service="$1"
    local property="$2"
    __fp_std_shell_process_process_output_ "systemctl show --property ${property} --value $(__fp_std_facts_systemd_normalize_service_ "${service}")"
}

__fp_std_facts_systemd_normalize_service_() {
    local service="$1"
    if __fp_std_shell_process_process_ok_ "printf '%s' ${service} | grep -Eq '\\.(service|socket|device|mount|automount|swap|target|path|timer|slice|scope)$'"; then
        printf '%s\n' "${service}"
    else
        printf '%s\n' "${service}.service"
    fi
}

__fp_std_facts_yum_repositories_() {
    __fp_std_shell_process_process_output_ 'cat /etc/yum.conf /etc/yum.repos.d/*.repo 2>/dev/null'
}

__fp_std_ops_apk_update_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'apk update' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_apk_upgrade_() {
    local hosts="$1"
    local available="$2"
    local sudo="$3"
    if [[ "${available}" == 'true' ]]; then
        __fp_std_ops_server_shell_ 'apk upgrade --available' "${hosts}" '' '' '' '' "${sudo}" ''
    else
        __fp_std_ops_server_shell_ 'apk upgrade' "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_apk_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local update="$5"
    local upgrade="$6"
    local sudo="$7"
    if [[ "${update}" == 'true' ]]; then
        __fp_std_ops_apk_update_ "${hosts}" "${sudo}"
    fi
    if [[ "${upgrade}" == 'true' ]]; then
        __fp_std_ops_apk_upgrade_ "${hosts}" '' "${sudo}"
    fi
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "apk upgrade ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "apk add ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "apk del ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_apt_update_() {
    local hosts="$1"
    local sudo="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_std_ops_server_shell_ 'DEBIAN_FRONTEND=noninteractive apt-get update -y' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_apt_upgrade_() {
    local hosts="$1"
    local sudo="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_std_ops_server_shell_ 'DEBIAN_FRONTEND=noninteractive apt-get upgrade -y' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_apt_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local update="$5"
    local sudo="$6"
    local only_if="$7"
    local unless="$8"
    local creates="$9"
    local removes="$10"
    if [[ "${update}" == 'true' ]]; then
        __fp_std_ops_apt_update_ "${hosts}" "${sudo}" "${only_if}" "${unless}" "${creates}" "${removes}"
    fi
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "DEBIAN_FRONTEND=noninteractive apt-get install -y --only-upgrade ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "DEBIAN_FRONTEND=noninteractive apt-get install -y ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "DEBIAN_FRONTEND=noninteractive apt-get remove -y ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
    fi
}

__fp_std_ops_apt_repo_() {
    local src="$1"
    local hosts="$2"
    local filename="$3"
    local present="$4"
    local sudo="$5"
    if [[ "${filename}" == '' ]]; then
        if [[ "${present}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "printf '%s
' ${src} >> /etc/apt/sources.list" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "grep -v -F ${src} /etc/apt/sources.list > /etc/apt/sources.list.fp-shell && mv /etc/apt/sources.list.fp-shell /etc/apt/sources.list" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        if [[ "${present}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "printf '%s
' ${src} >> /etc/apt/sources.list.d/${filename}.list" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "grep -v -F ${src} /etc/apt/sources.list.d/${filename}.list > /etc/apt/sources.list.d/${filename}.list.fp-shell && mv /etc/apt/sources.list.d/${filename}.list.fp-shell /etc/apt/sources.list.d/${filename}.list" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    fi
}

__fp_std_ops_brew_update_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'brew update' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_brew_upgrade_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'brew upgrade' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_brew_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local update="$5"
    local upgrade="$6"
    local sudo="$7"
    if [[ "${update}" == 'true' ]]; then
        __fp_std_ops_brew_update_ "${hosts}" "${sudo}"
    fi
    if [[ "${upgrade}" == 'true' ]]; then
        __fp_std_ops_brew_upgrade_ "${hosts}" "${sudo}"
    fi
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "brew upgrade ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "brew install ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "brew uninstall ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_brew_casks_() {
    local casks="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local upgrade="$5"
    local sudo="$6"
    if [[ "${upgrade}" == 'true' ]]; then
        __fp_std_ops_server_shell_ 'brew upgrade --cask' "${hosts}" '' '' '' '' "${sudo}" ''
    fi
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "brew upgrade --cask ${casks}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "brew install --cask ${casks}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "brew uninstall --cask ${casks}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_brew_tap_() {
    local src="$1"
    local hosts="$2"
    local present="$3"
    local url="$4"
    local sudo="$5"
    if [[ "${present}" == 'true' ]]; then
        if [[ "${url}" == '' ]]; then
            __fp_std_ops_server_shell_ "brew tap ${src}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "brew tap ${src} ${url}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "brew untap ${src}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_cargo_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local sudo="$5"
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "cargo install ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "cargo install ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "cargo uninstall ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_choco_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local sudo="$5"
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "choco update -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "choco install -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "choco uninstall -y -x ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_choco_install_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'Set-ExecutionPolicy Bypass -Scope Process -Force ; iex ((New-Object System.Net.WebClient).DownloadString('"'"'https://chocolatey.org/install.ps1'"'"'))' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_dnf_update_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'dnf update -y' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_dnf_key_() {
    local src="$1"
    local hosts="$2"
    local sudo="$3"
    __fp_std_ops_server_shell_ "rpm --import ${src}" "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_dnf_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local update="$5"
    local clean="$6"
    local sudo="$7"
    if [[ "${clean}" == 'true' ]]; then
        __fp_std_ops_server_shell_ 'dnf clean all' "${hosts}" '' '' '' '' "${sudo}" ''
    fi
    if [[ "${update}" == 'true' ]]; then
        __fp_std_ops_dnf_update_ "${hosts}" "${sudo}"
    fi
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "dnf update -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "dnf install -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "dnf remove -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_files_copy_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_shell_backend_shell_copy_ "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_copy_local_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_shell_backend_shell_copy_local_ "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_copy_ssh_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_shell_backend_shell_copy_ssh_ "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_copy_docker_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_shell_backend_shell_copy_docker_ "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_copy_kubectl_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_shell_backend_shell_copy_kubectl_ "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_copy_winrm_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_shell_backend_shell_copy_winrm_ "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_copy_chroot_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_shell_backend_shell_copy_chroot_ "${hosts}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_template_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_std_shell_backend_shell_template_ "${hosts}" "${src}" "${dest}" "${vars}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

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

__fp_std_ops_files_template_ssh_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_std_shell_backend_shell_template_ssh_ "${hosts}" "${src}" "${dest}" "${vars}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_template_chroot_() {
    local src="$1"
    local dest="$2"
    local hosts="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_std_shell_backend_shell_template_chroot_ "${hosts}" "${src}" "${dest}" "${vars}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

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

__fp_std_ops_files_rsync_local_() {
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
    __fp_std_shell_backend_shell_rsync_local_ "${hosts}" "${flags}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_rsync_remote_() {
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
    __fp_std_shell_backend_shell_rsync_remote_ "${hosts}" "${flags}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_files_rsync_chroot_() {
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
    __fp_std_shell_backend_shell_rsync_chroot_ "${hosts}" "${flags}" "${src}" "${dest}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_git_clone_() {
    local repo="$1"
    local dest="$2"
    local hosts="$3"
    local branch="$4"
    local depth="$5"
    local sudo="$6"
    local only_if="$7"
    local unless="$8"
    local creates="$9"
    local removes="$10"
    if [[ "${branch}" == '' ]]; then
        if [[ "${depth}" == '' ]]; then
            __fp_std_ops_server_shell_ "git clone ${repo} ${dest}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "git clone --depth ${depth} ${repo} ${dest}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
        fi
    else
        if [[ "${depth}" == '' ]]; then
            __fp_std_ops_server_shell_ "git clone --branch ${branch} ${repo} ${dest}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "git clone --branch ${branch} --depth ${depth} ${repo} ${dest}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
        fi
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

__fp_std_ops_git_checkout_() {
    local path="$1"
    local rev="$2"
    local hosts="$3"
    local sudo="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_std_ops_server_shell_ "git checkout ${rev}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" "${path}"
}

__fp_std_ops_npm_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local directory="$5"
    local sudo="$6"
    if [[ "${directory}" == '' ]]; then
        if [[ "${present}" == 'true' ]]; then
            if [[ "${latest}" == 'true' ]]; then
                __fp_std_ops_server_shell_ "npm update -g ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            else
                __fp_std_ops_server_shell_ "npm install -g ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        else
            __fp_std_ops_server_shell_ "npm uninstall -g ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        if [[ "${present}" == 'true' ]]; then
            if [[ "${latest}" == 'true' ]]; then
                __fp_std_ops_server_shell_ "cd ${directory} && npm update ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            else
                __fp_std_ops_server_shell_ "cd ${directory} && npm install ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        else
            __fp_std_ops_server_shell_ "cd ${directory} && npm uninstall ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    fi
}

__fp_std_ops_packages_apt_update_() {
    local hosts="$1"
    local sudo="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_std_ops_server_shell_ 'apt-get update' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_packages_apt_install_() {
    local packages="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "apt-get install -y ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_packages_yum_install_() {
    local packages="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "yum install -y ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_packages_dnf_install_() {
    local packages="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "dnf install -y ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_packages_apk_add_() {
    local packages="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "apk add ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_packages_pacman_sync_() {
    local packages="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "pacman -S --noconfirm ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_packages_brew_install_() {
    local packages="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "brew install ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_packages_choco_install_() {
    local packages="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "choco install -y ${packages}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_pacman_update_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'pacman -Sy' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_pacman_upgrade_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'pacman --noconfirm -Su' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_pacman_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local update="$4"
    local upgrade="$5"
    local sudo="$6"
    if [[ "${update}" == 'true' ]]; then
        __fp_std_ops_pacman_update_ "${hosts}" "${sudo}"
    fi
    if [[ "${upgrade}" == 'true' ]]; then
        __fp_std_ops_pacman_upgrade_ "${hosts}" "${sudo}"
    fi
    if [[ "${present}" == 'true' ]]; then
        __fp_std_ops_server_shell_ "pacman --noconfirm -S ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    else
        __fp_std_ops_server_shell_ "pacman --noconfirm -R ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_pip_virtualenv_() {
    local path="$1"
    local hosts="$2"
    local python="$3"
    local venv="$4"
    local site_packages="$5"
    local always_copy="$6"
    local present="$7"
    local sudo="$8"
    if [[ "${present}" == 'true' ]]; then
        if [[ "${venv}" == 'true' ]]; then
            if [[ "${python}" == '' ]]; then
                __fp_std_ops_server_shell_ "python -m venv ${path}" "${hosts}" '' '' '' '' "${sudo}" ''
            else
                __fp_std_ops_server_shell_ "${python} -m venv ${path}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        else
            __fp_std_ops_server_shell_ "virtualenv ${path}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "rm -rf ${path}" "${hosts}" '' '' '' '' "${sudo}" ''
    fi
}

__fp_std_ops_pip_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local requirements="$5"
    local pip="$6"
    local virtualenv="$7"
    local sudo="$8"
    if [[ "${virtualenv}" != '' ]]; then
        __fp_std_ops_pip_virtualenv_ "${virtualenv}" "${hosts}" '' '' '' '' 'true' "${sudo}"
    fi
    if [[ "${pip}" == '' ]]; then
        if [[ "${requirements}" != '' ]]; then
            if [[ "${present}" == 'true' ]]; then
                if [[ "${latest}" == 'true' ]]; then
                    __fp_std_ops_server_shell_ "pip install --upgrade -r ${requirements}" "${hosts}" '' '' '' '' "${sudo}" ''
                else
                    __fp_std_ops_server_shell_ "pip install -r ${requirements}" "${hosts}" '' '' '' '' "${sudo}" ''
                fi
            else
                __fp_std_ops_server_shell_ "pip uninstall --yes -r ${requirements}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        fi
        if [[ "${packages}" == '' ]]; then
            printf '%s\n' 'true'
        else
            if [[ "${present}" == 'true' ]]; then
                if [[ "${latest}" == 'true' ]]; then
                    __fp_std_ops_server_shell_ "pip install --upgrade ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
                else
                    __fp_std_ops_server_shell_ "pip install ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
                fi
            else
                __fp_std_ops_server_shell_ "pip uninstall --yes ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        fi
    else
        if [[ "${requirements}" != '' ]]; then
            if [[ "${present}" == 'true' ]]; then
                if [[ "${latest}" == 'true' ]]; then
                    __fp_std_ops_server_shell_ "${pip} install --upgrade -r ${requirements}" "${hosts}" '' '' '' '' "${sudo}" ''
                else
                    __fp_std_ops_server_shell_ "${pip} install -r ${requirements}" "${hosts}" '' '' '' '' "${sudo}" ''
                fi
            else
                __fp_std_ops_server_shell_ "${pip} uninstall --yes -r ${requirements}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        fi
        if [[ "${packages}" == '' ]]; then
            printf '%s\n' 'true'
        else
            if [[ "${present}" == 'true' ]]; then
                if [[ "${latest}" == 'true' ]]; then
                    __fp_std_ops_server_shell_ "${pip} install --upgrade ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
                else
                    __fp_std_ops_server_shell_ "${pip} install ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
                fi
            else
                __fp_std_ops_server_shell_ "${pip} uninstall --yes ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
            fi
        fi
    fi
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

__fp_std_ops_server_shell_ssh_() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(__fp_std_shell_backend_command_with_options_ "${command}" "${cwd}" "${sudo}")"
    __fp_std_shell_backend_shell_run_ssh_ "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_server_shell_docker_() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(__fp_std_shell_backend_command_with_options_ "${command}" "${cwd}" "${sudo}")"
    __fp_std_shell_backend_shell_run_docker_ "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_server_shell_kubectl_() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(__fp_std_shell_backend_command_with_options_ "${command}" "${cwd}" "${sudo}")"
    __fp_std_shell_backend_shell_run_kubectl_ "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_server_shell_winrm_() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(__fp_std_shell_backend_command_with_options_ "${command}" "${cwd}" "${sudo}")"
    __fp_std_shell_backend_shell_run_winrm_ "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
    runtime_last_changed
}

__fp_std_ops_server_shell_chroot_() {
    local command="$1"
    local hosts="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    local sudo="$7"
    local cwd="$8"
    local command="$(__fp_std_shell_backend_command_with_options_ "${command}" "${cwd}" "${sudo}")"
    __fp_std_shell_backend_shell_run_chroot_ "${hosts}" "${command}" "${only_if}" "${unless}" "${creates}" "${removes}"
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

__fp_std_ops_server_utils_reboot_() {
    local hosts="$1"
    local sudo="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_std_ops_server_shell_ 'reboot' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_services_restart_() {
    local name="$1"
    local hosts="$2"
    local sudo="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_std_ops_server_shell_ "systemctl restart ${name}" "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
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

__fp_std_ops_systemd_daemon_reload_() {
    local hosts="$1"
    local sudo="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_std_ops_server_shell_ 'systemctl daemon-reload' "${hosts}" "${only_if}" "${unless}" "${creates}" "${removes}" "${sudo}" ''
}

__fp_std_ops_systemd_normalize_service_() {
    local service="$1"
    if __fp_std_shell_process_process_ok_ "printf '%s' ${service} | grep -Eq '\\.(service|socket|device|mount|automount|swap|target|path|timer|slice|scope)$'"; then
        printf '%s\n' "${service}"
    else
        printf '%s\n' "${service}.service"
    fi
}

__fp_std_ops_yum_update_() {
    local hosts="$1"
    local sudo="$2"
    __fp_std_ops_server_shell_ 'yum update -y' "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_yum_key_() {
    local src="$1"
    local hosts="$2"
    local sudo="$3"
    __fp_std_ops_server_shell_ "rpm --import ${src}" "${hosts}" '' '' '' '' "${sudo}" ''
}

__fp_std_ops_yum_packages_() {
    local packages="$1"
    local hosts="$2"
    local present="$3"
    local latest="$4"
    local update="$5"
    local clean="$6"
    local sudo="$7"
    if [[ "${clean}" == 'true' ]]; then
        __fp_std_ops_server_shell_ 'yum clean all' "${hosts}" '' '' '' '' "${sudo}" ''
    fi
    if [[ "${update}" == 'true' ]]; then
        __fp_std_ops_yum_update_ "${hosts}" "${sudo}"
    fi
    if [[ "${present}" == 'true' ]]; then
        if [[ "${latest}" == 'true' ]]; then
            __fp_std_ops_server_shell_ "yum update -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        else
            __fp_std_ops_server_shell_ "yum install -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
        fi
    else
        __fp_std_ops_server_shell_ "yum remove -y ${packages}" "${hosts}" '' '' '' '' "${sudo}" ''
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

__fp_std_shell_backend_host_password_() {
    local host="$1"
}

__fp_std_shell_backend_host_scheme_() {
    local host="$1"
}

__fp_std_shell_backend_host_chroot_directory_() {
    local host="$1"
}

__fp_std_shell_backend_run_local_host_() {
    local cmd="$1"
    invoke_expression "${cmd}"
}

__fp_std_shell_backend_copy_local_host_() {
    local src="$1"
    local dest="$2"
    copy_item "${src}" "${dest}"
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
            __fp_winrm_host_1="${host}"
__fp_winrm_address_2="${FP_WINRM_ADDRESS[$__fp_winrm_host_1]:-}"
__fp_winrm_user_3="${FP_WINRM_USER[$__fp_winrm_host_1]:-}"
__fp_winrm_password_4="${FP_WINRM_PASSWORD[$__fp_winrm_host_1]:-}"
__fp_winrm_scheme_5="${FP_WINRM_SCHEME[$__fp_winrm_host_1]:-http}"
__fp_winrm_port_6="${FP_WINRM_PORT[$__fp_winrm_host_1]:-}"
if [[ -z "${__fp_winrm_password_4}" ]]; then echo "winrm password is required for non-interactive bash target: ${__fp_winrm_host_1}" >&2; return 1; fi
FP_WINRM_ADDRESS="${__fp_winrm_address_2}" FP_WINRM_USER="${__fp_winrm_user_3}" FP_WINRM_PASSWORD="${__fp_winrm_password_4}" FP_WINRM_SCHEME="${__fp_winrm_scheme_5}" FP_WINRM_PORT="${__fp_winrm_port_6}" FP_WINRM_MODE='run' FP_WINRM_COMMAND="${cmd}" FP_WINRM_SOURCE='' FP_WINRM_DESTINATION='' pwsh -NoProfile -NonInteractive -Command '$ErrorActionPreference = "Stop"
$sessionArgs = @{ ComputerName = $env:FP_WINRM_ADDRESS }
if ($env:FP_WINRM_PORT) { $sessionArgs.Port = [int]$env:FP_WINRM_PORT }
$scheme = if ([string]::IsNullOrWhiteSpace($env:FP_WINRM_SCHEME)) { "http" } else { $env:FP_WINRM_SCHEME.ToLowerInvariant() }
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
}'
            ;;
        chroot)
            __fp_std_shell_backend_run_chroot_host_ "${host}" "${cmd}"
            ;;
        *)
            echo "unsupported transport: ${transport}" >&2; return 1
            ;;
    esac
}

__fp_std_shell_backend_copy_host_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local transport="$(__fp_std_shell_backend_host_transport_ "${host}")"
    case "${transport}" in
        local)
            __fp_std_shell_backend_copy_local_host_ "${src}" "${dest}"
            ;;
        ssh)
            __fp_std_shell_backend_copy_ssh_host_ "${host}" "${src}" "${dest}"
            ;;
        docker)
            __fp_std_shell_backend_copy_docker_host_ "${host}" "${src}" "${dest}"
            ;;
        kubectl)
            __fp_std_shell_backend_copy_kubectl_host_ "${host}" "${src}" "${dest}"
            ;;
        winrm)
            __fp_winrm_host_7="${host}"
__fp_winrm_address_8="${FP_WINRM_ADDRESS[$__fp_winrm_host_7]:-}"
__fp_winrm_user_9="${FP_WINRM_USER[$__fp_winrm_host_7]:-}"
__fp_winrm_password_10="${FP_WINRM_PASSWORD[$__fp_winrm_host_7]:-}"
__fp_winrm_scheme_11="${FP_WINRM_SCHEME[$__fp_winrm_host_7]:-http}"
__fp_winrm_port_12="${FP_WINRM_PORT[$__fp_winrm_host_7]:-}"
if [[ -z "${__fp_winrm_password_10}" ]]; then echo "winrm password is required for non-interactive bash target: ${__fp_winrm_host_7}" >&2; return 1; fi
FP_WINRM_ADDRESS="${__fp_winrm_address_8}" FP_WINRM_USER="${__fp_winrm_user_9}" FP_WINRM_PASSWORD="${__fp_winrm_password_10}" FP_WINRM_SCHEME="${__fp_winrm_scheme_11}" FP_WINRM_PORT="${__fp_winrm_port_12}" FP_WINRM_MODE='copy' FP_WINRM_COMMAND='' FP_WINRM_SOURCE="${src}" FP_WINRM_DESTINATION="${dest}" pwsh -NoProfile -NonInteractive -Command '$ErrorActionPreference = "Stop"
$sessionArgs = @{ ComputerName = $env:FP_WINRM_ADDRESS }
if ($env:FP_WINRM_PORT) { $sessionArgs.Port = [int]$env:FP_WINRM_PORT }
$scheme = if ([string]::IsNullOrWhiteSpace($env:FP_WINRM_SCHEME)) { "http" } else { $env:FP_WINRM_SCHEME.ToLowerInvariant() }
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
}'
            ;;
        chroot)
            __fp_std_shell_backend_copy_chroot_host_ "${host}" "${src}" "${dest}"
            ;;
        *)
            echo "unsupported transport for copy: ${transport}" >&2; return 1
            ;;
    esac
}

__fp_std_shell_backend_template_host_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local tmp="$(mktemp)"
    eval '${vars} envsubst < ${src} > ${tmp}'
    __fp_std_shell_backend_copy_host_ "${host}" "${tmp}" "${dest}"
    rm -f "${tmp}"
}

__fp_std_shell_backend_rsync_host_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local transport="$(__fp_std_shell_backend_host_transport_ "${host}")"
    case "${transport}" in
        local)
            rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "${flags}" "${src}" "${dest}"
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
        ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH -p "${port}" "${target}" "${cmd}"
    else
        ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH "${target}" "${cmd}"
    fi
}

__fp_std_shell_backend_copy_ssh_host_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local target="$(__fp_std_shell_backend_ssh_target_ "${host}")"
    local remote="${target}:${dest}"
    local port="$(__fp_std_shell_backend_host_port_ "${host}")"
    if [[ "${port}" != '' ]]; then
        scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH -P "${port}" "${src}" "${remote}"
    else
        scp -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH "${src}" "${remote}"
    fi
}

__fp_std_shell_backend_run_docker_host_() {
    local host="$1"
    local cmd="$2"
    local container="$(__fp_std_shell_backend_host_container_ "${host}")"
    local user="$(__fp_std_shell_backend_host_user_ "${host}")"
    if [[ "${user}" != '' ]]; then
        docker exec --user "${user}" "${container}" 'sh' '-lc' "${cmd}"
    else
        docker exec "${container}" 'sh' '-lc' "${cmd}"
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

__fp_std_shell_backend_run_chroot_host_() {
    local host="$1"
    local cmd="$2"
    chroot "$(__fp_std_shell_backend_host_chroot_directory_ "${host}")" 'sh' '-lc' "${cmd}"
}

__fp_std_shell_backend_copy_docker_host_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local container="$(__fp_std_shell_backend_host_container_ "${host}")"
    docker cp "${src}" "${container}:${dest}"
}

__fp_std_shell_backend_copy_kubectl_host_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local context="$(__fp_std_shell_backend_host_context_ "${host}")"
    local namespace="$(__fp_std_shell_backend_host_namespace_ "${host}")"
    local remote="$(__fp_std_shell_backend_host_pod_ "${host}"):${dest}"
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

__fp_std_shell_backend_chroot_path_() {
    local host="$1"
    local path="$2"
    printf '%s\n' "$(__fp_std_shell_backend_host_chroot_directory_ "${host}")${path}"
}

__fp_std_shell_backend_copy_chroot_host_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    __fp_std_shell_backend_copy_local_host_ "${src}" "$(__fp_std_shell_backend_chroot_path_ "${host}" "${dest}")"
}

__fp_std_shell_backend_rsync_remote_target_() {
    local host="$1"
    local address="$(__fp_std_shell_backend_host_address_ "${host}")"
    if [[ "${address}" == '' ]]; then
        echo "host is not rsync-reachable: missing address for ${host}" >&2; return 1
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
        rsync -e "ssh -p ${port}" "${flags}" "${src}" "${remote}"
    else
        rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "${flags}" "${src}" "${remote}"
    fi
}

__fp_std_shell_backend_rsync_chroot_host_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local target="$(__fp_std_shell_backend_chroot_path_ "${host}" "${dest}")"
    rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "${flags}" "${src}" "${target}"
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
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_host_ "${host}" "${command}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_run_local_() {
    local _host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_local_host_ "${command}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_run_ssh_() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_ssh_host_ "${host}" "${command}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_run_docker_() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_docker_host_ "${host}" "${command}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_run_kubectl_() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_kubectl_host_ "${host}" "${command}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_run_winrm_() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_winrm_host_13="${host}"
__fp_winrm_address_14="${FP_WINRM_ADDRESS[$__fp_winrm_host_13]:-}"
__fp_winrm_user_15="${FP_WINRM_USER[$__fp_winrm_host_13]:-}"
__fp_winrm_password_16="${FP_WINRM_PASSWORD[$__fp_winrm_host_13]:-}"
__fp_winrm_scheme_17="${FP_WINRM_SCHEME[$__fp_winrm_host_13]:-http}"
__fp_winrm_port_18="${FP_WINRM_PORT[$__fp_winrm_host_13]:-}"
if [[ -z "${__fp_winrm_password_16}" ]]; then echo "winrm password is required for non-interactive bash target: ${__fp_winrm_host_13}" >&2; return 1; fi
FP_WINRM_ADDRESS="${__fp_winrm_address_14}" FP_WINRM_USER="${__fp_winrm_user_15}" FP_WINRM_PASSWORD="${__fp_winrm_password_16}" FP_WINRM_SCHEME="${__fp_winrm_scheme_17}" FP_WINRM_PORT="${__fp_winrm_port_18}" FP_WINRM_MODE='run' FP_WINRM_COMMAND="${command}" FP_WINRM_SOURCE='' FP_WINRM_DESTINATION='' pwsh -NoProfile -NonInteractive -Command '$ErrorActionPreference = "Stop"
$sessionArgs = @{ ComputerName = $env:FP_WINRM_ADDRESS }
if ($env:FP_WINRM_PORT) { $sessionArgs.Port = [int]$env:FP_WINRM_PORT }
$scheme = if ([string]::IsNullOrWhiteSpace($env:FP_WINRM_SCHEME)) { "http" } else { $env:FP_WINRM_SCHEME.ToLowerInvariant() }
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
}'
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_run_chroot_() {
    local host="$1"
    local command="$2"
    local only_if="$3"
    local unless="$4"
    local creates="$5"
    local removes="$6"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_run_chroot_host_ "${host}" "${command}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_copy_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_copy_host_ "${host}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_copy_local_() {
    local _host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_copy_local_host_ "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_copy_ssh_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_copy_ssh_host_ "${host}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_copy_docker_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_copy_docker_host_ "${host}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_copy_kubectl_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_copy_kubectl_host_ "${host}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_copy_winrm_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_winrm_host_19="${host}"
__fp_winrm_address_20="${FP_WINRM_ADDRESS[$__fp_winrm_host_19]:-}"
__fp_winrm_user_21="${FP_WINRM_USER[$__fp_winrm_host_19]:-}"
__fp_winrm_password_22="${FP_WINRM_PASSWORD[$__fp_winrm_host_19]:-}"
__fp_winrm_scheme_23="${FP_WINRM_SCHEME[$__fp_winrm_host_19]:-http}"
__fp_winrm_port_24="${FP_WINRM_PORT[$__fp_winrm_host_19]:-}"
if [[ -z "${__fp_winrm_password_22}" ]]; then echo "winrm password is required for non-interactive bash target: ${__fp_winrm_host_19}" >&2; return 1; fi
FP_WINRM_ADDRESS="${__fp_winrm_address_20}" FP_WINRM_USER="${__fp_winrm_user_21}" FP_WINRM_PASSWORD="${__fp_winrm_password_22}" FP_WINRM_SCHEME="${__fp_winrm_scheme_23}" FP_WINRM_PORT="${__fp_winrm_port_24}" FP_WINRM_MODE='copy' FP_WINRM_COMMAND='' FP_WINRM_SOURCE="${src}" FP_WINRM_DESTINATION="${dest}" pwsh -NoProfile -NonInteractive -Command '$ErrorActionPreference = "Stop"
$sessionArgs = @{ ComputerName = $env:FP_WINRM_ADDRESS }
if ($env:FP_WINRM_PORT) { $sessionArgs.Port = [int]$env:FP_WINRM_PORT }
$scheme = if ([string]::IsNullOrWhiteSpace($env:FP_WINRM_SCHEME)) { "http" } else { $env:FP_WINRM_SCHEME.ToLowerInvariant() }
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
}'
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_copy_chroot_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local only_if="$4"
    local unless="$5"
    local creates="$6"
    local removes="$7"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_copy_chroot_host_ "${host}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_template_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_template_host_ "${host}" "${src}" "${dest}" "${vars}"
        __fp_last_changed=1
    fi
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
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        local tmp="$(mktemp)"
        eval '${vars} envsubst < ${src} > ${tmp}'
        __fp_std_shell_backend_copy_local_host_ "${tmp}" "${dest}"
        rm -f "${tmp}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_template_ssh_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        local tmp="$(mktemp)"
        eval '${vars} envsubst < ${src} > ${tmp}'
        __fp_std_shell_backend_copy_ssh_host_ "${host}" "${tmp}" "${dest}"
        rm -f "${tmp}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_template_chroot_() {
    local host="$1"
    local src="$2"
    local dest="$3"
    local vars="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        local tmp="$(mktemp)"
        eval '${vars} envsubst < ${src} > ${tmp}'
        __fp_std_shell_backend_copy_chroot_host_ "${host}" "${tmp}" "${dest}"
        rm -f "${tmp}"
        __fp_last_changed=1
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
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_rsync_host_ "${host}" "${flags}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_rsync_local_() {
    local _host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        rsync -e "ssh -o ControlMaster=auto -o ControlPersist=60 -o ControlPath=$SSH_CONTROL_PATH" "${flags}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_rsync_remote_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_rsync_remote_host_ "${host}" "${flags}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_backend_shell_rsync_chroot_() {
    local host="$1"
    local flags="$2"
    local src="$3"
    local dest="$4"
    local only_if="$5"
    local unless="$6"
    local creates="$7"
    local removes="$8"
    __fp_last_changed=0
    if __fp_std_shell_backend_should_apply_ "${only_if}" "${unless}" "${creates}" "${removes}"; then
        __fp_std_shell_backend_rsync_chroot_host_ "${host}" "${flags}" "${src}" "${dest}"
        __fp_last_changed=1
    fi
}

__fp_std_shell_capabilities_capabilities_has_rsync_() {
    __fp_std_facts_has_command_ 'rsync'
}

__fp_std_shell_capabilities_capabilities_has_ssh_() {
    __fp_std_facts_has_command_ 'ssh'
}

__fp_std_shell_capabilities_capabilities_has_docker_() {
    __fp_std_facts_has_command_ 'docker'
}

__fp_std_shell_capabilities_capabilities_has_kubectl_() {
    __fp_std_facts_has_command_ 'kubectl'
}

__fp_std_shell_capabilities_capabilities_has_pwsh_() {
    __fp_std_facts_has_command_ 'pwsh'
}

__fp_std_shell_capabilities_capabilities_host_supports_rsync_() {
    local host="$1"
    case "$(__fp_std_facts_host_transport_ "${host}")" in
        ssh)
            ;;
        docker)
            ;;
        kubectl)
            ;;
        winrm)
            ;;
        chroot)
            __fp_std_shell_capabilities_capabilities_has_rsync_ 
            ;;
        local)
            __fp_std_shell_capabilities_capabilities_has_rsync_ 
            ;;
        *)
            printf '%s\n' 'false'
            ;;
    esac
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

__fp_std_shell_process_process_stdout_append_() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} >> ${path}"
}

__fp_std_shell_process_process_stderr_to_() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} 2> ${path}"
}

__fp_std_shell_process_process_stderr_append_() {
    local command="$1"
    local path="$2"
    printf '%s\n' "${command} 2>> ${path}"
}

__fp_std_shell_process_process_run_() {
    local command="$1"
    __fp_std_ops_server_shell_local_ "${command}" 'localhost' '' '' '' '' '' ''
}

__fp_std_shell_process_process_ok_() {
    local command="$1"
    bash -lc "${command}"
}

__fp_std_shell_process_process_output_() {
    local command="$1"
    bash -lc "${command}"
}

inventory() {
}

