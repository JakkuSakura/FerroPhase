#!/usr/bin/env bash
set -euo pipefail

output_dir=${1:?usage: scripts/codegen_libc.sh OUTPUT_DIR [HEADER ...]}
shift

fp_bin=${FP_BIN:-fp}
clang_bin=${CLANG:-clang}
command -v "$fp_bin" >/dev/null 2>&1 || {
    echo "fp executable not found; set FP_BIN or install fp" >&2
    exit 1
}
command -v "$clang_bin" >/dev/null 2>&1 || {
    echo "clang executable not found; set CLANG or install clang" >&2
    exit 1
}

common_headers=(
    assert.h complex.h ctype.h errno.h fenv.h float.h inttypes.h iso646.h
    limits.h locale.h math.h setjmp.h signal.h stdalign.h stdarg.h stdatomic.h
    stdbool.h stddef.h stdint.h stdio.h stdlib.h stdnoreturn.h string.h
    tgmath.h threads.h time.h uchar.h wchar.h wctype.h
)

posix_headers=(
    aio.h arpa/inet.h dirent.h dlfcn.h fcntl.h fnmatch.h glob.h grp.h iconv.h
    langinfo.h libgen.h monetary.h mqueue.h net/if.h netdb.h netinet/in.h
    netinet/tcp.h poll.h pthread.h pwd.h regex.h sched.h search.h semaphore.h
    spawn.h strings.h syslog.h tar.h termios.h ucontext.h unistd.h utime.h
    sys/ipc.h sys/mman.h sys/msg.h sys/resource.h sys/select.h sys/sem.h
    sys/shm.h sys/socket.h sys/stat.h sys/statvfs.h sys/time.h sys/times.h
    sys/types.h sys/un.h sys/utsname.h sys/wait.h
)

linux_headers=(
    execinfo.h malloc.h sys/auxv.h sys/epoll.h sys/eventfd.h sys/inotify.h
    sys/prctl.h sys/sendfile.h sys/signalfd.h sys/syscall.h sys/xattr.h
)

darwin_headers=(
    libproc.h mach/mach.h mach-o/dyld.h sys/appleapiopts.h sys/event.h
    sys/clonefile.h sys/kqueue.h sys/mount.h sys/param.h sys/proc_info.h
    sys/random.h sys/sysctl.h sys/ucred.h sys/xattr.h
)

freebsd_headers=(
    sys/capsicum.h sys/cpuset.h sys/jail.h sys/kbio.h sys/link_elf.h
    sys/procctl.h sys/sbuf.h sys/signalvar.h sys/sysctl.h sys/ucontext.h
)

windows_headers=(
    io.h process.h share.h stdio.h windows.h winsock2.h ws2tcpip.h
)

host_os() {
    case "$(uname -s)" in
        Darwin) printf 'macos\n' ;;
        Linux) printf 'linux\n' ;;
        FreeBSD) printf 'freebsd\n' ;;
        MINGW*|MSYS*|CYGWIN*) printf 'windows\n' ;;
        *) printf '%s\n' "${TARGET_OS:-unknown}" ;;
    esac
}

target_triple() {
    case "$1" in
        linux) printf '%s\n' "${LINUX_TARGET:-x86_64-unknown-linux-gnu}" ;;
        macos) printf '%s\n' "${MACOS_TARGET:-$(uname -m)-apple-darwin}" ;;
        ios) printf '%s\n' "${IOS_TARGET:-aarch64-apple-ios}" ;;
        freebsd) printf '%s\n' "${FREEBSD_TARGET:-x86_64-unknown-freebsd}" ;;
        windows) printf '%s\n' "${WINDOWS_TARGET:-x86_64-pc-windows-msvc}" ;;
        *) return 1 ;;
    esac
}

headers_for_target() {
    printf '%s\n' "${common_headers[@]}" "${posix_headers[@]}"
    case "$1" in
        linux) printf '%s\n' "${linux_headers[@]}" ;;
        macos|ios) printf '%s\n' "${darwin_headers[@]}" ;;
        freebsd) printf '%s\n' "${freebsd_headers[@]}" ;;
        windows) printf '%s\n' "${windows_headers[@]}" ;;
    esac
}

available_headers() {
    local target="$1"
    local flags="$2"
    local header
    local probe
    local accepted
    probe=$(mktemp "${TMPDIR:-/tmp}/fp-libc-probe.XXXXXX.c")
    accepted=$(mktemp "${TMPDIR:-/tmp}/fp-libc-accepted.XXXXXX")
    while IFS= read -r header; do
        cp "$accepted" "$probe"
        printf '#include <%s>\n' "$header" >> "$probe"
        if "$clang_bin" $flags -fsyntax-only "$probe" >/dev/null 2>&1; then
            printf '%s\n' "$header"
            printf '#include <%s>\n' "$header" >> "$accepted"
        else
            printf 'fp libc: skipping unavailable %s header for %s\n' "$header" "$target" >&2
        fi
    done < <(headers_for_target "$target" | awk '!seen[$0]++')
    rm -f "$probe" "$accepted"
}

generate_target() {
    local target="$1"
    local output="$2/$target.fp"
    local triple
    local flags
    local wrapper
    local generated
    triple=$(target_triple "$target") || {
        echo "fp libc: unsupported target OS: $target" >&2
        return 1
    }
    flags="-target $triple -D_POSIX_C_SOURCE=200809L"
    wrapper=$(mktemp "${TMPDIR:-/tmp}/fp-libc.XXXXXX.c")
    generated=$(mktemp "${TMPDIR:-/tmp}/fp-libc.XXXXXX.fp")
    trap 'rm -f "$wrapper" "$generated"' RETURN
    while IFS= read -r header; do
        printf '#include <%s>\n' "$header" >> "$wrapper"
    done < <(available_headers "$target" "$flags")
    if [[ ! -s "$wrapper" ]]; then
        echo "fp libc: no headers available for $target" >&2
        return 1
    fi
    FP_CLANG_FLAGS="$flags" "$fp_bin" compile "$wrapper" \
        --package libc --target fp --skip-typing --output "$generated" || return 1
    {
        printf 'use super::{char, void};\n\n'
        sed '/^pub type void = ();$/d; /^pub type char = u8;$/d' "$generated"
    } > "$output"
    rm -f "$wrapper" "$generated"
    trap - RETURN
}

mkdir -p "$output_dir"

if (($# > 0)); then
    target_os_list="${TARGET_OS:-$(host_os)}"
    target_headers=("$@")
else
    target_os_list="${TARGET_OS:-$(host_os)}"
    target_headers=()
fi

if ((${#target_headers[@]} > 0)); then
    # Explicit headers are retained for focused regeneration and debugging.
    tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/fp-libc.XXXXXX")
    tmp_output=$(mktemp "${TMPDIR:-/tmp}/fp-libc.XXXXXX.fp")
    trap 'rm -rf "$tmp_dir" "$tmp_output"' EXIT
    wrapper="$tmp_dir/libc.c"
    for header in "${target_headers[@]}"; do
        printf '#include <%s>\n' "$header" >> "$wrapper"
    done
    FP_CLANG_FLAGS="${FP_CLANG_FLAGS:-}" "$fp_bin" compile "$wrapper" \
        --package libc --target fp --skip-typing --output "$tmp_output"
    {
        printf 'use super::{char, void};\n\n'
        sed '/^pub type void = ();$/d; /^pub type char = u8;$/d' "$tmp_output"
    } > "$output_dir/${TARGET_OS:-$(host_os)}.fp"
else
    IFS=',' read -r -a targets <<< "$target_os_list"
    if [[ "$target_os_list" == "all" ]]; then
        targets=(linux macos ios freebsd windows)
    fi
    for target in "${targets[@]}"; do
        generate_target "$target" "$output_dir" || {
            echo "fp libc: target $target was not generated" >&2
        }
    done
fi
