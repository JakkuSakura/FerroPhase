#!/usr/bin/env python3
#
# Transitional generator: `scripts/generate_libc_bindings.fp` is intended to
# become the source of truth, and this Python version should eventually be
# transpiled from it.

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


REPO_ROOT = Path(__file__).resolve().parent.parent
OUTPUT_PATH = REPO_ROOT / "crates" / "fp-lang" / "src" / "std" / "libc" / "generated.fp"

CANONICAL_TYPE_ALIASES = [
    "pub type c_char = i8;",
    "pub type c_int = i32;",
    "pub type pid_t = i32;",
    "pub type size_t = usize;",
    "pub type ssize_t = isize;",
]

RUST_TO_FP_TYPES = {
    "!": "c_int",
    "i8": "i8",
    "i32": "i32",
    "isize": "isize",
    "u8": "u8",
    "u32": "c_int",
    "usize": "usize",
    "c_char": "c_char",
    "c_int": "c_int",
    "c_void": "u8",
    "pid_t": "pid_t",
    "size_t": "size_t",
    "ssize_t": "ssize_t",
    "__int32_t": "c_int",
    "__darwin_pid_t": "pid_t",
    "std::os::raw::c_char": "c_char",
    "std::os::raw::c_int": "c_int",
    "std::os::raw::c_void": "u8",
    "::std::os::raw::c_char": "c_char",
    "::std::os::raw::c_int": "c_int",
    "::std::os::raw::c_void": "u8",
    "::std::ffi::c_char": "c_char",
    "::std::ffi::c_int": "c_int",
    "::std::ffi::c_void": "u8",
}


@dataclass
class BindingSet:
    type_aliases: list[str]
    consts: list[str]
    externs: list[str]


def build_wrapper_header() -> str:
    return "\n".join(
        [
            "#include <sys/types.h>",
            "#include <sys/wait.h>",
            "#include <unistd.h>",
            "#include <fcntl.h>",
            "",
        ]
    )


def normalize_rust_type(raw: str) -> str:
    text = raw.strip()
    text = text.replace("::std::os::raw::", "")
    text = text.replace("::std::ffi::", "")
    text = text.replace("std::os::raw::", "")
    text = text.replace("std::ffi::", "")
    text = re.sub(r"\s+", " ", text)

    if 'extern "C" fn' in text:
        if text.startswith("::std::option::Option<") or text.startswith("std::option::Option<"):
            return "::std::option::Option<usize>"
        return "usize"

    if text in RUST_TO_FP_TYPES:
        return RUST_TO_FP_TYPES[text]
    if text.startswith("*const "):
        return f"*const {normalize_rust_type(text[len('*const '):])}"
    if text.startswith("*mut "):
        return f"*mut {normalize_rust_type(text[len('*mut '):])}"
    return text


def normalize_function_arg_type(raw: str) -> str:
    normalized = normalize_rust_type(raw)
    if normalized == "*const c_char":
        return "&std::ffi::CStr"
    return normalized


def parse_bindgen_output(bindgen_rs: str) -> BindingSet:
    type_aliases = list(CANONICAL_TYPE_ALIASES)
    consts: list[str] = []
    externs: list[str] = []
    seen_type_aliases = set(CANONICAL_TYPE_ALIASES)
    seen_consts: set[str] = set()
    seen_externs: set[str] = set()

    type_pattern = re.compile(r"^pub type (\w+) = (.+);$", re.M)
    const_pattern = re.compile(r"^pub const (\w+): ([^=]+)=\s*(.+);$", re.M)
    fn_pattern = re.compile(r"pub fn (\w+)\s*\((.*?)\)\s*(?:->\s*([^;]+))?;", re.S)
    arg_pattern = re.compile(r"(\w+):\s*(.+)")
    extern_block_pattern = re.compile(r'unsafe extern "C"\s*\{(.*?)\}', re.S)

    for match in type_pattern.finditer(bindgen_rs):
        name, rust_ty = match.groups()
        alias = f"pub type {name} = {normalize_rust_type(rust_ty)};"
        if alias in seen_type_aliases:
            continue
        type_aliases.append(alias)
        seen_type_aliases.add(alias)

    for match in const_pattern.finditer(bindgen_rs):
        name, _rust_ty, _value = match.groups()
        if name in seen_consts:
            continue
        seen_consts.add(name)

    for extern_block in extern_block_pattern.finditer(bindgen_rs):
        block = extern_block.group(1)
        for match in fn_pattern.finditer(block):
            name, raw_args, raw_ret = match.groups()

            raw_args = re.sub(r"\s+", " ", raw_args.strip())
            if "..." in raw_args:
                if name == "open":
                    signature = (
                        'pub extern "C" fn open(path: &std::ffi::CStr, oflag: c_int, mode: mode_t) -> c_int;'
                    )
                    if signature not in seen_externs:
                        externs.append(signature)
                        seen_externs.add(signature)
                continue

            fp_args: list[str] = []
            if raw_args.strip():
                for raw_arg in split_args(raw_args):
                    arg_match = arg_pattern.match(raw_arg.strip())
                    if not arg_match:
                        raise ValueError(f"unable to parse function arg: {raw_arg}")
                    arg_name, arg_ty = arg_match.groups()
                    fp_args.append(f"{arg_name}: {normalize_function_arg_type(arg_ty)}")

            signature = f'pub extern "C" fn {name}({", ".join(fp_args)})'
            if raw_ret and raw_ret.strip() != "()":
                signature += f" -> {normalize_rust_type(raw_ret)}"
            signature += ";"
            if signature in seen_externs:
                continue
            externs.append(signature)
            seen_externs.add(signature)

    return BindingSet(type_aliases=type_aliases, consts=consts, externs=externs)


def split_args(raw_args: str) -> list[str]:
    args: list[str] = []
    current: list[str] = []
    depth = 0
    for ch in raw_args:
        if ch == "," and depth == 0:
            args.append("".join(current).strip())
            current = []
            continue
        if ch in "(<[":
            depth += 1
        elif ch in ")>]":
            depth -= 1
        current.append(ch)
    if current:
        args.append("".join(current).strip())
    return args


def render_output(bindings: BindingSet) -> str:
    header = [
        "// @generated by scripts/generate_libc_bindings.py",
        "// Regenerate with: python3 scripts/generate_libc_bindings.py",
        "// Source: bindgen over libc wrapper headers.",
    ]
    sections: list[str] = ["\n".join(header), ""]
    sections.extend(bindings.type_aliases)
    sections.append("")
    sections.extend(bindings.consts)
    sections.append("")
    sections.extend(bindings.externs)
    sections.append("")
    return "\n".join(sections)


def run_bindgen() -> BindingSet:
    bindgen = shutil.which("bindgen")
    if bindgen is None:
        raise RuntimeError(
            "bindgen was not found in PATH. Install it with `cargo install bindgen-cli`."
        )

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        wrapper = temp_path / "wrapper.h"
        wrapper.write_text(build_wrapper_header(), encoding="utf-8")

        cmd = [
            bindgen,
            str(wrapper),
            "--no-layout-tests",
            "--no-doc-comments",
        ]

        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        return parse_bindgen_output(result.stdout)


def write_if_changed(path: Path, content: str) -> bool:
    if path.exists() and path.read_text(encoding="utf-8") == content:
        return False
    path.write_text(content, encoding="utf-8")
    return True


def main(argv: Iterable[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Generate fp std::libc bindings.")
    parser.add_argument(
        "--output",
        type=Path,
        default=OUTPUT_PATH,
        help="Path to generated.fp output",
    )
    args = parser.parse_args(list(argv) if argv is not None else None)

    try:
        bindings = run_bindgen()
    except RuntimeError as exc:
        print(str(exc), file=sys.stderr)
        return 1
    except subprocess.CalledProcessError as exc:
        print(exc.stderr, file=sys.stderr)
        return 1
    except ValueError as exc:
        print(str(exc), file=sys.stderr)
        return 1

    content = render_output(bindings)
    changed = write_if_changed(args.output, content)
    action = "updated" if changed else "unchanged"
    print(f"{args.output} {action} via bindgen")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
