# Container Transpilation Examples

FerroPhase can treat several "container" inputs as sources for transpilation.

The high-level model is:

`container` → (parse/lift) → `LIR` or `AsmIR` → (emit) → `container`

This document focuses on CLI examples you can run locally.

## JVM `.class` → native object

Transpile a JVM classfile into a native object for a specific target triple:

```bash
fp compile Hello.class \
  --lang jvm-bytecode \
  --backend binary \
  --emitter native \
  --target-triple x86_64-unknown-linux-gnu \
  --output Hello.o
```

## JVM `.jar` → native object

```bash
fp compile Hello.jar \
  --lang jvm-bytecode \
  --backend binary \
  --emitter native \
  --target-triple x86_64-unknown-linux-gnu \
  --output Hello.o
```

Current limitation: the `.jar` reader only supports the `stored` ZIP method (no compression) and rejects JARs that use data descriptors.

## CIL `.il` → native object

```bash
fp compile Hello.il \
  --lang cil \
  --backend binary \
  --emitter native \
  --target-triple x86_64-unknown-linux-gnu \
  --output Hello.o
```

Current limitation: `.dll/.exe` → native is not implemented yet (textual `.il` only).

## URCL → native object

```bash
fp compile prog.urcl \
  --lang urcl \
  --backend binary \
  --emitter native \
  --target-triple x86_64-unknown-linux-gnu \
  --output prog.o
```

## Go asm container → native object

```bash
fp compile prog.goasm \
  --lang goasm \
  --backend binary \
  --emitter native \
  --target-triple x86_64-unknown-linux-gnu \
  --output prog.o
```

## Native "AsmIR text" `.s` → cross-ISA `.s`

FerroPhase's native assembler text format is currently a small, typed, SSA-like syntax.

Example input (`x86_64`-flavoured):

```asm
.globl main
main:
bb0:
    add v1:64, v2:64, 4
    ret
```

Transpile to `aarch64` assembly text by selecting a target triple:

```bash
fp compile main.s \
  --lang x86_64-asm \
  --backend binary \
  --emitter native \
  --target-triple aarch64-unknown-linux-gnu \
  --output main.aarch64.s
```

