# Intrinsics: General vs Strict

This document defines the intrinsic model and how it flows through the pipeline.

## Terminology

- **General intrinsics**: Everything that is *not* a strict intrinsic. This
  includes helper macros, std functions, container helpers, and any other
  framework-recognized operations that are normalized, typed, or evaluated with
  intrinsic support.
- **Strict intrinsics**: Functions under `std::intrinsic::...` that carry
  `#[lang]` items. These are the only lang-item-backed intrinsics and are meant
  to be invoked by std wrappers, not user code.
- **Keywords** (`const`, `quote`, `splice`): Staging features handled by the
  parser and const evaluator. They are not intrinsics.
- **Builtins** (`emit!`): Parse-time sugar lowered during normalization. They are
  not intrinsics.

## Core Model

Intrinsic handling is AST-centric. Frontends lower their surface syntax into a
canonical AST, type inference annotates that AST, and both the interpreter and
backends consume the same typed structures.

The system is split by responsibility:

1. **Normalization (frontend-facing)**
   - Recognizes general intrinsic spellings and rewrites them into canonical
     forms.
   - Canonical forms are stable `std::...` symbols or `IntrinsicCallKind`
     variants.

2. **Materialization (backend-facing)**
   - Maps canonical intrinsics to backend implementations (interpreter, LLVM,
     transpilers).
   - Enforces target-specific constraints (const-only, runtime-only, unsupported).

## Canonical Forms

General intrinsics normalize to one of two canonical shapes:

- **Canonical symbols**: `std::...` paths that are resolved by the intrinsic
  registry. Example: `std::type::size_of`.
- **IntrinsicCallKind tags**: AST nodes that explicitly carry intrinsic meaning
  (used for method-style helpers and some compile-time-only operations).

Strict intrinsics are always `std::intrinsic::...` and always `#[lang]` items.
They exist so the compiler can attach first-class semantics while keeping the
surface API in std wrappers.

## Intrinsic Families

### Function-style intrinsics

Function-style intrinsics live in the canonical registry and are lowered to
`std::...` symbols. Examples include `std::io::print`, `std::alloc::realloc`, and
`std::type::size_of`. These can be implemented by the interpreter or lowered to
native runtime calls by backends.

### Type utilities

Type utilities are intrinsics that create or inspect type metadata. General
intrinsic spellings (macros or helpers) normalize to canonical calls or strict
intrinsics.

Examples:

- `sizeof!` -> `std::type::size_of` (general intrinsic)
- `clone_struct!` -> `IntrinsicCallKind::CloneStruct` (general intrinsic)
- `std::intrinsic::create_struct` -> `IntrinsicCallKind::CreateStruct` (strict)
- `std::intrinsic::addfield` -> `IntrinsicCallKind::AddField` (strict)

Strict type utilities are invoked by std wrappers such as `std::meta::TypeBuilder`.
User-facing APIs should continue to use wrappers or general intrinsic spellings.

### Method-style helpers

A small set of method calls are treated as general intrinsics. The frontend tags
these calls with `IntrinsicCallKind` so typing and const evaluation can handle
them directly. After const evaluation they reduce to ordinary method calls or
constants.

Examples:

- `.len()` -> `IntrinsicCallKind::Len`
- `.has_field(name)` -> `IntrinsicCallKind::HasField`
- `.field_type(name)` -> `IntrinsicCallKind::FieldType`
- `.method_count()` -> `IntrinsicCallKind::MethodCount`

## Pipeline Integration

1. Frontend parsing produces a regular AST with surface spellings.
2. **Normalization** rewrites intrinsic spellings into canonical forms.
3. Type inference annotates the canonical AST.
4. Const evaluation executes intrinsic calls and may mutate the AST.
5. Backends materialize remaining intrinsics for their target.

## Implementation Notes

- The shared data model lives in `crates/fp-core/src/intrinsics`.
- Lang items are collected from std modules and registered before intrinsic
  normalization and const evaluation.
- `std::intrinsic` is the only home for `#[lang]` items.
- Std wrappers should call strict intrinsics; user code should not.

## Extension Checklist

To add a new intrinsic:

1. Decide whether it is **general** (most cases) or **strict** (requires
   compiler support via `#[lang]`).
2. Add or update canonical forms in the intrinsic registry.
3. Teach the frontend normalizer how to rewrite the surface spelling.
4. Provide interpreter behavior and backend materialization as needed.
5. Add tests or examples that exercise const evaluation and runtime behavior.
