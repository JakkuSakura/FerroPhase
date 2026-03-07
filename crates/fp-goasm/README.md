# fp-goasm

Go assembler text emitter for FerroPhase LIR.

Current scope:
- Emits Go assembly text for `amd64` and `arm64`
- Validated with `go tool asm` in tests when Go is available
- Unsupported LIR constructs are emitted as explicit comments instead of fake instructions

CLI usage:
- `fp compile ... --backend binary --emitter goasm`
