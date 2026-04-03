#![no_std]

use core::arch::global_asm;

// Linux x86_64: exit(0)
//   mov rax, 60
//   mov rdi, 0
//   syscall
//
// Keep this assembly intentionally tiny so the object lifter can handle it.

global_asm!(
    r#"
    .section .text
    .global main
    .type main,@function
main:
    mov rax, 60
    mov rdi, 0
    syscall
    ret
"#
);
