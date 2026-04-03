#![no_std]

use core::arch::global_asm;

// Linux x86_64: getpid(); exit(0)
//   mov rax, 39
//   syscall
//   mov rax, 60
//   mov rdi, 0
//   syscall

global_asm!(
    r#"
    .section .text
    .global main
    .type main,@function
main:
    mov rax, 39
    syscall
    mov rax, 60
    mov rdi, 0
    syscall
    ret
"#
);
