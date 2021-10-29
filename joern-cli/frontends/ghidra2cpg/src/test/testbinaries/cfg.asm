;; Built with `nasm -felf64 cfg.asm -g` and `gcc cfg.o -o cfg.bin`
global main

section .text

main:
    push rbp
    mov  rbp, rsp
    mov  rcx, 10
    xor  rax, rax

.loop:
    cmp rcx, 0
    jle .end
    add rax, 2
    sub rcx, 1
    jmp .loop

.end:
    mov rsp, rbp
    pop rbp
    ret
