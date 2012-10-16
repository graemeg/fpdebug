;[bits 64]
global _start

section .data
message db "Hello World!",0x0a		; message and newline

section .text
_start:
    ; sys_write(stdout, message, length)
mov rax,1	; sys_write
mov rdi,1	; stdout
mov rsi,message	; message
mov rdx,13	; message string length
syscall

    ; sys_exit(return_value)
mov rax,60	; sys_exit
mov rdi,0	; return/exit value of 0
syscall
