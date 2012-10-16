section .text
    ; The _start symbol must be declared for the linker (ld)
    global _start

_start:

    ; Prepare arguments for the sys_write system call:
    ;   - eax: system call number (sys_write)
    ;   - ebx: file descriptor (stdout)
    ;   - ecx: pointer to string
    ;   - edx: string length
    mov edx, len
    mov ecx, msg
    mov ebx, 1
    mov eax, 4

    ; Execute the sys_write system call
    int 0x80

    ; Execute the sys_exit system call
    mov eax, 1
    int 0x80

section .data
msg db 'Hello World!', 0xa
len equ $ - msg

