bits 64
section .text
global _start

%include "unistd_64.inc"

_start:

read_one:
	push rax ; make some room on the stack
	mov eax, sys_read
	mov edi, 0 ; STDIN_FILENO
	mov rsi, rsp ; buf
	mov edx, 1 ; count
	syscall
	pop rax ; get char from buf

write_one:
	push rax ; put char on the stack
	mov eax, sys_write
	mov edi, 1 ; STDOUT_FILENO
	mov rsi, rsp ; buf
	mov edx, 1 ; count
	syscall
	pop rax ; restore stack

exit:
	xor eax, eax
	mov rax, sys_exit
	syscall
