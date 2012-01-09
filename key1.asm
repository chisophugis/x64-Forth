bits 64
section .text
global _start

%include "unistd_64.inc"

; RAX contains TOS
; RSI contains a pointer to second on data stack

%macro dup 0
	lea	rsi, [rsi-4]
	mov	[rsi], rax
%endmacro

; read a char, and leave it in al
; clobbers:
;	rax rdi rsi rdx (rcx r11)
key:
	xor	eax, eax
	push	rax
	mov	eax, sys_read
	mov	edi, 0 ; STDIN_FILENO
	mov	rsi, rsp ; buf
	mov	edx, 1 ; read just 1 char
	syscall
	pop	rax
	ret


; expect char to print in al
; clobbers:
;	rax rdi rsi rdx (rcx r11)
emit:
	push	rax ; rely on little-endian
	mov	eax, sys_write
	mov	edi, 1 ; STDOUT_FILENO
	mov	rsi, rsp ; buf
	mov	edx, 1 ; count
	syscall
	pop	rax
	ret

; DOC
; registers that we care about:
;	rax - TOS
;	rsi - pointer to second on stack
;	rsp - top of return stack

; DOC
; linux x64 syscall abi
;	rax: syscall#
;	rdi rsi rdx r10 r8 r9: arg1-6
;	rcx r11: clobbered


; make a syscall, but save and restore the registers that we care about
%macro safe_syscall 2 ; syscall# argcount

	%if %2 >= 1
		mov rdi, rax
	%endif
	%if %2 >= 2
		lodsq
		mov rbx, rax ; store in rbx temporarily, put in rsi later
	%endif


	; push regs to be saved
	; load params off data stack
	; invoke the call
%endmacro

; make a syscall with zero args ( -- kernelret )
%macro safe_syscall0 1 ; syscall#
	dup
	mov rax, %1
	syscall
%endmacro


; something like:
; %if %2 > 1
;	drop
;	; push rdi
;	xchg rax, rdi
; %endif
; %if %2 > 2
;	drop
;	push rsi ; on return stack, OK
;	xchg rax, rsi
; %endif
; AND THEN LATER REMEMBER TO POP the right values from the return stack



_start:
	cld ; lodsq should take off of data stack
.loop:
	call key
	cmp al, '5' ; special 3C AL,Ib encoding
	jg exit ; jump if rax is greater
	call emit
	jmp .loop

exit:
	mov	edi, eax
	xor	eax, eax
	mov	rax, sys_exit
	syscall
