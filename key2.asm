bits 64
section .text
global _start


%include "unistd_64.inc"

; DOC
; registers that we care about:
;	rax - TOS
;	rsi - pointer to second item on stack
;	rsp - top of return stack

; DOC
; linux x64 syscall abi
;	rax: syscall#
;	rdi rsi rdx r10 r8 r9: arg1-6
;	rcx r11: clobbered

%macro dup 0
	lea	rsi, [rsi - 8]
	mov	[rsi], rax
%endmacro

%macro drop 0
	lodsq
%endmacro

; : key 1 tib 0 sys_read syscall3 drop tib c@ ;
key:
	dup
	mov eax, 1
	dup
	mov rax, tib
	dup
	mov eax, 0 ; STDIN_FILENO
	dup
	mov eax, sys_read
	call syscall3
	drop ; kernel ret
	mov al, [tib]
	ret

; : emit tob ! 1 tob 1 sys_write syscall3 drop ;
emit:
	mov [tob], al
	mov eax, 1 ; count
	dup
	mov rax, tob ; buf
	dup
	mov eax, 2 ; stderr fd (no buffering)
	dup
	mov eax, sys_write
	call syscall3
	drop ; ignore the kernelret
	ret


; ( syscall# arg1 arg2 arg3 -- kernelret )
syscall3:
	; syscall# already in rax
	mov rdi, [rsi]
	mov rcx, [rsi + 8] ; tmp, move to rsi later
	mov rdx, [rsi + 16]
	lea rsi, [rsi + 24] ; pop the data stack
	movq mm0, rsi ; temporary place to stash it
	mov rsi, rcx
	syscall
	movq rsi, mm0
	ret


; TODO: print TOS as a number (hex)

init:
	cld ; data stack grows down
	mov rsi, rsp
	sub rsi, 0x1000 ; put data stack one page below return stack
	ret

_start:
	call init

	mov r13, 5
.loop:
	call key
	call emit
	sub r13, 1
	test r13, r13
	jnz .loop

exit:
	mov	edi, eax
	xor	eax, eax
	mov	eax, sys_exit
	syscall

section .data
; text input buffer and text output buffer
tib	times 128 db 0
tob	times 128 db 0



; note to self: mmap takes 6 args
