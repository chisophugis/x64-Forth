bits 64
section .text
global _start

;default rel

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

emit_n:
	dup
	mov rax, tob ; buf
	dup
	mov eax, 2 ; stderr fd (no buffering)
	dup
	mov eax, sys_write
	call syscall3
	drop ; ignore the kernelret
	ret

; TODO write this routine in LLVM IR and see what the LLVM codegen can come
; up with

; h.
; most of the work is just to unpack each nybble into its own byte
hexdot:
	bswap rax ; want to print out MSB first
	mov rcx, 0xF0F0F0F0F0F0F0F0
	and rcx, rax
	xor rax, rcx ; low nybbles of old rax
	shr rcx, 4 ; high nybbles of old rax
	movq xmm0, rax
	movq xmm1, rcx
	punpcklbw xmm1, xmm0
	movdqa xmm0, [hexdigits]
	pshufb xmm0, xmm1
	movdqa [tob], xmm0

	mov eax, 16 ; count
	jmp emit_n


; different ways of doing the same thing
;
;	mov ecx, 0xF0F0F0F0
;	shrd rcx, rcx, 32 ; fill up the rest
;
;	mov rcx, 0xF0F0F0F0F0F0F0F0
;
;	pcmpeqw mm0, mm0 ; set mm0 to all 0xFFF...
;	psllw mm0, 8
;	movq rax, mm0
;
;	mov eax, 0xF0F0F0F0
;	movd mm0, eax
;	punpcklbw mm0, mm0
;
;	mov al, 0xF0
;	movd mm0, eax
;	pxor mm1, mm1
;	pshufb mm0, mm1


; vector idioms
; set a vector register to all 0's
;	pxor xmm0, xmm0
; set a vector register to all 1's
;	pcmpeq[bwd] xmm0, xmm0
; extend low {byte,word,dword,qword}-wise pattern to high
;	punpckl{bw,wd,dq,qdq} xmm0, xmm0

; repeatedly doing pshufb on itself to a register effectively amounts to
; permuting it, based on itself. This could be interesting if there are
; multiple permutation cycles (of different period) inside the vector
; register
; in general, pshufb can compose permutations

; to broadcast the lowest byte of xmm0, do
;	punpcklbw xmm0, xmm0
;	punpcklbw xmm0, xmm0 ; punpcklwd would work too, but prob be slower
;	punpcklbw xmm0, xmm0
;	punpcklbw xmm0, xmm0
; or if a scratch register is available:
;	pxor xmm1, xmm1
;	pshufb xmm0, xmm1
; to propagate the lowest word, do
;	punpcklwd xmm0, xmm0
;	punpcklwd xmm0, xmm0
;	punpcklwd xmm0, xmm0

init:
	cld ; data stack grows down
	mov rsi, rsp
	sub rsi, 0x1000 ; put data stack one page below return stack
	ret

_start:
	call init

	mov rax, 0xdeadbeefcafebabe
	call hexdot


	mov r13, 5
.loop:
	call key

	dup
	call hexdot

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
align 16
; text input buffer and text output buffer
tib	times 128 db 0
tob	times 128 db 0
hexdigits:
	db '0123456789abcdef'



; note to self: mmap takes 6 args
