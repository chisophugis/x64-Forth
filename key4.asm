bits 64
global _start
;default rel
%include "unistd_64.inc"

; DOC
; registers that we care about:
;	rax - TOS (Top Of Stack)
;	rsi - pointer to second item on stack
;	rsp - top of return stack
;	rdx - Address register ("a")

; DOC
; Linux x64 syscall ABI
;	rax: syscall#
;	rdi rsi rdx r10 r8 r9: arg1-6
;	rcx r11: clobbered

; ================
;  Utility Macros
; ================

%macro dup 0
	lea	rsi, [rsi - 8]
	mov	[rsi], rax
%endmacro

%macro drop 0
	lodsq
%endmacro

%macro over 0
	dup
	mov rax, [rsi + 8]
%endmacro

; quick and dirty macro for simple sanity checking
; will fail to assemble if the two args are not numerically equal.
; when the assertion fails, yasm will say something like:
;	"error: multiple is negative"
%macro STATIC_ASSERT_EQ 2
	; if x != 0, then (-x)|x is -1 and assembly fails
	; if x == 0, then (-x)|x is 0 and this assembles away to nothing
	; y ^ z == 0 iff y == z
	times  ((%1)^(%2)) | (-((%1)^(%2))) db 0
%endmacro


section .data
align 16
tib	times 128 db 0 ; text input buffer
tob	times 128 db 0 ; text output buffer
pad	times 128 db 0 ; scratch space

align 16 ; must be aligned for movdqa ('a' stands for "aligned")
hexdigits:
	db '0123456789abcdef'

; indicates whether we have seen a newline while reading
nl?	db 0


; ============
;  Dictionary
; ============

%macro dict_name 1 ; textname
%%beginchars:
	db %1
	times 8-($-%%beginchars) db 0
%endmacro

%macro dict_symb 1 ; symbolname
	dq %1
%endmacro

; DOC
; each entry in the dictionary is up to 8 chars, the remaining chars are
; filled with '\0'. thus, only 8 chars of a name are significant.

; DOC
; eventually, there will be two dictionaries, a "compile time" and
; "run-time" dictionary.

; number of entries in the dictionary
entry_cnt:
	dq dict.len / 8

align 16
dict: ; the spellings of words
	dict_name '.'
	dict_name 'cr'
	dict_name '+'
	dict_name '~'
	dict_name 'exit'
	dict_name 'time'
	dict_name 'emit'
.len	equ $-dict

align 16
dict_offsets: ; the code offsets corresponding to the spellings
	dict_symb hexdot
	dict_symb cr
	dict_symb plus
	dict_symb invert
	dict_symb exit
	dict_symb time
	dict_symb emit
.len	equ $-dict_offsets

STATIC_ASSERT_EQ dict.len, dict_offsets.len


; =======
;  Words
; =======

section .text

plus:
	add rax, [rsi]
	add rsi, 8
	ret

invert:
	not rax
	ret

time:
	dup
	rdtsc
	shl rdx, 32
	or rdx, rax
	mov rax, rdx
	ret

; assumes word has just been read onto the TIB, length in TOS
find:
	mov ecx, 8
	sub ecx, eax
	shl ecx, 3 ; (8-len) * 8 == #bits to zero
	mov rax, [tib]
	shl rax, cl ; zero topmost (8-len)*8 bits
	shr rax, cl

	mov ecx, [entry_cnt]
	lea rdi, [dict - 8 + rcx*8]
	std
	repne scasq
	cld
	ret ; leaves rcx with index of entry, nz if not found

; tightly coupled with find. takes index from rcx left in find
execute:
	push rax
	call find
	pop rax
	jnz number ; maybe its a number then?
	drop ; the spelling of the word
	jmp [dict_offsets + rcx*8]

; ( syscall# arg1 arg2 arg3 -- kernelret )
syscall3:
	; syscall# already in rax
	mov rdi, [rsi]
	mov rcx, [rsi + 8] ; tmp, move to rsi later
	push rdx
	mov rdx, [rsi + 16]
	lea rsi, [rsi + 24] ; pop the data stack
	push rsi
	mov rsi, rcx
	syscall
	pop rsi
	pop rdx
	ret

; get a single key of input from the user
;	: key 1 pad 0 sys_read syscall3 drop pad c@ ;
key:
	dup
	mov eax, 1
	dup
	mov rax, pad
	dup
	mov eax, 0 ; STDIN_FILENO
	dup
	mov eax, sys_read
	call syscall3
	xor eax, eax ; ignore the kernelret
	mov al, [pad]
	ret

; write the character in TOS to stdout
;	: emit pad c! 1 pad 1 sys_write syscall3 drop ;
emit:
	mov [tob], al
	mov eax, 1 ; count
; FALLTHRU
emit_n:
	dup
	mov rax, tob ; buf
	dup
	mov eax, 1 ; STDOUT_FILENO
	dup
	mov eax, sys_write
	call syscall3
	drop ; ignore the kernelret
	ret

; .
; print out TOS (in hex)
; most of the work is just to unpack each nybble into its own byte
hexdot:
	bswap rax ; want to print out MSB first
	mov rcx, 0xF0F0F0F0F0F0F0F0
	and rcx, rax
	xor rax, rcx ; rax := low nybbles of old rax
	shr rcx, 4 ; rcx := high nybbles of old rax
	movq xmm0, rax
	movq xmm1, rcx
	punpcklbw xmm1, xmm0
	movdqa xmm0, [hexdigits] ; could fetch this earlier into another xmm
	pshufb xmm0, xmm1
	movdqa [tob], xmm0

	mov eax, 16 ; count
	jmp emit_n

; word ( -- cnt )
; read a word into the tib, leave count on stack
; more-or-less equivalent forth:
;	: ws? dup bl = over '\n' = or ;
;	: skipws key ws? if skipws fi ;
;	: #read tib a - ;
;	: doword c!+ key ws? if exit fi doword ;
;	: word skipws tib >a doword #read ;


word_: ; name collision with assembler builtin 'word'
.skipws:
	call key
	call ws?
	jnz .skipws

	mov rdx, tib
.doword:
	mov [rdx], al
	add rdx, 1
	drop
	push rdx
	call key
	call ws?
	pop rdx
	jz .doword

.#read:
	sub rdx, tib
	mov rax, rdx
	ret

ws?:
	xor ecx, ecx
	cmp al, 0x0A ; '\n'
	setz cl
	add byte [nl?], cl ; XXX so that we only print one "ok" per line
	cmp al, 0x20 ; ' '
	setz ch
	or cl, ch
	ret



; given a length in TOS, convert that many characters off of the tib to a
; number.
; XXX: could maybe pass the buffer as an argument as well (it may be easier
; to just vector "key")
;
; more-or-less equivalent forth
;	: within >r over <= over r> < and ;
;	: a-f? 61 67 within ;
;	: 0-9? 30 3a within ;
;	: c># 0-9? if '0' - exit fi a-f? if 'a'-10 - exit fi abort ;
;	: number tib >a 0 swap for 4 lshift c@+ c># + next ;

number:
	mov rdx, tib
	push rax
	xor eax, eax
.loop:
	shl rax, 4
	dup
	xor eax, eax
	mov al, [rdx]
	add rdx, 1
	call c_to_#
	add rax, [rsi]
	add rsi, 8
	sub qword [rsp], 1
	jnz .loop
	add rsp, 8
	ret

; XXX: better to not do this with branches
c_to_#:
	call zero_to_9?
	jz .after
	sub al, '0'
	ret
.after:
	call a_to_f?
	jz .after1
	sub al, 'a'-10
	ret
.after1:
	call A_to_F?
	jz abort
	sub al, 'A'-10
	ret

; this is kind of fragile
; (but note that it is branchless!)
within:
	push rax
	drop
	over
	xor ecx, ecx
	cmp rax, [rsi]
	setb cl
	mov eax, ecx
	sub rax, 1
	add rsi, 8
	over
	dup
	pop rax
	xor ecx, ecx
	cmp rax, [rsi]
	setbe cl
	mov eax, ecx
	sub rax, 1
	add rsi, 24
	and rax, [rsi - 16]
	mov rax, [rsi - 8]
	ret

zero_to_9?:
	dup
	mov eax, 0x30
	dup
	mov eax, 0x3A
	jmp within

a_to_f?:
	dup
	mov eax, 0x61
	dup
	mov eax, 0x67
	jmp within

A_to_F?:
	dup
	mov eax, 0x41
	dup
	mov eax, 0x47
	jmp within

; this will die with SIGILL instead of SIGSEGV, which will distinguish it
; from the "usual" crashes we run into.
abort:
	ud2

; pass exit status on stack
exit:
	mov	edi, eax
	mov	eax, sys_exit
	syscall


; ========================
;  Helper output routines
; ========================

cr:
	dup
	mov eax, 0x0a
	mov [tob], eax
	mov eax, 1
	call emit_n
	ret

ok:
	test byte [nl?], -1
	jz .dontprintit
	mov byte [nl?], 0
	dup
	mov eax, 0x0a6b6f20 ; " ok\n"
	mov [tob], eax
	mov eax, 4
	call emit_n
.dontprintit
	ret

; ========
;  "main"
; ========

_start:
.init:
	cld ; data stack grows down
	and rsp, -0x1000 ; page-align down
	mov rsi, rsp
	sub rsi, 0x1000 ; put data stack one page below return stack

.loop:
	call word_
	call execute
	call ok
	jmp .loop



; note to self: mmap takes 6 args
