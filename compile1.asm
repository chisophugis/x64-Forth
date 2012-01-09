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
;
;	rcx rbx - scratch space (don't really care though, can clobber)

; DOC
; Linux x64 syscall ABI
;	rax: syscall#
;	rdi rsi rdx r10 r8 r9: arg1-6
;	rcx r11: clobbered

; ================
;  Utility Macros
; ================

; there are many ways to implement these, but these are used since they
; don't modify FLAGS

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



; =========
;  Control
; =========


; this is where the next code we compile goes
var_HERE:
	dq TEXT_START + TEXT_LEN

action:
	dq interpret

; XXX these change together as far as I can tell, it would be better to
; unify them.
cur_spell:
	dq forth_spellings
cur_off:
	dq forth_offsets
cur_cnt:
	dq forth_cnt


; number of entries in the forth dictionary
forth_cnt:
	dq forth_spellings.len / 8

; number of entries in the macro dictionary
macro_cnt:
	dq macro_spellings.len / 8

; ==============
;  Dictionaries
; ==============

; DOC
; each entry in the dictionary is up to 8 chars, the remaining chars are
; filled with '\0'. thus, only 8 chars of a name are significant.

; DOC
; there are two dictionaries, a "compile time" (macro) and "run-time"
; (forth) dictionary

%macro dict_name 1 ; spelling
%%beginchars:
	db %1
	times 8-($-%%beginchars) db 0
%endmacro

%macro dict_symb 1 ; symbolname
	dq %1
%endmacro

align 4096, db 0
forth_spellings: ; the spellings of words
	dict_name '.'
	dict_name '.s'
	dict_name 'here'
	dict_name '+'
	dict_name '-'
	dict_name '~'
	dict_name '_exit'
	dict_name 'time'
	dict_name 'emit'
	dict_name 'dup'
	dict_name 'syscall6'
	dict_name '1,'
	dict_name '2,'
	dict_name '3,'
	dict_name '4,'
	dict_name 'jmp'
	dict_name 'create'
	dict_name 'forth'
	dict_name 'macro'
	dict_name ']'
	dict_name 'abort'
.len	equ $-forth_spellings

align 4096, db 0
forth_offsets: ; the code offsets corresponding to the spellings
	dict_symb hexdot
	dict_symb dot_s
	dict_symb here
	dict_symb plus
	dict_symb minus
	dict_symb invert
	dict_symb _exit
	dict_symb time
	dict_symb emit
	dict_symb dup_
	dict_symb syscall6
	dict_symb _1comma
	dict_symb _2comma
	dict_symb _3comma
	dict_symb _4comma
	dict_symb _jmp
	dict_symb create
	dict_symb forth
	dict_symb macro
	dict_symb rbrack
	dict_symb abort
.len	equ $-forth_offsets

STATIC_ASSERT_EQ forth_spellings.len, forth_offsets.len

align 4096, db 0
macro_spellings:
	dict_name '['
	dict_name 'exit'
	dict_name 'dup'
.len	equ $-macro_spellings

align 4096, db 0
macro_offsets: ; the code offsets corresponding to the spellings
	dict_symb lbrack
	dict_symb exit
	dict_symb cdup
.len	equ $-macro_offsets

STATIC_ASSERT_EQ macro_spellings.len, macro_offsets.len


; =======
;  Words
; =======

section .text
align 4096
TEXT_START:

; DOC
; "here" is Forth parlance for the next location into which to compile
; words/data with comma (',').

; where to compile code into
here:
	dup
	mov rax, [var_HERE]
	ret

; write into here
comma:
	mov ecx, 8
.shared:
	mov rdx, [var_HERE]
	mov [rdx], rax ; take advantage of little-endian
	lea rdx, [rdx + rcx]
	drop
	mov [var_HERE], rdx
	ret

_4comma:
	mov ecx, 4
	jmp comma.shared

_3comma:
	mov ecx, 3
	jmp comma.shared

_2comma:
	mov ecx, 2
	jmp comma.shared

_1comma:
	mov ecx, 1
	jmp comma.shared


_jmp:
	mov rcx, rax
	drop
	jmp rcx

; create an entry in the current dictionary
create:
	call word_
	call snorm
	mov rdi, [cur_cnt] ; one level of indirection
	mov rdx, [rdi]
	mov rcx, [cur_spell]
	mov [rcx + rdx*8], rax
	mov rax, [var_HERE]
	mov rcx, [cur_off]
	mov [rcx + rdx*8], rax
	add rdx, 1
	mov [rdi], rdx
	drop
	ret

; switch to compiling into forth dictionary
forth:
	mov qword [cur_spell], forth_spellings
	mov qword [cur_off], forth_offsets
	mov qword [cur_cnt], forth_cnt
	ret

; switch to compiling into macro dictionary
macro:
	mov qword [cur_spell], macro_spellings
	mov qword [cur_off], macro_offsets
	mov qword [cur_cnt], macro_cnt
	ret

; exit
; compile a return
; TODO tail call elimination (first need to keep track of the last
; instruction compiled though)
exit:
	mov rcx, [var_HERE]
	mov [rcx], byte 0xc3 ; ret
	add qword [var_HERE], 1
	ret

; compile a dup
cdup:
	mov rcx, 0x068948f8768d48 ; same as what the dup macro expands to
	mov rbx, [var_HERE]
	mov [rbx], rcx
	add rbx, 7 ; 7 bytes
	mov [var_HERE], rbx
	ret

; .s
; print out the stack nondestructively
dot_s:
	dup
	mov rcx, rsi
.loop:
	mov rax, [rcx]
	push rcx
	call hexdot
	dup
	call cr
	pop rcx
	add rcx, 8
	test rcx, 0x0FFF ; page boundary
	jnz .loop
	drop
	ret

dup_:
	dup
	ret

plus:
	add rax, [rsi]
	add rsi, 8
	ret

minus:
	mov rcx, rax
	drop
	sub rax, rcx
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

; string normalize
; put a string just read onto the TIB into the normalized form that we use
; in the dictionary
; ( len -- normalized )
snorm:
	mov ecx, 8
	sub ecx, eax
	shl ecx, 3 ; (8-len) * 8 == #bits to zero
	mov rax, [tib]
	shl rax, cl ; zero topmost (8-len)*8 bits
	shr rax, cl
	ret

; assume word has just been read onto the TIB, length in TOS
mfind:
	call snorm ; XXX call before due to RCX conflict
	mov ecx, [macro_cnt]
	lea rdi, [macro_spellings - 8 + rcx*8]
	jmp find.shared

find:
	call snorm
	mov ecx, [forth_cnt]
	lea rdi, [forth_spellings - 8 + rcx*8]

.shared: ; shared by both find and mfind
	std
	repne scasq
	cld
	ret ; leaves rcx with index of entry, nz if not found

; [
; go to interpretation mode
lbrack:
	mov qword [action], interpret
	ret
; ]
; go to compilation mode
rbrack:
	mov qword [action], compile
	ret


; tightly coupled with find. takes index from rcx left in find
; len on TOS
interpret:
	push rax ; XXX hack so that we can pass len to number
	call find
	pop rax
	jnz number ; maybe its a number then?
	drop ; the length
	jmp [forth_offsets + rcx*8]

; len on TOS
; it may be helpful to look at interpret first
compile:
	push rax
	call mfind
	pop rax
	jnz .compile_call
	drop
	jmp [macro_offsets + rcx*8]

.compile_call:
	push rax
	call find
	pop rax
	jnz .compile_number
	drop
	mov rdi, [var_HERE]
	mov byte [rdi], 0xE8 ; call Jz (pcrel imm32)
	add rdi, 5 ; take into account imm32; rdi := RIP when executing inst
	mov rbx, [forth_offsets + rcx*8] ; &inst ; could reuse rcx
	; RIP + imm32 == &inst <==> rdi + imm32 == rbx <==> rbx - rdi == imm32
	sub rbx, rdi
	mov dword [rdi-4], ebx
	mov [var_HERE], rdi
	ret

; XXX using inefficient mov with immediate QWORD (kind of big, I imagine
; that most constants are not going to be big enough to need that).
.compile_number:
	call cdup ; don't clobber TOS
	call number
	mov rdi, [var_HERE]
	mov dword [rdi], 0xB848 ; REX.W MOV r64, imm64
	add rdi, 2
	mov [rdi], rax
	drop
	add rdi, 8
	mov qword [var_HERE], rdi
	ret

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

; ( syscall# arg1 arg2 arg3 arg4 arg5 arg6 -- kernelret )
syscall6:
	mov rdi, [rsi]
	mov rcx, [rsi + 8] ; move to rsi later
	push rdx
	mov rdx, [rsi + 16]
	mov r10, [rsi + 24]
	mov r8, [rsi + 32]
	mov r9, [rsi + 40]
	lea rsi, [rsi + 48]
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
;	: skipws key ws? if drop skipws fi ;
;	: #read tib a - ;
;	: doword c!+ key ws? if exit fi doword ;
;	: word skipws tib >a doword #read ;
word_: ; name collision with assembler builtin 'word'
.skipws:
	call key
	call .ws?
	jz .after
	drop
	jmp .skipws

.after:
	mov rdx, tib
.doword:
	mov [rdx], al
	add rdx, 1
	drop
	push rdx
	call key
	call .ws?
	pop rdx
	jz .doword

.#read:
	sub rdx, tib
	mov rax, rdx
	ret

; could just check if <= 20
; since for color we might want a couple different kinds of spaces
.ws?:
	xor ecx, ecx
	cmp al, 0x0A ; '\n'
	setz cl
	call .ok
	cmp al, 0x20 ; ' '
	setz ch
	or cl, ch
	ret

; XXX this OK is so naive. it just fires every time a newline is
; encountered. it should fire after the last word on a line is executed.
.ok:
	test cl, cl
	jz .dontprintit
	dup
	mov eax, 0x0a6b6f20 ; " ok\n"
	mov [tob], eax
	mov eax, 4
	call emit_n
.dontprintit
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

; XXX: better to do this without so many branches
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


; =================
;  Helper routines
; =================

cr:
	dup
	mov eax, 0x0a
	mov [tob], eax
	mov eax, 1
	call emit_n
	ret

; this will die with SIGILL instead of SIGSEGV, which will distinguish it
; from the "usual" crashes we run into.
abort:
	ud2

; ( exit-status -- )
_exit:
	mov	edi, eax
	mov	eax, sys_exit
	syscall

; ========
;  "main"
; ========

_start:
.init:
	cld ; data stack grows down
	and rsp, -0x1000 ; page-align down
	mov rsi, rsp
	sub rsi, 0x1000 ; put data stack one page below return stack

; make code pages writable
	dup
	mov eax, 0x7 ; PROT_READ|PROT_WRITE|PROT_EXEC
	dup
	mov eax, 0x1000 ; 1 page
	dup
	mov rax, TEXT_START
	dup
	mov eax, sys_mprotect
	call syscall3
	drop ; kernelret

; once we reach a critical mass of self-sufficiency, then we will be able
; to really prune the system to make it super tight.

; TODO define essential words (in Forth!)
; TODO save the current forth state, so that you can come back to it

.loop:
	call word_
	call [action]
	jmp .loop

TEXT_LEN equ $-TEXT_START


; note to self: the reason a, is cyan is that it must do an offset
; calculation for the PC-rel CALL
