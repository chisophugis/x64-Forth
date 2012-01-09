bits 64
global _start
;default rel
%include "unistd_64.inc"

; DOC
; registers that we care about:
;	rax - TOS (Top Of Stack)
;	rsi - pointer to second item on stack
;	rsp - top of return stack
;	rdx - A (Address register)

; DOC
; Linux x64 syscall ABI
;	rax: syscall#
;	rdi rsi rdx r10 r8 r9: arg1-6
;	rcx r11: clobbered

; ================
;  Utility Macros
; ================

; there are many ways to implement these stack macros, but these are used
; since they don't modify any flags

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

%macro nip 0
	lea rsi, [rsi+8]
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

align 16 ; must be aligned for movdqa (the 'a' stands for "aligned")
hexdigits:
	db '0123456789abcdef'

; =========
;  Control
; =========

; this is where the next code we compile goes
var_HERE:
	dq TEXT_START + TEXT_LEN

; DOC
; 'list' is a short name for "last instruction stored"

; address of beginning of last instruction compiled.
; we don't follow this religiously, but it is necessary for the tail-call
; optimization, so we primarily are interested in keeping track of it
; whenever we compile a CALL to a word, because we want to be able to go
; back and change that call to a JMP when appropriate
list:
	dq 0

action:
	dq interpret

; XXX these change together as far as I can tell, it would be better to
; unify them. (extra level of indirection?)
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
	dict_name 'base.'
	dict_name '.s'
	dict_name 'here'
	dict_name '_exit'
	dict_name 'emit'
	dict_name 'syscall3'
	dict_name 'syscall6'
	dict_name '1,'
	dict_name '2,'
	dict_name '3,'
	dict_name '4,'
	dict_name ','
	dict_name 'jmp'
	dict_name 'create'
	dict_name 'forth'
	dict_name 'macro'
	dict_name ']'
	dict_name 'abort'
	dict_name 'key'
	dict_name "'"
	dict_name "''"
	dict_name 'rel32,'
	dict_name 'lit,'
.len	equ $-forth_spellings

align 4096, db 0
forth_offsets: ; the code offsets corresponding to the spellings
	dict_symb hexdot
	dict_symb base_dot
	dict_symb dot_s
	dict_symb here
	dict_symb _exit
	dict_symb emit
	dict_symb syscall3
	dict_symb syscall6
	dict_symb _1comma
	dict_symb _2comma
	dict_symb _3comma
	dict_symb _4comma
	dict_symb comma
	dict_symb _jmp
	dict_symb create
	dict_symb forth
	dict_symb macro
	dict_symb rbrack
	dict_symb abort
	dict_symb key
	dict_symb tick
	dict_symb tick_prime
	dict_symb rel32_comma
	dict_symb lit_comma
.len	equ $-forth_offsets

STATIC_ASSERT_EQ forth_spellings.len, forth_offsets.len

align 4096, db 0
macro_spellings:
	dict_name '['
	dict_name ';'
	dict_name ';;' ; same as semi, but don't exit compile mode
	dict_name 'dup'
	dict_name 'fi'
.len	equ $-macro_spellings

align 4096, db 0
macro_offsets: ; the code offsets corresponding to the spellings
	dict_symb lbrack
	dict_symb semi
	dict_symb exit
	dict_symb cdup
	dict_symb fi
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
	mov rax, [var_HERE] ; XXX maybe use rel? template for other variables
	ret

; write into here
comma:
	mov ecx, 8
.shared:
	mov rbx, [var_HERE]
	mov [rbx], rax ; take advantage of little-endian
	lea rbx, [rbx + rcx]
	drop
	mov [var_HERE], rbx
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

; ' ( -- xt ) get the address of a (forth) word's code
tick:
	call word_
	call find
	jnz abort ; not found
	mov rbx, forth_offsets
	mov rax, [rbx + rcx*8] ; clobber TOS === snorm'd word
	ret

; '' ( -- xt ) get the address of a (macro) word's code
tick_prime:
	call word_
	call mfind
	jnz abort ; not found
	mov rbx, macro_offsets
	mov rax, [rbx + rcx*8] ; clobber TOS === snorm'd word
	ret

; create an entry in the current dictionary
create:
	call word_
	call snorm
	mov rdi, [cur_cnt] ; extra level of indirection
	mov rbx, [rdi]
	mov rcx, [cur_spell]
	mov [rcx + rbx*8], rax
	mov rax, [var_HERE]
	mov rcx, [cur_off]
	mov [rcx + rbx*8], rax
	add rbx, 1
	mov [rdi], rbx
	drop
	ret

; XXX this is a hack. and it bloats the code (together they occupy 74 bytes!)
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

; ;
; like ;; but changes action back to interpret as well
semi:
	call lbrack ; return to intpretation mode
; ;;
; compile a return; does the tail call elimination
exit:
	mov rdi, [list]
	mov rbx, [var_HERE]
	sub rbx, 5
	cmp rbx, rdi
	jnz .compile_ret
	cmp byte [rdi], 0xE8 ; is it a CALL rel32?
	jnz .compile_ret
	add byte [rdi], 1 ; CALL -> JMP
	ret
.compile_ret:
	add rbx, 5
	add qword [var_HERE], 1
	mov [rbx], byte 0xc3 ; ret
	ret

; compile a dup
; this is pretty trivial now, but eventually it will do a nontrivial
; lookback optimization.
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
	test rcx, 0x0FFF ; 4K page boundary
	jnz .loop
	drop
	ret

; fi
; fixup the jump target of an if
fi: ; RIP + rel8 = here <==> rax + rel8 = here <==> rel8 = here - rax
	mov rcx, [var_HERE]
	sub rcx, rax
	mov byte [rax-1], cl
	drop
	ret

; snorm ( len -- normalized ) "string normalize"
; put a string just read onto the TIB into the normalized form that we use
; in the dictionary
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

; rel32,
; ( adr -- ) compile TOS as a rel32 that e.g. CALL or JMP might need
rel32_comma:
	mov rdi, [var_HERE]
	add rdi, 4 ; take into account the space occupied by the rel32
	; RIP + rel32 == &inst <==> rdi + rel32 == rax <==> rel32 = rax - rdi
	sub rax, rdi
	mov dword [rdi-4], eax
	mov [var_HERE], rdi
	drop
	ret

; len in TOS. it may be helpful to look at interpret first
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
	mov rdi, [var_HERE]
	mov [list], rdi
	mov byte [rdi], 0xE8 ; call Jz (rel32)
	add rdi, 1
	mov [var_HERE], rdi
	mov rax, [forth_offsets + rcx*8]
	jmp rel32_comma

; XXX using inefficient MOV with immediate QWORD (kind of big, most
; constants are not big enough to need that).
.compile_number:
	call number
lit_comma: ; compile a literal
	call cdup ; don't clobber TOS
	mov rdi, [var_HERE]
	mov dword [rdi], 0xB848 ; REX.W MOV r64, imm64
	add rdi, 2
	mov [rdi], rax
	drop
	add rdi, 8
	mov qword [var_HERE], rdi
	ret

; XXX try to overlay all the syscall[0-6] words
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
	test rax, rax
	jz _exit_success ; EOF
	xor eax, eax ; ignore the kernelret
	mov al, [pad]
	ret

; simple output
emit: ; write the character in TOS to stdout
	mov [tob], al
	mov eax, 1 ; count
emit_n: ; write TOS characters from tob to stdout
	dup
	mov rax, tob ; buf
emit_n_with_buf: ; write SOS chars from address in TOS to stdout
	dup
	mov eax, 1 ; STDOUT_FILENO
	dup
	mov eax, sys_write
	call syscall3
	drop ; ignore the kernelret
	ret

; .
; print out TOS (in full-width hex)
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

; base. ( n base -- )
; print out a number in the given base (must be <= 10)
; this is not intended to be used directly.
base_dot:
	mov ebx, eax
	drop
	mov ecx, 64 ; max len
.loop:
	xor edx, edx
	div rbx
	sub ecx, 1
	add dl, '0'
	mov byte [tob+rcx], dl
	test rax, rax
	jnz .loop

	mov eax, 64
	sub eax, ecx
	dup
	lea rax, [tob+rcx]
	jmp emit_n_with_buf

; word ( -- cnt )
; read a word into the tib, leave count on stack
; more-or-less equivalent forth:
;	: ws? dup bl = over '\n' = or ;
;	: skipws key ws? if drop skipws fi ;
;	: #read tib a - ;
;	: doword c!+ key ws? if exit fi doword ;
;	: word skipws tib >a doword #read ;
word_: ; '_' due to name collision with assembler builtin 'word'
.skipws:
	call key
	call .ws?
	jz .after
	drop
	jmp .skipws

.after:
	mov rbx, tib
.doword:
	mov [rbx], al
	add rbx, 1
	drop
	push rbx
	call key
	call .ws?
	pop rbx
	jz .doword

.#read:
	sub rbx, tib
	mov rax, rbx
	ret

.ws?:
	xor ecx, ecx
	cmp al, 0x0A ; '\n'
	setz cl
	call .ok
	cmp al, 0x20 ; ' '
	setz ch
	or cl, ch
	ret

; XXX this OK is so naive. it just fires every time a newline is encountered.
.ok:
	test cl, cl
	jz .dontprintit
	dup
	mov eax, 0x0a6b6f20 ; " ok\n"
	mov [tob], eax
	mov eax, 4
	jmp emit_n
.dontprintit:
	ret

; given a length in TOS, convert that many characters off of the tib to a
; number.
;
; more-or-less equivalent forth
;	: within >r over <= over r> < and ;
;	: a-f? 61 67 within ;
;	: 0-9? 30 3a within ;
;	: c># 0-9? if '0' - exit fi a-f? if 'a'-10 - exit fi abort ;
;	: number tib >a 0 swap for 4 lshift c@+ c># + next ;
number:
	mov rbx, tib
	push rax
	xor eax, eax
.loop:
	shl rax, 4
	dup
	xor eax, eax
	mov al, [rbx]
	add rbx, 1
	call c_to_#
	add rax, [rsi]
	add rsi, 8
	sub qword [rsp], 1
	jnz .loop
	add rsp, 8
	ret

c_to_#: ; XXX: better to do this without so many branches
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

; XXX together these take up 72 bytes
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

; abort
; this is where you jump to when there is nothing else for you to do
abort:
	call cr
	call cr
	dup
	mov eax, 8
	dup
	mov rax, tib
	call emit_n_with_buf
	call cr
	dup
	mov rax, 0x0000DEAD0000DEAD
	call dot_s
	add rsi, 0x0FFF ; assume stacks are < 4KB
	add rsp, 0x0FFF
	and rsi, -0x1000
	and rsp, -0x1000
	xor eax, eax
	jmp mainloop

_exit_success:
	mov eax, 0
_exit: ; ( exit-status -- )
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

; make code page writable
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

; TODO acquire file-backed memory mapped page
; TODO maybe aquire another page for the code dictionary
; TODO later: make the "control block" a real block that the editor displays
; TODO later: (color) editor (disable canonical mode?)
; TODO later: save the current forth state, so that you can come back to it
; TODO later: where to store mutable data? (can't store next to code, will make
; processor very unhappy) (hint: store inside source blocks) data dictionary?
; TODO later: switch over from "keystrokes type characters of words" to
; "each keystroke vectors though a table which gives it a behavior"

mainloop:
	call word_
	call [action]
	jmp mainloop

TEXT_LEN equ $-TEXT_START


; PLAN
; the current plan is to make a "color editor" in this Forth, and use that
; to help along a newly written from scratch Forth

; NEXT
; adhere to rules "by design":
;	not modifying flags in assembler
;	tracking last literal compiled (list+4? maybe last two)
;	tracking last call compiled
;	all memory at known relative offsets (full PIC?)
; optimizations (require tracking list):
;	optimize tail calls to jumps
;	backtrack over literals and embed them as immediate operands
;		there are even imm8 versions, so you can save a lot of space
;	erase unnecessary DROPs (e.g. ?dup)
; consider:
;	having another address register B besides A
;		With good register management, we should be able to pull
;		this off without causing any register pressure for the
;		assembler code. (also, since we basically have to use REX
;		for everything anyway we can use r8-r15 "for free")

; IDEA can have a special space for each "color", and every time one is
; encountered, a vectored jump is made through a table of pointers which
; defines how that word is interpreted e.g. a "red space" would basically
; vector to CREATE.

; source code is printed by simply replacing the vectors for each of the
; space types to be something which emits the appropriate visual. one
; register can even be dedicated to being a pointer to the base of a table
; of "space interpretations"

; I also like the idea of having an "immediate buffer" that serves the role
; of line-buffered "lines". i.e. the "word prototypes", which then get
; pushed into the editor buffer as a defined word. this can be accomplished
; easily by vectoring the "spaces" to different meanings in the buffer.
; This will allow us to "compile and execute" interactively.

; note: a lot of the things that in colorForth are macros don't necessarily
; need to be. Some, like if and ; must be macros, but a lot of the other macros
; are just optimizations.

; IDEA
; have the control block actually be a real block, with all of the
; variables being "live updating" (magenta) variables (will want to have
; each one have its own word anyway). this would make a good "first block"
; for the editor, since it already contains all of its values.

; IDEA
; vector each keystroke with one level of indirection. first the key
; indexes into a *byte* table, to save space. Then the value at that byte
; is used to vector through a table of code pointers, passing the key's
; value as an argument.

; RANDOM
; backspace puts words onto a stack, "paste" puts words from the top of the
; stack in (and puts the cursor AFTER) the pasted word

; RANDOM
; can have a magic "circular stack" by using XMM shuffles

; NOTE
; for the implementation of */ and / that I have in Forth, the remainder is
; left in the A register.
