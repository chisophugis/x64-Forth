; Floppy boot segment

org 0 ; actually 7c00
start: jmp  start0
    nop

    db 'cmcf 1.0'
    dw 512     ; bytes/sector
    db 1       ; sector/cluster
    dw 1       ; sector reserved
    db 2       ; FATs
    dw 16*14   ; root directory entries
    dw 80*2*18 ; sectors
    db 0F0h    ; media
    dw 9       ; sectors/FAT
    dw 18      ; sectors/track
    dw 2       ; heads
    dd 0       ; hidden sectors
    dd 80*2*18 ; sectors again
    db 0       ; drive
;    db 0
;    db 29h     ; signature
;    dd 44444444h     ; serial
;    db 'COLOR FORTH' ; label
;    db '        '

command  db 0
         db 0   ; head, drive
cylinder db 0
         db 0   ; head
         db 1   ; sector
         db 2   ; 512 bytes/sector
         db 18  ; sectors/track
         db 1bh ; gap
         db 0ffh
ALIGN 4
nc dd 9 ; Forth+Icons+blocks 24-161
gdt dw 17h
    dd offset gdt0
ALIGN 8
gdt0 dw 0, 0, 0, 0
    dw 0FFFFh, 0, 9A00h, 0CFh ; code
    dw 0FFFFh, 0, 9200h, 0CFh ; data

; Code is compiled in Protected 32-bit mode.
; Hence  org $-2  to fix 16-bit words
; and 4 hand-assembled instructions.
; and EAX and AX exchanged
; This code is in Real 16-bit mode

start0: mov  EAX, 4F02h ; Video mode
org $-2
    mov  EBX, vesa ; hp*vp rgb: 565
org $-2
    int  10h
    cli
    xor  AX, AX    ; Move code to 0
    mov  BX, AX
    mov  EBX, CS
    mov  DS, EBX
    mov  ES, EAX
    mov  DI, AX
    mov  SI, AX
    call $+5 ; Where are we? IP+4*CS
org $-2
loc: pop  ESI
    sub  ESI, offset loc-offset start
org $-2
    mov  ECX, 512/4
org $-2
    rep movsw
;    jmp  0:relocate
    db 0eah
    dw offset relocate-offset start, 0

relocate: ; This code is executed from 0
    mov  DS, EAX
;   lgdt fword ptr gdt
    db 0fh, 1, 16h
    dw offset gdt-offset start
    mov  AL, 1
    mov  CR0, EAX
;    jmp  8:protected
    db 0eah
    dw offset protected-offset start, 8

protected: ; Now in Protected 32-bit mode
    mov  AL, 10h
    mov  DS, EAX
    mov  ES, EAX
    mov  SS, EAX
    mov  ESP, Gods
    xor  ECX, ECX

A20: mov  AL, 0d1h
    out  64h, AL
@@:     in   AL, 64h
        and  AL, 2
        jnz  @b
    mov  AL, 4bh
    out  60h, AL

    call dma
    shl  EBX, 4
    add  ESI, EBX
    cmp  dword ptr [ESI], 44444444h ; Boot?
    jnz  cold
        mov  CX, 63*100h-80h ; Nope
        rep movsd
        mov  ESI, Godd
        jmp  start2

cold:   call sense_
        jns  cold
    mov  ESI, Godd
    xor  EDI, EDI ; Cylinder 0 on top of Address 0
    mov  CL, byte ptr nc
@@:     push ECX
        call READ
        inc  cylinder
        pop  ECX
        loop @b
start2: call stop
    jmp  start1

us equ 1000/6
ms equ 1000*us
SPIN: mov  CL, 1ch
    call onoff
;    mov  DX, 3f2h
;    out  DX, AL
@@:     call sense_
        jns  @b
    mov  cylinder, 0 ; calibrate
    mov  AL, 7
    mov  CL, 2
    call cmd
    mov  ECX, 500*ms
@@:     loop @b
cmdi:   call sense_
        js   cmdi
    ret

ready: ;call delay
    mov  DX, 3f4h
@@:     in   AL, DX
        out  0e1h, AL
        shl  AL, 1
        jnc  @b
    lea  EDX, [EDX+1]
    ret

transfer: mov  CL, 9
cmd: lea  EDX, command
    mov  [EDX], AL
cmd0: push ESI
    mov  ESI, EDX
cmd1:   call ready
        jns  @f
            in   AL, DX
            jmp  cmd1
@@:     lodsb
        out  DX, AL
        out  0e1h, AL
        loop cmd1
    pop  ESI
;delay: mov  EAX, us
;@@:     dec  EAX
;        jnz  @b
    ret

sense_: mov  AL, 8
    mov  ECX, 1
    call cmd
@@:     call ready
        jns  @b
    in   AL, DX
    out  0e1h, AL
    and  AL, AL
;    cmp  AL, 80h
    ret

seek:   call sense_
        jns  seek
    ret

stop: mov  CL, 0ch ; Motor off
onoff: DUP_
    mov  AL, CL
    mov  DX, 3f2h
    out  DX, AL
    out  0e1h, AL
    DROP
    ret

dma: mov  word ptr command+1, 3a2h ; l2 s6 u32 ms (e 2)
    mov  AL, 3 ; timing
    mov  CL, 3
    call cmd
    mov  word ptr command+1, 7000h ; +seek -fifo -poll
    mov  AL, 13h ; configure
    mov  CL, 4
    call cmd
    mov  dword ptr command, ECX ; 0
    ret

READ: call SEEK
    mov  AL, 0e6h ; Read normal data
    call TRANSFER
    mov  CX, 18*2*512
@@:     call ready
        in   AL, DX
        out  0e1h, AL
        stosb
        next @b
    ret

WRITE: call SEEK
    mov  AL, 0c5h ; Write data
    call TRANSFER
    mov  CX, 18*2*512
@@:     call ready
        lodsb
        out  DX, AL
        out  0e1h, AL
        next @b
    ret

org 1feh ; Mark boot sector
    dw 0aa55h
    dd 44444444h ; Mark COLOR.COM

flop: mov  cylinder, AL ; c-cx
    DUP_
    mov  DX, 3f2h
    in   AL, DX
    out  0e1h, AL
    test AL, 10h
    jnz  @f
        jmp  spin
@@: ret

readf: call flop ; ac-ac
    push EDI
    mov  EDI, [ESI+4]
    shl  EDI, 2
    call READ
    pop  EDI
readf1: DROP
    inc  EAX
    add  dword ptr [ESI], 1200h
    ret

writef: call flop ; ac-ac
    push ESI
    mov  ESI, [ESI+4]
    shl  ESI, 2
    call WRITE
    pop  ESI
    jmp  readf1

seekf: call flop ; c-c
;    call delay
    call seek
    mov  AL, 0fh
    mov  CL, 3
    call cmd
    call cmdi
    DROP
    ret

cmdf: mov  ECX, EAX ; an
    DROP
    lea  EDX, [EAX*4]
    call cmd0
    DROP
    ret

readyf: DUP_
    call ready
    DROP
    ret
