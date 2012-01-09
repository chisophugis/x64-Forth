;Generic graphics

ALIGN 4
frame dd 2000000h-hp*vp*2 ; 32 M
displ dd 0f0000000h ; fujitsu
fore  dd 0f7deh
xc    dd 0
yc    dd 0

rgb: ror  EAX, 8
    shr  AX, 2
    ror  EAX, 6
    shr  AL, 3
    rol  EAX, 6+5
    and  EAX, 0f7deh
    ret

white: DUP_
    mov  EAX, 0ffffffh
color: call rgb
    mov  fore, EAX
    DROP
    ret

north: mov  EDX, 0cf8h
    out  DX, EAX
    add  EDX, 4
    in   EAX, DX
    ret

dev: mov  EAX, 80001008h ; Find display, start at device 2
    mov  ECX, 31-1 ; end with AGP: 10008, bus 1, dev 0
@@:     DUP_
        call north
        and  EAX, 0ff000000h
        cmp  EAX, 3000000h
        DROP
        jz   @f
        add  EAX, 800h
        next @b
@@: ret

ati0: call dev
    or   dword ptr [EAX-4], 2 ; Enable memory
    add  AL, 24h-8 ; look for prefetch
    mov  CL, 5
@@:     DUP_
        call north
        xor  AL, 8
        jz   @f
        DROP
        sub  EAX, 4
        next @b
    DUP_
    call north
    and  EAX, 0fffffff0h
@@: mov  displ, EAX
    DROP
    ret

fifof: DROP
graphic: ret

switch:
;    DUP_
    push ESI
    mov  ESI, frame
    push EDI
    mov  EDI, displ ; 0f2000000h eMonster Nvidia
;    xor  EAX, EAX
    mov  ECX, hp*vp/2
;@@:     lodsd
;        add  EAX, [EDI]
;        rcr  EAX, 1
;        and  EAX, 0f7def7deh
;        stosd
;        next @b
    rep movsd
    pop  EDI
    pop  ESI
;    DROP
    jmp  PAUSE

clip: mov  EDI, xy
    mov  ECX, EDI
    test CX, CX
    jns  @f
        xor  ECX, ECX
@@: and  ECX, 0ffffh
    mov  yc, ECX
    imul ECX, hp*2
;    shl  ECX, 10+1
    sar  EDI, 16
    jns  @f
        xor  EDI, EDI
@@: mov  xc, EDI
    lea  EDI, [EDI*2+ECX]
    add  EDI, frame
    ret

bit16: lodsw
    xchg AL, AH
    mov  ECX, 16
b16: shl  AX, 1
        jnc  @f
            mov  [EDI], DX
@@:     add  EDI, 2
        next b16
    ret

bit32: lodsw
    xchg AL, AH
    mov  ECX, 16
b32: shl  EAX, 1
        jnc  @f
            mov  [EDI], DX
            mov  [EDI+2], DX
            mov  [EDI+hp*2], DX
            mov  [EDI+hp*2+2], DX
@@:     add  EDI, 4
        next b32
    ret

emit: call qcr
    push ESI
    push EDI
    push EDX
     imul EAX, 16*24/8
     lea  ESI, icons[EAX]
     call clip
     mov  EDX, fore
     mov  ECX, 24
@@:     push ECX
        call bit16
        add  EDI, (hp-16)*2
        pop  ECX
        next @b
    pop  EDX
    pop  EDI
    pop  ESI
BL_: DROP
SPACE: add  xy, iw*10000h
    ret

emit2: push ESI
    push EDI
    push EDX
     imul EAX, 16*24/8
     lea  ESI, icons[EAX]
     call clip
     mov  EDX, fore
     mov  ECX, 24
@@:     push ECX
        call bit32
        add  EDI, (2*hp-16*2)*2
        pop  ECX
        next @b
    pop  EDX
    pop  EDI
    pop  ESI
    add  xy, iw*10000h*2
    DROP
    ret

text1: call WHITE
    mov  lm, 3
    mov  rm, hc*iw
    jmp  TOP

line: call clip
    mov  ECX, [ESI]
    shl  ECX, 1
    sub  EDI, ECX
    mov  ECX, EAX
    mov  EAX, fore
    rep stosw
    inc  xy
    DROP
    DROP
    ret

box: call clip
    cmp  EAX, vp+1
    js   @f
        mov  EAX, vp
@@: mov  ECX, EAX
    sub  ECX, yc
    jng  no
    cmp  dword ptr [ESI], hp+1
    js   @f
        mov  dword ptr [ESI], hp
@@: mov  EAX, xc
    sub  [ESI], EAX
    jng  no
    mov  EDX, hp
    sub  EDX, [ESI]
    shl  EDX, 1
    mov  EAX, fore
@@:     push ECX
         mov  ECX, [ESI]
         rep stosw
         add  EDI, EDX
        pop  ECX
        next @b
no: DROP
    DROP
    ret
