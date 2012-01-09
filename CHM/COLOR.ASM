;colorForth, 2001 Jul 22, Chuck Moore, Public Domain

.MODEL tiny
.486p
only SEGMENT USE32
ASSUME DS:only

next MACRO adr
    dec  ECX
    jnz  adr
ENDM

DUP_ MACRO
    lea  ESI, [ESI-4]
    mov  [ESI], EAX
ENDM

DROP MACRO
    lodsd
ENDM

;hp equ 800
;vp equ 600
;vesa equ 114h
hp equ 1024
vp equ 768
vesa equ 117h
buffer equ 604*256
include boot.asm ; boot boot0 hard

;   100000 dictionary
;    a0000 top of return stack
;    9f800 top of data stack
;    9d800 free
;    97000 floppy buffer
;     4800 source
icons equ 12*256*4 ; 3000
;     7c00 BIOS boot sector
;        0 Forth

warm: DUP_
start1: call ATI0
;    mov  screen, offset nul
;    xor  EAX, EAX
    call show0
    mov  forths, (forth1-forth0)/4
    mov  macros, (macro1-macro0)/4
    mov  EAX, 18
    call LOAD
    jmp  ACCEPT

Gods equ 28000h*4 ; 0A0000h
Godd equ Gods-750*4
mains equ Godd-1500*4
maind equ mains-750*4
ALIGN 4
    me dd offset God
screen dd 0 ; logo

ROUND: call unPAUSE
God     dd 0 ; Gods-2*4
    call unPAUSE
main    dd 0 ; mains-2*4
    jmp  ROUND

PAUSE: DUP_
    push ESI
    mov  EAX, me
    mov  [EAX], ESP
    add  EAX, 4
    jmp  EAX

unPAUSE: pop  EAX
    mov  ESP, [EAX]
    mov  me, EAX
    pop  ESI
    DROP
    ret

ACT: mov  EDX, maind-4
    mov  [EDX], EAX
    mov  EAX, mains-4
    pop  [EAX]
    sub  EAX, 4
    mov  [EAX], EDX
    mov  main, EAX
    DROP
    ret

show0: call show
    ret
show: pop  screen
    DUP_
    xor  EAX, EAX
    call ACT
@@:     call graphic
        call [screen]
        call SWITCH
        inc  EAX
        jmp  @b

c_:  mov  ESI, Godd+4
    ret

mark: mov  ECX, macros
    mov  mk, ECX
    mov  ECX, forths
    mov  mk+4, ECX
    mov  ECX, H
    mov  mk+2*4, ECX
    ret

empty: mov  ECX, mk+2*4
    mov  H, ECX
    mov  ECX, mk+4
    mov  forths, ECX
    mov  ECX, mk
    mov  macros, ECX
    mov  class, 0
    ret

mFIND: mov  ECX, macros
    push EDI
    lea  EDI, [macro0-4+ECX*4]
    jmp  @f

FIND: mov  ECX, forths
    push EDI
    lea  EDI, [forth0-4+ECX*4]
@@: std
    repne scasd
    cld
    pop  EDI
    ret

EX1: dec  words ; from keyboard
    jz   @f
        DROP
        jmp  EX1
@@: call FIND
    jnz  ABORT1
        DROP
        jmp  [forth2+ECX*4]

execute: mov  lit, offset alit
    DUP_
    mov  EAX, [-4+EDI*4]
ex2: and  EAX, -20o
    call FIND
    jnz  ABORT
        DROP
        jmp  [forth2+ECX*4]

ABORT: mov  curs, EDI
    shr  EDI, 10-2
    mov  blk, EDI
ABORT1: mov  ESP, Gods
    mov  spaces+3*4, offset forthd
    mov  spaces+4*4, offset qcompile
    mov  spaces+5*4, offset cnum
    mov  spaces+6*4, offset cshort
    mov  EAX, 57o ; ?
    call ECHO_
    jmp  ACCEPT

sDEFINE: pop  aDEFINE
    ret
MACRO_: call sDEFINE
macrod: mov  ECX, macros
    inc  macros
    lea  ECX, [macro0+ECX*4]
    jmp  @f

FORTH: call sDEFINE
forthd: mov  ECX, forths
    inc  forths
    lea  ECX, [forth0+ECX*4]
@@: mov  EDX, [-4+EDI*4]
    and  EDX, -20o
    mov  [ECX], EDX
    mov  EDX, h
    mov  [forth2-forth0+ECX], EDX
    lea  EDX, [forth2-forth0+ECX]
    shr  EDX, 2
    mov  last, EDX
    mov  list, ESP
    mov  lit, offset adup
    test class, -1
    jz   @f
        jmp  [class]
@@: ret

cdrop: mov  EDX, h
    mov  list, EDX
    mov  byte ptr [EDX], 0adh ; lodsd
    inc  h
    ret

qdup: mov  EDX, h
    dec  EDX
    cmp  list, EDX
    jnz  cdup
    cmp  byte ptr [EDX], 0adh
    jnz  cdup
        mov  h, EDX
        ret
cdup: mov  EDX, h
    mov  dword ptr [EDX], 89fc768dh
    mov  byte ptr [4+EDX], 06
    add  h, 5
    ret

adup: DUP_
    ret

var1: DUP_
    mov  EAX, [4+forth0+ECX*4]
    ret
variable: call forthd
    mov  [forth2-forth0+ECX], offset var1
    inc  forths ; dummy entry for source address
    mov  [4+ECX], EDI
    call macrod
    mov  [forth2-forth0+ECX], offset @f
    inc  macros
    mov  [4+ECX], EDI
    inc  EDI
    ret
@@: call [lit]
    mov  EAX, [4+macro0+ECX*4]
    jmp  @f

cNUM: call [lit]
    mov  EAX, [EDI*4]
    inc  EDI
    jmp  @f

cSHORT: call [lit]
    mov  EAX, [-4+EDI*4]
    sar  EAX, 5
@@: call literal
    DROP
    ret

alit: mov lit, offset adup
literal: call qDUP
    mov  EDX, list
    mov  list+4, EDX
    mov  EDX, h
    mov  list, EDX
    mov  byte ptr [EDX], 0b8h
    mov  [1+EDX], EAX
    add  h, 5
    ret

qCOMPILE: call [lit]
    mov  EAX, [-4+EDI*4]
    and  EAX, -20o
    call mFIND
    jnz  @f
        DROP
        jmp  [macro2+ECX*4]
@@: call FIND
    mov  EAX, [forth2+ECX*4]
@@: jnz  ABORT
call_: mov  EDX, h
    mov  list, EDX
    mov  byte ptr [EDX], 0e8h
    add  EDX, 5
    sub  EAX, EDX
    mov  [-4+EDX], EAX
    mov  h, EDX
    DROP
    ret

COMPILE: call [lit]
    mov  EAX, [-4+EDI*4]
    and  EAX, -20o
    call mFIND
    mov  EAX, [macro2+ECX*4]
    jmp  @b

SHORT_: mov lit, offset alit
    DUP_
    mov  EAX, [-4+EDI*4]
    sar  EAX, 5
    ret

NUM: mov lit, offset alit
    DUP_
    mov  EAX, [EDI*4]
    inc  EDI
    ret

comma: mov  ECX, 4
@@: mov  EDX, h
    mov  [EDX], EAX
    mov  EAX, [ESI] ; drop
    lea  EDX, [EDX+ECX]
    lea  ESI, [ESI+4]
    mov  h, EDX
;    DROP
    ret

comma1: mov  ECX, 1
    jmp  @b

comma2: mov  ECX, 2
    jmp  @b

comma3: mov  ECX, 3
    jmp  @b

semi: mov  EDX, h
    sub  EDX, 5
    cmp  list, EDX
    jnz  @f
    cmp  byte ptr [EDX], 0e8h
    jnz  @f
        inc  byte ptr [EDX] ; jmp
        ret
@@: mov  byte ptr [5+EDX], 0c3h ; ret
    inc  h
    ret

then: mov  list, ESP
    mov  EDX, h
    sub  EDX, EAX
    mov  [-1+EAX], DL
    DROP
    ret

begin: mov  list, ESP
here: DUP_
    mov  EAX, h
    ret

qlit: mov  EDX, h
    lea  EDX, [EDX-5]
    cmp  list, EDX
    jnz  @f
    cmp  byte ptr [EDX], 0b8h
    jnz  @f
        DUP_
        mov  EAX, list+4
        mov  list, EAX
        mov  EAX, [1+EDX]
        cmp  dword ptr [EDX-5], 89fc768dh ; dup
        jz   q1
            mov  h, EDX
            jmp  cdrop            
q1:     add  h, -10 ; flag nz
        ret
@@: xor  EDX, EDX ; flag z
    ret

less: cmp  [ESI], EAX
    js   @f ; flag nz
        xor  ECX, ECX ; flag z
@@: ret

qIGNORE: test dword ptr [-4+EDI*4], -20o
    jnz  nul
        pop  EDI
        pop  EDI
nul: ret

jump: pop  EDX
    add  EDX, EAX
    lea  EDX, [5+EAX*4+EDX]
    add  EDX, [-4+EDX]
    DROP
    jmp  EDX

LOAD: shl  EAX, 10-2
    push EDI
    mov  EDI, EAX
    DROP
INTER:  mov  EDX, [EDI*4]
        inc  EDI
        and  EDX, 17o
        call spaces[EDX*4]
        jmp  INTER

ALIGN 4
 spaces dd offset qIGNORE, offset execute, offset NUM
aDEFINE dd 5+offset MACRO_ ; offset macrod ?
        dd offset qCOMPILE, offset cNUM, offset cSHORT, offset COMPILE
        dd offset SHORT_, offset nul, offset nul, offset nul
        dd offset variable, offset nul, offset nul, offset nul

   lit dd offset adup
    mk dd 0, 0, 0
     H dd 40000h*4
  last dd 0
 class dd 0
  list dd 0, 0
macros dd 0
forths dd 0
;macro0 dd (3 shl 4+1)shl 24 ; or
;       dd ((5 shl 4+6)shl 7+140o)shl 17 ; and
;       dd 173o shl 25 ; +
macro0 dd 170o shl 25 ; ;
       dd ((140o shl 7+146o)shl 7+142o)shl 11 ; dup
       dd (((177o shl 7+140o)shl 7+146o)shl 7+142o)shl 4 ; ?dup
       dd (((140o shl 4+1)shl 4+3)shl 7+142o)shl 10 ; drop
;       dd ((6 shl 4+7)shl 7+142o)shl 17 ; nip
       dd (((2 shl 7+144o)shl 4+4)shl 4+6)shl 13 ; then
       dd ((((143o shl 4+4)shl 5+25o)shl 4+7)shl 4+6)shl 8 ; begin
macro1 dd 128 dup (0)
forth0 dd (((143o shl 4+3)shl 4+3)shl 4+2)shl 13 ; boot
       dd (((27o shl 4+5)shl 4+1)shl 5+21o)shl 14 ; warm
       dd ((((142o shl 4+5)shl 7+146o)shl 5+20o)shl 4+4)shl 5 ; pause
       dd ((((21o shl 4+5)shl 5+22o)shl 4+1)shl 4+3)shl 10 ; MACRO
       dd ((((26o shl 4+3)shl 4+1)shl 4+2)shl 7+144o)shl 8 ; FORTH
       dd 22o shl 27 ; c
       dd (((20o shl 4+2)shl 4+3)shl 7+142o)shl 12 ; stop
       dd (((1 shl 4+4)shl 4+5)shl 7+140o)shl 13 ; read
       dd ((((27o shl 4+1)shl 4+7)shl 4+2)shl 4+4)shl 11 ; write
       dd (6 shl 5+22o)shl 23 ; nc
       dd (((((22o shl 4+3)shl 5+21o)shl 5+21o)shl 4+5)shl 4+6)shl 5; comman d
       dd (((20o shl 4+4)shl 4+4)shl 7+164o)shl 12 ; seek
       dd ((((1 shl 4+4)shl 4+5)shl 7+140o)shl 5+23o)shl 8 ; ready
;       dd (((22o shl 5+24o)shl 4+1)shl 4+7)shl 14 ; clri
       dd ((5 shl 5+22o)shl 4+2)shl 19 ; ACT
       dd (((20o shl 7+144o)shl 4+3) shl 5+27o)shl 11 ; SHOW
       dd (((24o shl 4+3)shl 4+5)shl 7+140o)shl 12 ; LOAD
       dd (((144o shl 4+4)shl 4+1)shl 4+4)shl 13 ; here
       dd (((177o shl 5+24o)shl 4+7)shl 4+2)shl 12 ; ?lit
       dd (153o shl 7+176o) shl 18 ; 3,
       dd (152o shl 7+176o) shl 18 ; 2,
       dd (151o shl 7+176o) shl 18 ; 1,
       dd 176o shl 25 ; ,
       dd (((24o shl 4+4)shl 5+20o)shl 5+20o)shl 13 ; less
       dd (((162o shl 7+146o)shl 5+21o)shl 7+142o)shl 6 ; jump
       dd (((((5 shl 5+22o)shl 5+22o)shl 4+4)shl 7+142o)shl 4+2)shl 3 ; accept
       dd ((142o shl 4+5)shl 7+140o)shl 14 ; pad
       dd ((((4 shl 4+1)shl 4+5)shl 5+20o)shl 4+4)shl 11 ; erase
       dd (((22o shl 4+3)shl 7+142o)shl 5+23o)shl 11 ; copy
       dd (((21o shl 4+5)shl 4+1)shl 7+164o)shl 12 ; mark
       dd (((4 shl 5+21o)shl 7+142o)shl 4+2)shl 12 ; empt
       dd (((4 shl 5+21o)shl 4+7)shl 4+2)shl 15 ; emit
       dd ((((140o shl 4+7)shl 5+25o)shl 4+7)shl 4+2)shl 8 ; digit
       dd ((((152o shl 4+4)shl 5+21o)shl 4+7)shl 4+2)shl 8 ; 2emit
       dd 165o shl 25 ; .
       dd (144o shl 7+165o)shl 18 ; h.
       dd ((144o shl 7+165o)shl 4+6)shl 14 ; h.n
       dd (22o shl 4+1)shl 23 ; CR
       dd ((((20o shl 7+142o)shl 4+5)shl 5+22o)shl 4+4)shl 7 ; space
       dd (((140o shl 4+3)shl 5+27o)shl 4+6)shl 12 ; DOWN
       dd (((4 shl 7+140o)shl 4+7)shl 4+2)shl 13 ; edit
       dd 4 shl 28 ; E
;       dd (((26o shl 4+3)shl 4+6)shl 4+2)shl 15 ; font
       dd (24o shl 5+21o)shl 22 ; lm
       dd (1 shl 5+21o)shl 23 ; rm
       dd ((((25o shl 4+1)shl 4+5)shl 7+142o)shl 7+144o)shl 5 ; graph ic
       dd (((2 shl 4+4)shl 7+145o)shl 4+2)shl 13 ; text
;       dd (153o shl 7+140o)shl 18 ; 3d
;       dd (((((1 shl 4+4)shl 4+6)shl 7+140o)shl 4+4)shl 4+1)shl 5 ; render
;       dd ((((141o shl 4+4)shl 4+1)shl 4+2)shl 4+4)shl 9 ; verte x
;       dd ((((26o shl 4+1)shl 4+3)shl 4+6)shl 4+2)shl 11 ; front
;       dd ((2 shl 4+3)shl 7+142o)shl 17 ; top
;       dd (((20o shl 4+7)shl 7+140o)shl 4+4)shl 12 ; side
       dd ((((164o shl 4+4)shl 5+23o)shl 7+143o)shl 4+3)shl 5 ; keybo ard
       dd (((140o shl 4+4)shl 7+143o)shl 7+146o)shl 7 ; debu g
       dd (5 shl 4+2)shl 24 ; at
       dd ((173o shl 4+5)shl 4+2)shl 17 ; +at
       dd (145o shl 5+23o)shl 20 ; xy
       dd ((26o shl 4+3)shl 7+141o)shl 16 ; fov
       dd (((26o shl 4+7)shl 5+26o)shl 4+3)shl 14 ; fifo
       dd ((143o shl 4+3)shl 7+145o)shl 14 ; box
       dd (((24o shl 4+7)shl 4+6)shl 4+4)shl 15 ; line
       dd ((((22o shl 4+3)shl 5+24o)shl 4+3)shl 4+1)shl 10 ; color
;       dd (((22o shl 5+24o)shl 4+7)shl 7+142o)shl 11 ; clip
       dd (((((3 shl 5+22o)shl 4+2)shl 4+5)shl 4+6)shl 4+2)shl 7 ; octant
       dd (20o shl 7+142o)shl 20 ; sp
       dd (((24o shl 4+5)shl 5+20o)shl 4+2)shl 14 ; last
       dd (((((146o shl 4+6)shl 7+142o)shl 4+5)shl 5+22o))shl 5 ; unpac k
;       dd (((142o shl 4+5)shl 5+22o)shl 7+164o)shl 9 ; pack
forth1 dd 512 dup (0)
;macro2 dd offset cOR
;       dd offset cAND
;       dd offset PLUS
macro2 dd offset semi
       dd offset cdup
       dd offset qdup
       dd offset cdrop
;       dd offset nip
       dd offset then
       dd offset begin
       dd 128 dup (0)
forth2 dd offset boot
       dd offset warm
       dd offset PAUSE
       dd offset MACRO_
       dd offset FORTH
       dd offset c_
       dd offset stop
       dd offset readf
       dd offset writef
       dd offset nc_
       dd offset cmdf
       dd offset seekf
       dd offset readyf
       dd offset ACT
       dd offset SHOW
       dd offset LOAD
       dd offset here
       dd offset qlit
       dd offset COMMA3
       dd offset COMMA2
       dd offset COMMA1
       dd offset COMMA
       dd offset less
       dd offset jump
       dd offset ACCEPT
       dd offset pad
       dd offset erase
       dd offset copy
       dd offset mark
       dd offset empty
       dd offset emit
       dd offset eDIG
       dd offset emit2
       dd offset dot10
       dd offset hdot
       dd offset hdotn
       dd offset CR
       dd offset space
       dd offset DOWN
       dd offset edit
       dd offset E
;       dd offset font
       dd offset LMs
       dd offset RMs
       dd offset graphic
       dd offset text1
;       dd offset set3d
;       dd offset render
;       dd offset vertex
;       dd offset front
;       dd offset top_
;       dd offset side
       dd offset keyboard
       dd offset debug
       dd offset at
       dd offset pat
       dd offset xy_
       dd offset fov_
       dd offset fifof
       dd offset box
       dd offset line
       dd offset color
;       dd offset clip
       dd offset octant
       dd offset sps
       dd offset last_
       dd offset unpack
;       dd offset pack
       dd 512 dup (0)

boot: mov  AL, 0FEh ; Reset
    out  64h, AL
    jmp  $

erase: mov  ECX, EAX
    shl  ECX, 8
    DROP
    push EDI
     mov  EDI, EAX
     shl  EDI, 2+8
     xor  EAX, EAX
     rep stosd
    pop EDI
    DROP
    ret

;move: mov  ECX, EAX
;    DROP
;    mov  EDI, EAX
;    shl  EDI, 2
;    DROP
;    push ESI
;     mov  ESI, EAX
;     shl  ESI, 2
;     rep movsd
;    pop  ESI
;    DROP
;    ret

copy: cmp  EAX, 12
    jc   ABORT1
    mov  EDI, EAX
    shl  EDI, 2+8
    push ESI
     mov  ESI, blk
     shl  ESI, 2+8
     mov  ECX, 256
     rep movsd
    pop  ESI
    mov  blk, EAX
    DROP
    ret

debug: mov  xy, 3*10000h+(vc-2)*ih+3
    DUP_
    mov  EAX, God
    push [EAX]
    call dot
    DUP_
    pop  EAX
    call dot
    DUP_
    mov  EAX, main
    call dot
    DUP_
    mov  EAX, ESI
    jmp  dot

iw equ 16+6
ih equ 24+6
hc equ hp/iw ; 46
vc equ vp/ih ; 25
ALIGN 4
xy  dd 3*10000h+3
lm  dd 3
rm  dd hc*iw ; 1012
xycr dd 0
fov dd 10*(2*vp+vp/2)

nc_: DUP_
    mov  EAX, (offset nc-offset start)/4
    ret

xy_: DUP_
    mov  EAX, (offset xy-offset start)/4
    ret

fov_: DUP_
    mov  EAX, (offset fov-offset start)/4
    ret

sps: DUP_
    mov  EAX, (offset spaces-offset start)/4
    ret

last_: DUP_
    mov  EAX, (offset last-offset start)/4
    ret

include gen.asm ; cce.asm pio.asm ATI128.asm ATI64.asm gen.asm

yellow equ 0ffff00h
CYAN: DUP_
    mov  EAX, 0ffffh
    jmp  color
MAGENTA: DUP_
    mov  EAX, 0ff00ffh
    jmp  color
SILVER: DUP_
    mov  EAX, 0c0c0c0h
    jmp  color
BLUE: DUP_
    mov  EAX, 4040ffh
    jmp  color
RED: DUP_
    mov  EAX, 0ff0000h
    jmp  color
GREEN: DUP_
    mov  EAX, 8000ff00h
    jmp  color

history db 11 dup (0)
ECHO_: push ESI
     mov  ECX, 11-1
     lea  EDI, history
     lea  ESI, [1+EDI]
     rep movsb
    pop  ESI
    mov  history+11-1, AL
    DROP
    ret

RIGHT: DUP_
    mov  ECX, 11
    lea  EDI, history
    xor  EAX, EAX
    rep stosb
    DROP
    ret

DOWN: DUP_
    xor  EDX, EDX
    mov  ECX, ih
    div  ECX
    mov  EAX, EDX
    add  EDX, 3*10000h+8000h-ih+3
    mov  xy, EDX
ZERO: test EAX, EAX
    mov  EAX, 0
    jnz  @f
        inc  EAX
@@: ret

blank: DUP_
    xor  EAX, EAX
    mov  xy, EAX
    call color
    DUP_
    mov  EAX, hp
    DUP_
    mov  EAX, vp
    jmp  box

TOP: mov  ECX, lm
    shl  ECX, 16
    add  ECX, 3
    mov  xy, ECX
    mov  xycr, ECX
    ret

qcr: mov  CX, word ptr xy+2
    cmp  CX, word ptr rm
    js   @f
cr: mov  ECX, lm
    shl  ECX, 16
    mov  CX, word ptr xy
    add  ECX, ih
    mov  xy, ECX
@@: ret

LMs: mov  lm, EAX
    DROP
    ret

RMs: mov  rm, EAX
    DROP
    ret

at: mov  word ptr xy, AX
    DROP
    mov  word ptr xy+2, AX
    DROP
    ret

pAT: add  word ptr xy, AX
    DROP
    add  word ptr xy+2, AX
    DROP
    ret

;cl1: xor  EAX, EAX
;    mov  [ESI], EAX
;    ret
;clip: movsx EDX, word ptr xy
;    cmp  EDX, vp
;    jns  cl1
;    add  EAX, EDX
;    js   cl1
;    test EDX, EDX
;    jns  @f
;        xor  EDX, EDX
;@@: cmp  EAX, vp
;    js   @f
;        mov  EAX, vp
;@@: sub  EAX, EDX
;    mov  word ptr xy, DX
;    movsx EDX, word ptr xy+2
;    cmp  EDX, hp
;    jns  cl1
;    add  [ESI], EDX
;    js   cl1
;    test EDX, EDX
;    jns  @f
;        xor  EDX, EDX
;@@: cmp  dword ptr [ESI], hp
;    js   @f
;        mov  dword ptr [ESI], hp
;@@: sub  [ESI], EDX
;    mov  word ptr xy+2, DX
;    ret

octant: DUP_
    mov  EAX, 43h ; poly -last y+ x+ ;23h ; last y+ x+
    mov  EDX, [4+ESI]
    test EDX, EDX
    jns  @f
        neg  EDX
        mov  [4+ESI], EDX
        xor  AL, 1
@@: cmp  EDX, [ESI]
    jns  @f
        xor  AL, 4
@@: ret

; Keyboard
EIGHT: add  EDI, 12
    call FOUR
    call SPACE
    sub  EDI, 16
FOUR: mov  ECX, 4
FOUR1:  push ECX
        DUP_
        xor  EAX, EAX
        mov  AL, [4+EDI]
        inc  EDI
        call EMIT
        pop  ECX
        next FOUR1
    ret

stack: mov  EDI, Godd-4
@@: mov  EDX, God
    cmp  [EDX], EDI
    jnc  @f
        DUP_
        mov  EAX, [EDI]
        sub  EDI, 4
        call qDOT
        jmp  @b
@@: ret

KEYBOARD: call text1
    mov  EDI, board
    DUP_
    mov  EAX, keyc
    call color
    mov  rm, hc*iw
    mov  lm, hp-9*iw+3
    mov  xy, (hp-9*iw+3)*10000h+vp-4*ih+3
    call EIGHT
    call EIGHT
    call EIGHT
    call CR
    add  xy, 4*iw*10000h
    mov  EDI, shift
    add  EDI, 4*4-4
    mov  ECX, 3
    call FOUR1
    mov  lm, 3
    mov  word ptr xy+2, 3
    call stack
    mov  word ptr xy+2, hp-(11+9)*iw+3
    lea  EDI, history-4
    mov  ECX, 11
    jmp  FOUR1

alpha db 15o, 12o,  1 , 14o
      db 24o,  2 ,  6 , 10o
      db 23o, 11o, 17o, 21o
      db 22o, 13o, 16o,  7
      db  5 ,  3 ,  4 , 26o
      db 27o, 44o, 25o, 20o
graphics db 31o, 32o, 33o,  0 
         db 34o, 35o, 36o, 30o
         db 37o, 40o, 41o, 57o
         db 51o, 50o, 52o, 54o ; : ; ! @
         db 46o, 42o, 45o, 56o ; Z J . ,
         db 55o, 47o, 53o, 43o ; * / + -
numbers db 31o, 32o, 33o,  0
        db 34o, 35o, 36o, 30o
        db 37o, 40o, 41o,  0
        db  0,   0 ,  0 ,  0
        db  0,   0 ,  0 ,  0
        db  0,   0 ,  0 ,  0
octals db 31o, 32o, 33o,  0
       db 34o, 35o, 36o, 30o
       db 37o, 40o, 41o,  0
       db  0 ,  5 , 23o, 12o
       db  0 , 20o,  4 , 16o
       db  0 ,  0 ,  0 ,  0
LETTER: cmp  AL, 4
    js   @f
        mov  EDX, board
        mov  AL, [EDX][EAX]
@@: ret

keys db 16, 17, 18, 19,  0,  0,  4,  5 ; 20
     db  6,  7,  0,  0,  0,  0, 20, 21
     db 22, 23,  0,  0,  8,  9, 10, 11 ; 40
     db  0,  0,  0,  0, 24, 25, 26, 27
     db  0,  1, 12, 13, 14, 15,  0,  0 ; 60 N
     db  3,  2 ; alt space
KEY: DUP_
    xor  EAX, EAX
@@:     call PAUSE
        in   AL, 144o
        test AL, 1
        jz   @b
    in   AL, 140o
    test AL, 360o
    jz   @b
    cmp  AL, 72o
    jnc  @b
    mov  AL, [keys-20o+EAX]
    ret

ALIGN 4
graph0 dd offset nul0, offset nul0, offset nul0, offset ALPH0
       db  0 ,  0 ,  5 , 0 ;     a
graph1 dd offset WORD0, offset X, offset LJ, offset ALPH
       db 25o, 45o,  5 , 0 ; x . a
alpha0 dd offset nul0, offset nul0, offset NUMBER, offset STAR0
       db  0 , 41o, 55o, 0 ;   9 *
alpha1 dd offset WORD0, offset X, offset LJ, offset GRAPH
       db 25o, 45o, 55o, 0 ; x . *
 numb0 dd offset nul0, offset MINUS, offset ALPHn, offset OCTAL
       db 43o,  5 , 16o, 0 ; - a f
 numb1 dd offset NUMBER0, offset Xn, offset ENDN, offset NUMBER0
       db 25o, 45o,  0 , 0 ; x .

board   dd offset alpha-4
shift   dd offset alpha0
base    dd 10
current dd offset decimal
keyc    dd yellow
chars   dd 1
aword   dd offset EX1
anumber dd offset nul
words   dd 1

nul0: DROP
    jmp  @f
ACCEPT:
acceptn: mov  shift, offset alpha0
    lea  EDI, alpha-4
ACCEPT1: mov  board, EDI
@@: call KEY
    cmp  AL, 4
    jns  first
    mov  EDX, shift
    jmp  dword ptr [EDX+EAX*4]

bits db 28
@@: add  EAX, 120o
    mov  CL, 7
    jmp  @f
PACK: cmp  AL, 20o
    jnc  @b
        mov  CL, 4
        test AL, 10o
        jz   @f
        inc  ECX
        xor  AL, 30o
@@: mov  EDX, EAX
    mov  CH, CL
@@: cmp  bits, CL
    jnc  @f
        shr  AL, 1
        jc   FULL
        dec  CL
        jmp  @b
@@: shl  dword ptr [ESI], CL
    xor  [ESI], EAX
    sub  bits, CL
    ret

LJ0: mov  CL, bits
    add  CL, 4
    shl  dword ptr [ESI], CL
    ret

LJ: call LJ0
    DROP
    ret

FULL: call LJ0
    inc  words
    mov  bits, 28
    sub  bits, CH
    mov  EAX, EDX
    DUP_
    ret

X:  call RIGHT
    mov  EAX, words
    lea  ESI, [EAX*4+ESI]
    DROP
    jmp  ACCEPT

WORD_: call RIGHT
    mov  words, 1
    mov  chars, 1
    DUP_
    mov  dword ptr [ESI], 0
    mov  bits, 28
WORD1:  call LETTER
        jns  @f
            mov  EDX, shift
            jmp  dword ptr [EDX+EAX*4]
@@:     test AL, AL
        jz   WORD0
        DUP_
        call ECHO_
        call PACK
        inc  chars
WORD0:  DROP
        call KEY
        jmp  WORD1

decimal: mov  base, 10
    mov  shift, offset numb0
    mov  board, offset numbers-4
    ret

hex: mov  base, 16
    mov  shift, offset numb0 ; oct0
    mov  board, offset octals-4
    ret

octal: xor current, (offset decimal-offset start) xor (offset hex-offset start)
    xor  byte ptr numb0+18, 41o xor 16o ; f vs 9
    call current
    jmp  NUMBER0

Xn: DROP
    DROP
    jmp  ACCEPTn

;      db  0,  0,  0,  0
digit db 14, 10,  0,  0
      db  0,  0, 12,  0,  0,  0, 15,  0
      db 13,  0,  0, 11,  0,  0,  0,  0
      db  0,  1,  2,  3,  4,  5,  6,  7
      db  8,  9
sign db 0
MINUS: ; mov  AL, 43o ; -
    mov  sign, AL
    jmp  NUMBER2

NUMBER0: DROP
    jmp  NUMBER3
NUMBER: call current
    mov  sign, 0
    xor  EAX, EAX
NUMBER3: call KEY
    call LETTER
    jns  @f
        mov  EDX, shift
        jmp  dword ptr [EDX+EAX*4]
@@: test AL, AL
    jz   NUMBER0
    mov  AL, [digit-4+EAX]
    test sign, 37o
    jz   @f
        neg  EAX
@@: mov  EDX, [ESI]
    imul EDX, base
    add  EDX, EAX
@@: mov  [ESI], EDX
NUMBER2: DROP
    mov  shift, offset numb1
    jmp  NUMBER3

ENDN: DROP
    call [anumber]
    jmp  ACCEPTn

ALPHn: DROP
ALPH0: mov  shift, offset alpha0
    lea  EDI, alpha-4
    jmp  @f
STAR0: mov  shift, offset graph0
    lea  EDI, graphics-4
@@: DROP
    jmp  ACCEPT1

ALPH: mov  shift, offset alpha1
    lea  EDI, alpha-4
    jmp  @f
GRAPH: mov  shift, offset graph1
    lea  EDI, graphics-4
@@: mov  board, EDI
    jmp  WORD0

first: add  shift, 4*4+4
    call WORD_
    call [aword]
    jmp  ACCEPT

hicon db 30o, 31o, 32o, 33o, 34o, 35o, 36o, 37o
      db 40o, 41o,  5 , 23o, 12o, 20o,  4 , 16o 
eDIG1: DUP_
eDIG: push ECX
     mov  AL, hicon[EAX]
     call EMIT
    pop  ECX
    ret

oDIG: rol  EAX, 4
    DUP_
    and  EAX, 0Fh
    ret

hdotn: mov  EDX, EAX
    neg  EAX
    lea  ECX, [32+EAX*4]
    DROP
    rol  EAX, CL
    mov  ECX, EDX
    jmp  @f
hdot: mov  ECX, 8
@@:     call oDIG
        call eDIG
        next @b
    DROP
    ret

dot: mov  ECX, 7
@@:     call oDIG
        jnz  @h
        DROP
        next @b
    inc  ECX
@@:     call oDIG
@h1:    call eDIG
        next @b
    call space
    DROP
    ret
@h: inc  ECX
    jmp  @h1

qdot: cmp  base, 10
    jnz  dot
dot10: mov  EDX, EAX
    test EDX, EDX
    jns  @f
        neg  EDX
        DUP_
        mov  EAX, 43o
        call EMIT
@@: mov  ECX, 8
@@:     mov  EAX, EDX
        xor  EDX, EDX
        div  tens[ECX*4]
        test EAX, EAX
        jnz  D_1
        dec  ECX
        jns  @b
    jmp  D_2
@@:     mov  EAX, EDX
        xor  EDX, EDX
        div  tens[ECX*4]
D_1:    call eDIG1
        dec  ECX
        jns  @b
D_2: mov  EAX, EDX
    call eDIG1
    call space ; spcr
    DROP
    ret

unPACK: DUP_
    test EAX, EAX
    js   @f
        shl  dword ptr [ESI], 4
        rol  EAX, 4
        and  EAX, 7
        ret
@@: shl  EAX, 1
    js   @f
        shl  dword ptr [ESI], 5
        rol  EAX, 4
        and  EAX, 7
        xor  AL, 10o
        ret
@@: shl  dword ptr [ESI], 7
    rol  EAX, 6
    and  EAX, 77o
    sub  AL, 20o
    ret

qRING: DUP_
    inc  dword ptr [ESI]
    cmp  curs, EDI ; from abort, insert
    jnz  @f
        mov  curs, EAX        
@@: cmp  EAX, curs
    jz   RING
    jns  @f
        mov  pcad, EDI
@@: DROP
    ret

RING: mov  cad, EDI
    sub  xy, iw*10000h ; bksp
    DUP_
    mov  EAX, 0e04000h
    call color
    mov  EAX, 60o
    mov  CX, word ptr xy+2
    cmp  CX, word ptr rm
    js   @f
        call EMIT
        sub  xy, iw*10000h ; bksp
        ret
@@: jmp  EMIT

rW: mov  CX, word ptr xy+2
    cmp  CX, word ptr lm
    jz   @f
        call cr
@@: call RED
    jmp  TYPE_

gW: call GREEN
    jmp  TYPE_
mW: call CYAN
    jmp  TYPE_
wW: DUP_
    mov  EAX, yellow
    call color
    jmp  TYPE_

type0: sub  xy, iw*10000h ; call bspcr
    test dword ptr [-4+EDI*4], -20o
    jnz  type1
        dec  EDI
        mov  lcad, EDI
        call space
        call qRING
        pop  EDX ; End of block
        DROP
        jmp  KEYBOARD

Cap: call white
    DUP_
    mov  EAX, [-4+EDI*4]
    and  EAX, -20o
    call unPACK
    add  AL, 48
    call EMIT
    jmp  type2

CAPS: call white
    DUP_
    mov  EAX, [-4+EDI*4]
    and  EAX, -20o
@@:     call unPACK
        jz   @f
        add  AL, 48
        call EMIT
        jmp  @b

text: call WHITE
TYPE_:
type1: DUP_
    mov  EAX, [-4+EDI*4]
    and  EAX, -20o
type2:  call unPACK
        jz   @f
        call EMIT
        jmp  type2
@@: call space
    DROP
    DROP
    ret

gsW: mov  EDX, [-4+EDI*4]
    sar  EDX, 5
    jmp  gnW1

var: call MAGENTA
    call TYPE_
gnW: mov  EDX, [EDI*4]
    inc  EDI
gnW1: DUP_
    mov  EAX, 0f800h ; Green
    cmp  bas, offset dot10
    jz   @f        
        mov  EAX, 0c000h ; dark green
    jmp  @f

sW: mov  EDX, [-4+EDI*4]
    sar  EDX, 5
    jmp  nW1

nW: mov  EDX, [EDI*4]
    inc  EDI
nW1: DUP_
    mov  EAX, yellow
    cmp  bas, offset dot10
    jz   @f        
        mov  EAX, 0c0c000h ; dark yellow
@@: call color
    DUP_
    mov  EAX, EDX
    jmp  [bas]

REFRESH: call SHOW
    call blank
    call text1
    DUP_            ; Counter
    mov  EAX, lcad
    mov  cad, EAX ; for curs beyond end
    xor  EAX, EAX
    mov  EDI, blk
    shl  EDI, 10-2
    mov  pcad, EDI ; for curs=0
ref1:   test dword ptr [EDI*4], 0fh
        jz   @f
            call qRING
@@:     mov  EDX, [EDI*4]
        inc  EDI
        mov  bas, offset dot10
        test DL, 20o
        jz   @f
            mov  bas, offset dot
@@:     and  EDX, 17o
        call display[EDX*4]
        jmp  ref1

ALIGN 4
display dd offset TYPE0, offset wW, offset nW, offset rW
        dd offset gW, offset gnW, offset gsW, offset mW
        dd offset sW, offset text, offset Cap, offset CAPS
        dd offset var, offset nul, offset nul, offset nul
tens dd 10, 100, 1000, 10000, 100000, 1000000
     dd 10000000, 100000000, 1000000000
bas dd offset dot10
blk    dd 18
curs   dd 0
cad    dd 0
pcad   dd 0
lcad   dd 0
trash  dd buffer*4
ekeys dd offset nul, offset DEL, offset Eout, offset destack
      dd offset act1, offset act3, offset act4, offset shadow
      dd offset mcur, offset mmcur, offset ppcur, offset pcur
      dd offset mblk, offset actv, offset act7, offset pblk
      dd offset nul, offset act11, offset act10, offset act9
      dd offset nul, offset nul, offset nul, offset nul
ekbd0 dd offset nul, offset nul, offset nul, offset nul
     db 25o, 45o,  7 ,  0  ; x  .  i
ekbd db 17o,  1 , 15o, 55o ; w  r  g  *
     db 14o, 26o, 20o,  1  ; l  u  d  r
     db 43o, 11o, 12o, 53o ; -  m  c  +
     db  0 , 70o, 72o,  2  ;    S  C  t
     db  0 ,  0 ,  0 ,  0
     db  0 ,  0 ,  0 ,  0
actc dd yellow, 0, 0ff0000h, 0c000h, 0, 0, 0ffffh
     dd 0, 0ffffffh, 0ffffffh, 0ffffffh, 8080ffh
vector dd 0
action db 1

act1: mov  AL, 1
    jmp  @f
act3: mov  AL, 3
    jmp  @f
act4: mov  AL, 4
    jmp  @f
act9: mov  AL, 9
    jmp  @f
act10: mov  AL, 10
    jmp  @f
act11: mov  AL, 11
    jmp  @f
act7: mov  AL, 7
@@: mov  action, AL
    mov  EAX, [actc-4+EAX*4]
    mov  aword, offset insert
actn: mov  keyc, EAX
    pop  EAX
    DROP
    jmp  ACCEPT

actv: mov  action, 12
    mov  EAX, 0ff00ffh ; Magenta
    mov  aword, offset @f
    jmp  actn

@@: DUP_
    xor  EAX, EAX
    inc  words
    jmp  insert    

mcur: dec  curs
    jns  @f
pcur: inc  curs
@@: ret

mmcur: sub  curs, 8
    jns  @f
        mov  curs, 0
@@: ret
ppcur: add  curs, 8
    ret

pblk: add  blk, 2
    add  dword ptr [ESI], 2
    ret
mblk: cmp  blk, 20
    js   @f
        sub  blk, 2 
        sub  dword ptr [ESI], 2
@@: ret

shadow: xor  blk, 1
    xor  dword ptr [ESI], 1
    ret

E0: DROP
    jmp  @f

edit: mov  blk, EAX
    DROP
E:  DUP_
    mov  EAX, blk
    mov  anumber, offset FORMAT
    mov  byte ptr alpha0+4*4, 45o ; .
    mov  alpha0+4, offset E0
    call REFRESH
@@: mov  shift, offset ekbd0
    mov  board, offset ekbd-4
    mov  keyc, yellow
@@:     call KEY
        call ekeys[EAX*4]
        DROP
        jmp  @b

eout: pop  EAX
    DROP
    DROP
    mov  aword, offset EX1
    mov  anumber, offset nul
    mov  byte ptr alpha0+4*4, 0
    mov  alpha0+4, offset nul0
    mov  keyc, yellow
    jmp  ACCEPT

destack: mov  EDX, trash
    cmp  EDX, buffer*4
    jnz  @f
        ret
@@: sub  EDX, 2*4
    mov  ECX, [EDX+1*4]
    mov  words, ECX
@@:     DUP_
        mov  EAX, [EDX]
        sub  EDX, 1*4
        next @b
    add  EDX, 1*4
    mov  trash, EDX

insert0: mov  ECX, lcad  ; room available?
     add  ECX, words
     xor  ECX, lcad
     and  ECX, -100h
     jz   insert1
         mov  ECX, words ; no
@@:          DROP
             next @b
         ret
insert1: push ESI
     mov  ESI, lcad
     mov  ECX, ESI
     dec  ESI
     mov  EDI, ESI
     add  EDI, words
     shl  EDI, 2
     sub  ECX, cad
     js   @f
         shl  ESI, 2
         std
         rep movsd
         cld
@@: pop  ESI
    shr  EDI, 2
    inc  EDI
    mov  curs, EDI ; like abort
    mov  ECX, words
@@:     dec  EDI
        mov  [EDI*4], EAX
        DROP ; requires cld
        next @b
    ret

insert: call insert0
    mov  CL, action
    xor  [EDI*4], CL
    jmp  ACCEPT

FORMAT: test action, 12o ; ignore 3 and 9
    jz   @f
        DROP
        ret
@@: mov  EDX, EAX
    and  EDX, 0FC000000h
    jz   @f
        cmp  EDX, 0FC000000h
        jnz  FORMAT2
@@: shl  EAX, 5
    xor  AL, 2 ; 6
    cmp  action, 4
    jz   @f
        xor  AL, 13o ; 8
@@: cmp  base, 10
    jz   @f
        xor  AL, 20o
@@: mov  words, 1
    jmp  insert

FORMAT2: DUP_
    mov  EAX, 1 ; 5
    cmp  action, 4
    jz   @f
        mov  AL, 3 ; 2
@@: cmp  base, 10
    jz   @f
        xor  AL, 20o
@@: xchg EAX, [ESI]
    mov  words, 2
    jmp  insert

DEL: call enstack
    mov  EDI, pcad
    mov  ECX, lcad
    sub  ECX, EDI
    shl  EDI, 2
    push ESI
     mov  ESI, cad
     shl  ESI, 2
     rep movsd
    pop  ESI
    jmp  mcur

enstack: DUP_
    mov  EAX, cad
    sub  EAX, pcad
    jz   ens
      mov  ECX, EAX
      xchg EAX, EDX
      push ESI
       mov  ESI, cad
       lea  ESI, [ESI*4-4]
       mov  EDI, trash
@@:      std
         lodsd
         cld
         stosd
         next @b
       xchg EAX, EDX
       stosd
       mov  trash, EDI
    pop  ESI
ens: DROP
    ret

pad: pop  EDX
    mov  vector, EDX
    add  EDX, 28*5
    mov  board, EDX
    sub  EDX, 4*4
    mov  shift, EDX
@@:     call KEY
        mov  EDX, vector
        add  EDX, EAX
        lea  EDX, [5+EAX*4+EDX]
        add  EDX, [-4+EDX]
        DROP
        call EDX
        jmp  @b

org (1200h-1)*4
    dd 0
end start
