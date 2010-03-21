.intel_syntax ;# floppy boot segment
.ifdef DMA
;# Floppy boot segment. Modified 7/17/01 for Asus P2B-D Floppy I/O Terry Loveall
;# Modified again by, or at least used by, Jeff Fox in his 2005 binaries
.endif

.org 0 ;# actually 7c00
start: jmp  start0
    nop
    .ascii "cmcf 1.0"
    .word 512     ;# bytes/sector
    .byte 1       ;# sector/cluster
    .word 1       ;# sector reserved
    .byte 2       ;# fats
    .word 16*14   ;# root directory entries
    .word 80*2*18 ;# sectors
    .byte 0x0f0    ;# media
    .word 9       ;# sectors/fat
    .word 18      ;# sectors/track
    .word 2       ;# heads
    .long 0       ;# hidden sectors
    .long 80*2*18 ;# sectors again
    .byte 0       ;# drive

command:
.ifdef CM2001
    .byte 0xc5 ;# in compiled color.com from CM website
;# probably just there from save-system or similar command? just a guess
.else
    .byte 0
.endif
    .byte 0   ;# head, drive
cylinder:
.ifdef QUESTIONABLE
    .byte 1 ;# 1 in compiled color.com from CM website
;# causes load to skip from cylinder 0 directly to cylinder 2,
;# missing bytes 0x4800 to 0x9000 in color.com image
;# same problem in Jeff Fox's 2005 code
.else
    .byte 0
.endif
    .byte 0   ;# head
    .byte 1   ;# sector
    .byte 2   ;# 512 bytes/sector
    .byte 18  ;# sectors/track
    .byte 0x1b ;# gap
    .byte 0x0ff
;# if you see any crud in disassembly here it shouldn't matter
;# different assemblers use different NOP instructions to pad out alignment
.ifdef CM2001
     mov eax, eax
.endif
.align 4
nc: .long 9 ;# forth+icons+blocks 24-161 ;# number of cylinders, 9 (out of 80)
gdt: .word start0 - gdt0 - 1
    .long gdt0
;# different assemblers use different NOP instructions to pad out alignment
.ifdef CM2001
     cs mov eax, eax
     cs mov eax, eax
.endif
.align 8 ;# more garbage possibly in disassembly here, ignore it
gdt0: .word 0, 0, 0, 0 ;# null descriptor, not used
    .word 0x0ffff, 0, 0x9a00, 0x0cf ;# code, linear addressing from 0
    .word 0x0ffff, 0, 0x9200, 0x0cf ;# data, linear addressing from 0

;# code is compiled in protected 32-bit mode.
;# hence (original code uses) .org .-2  to fix 16-bit words
;# and 4 hand-assembled instructions.
;# and eax and ax exchanged
;# this code is in real 16-bit mode

.code16
start0:
.ifdef AGP
    mov  ax, 0x4f02 ;# video mode
    mov  bx, vesa ;# hp*vp rgb: 565
.else  ;# use VBE call to determine RAM address of desired video mode
    mov  ax, 0x4f01 ;# get video mode info
    mov  cx, vesa ;# a 16-bit color linear video mode (5:6:5 rgb)
    mov  di, 0x7e00 ;# use buffer space just past loaded bootsector
    int  0x10
    mov  ax, 0x4f02 ;# set video mode
    mov  bx, cx ;# vesa mode
.endif
    int  0x10
    cli  ;# disable interrupts until we are set up to handle them (if ever)
.code32
    xor  ax, ax  ;# move code to 0
.ifdef QUESTIONABLE  ;# some things in CM's source which can be left out?
    mov  bx, ax
    mov  ebx, cs
    mov  ds, ebx
    mov  es, eax
    mov  di, ax
    mov  si, ax
.else
    mov  di, ax
    mov  bx, cs
    mov  ds, bx
    mov  es, ax ;# not necessary at boot but perhaps from comfile
.endif
.code16
    call loc ;# where are we? ip+4*cs
loc: pop  si
    sub  si, offset loc-offset start
    mov  cx, 512/4 ;# only 256 bytes unless...
;# compile as 32-bit code here so it moves longwords and not words
    data32 rep movsw
    jmp 0:offset relocate

relocate: ;# this code is executed from an offset of 0, not 0x7c00
    mov  ds, ax
    lgdt [gdt]
    mov  al, 1
    mov  cr0, eax
    jmp  8:offset protected ;# code selector is offset 8
.code32
protected: ;# now in protected 32-bit mode
    mov  al, 0x10 ;# linear data selector (offset into GDT)
    mov  ds, eax
    mov  es, eax
    mov  ss, eax
    mov  esp, offset gods ;# assembles as a dword ptr without 'offset'
.ifndef AGP
    push [ds:0x7e28] ;# physical memory pointer returned by VESA call
.endif
    xor  ecx, ecx

a20:
    mov  al, 0x0d1
    out  0x64, al ;# to keyboard
0:  in   al, 0x64
    and  al, 2
    jnz  0b
    mov  al, 0x4b
    out  0x60, al ;# to keyboard, enable A20
.ifdef DMA
    mov  eax, 512*18*2-1 ;# DMA channel 2 (0x47ff)
.endif
    call dma
    shl  ebx, 4
    add  esi, ebx
.ifdef QUESTIONABLE
    cmp  dword ptr [esi], 0x44444444 ;# boot?
    jnz  cold
.else
;# if we just copied the bootsector to 0, ESI will point to 0x7c00+200
    cmp  si, 0x7e00 ;# boot?
    jz   cold
.endif
    mov  cx, 63*0x100-0x80 ;# nope
    rep  movsd
    mov  esi, offset godd ;# 0x9f448, 3000 bytes below 0xa0000 (gods)
    jmp  start2

cold:
.ifndef DMA
    call sense_
    jns  cold
.endif
    mov  esi, offset godd ;# 0x9f448, 3000 bytes below 0xa0000 (gods)
    xor  edi, edi ;# cylinder 0 on top of address 0
.ifdef DMA
    call read
    inc byte ptr cylinder
.endif
    mov  cl, byte ptr nc ;# number of cylinders used
.ifdef DMA
    dec  cl
.endif
0:  push ecx
    call read
.ifndef DMA
    inc byte ptr cylinder
.endif
    pop  ecx
    loop 0b
start2:
    call stop
    jmp  start1 ;# start1 is outside of bootsector
.equ us, 1000/6
.equ ms, 1000*us
spin:
.ifdef DMA
    mov  al, 0x1c
    call onoff
    mov  ecx, 400*ms ;# what processor speed was this set for?
0:  loop 0b  ;# damn but I hate busy-waits (jc)
;#    mov  cylinder, cl ;# calibrate
    mov  al, 7 ;# recalibrate command
    mov  cl, 2
    jmp  cmdi
.else
    mov  cl, 0x1c
    call onoff
0:  call sense_
    jns  0b
    mov  byte ptr cylinder, 0 ;# calibrate
    mov  al, 7
    mov  cl, 2
    call cmd
    mov  ecx, 500*ms
0:  loop 0b
cmdi: call sense_
    js   cmdi
    ret
.endif

ready: ;#call delay
    mov  dx, 0x3f4
0:  in   al, dx
    debugout
    shl  al, 1
    jnc  0b
    lea  edx, [edx+1] ;# doesn't affect flags as INC would
    ret

.ifndef DMA
transfer: mov  cl, 9
cmd:
    lea  edx, command
    mov  [edx], al
cmd0:
    push esi
    mov  esi, edx
cmd1:
    call ready
    jns  0f
    in   al, dx
    jmp  cmd1
0:  lodsb
    out  dx, al
    debugout
    loop cmd1
    pop  esi
    ret
.else
cmd:
    lea  edx, command
    mov  [edx], al
    push esi
    mov  esi, edx
0:  call ready
    jns  cmd0
    in   al, dx
    debugout
    jmp  0b
cmd0:
    lodsb
    mov  ah, 0x1e  ;# delay loop in JF2005 code
    out  dx, al
1:
    debugout
    dec  ah
    jne  1b
    loop 0b
    pop  esi
    ret
.endif
sense_: mov  al, 8
.ifdef CM2001
    mov  ecx, 1
.else
    mov  cl, 1
.endif
    call cmd
.ifndef DMA
0:  call ready
    jns  0b
.else
    call ready
.endif
    in   al, dx
    debugout
    and  al, al
;#  cmp  al, 0x80
    ret

seek:
.ifdef DMA
    out 0xb, al
.endif
0:  call sense_
    jns  0b
.ifdef DMA
    mov  al, 0xf
    mov  cl, 3
cmdi:
    call cmd
0:  call sense_
    jz   0b
.endif
    ret

stop:
.ifndef DMA
    mov  cl, 0x0c ;# motor off
.else
    mov  dword ptr trash, buffer*4 ;# 0x97000 in CM2001, used for DMA?
    mov  al, 0x0c
.endif
onoff:
.ifndef DMA
    dup_
    mov  al, cl
.endif
    mov  dx, 0x3f2
.ifdef DMA
    mov  ah, 0xf  ;# for timing in JF2005 DMA version
.endif
    out  dx, al
0:  debugout
.ifndef DMA
    drop
.else
    dec  ah
    jnz  0b
.endif
    ret

dma:
.ifndef DMA
    mov  word ptr command+1, 0x3a2 ;# l2 s6 u32 ms (e 2)
    mov  al, 3 ;# timing
    mov  cl, 3
    call cmd
    mov  word ptr command+1, 0x7000 ;# +seek -fifo -poll
    mov  al, 0x13 ;# configure
    mov  cl, 4
    call cmd
;# the following instruction clears the cylinder number among other things
    mov  dword ptr command, ecx ;# 0
    ret
.else
    out  5, al ;# set DMA-1 ch.2 base and current count to 0x47ff
    mov  al, ah
    out  5, al
    mov  eax, buffer*4 ;# 0x97000 in CM2001
    out  4, al ;# set DMA-1 ch.2 base and current address to "trash" buffer
    mov  al, ah
    out  4, al
    shr  eax, 16 ;# load page register value into al (9)
    out  0x81, al ;# set DMA-1 page register 2 = 09
    mov  al, 0xb
    out  0xf, al ;# write all mask bits, address = 0xf, value = 0xb
    mov  word ptr command+1, 0x2a1 ;# 2 6 16 ms (e 2)
    mov  al, 3 ;# timing
    mov  cl, 3
    call cmd
    mov  word ptr command+1, 0
    ret

transfer:
    mov  cl, 9
    call cmd
    inc  byte ptr cylinder
0:  call ready
    jns  0b
    ret
.endif
read:
.ifdef DMA
    mov  al, 0x16 ;# read DMA 2
.endif
    call seek
    mov  al, 0x0e6 ;# read normal data
    call transfer
;# about to read 0x4800, or 18432 bytes
;# total of 165888 (0x28800) bytes in 9 cylinders, 1.44 MB in 80 cylinders
;# note that the first call overwrites the cylinder number with 1 from
;# CM's color.com image; that's why it skips from cylinder 0 to 2
.ifdef DMA
    push esi
    mov  esi, buffer*4
    mov  ecx, 512*18*2/4
    rep  movsd
    pop  esi  
.else
    mov  cx, 18*2*512 ;# two heads, 18 sectors/track, 512 bytes/sector
0:  call ready
    in   al, dx
    debugout
    stosb
    next 0b
.endif
    ret

;# don't need 'write' till after bootup
.ifndef CM2001
    .org 0x1fe
    .word 0x0aa55 ;# mark boot sector
    ;# end of boot sector
    .long 0x44444444 ;# mark color.com
.endif

write:
.ifdef DMA
    mov  edi, buffer*4
    mov  ecx, 512*18*2/4
    rep  movsd
    mov  al, 0x1a ;# write DMA 2
    call seek
    mov  al, 0xc5
    jmp  transfer
.else
    call seek
    mov  al, 0x0c5 ;# write data
    call transfer
    mov  cx, 18*2*512 ;#two heads, 18 sectors/track, 512 bytes/sector
0:  call ready
    lodsb
    out  dx, al
    debugout
    next 0b
    ret
.endif

.ifdef CM2001
    .org 0x1fe
    .word 0x0aa55 ;# mark boot sector
    ;# end of boot sector
    .long 0x44444444 ;# mark color.com
.endif

flop:
.ifdef DMA
    dup_
    call spin
    drop
    mov  ecx, eax
    drop
    mov  cylinder, al
    drop
    shl  eax, 2
    ret
.else
    mov  cylinder, al ;# c-cx
    dup_
    mov  dx, 0x3f2
    in   al, dx
    debugout
    test al, 0x10
    jnz  0f
    jmp  spin
0:  ret
.endif

.ifdef DMA
readf:
    call flop
    push edi
    mov  edi, eax
0:  push ecx
    call read
    pop  ecx
    next 0b
    pop  edi
readf1:
    call stop
    drop
    ret

save_:
    dup_
    xor  eax, eax 
    dup_
    dup_
    mov  eax, nc

writef:
    call flop
    push esi
    mov  esi, eax
0:  push ecx
    call write
    pop  ecx
    next 0b
    pop  ESI
    jmp  readf1
.else
readf:
    call flop ;# ac-ac
    push edi
    mov  edi, [esi+4]
    shl  edi, 2
    call read
    pop  edi
readf1:
    drop
    inc  eax
    add  dword ptr [esi], 0x1200
    ret

writef:
    call flop ;# ac-ac
    push esi
    mov  esi, [esi+4]
    shl  esi, 2
    call write
    pop  esi
    jmp  readf1

seekf:
    call flop ;# c-c
    call seek
    mov  al, 0x0f
    mov  cl, 3
    call cmd
    call cmdi
    drop
    ret

cmdf: mov  ecx, eax ;# an
    drop
    lea  edx, [eax*4]
    call cmd0
    drop
    ret

readyf: dup_
    call ready
    drop
    ret
.endif
