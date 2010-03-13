.intel_syntax ;#generic graphics

;# VESA mode numbers and screen sizes,
;# from http://www.mat.univie.ac.at/~gerald/laptop/vesafb.txt
;#    | 640x480  800x600  1024x768 1280x1024
;#----+-------------------------------------
;#256 |  0x101    0x103    0x105    0x107   
;#32k |  0x110    0x113    0x116    0x119   
;#64k |  0x111    0x114    0x117    0x11A   
;#16M |  0x112    0x115    0x118    0x11B   
.align 4
;# top of RAM is 0x2000000 with 32 megs; framebuffer is just below this
;# multiply vp (vertical pixels) by hp (horizontal pixels) by 2 bytes (16 bits)
;# to determine the location of the framebuffer
frame: .long 0x2000000-hp*vp*2 ;# 32 m
displ: .long 0x0f0000000 ;# fujitsu (physical address of video memory)
fore:  .long 0x0f7de ;# less-brightness white (silver) in 565 color mode
xc:    .long 0
yc:    .long 0

rgb: ;# change 8:8:8 bit format to 5:6:5
    ror  eax, 8 ;# rotate blue bits into upper word
    shr  ax, 2 ;# drop two low bits of green
    ror  eax, 6 ;# rotate green into upper word, leaving only red in AX
    shr  al, 3 ;# drop low 3 bits of red
    rol  eax, 6+5 ;# now rrrrrggggggbbbbb
    and  eax, 0b1111011111011110 ;# remove low bit of each color
    ret

white: dup_
    mov  eax, 0x0ffffff ;# 8:8:8 rgb, full brightness
color: call rgb ;# change to 5:6:5 bit format
    mov  fore + loadaddr, eax
    drop
    ret

fifof: drop
graphic: ret

switch: ;# refresh screen, then switch tasks
    push esi
    mov  esi, frame + loadaddr ;# source address, framebuffer
    push edi
    mov  edi, displ + loadaddr ;# linear memory of video controller
    mov  ecx, hp*vp/2 ;# number of 16-bits pixels, divided by 2 for dword movs
    rep  movsd ;# from buffer into video RAM
    pop  edi
    pop  esi
0:  jmp  pause ;# switch task

clip:
    mov edi, xy + loadaddr
    mov  ecx, edi
    test cx, cx
    jns  0f
    xor  ecx, ecx
0:  and  ecx, 0x0ffff
    mov  yc + loadaddr, ecx
    imul ecx, hp*2
    sar  edi, 16
    jns  0f
    xor  edi, edi
0:  mov  xc + loadaddr, edi
    lea  edi, [edi*2+ecx]
    add  edi, frame + loadaddr
    ret

bit16: lodsw
    xchg al, ah
    mov  ecx, 16
b16: shl  ax, 1
    jnc  0f
    mov  [edi], dx
0:  add  edi, 2
    next b16
    ret

bit32: lodsw
    xchg al, ah
    mov  ecx, 16
b32: shl  eax, 1
    jnc  0f
    mov  [edi], dx
    mov  [edi+2], dx
    mov  [edi+hp*2], dx
    mov  [edi+hp*2+2], dx
0:  add  edi, 4
    next b32
    ret

emit: ;# paint a character on the screen
    call qcr ;# issue CRLF if at end of line
    push esi ;# save registers we need...
    push edi
    push edx
    imul eax, 16*24/8 ;# index into icon table...
;# point to the bit-representation of this character
    lea  esi, [icons + eax] 
    call clip
    mov  edx, fore + loadaddr ;# get foreground color into EDX
    mov  ecx, 24
0:  push ecx
    call bit16
    add  edi, (hp-16)*2
    pop  ecx
    next 0b
    pop  edx ;# restore registers...
    pop  edi
    pop  esi
bl_: drop
space:
    add dword ptr xy + loadaddr, iw*0x10000
    ret

emit2: push esi
    push edi
    push edx
    imul eax, 16*24/8
    lea  esi, [icons + eax]
    call clip
    mov  edx, fore + loadaddr
    mov  ecx, 24
0:  push ecx
    call bit32
    add  edi, (2*hp-16*2)*2
    pop  ecx
    next 0b
    pop  edx
    pop  edi
    pop  esi
    add  dword ptr xy + loadaddr, iw*0x10000*2
    drop
    ret

text1: call white
    mov  dword ptr lm + loadaddr, 3
    mov  dword ptr rm + loadaddr, hc*iw
    jmp  top

line: call clip
    mov  ecx, [esi]
    shl  ecx, 1
    sub  edi, ecx
    mov  ecx, eax
    mov  eax, fore + loadaddr
    rep stosw
    inc dword ptr xy + loadaddr
    drop
    drop
    ret

box: ;# draw a box and fill with foreground color
    call clip
    cmp  eax, vp+1 ;# past vertical end of screen?
    js   0f ;# continue if not
    mov  eax, vp ;# else set vertical parameter to end of screen
0:  mov  ecx, eax
    sub  ecx, yc + loadaddr
    jng  no
    cmp  dword ptr [esi], hp+1
    js   0f
    mov  dword ptr [esi], hp
0:  mov  eax, xc + loadaddr
    sub  [esi], eax
    jng  no
    mov  edx, hp
    sub  edx, [esi]
    shl  edx, 1
    mov  eax, fore + loadaddr
0:  push ecx
    mov  ecx, [esi]
    rep stosw
    add  edi, edx
    pop  ecx
    next 0b
no: drop
    drop
    ret
