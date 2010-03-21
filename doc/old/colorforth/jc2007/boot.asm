.intel_syntax ;# floppy boot segment
;# 0x000-0x400 is BIOS interrupt table
;# 0x400-0x500 is BIOS system information area
;# we can use starting at 0x500

.equ upper_right, 158 ;# address of upper-right corner of screen
.ifdef LOADBLOCKS
 ;# this 'ifdef' is only here to trigger the Makefile to include this number
 ;# but let's use it to define other constants while we're at it
 .equ HEADS, 2
 .equ SECTORS_PER_TRACK, 18
 .equ BYTES_PER_CYLINDER, 512 * HEADS * SECTORS_PER_TRACK
 .equ LOAD_CYLINDERS, (LOADBLOCKS * 1024) / BYTES_PER_CYLINDER
 .if (LOADBLOCKS * 1024) % BYTES_PER_CYLINDER > 0
  .equ LOAD_CYLINDERS, LOAD_CYLINDERS + 1
 .endif
.endif
.macro showprogress
  .ifdef DEBUGGING
  call progress
  .endif
.endm
.macro shownumber
 .ifdef DEBUGGING
 call shownumber
 .endif
.endm

.org 0 ;# actually 0x7c00, where the BIOS loads the bootsector
start:
    jmp  start0
bootmode: ;# this byte after the short jmp above is normally just padding
;# we will use it to indicate a startup from MSDOS
;# also to indicate El Torito boot
.ifdef EL_TORITO_BOOT
    .byte 0xfe ;# even number so we can use MSDOS check odd parity
.else
    .byte 0 ;# pad to offset 3
.endif
    .ascii "cmcf 1.0"
bps:
    .word 512     ;# bytes/sector
    .byte 1       ;# sector/cluster
    .word 1       ;# sector reserved
    .byte 2       ;# fats
    .word 16*14   ;# root directory entries
    .word 80 * HEADS * SECTORS_PER_TRACK ;# sectors
    .byte 0x0f0   ;# media
    .word 9       ;# sectors/fat
spt:
    .word SECTORS_PER_TRACK ;# sectors/track
heads:
    .word HEADS   ;# heads
    .long 0       ;# hidden sectors
    .long 80 * HEADS * SECTORS_PER_TRACK ;# sectors again
drive:
    .byte 0       ;# drive
cylinder: .word 0
.align 4
;# forth+icons+blocks 24-LOADBLOCKS ;# number of cylinders to load (out of 80)
;# needs to be on longword boundary, see nc_ in color.asm
nc:  .long LOAD_CYLINDERS
gdt: .word gdt_end - gdt0 - 1 ;# GDT limit
    .long gdt0 + loadaddr ;# pointer to start of table
.align 8
gdt0: .word 0, 0, 0, 0 ;# start of table must be a null entry
    .equ code32p, . - gdt0
    .word 0xffff, 0, 0x9a00, 0xcf ;# 32-bit protected-mode code
    .equ data32p, . - gdt0
    .word 0xffff, 0, 0x9200, 0xcf ;# 32-bit protected-mode data
    .equ code16r, . - gdt0
    .word 0xffff, 0, 0x9a00, 0x00 ;# 16-bit real-mode code
    .equ data16r, . - gdt0
    .word 0xffff, 0, 0x9200, 0x00 ;# 16-bit real-mode data
gdt_end:
real_mode_idt:
    .word 0xffff ;# limit
    .long 0 ;# base is that of BIOS interrupt table
.code16
start0:
    cmp  word ptr cs:0, 0x20cd ;# INT 20h at start of .com program header
    jnz  0f
    dec  byte ptr [cs: bootmode + 0x100]
0:  cli
    push cs ;# need to set DS=CS, especially on VMWare, DS was 0x40
    pop  ds
    zero ss ;# set stack to something low and out of the way
    mov  esp, iobuffer + 0x1000
    call whereami ;# set EBP to load location
    call relocate ;# move past BIOS stuff and buffer space
    mov  esp, gods ;# stack pointer now where it really belongs
    call whereami ;# set EBP to where we relocated
    showprogress
    data32 call protected_mode
.code32
    call a20 ;# set A20 gate to enable access to addresses with 1xxxxx
    mov  al, byte ptr bootmode + loadaddr
    or   al, al  ;# negative if MSDOS-loaded or El Torito non-emulation mode
    jz   0f ;# otherwise we need to load bootcode from floppy
9:  mov  esi, godd ;# set up data stack pointer for 'god' task
    jmp  start1
0:  mov  byte ptr [es: 0xb8000 | (upper_right - 8)], 'P'
    call unreal_mode
.code16
    ;# fall through to cold-start routine
cold:
    mov  edi, loadaddr  ;# start by overwriting this code
    ;# that's why it's so critical that 'cylinder' be reset to zero before
    ;# saving the image; if it's anything but that, when this block is
    ;# overwritten, 'cylinder' will be changed, and cylinders will be skipped
    ;# in loading, making the bootblock unusable
    call read
    inc  byte ptr cylinder + loadaddr
    xor  cx, cx
    mov  cl, nc + loadaddr ;# number of cylinders used
    dec  cx
0:  push cx
    call read
    inc  byte ptr cylinder + loadaddr
    mov  dx, word ptr cylinder + loadaddr
    shownumber
    pop  cx
    loop 0b
    showprogress
    data32 call protected_mode
.code32
    jmp  9b

.code16
whereami: ;# set EBP as PC-relative pointer
    xor  ebp, ebp ;# clear first or this might not work
    mov  eax, ebp
    mov  bp, cs ;# get CS segment register
    shl  ebp, 4 ;# shift segment to where it belongs in 32-bit absolute addr
    pop  ax ;# get return address
    push ax ;# we still need to return to it
    ;# note: the following only works if this called from first 256 bytes
    and  ax, 0xff00 ;# clear low 8 bits of address
    add  ebp, eax ;# EBP now contains 32-bit absolute address of 'start'
    ret

read:
;# about to read 0x4800, or 18432 bytes
;# total of 165888 (0x28800) bytes in 9 cylinders, 1.44 MB in 80 cylinders
;# (low 8 bits of) cylinder number is in CH, (6 bit) sector number in CL
    push edx  ;# used for debugging during boot
    mov  ebx, iobuffer
    push ebx
    mov  ax, (2 << 8) + 36  ;# 18 sectors per head, 2 heads
    mov  ch, [cylinder + loadaddr]
    mov  cl, 1  ;# sector number is 1-based, and we always read from first
    ;#mov  dx, cx
    ;#shownumber
    mov  dx, 0x0000 ;# head 0, drive 0
    int  0x13
    mov  edx, esi  ;# temporarily store parameter stack pointer in EDX
    pop  esi  ;# load iobuffer
    mov  ecx, 512*18*2/4
    ;#showprogress
    addr32 rep movsd ;# move to ES:EDI location preloaded by caller
    ;#showprogress
    mov  esi, edx  ;# restore parameter stack pointer
    pop  edx
    ret

.ifdef DEBUGGING
.code16
progress: ;# show progress indicator
    pop  dx  ;# grab return address
    push dx  ;# and put back on stack
shownumber: ;# alternate entry point, preload DX register
    push ax
    push bx
    push cx
    push 0xb800 ;# video display RAM
    pop  es
    mov  cx, 4  ;# four digit address to display
    mov  bx, upper_right ;# corner of screen
0:  mov  al, dl ;# get number as hex digit
    and  al, 0xf
    add  al, (0x100 - 10) ;# will force a carry if > 9
    jnc  1f
    add  al, ('a' - '9' - 1)
1:  add  al, ('9' + 1)
    mov  [es:bx], al ;# assumes ES=0xb800, text mode 3 video RAM
    shr  dx, 4 ;# next higher hex digit
    sub  bx, 2 ;# move to previous screen position
    loop 0b
    mov  ax, cs ;# restore segment registers
    mov  es, ax
    ;# restore registers
    pop  cx
    pop  bx
    pop  ax
    ret
.endif

relocate:  ;# move code from where DOS or BIOS put it, to where we want it
;# it would seem to be trivial, but in cases where the 64K span of source
;# and destination overlap, we overwrite this code and crash. it happened.
;# so now we move everything to end of 640K base RAM and then move back
;# to its final destination.
    cmp  byte ptr bootmode + loadaddr, 0xfe ;# already in correct place?
    jz   9f  ;# if so, skip the difficult stuff
    mov  ax, (0xa0000 - 0x10000) >> 4 ;# segment address of relocation
    mov  es, ax
    mov  eax, ebp ;# get source address
    shr  eax, 4
    mov  ds, eax
    mov  cx, 0x10000 >> 1 ;# moving 64K in words
    mov  si, 0xfffe
    mov  di, si
    push cx
    std  ;# move words from 0xfffe to 0
    rep  movsw
;# now we need to do a tricky jump from here to where the relocated code is,
;# otherwise we still run the risk of a code overwrite.
    mov  ax, (offset 5f - offset start)
    push es
    push ax
    lret ;# "return" to following address, in the relocated segment
5:  pop  cx
    xor  si, si ;# now move from 0 
    mov  di, si
    push es ;# destination now becomes source
    pop  ds
    mov  ax, loadaddr >> 4
    mov  es, ax
    cld
    rep  movsw
9:  xor  ax, ax ;# now all segments are 0
    mov  ds, ax
    mov  es, ax
    pop  ax ;# get return address, but we want to "ret" to its new address
    and  ax, 0xff ;# this routine must be called from first 256 bytes!
    add  ax, loadaddr
    push 0 ;# return with CS=0
    push ax
    lret

protected_mode:
    cli  ;# we're not set up to handle interrupts in protected mode
    lgdt [gdt + loadaddr]
    mov  eax, cr0
    or   al, 1
    mov  cr0, eax
    jmp  code32p: offset pm + loadaddr
.code32
pm: mov  ax, data32p ;# set all segment registers to protected-mode selector
    mov  ds, ax
    mov  es, ax
    mov  ss, ax  ;# same base as before (0), or ret wouldn't work!
    ret  ;# now it's a 32-bit ret; no "ret far" needed

unreal_mode:
    jmp  code16r: offset code16p + loadaddr
.code16
code16p: ;# that far jump put us in 16-bit protected mode
;# now let's put at least the stack segment back to 16 bits
    mov  ax, data16r
;# any segments left commented out will be in "unreal" mode
    mov  ss, ax
    ;#mov  ds, ax
    ;#mov  es, ax
    mov  eax, cr0
    and  al, 0xfe ;# mask out bit 0, the PE (protected-mode enabled) bit
    mov  cr0, eax
    jmp  0:offset unreal + loadaddr
unreal:  ;# that far jump put us back to a realmode CS
    xor  ax, ax
    mov  ss, ax
    mov  ds, ax
    mov  es, ax
    lidt [real_mode_idt + loadaddr]
    sti  ;# re-enable interrupts
    data32 ret ;# adjust stack appropriately for call from protected mode

a20:
    mov  al, 0x0d1
    out  0x64, al ;# to keyboard
0:  in   al, 0x64
    and  al, 2
    jnz  0b
    mov  al, 0x4b
    out  0x60, al ;# to keyboard, enable A20
    ret

;# don't need 'write' till after bootup
    .org 0x1fe + start
    .word 0x0aa55 ;# mark boot sector
    ;# end of boot sector

write:
    mov  edi, iobuffer ;# destination address
    mov  ebx, edi  ;# save in EBX for BIOS call
    mov  ecx, 512*18*2/4 ;# using 32-bit MOVS instruction
    addr32 rep movsd
    mov  ax, (3 << 8) + 36  ;# 18 sectors per head, 2 heads
    mov  dx, 0x0000 ;# head 0, drive 0
    mov  ch, [cylinder + loadaddr] ;# cylinder number in CH
    mov  cl, 1  ;# sector number is 1-based
    int  0x13  ;# let BIOS handle the nitty-gritty floppy I/O
    ret

graphicsmode:
    mov  ax, 0x4f01 ;# get video mode info
    mov  cx, vesa ;# a 16-bit color linear video mode (5:6:5 rgb)
    mov  di, iobuffer ;# use floppy buffer, not during floppy I/O!
    int  0x10
    mov  ax, 0x4f02 ;# set video mode
    mov  bx, cx ;# vesa mode
    int  0x10
    mov  ebx, iobuffer + 0x28 ;# linear frame buffer address
    mov  eax, [ebx]
    mov  [displ + loadaddr], eax
    ret

.code32 ;# these routines called from high-level Forth (protected mode)
readf:
    mov  cylinder + loadaddr, al
    dup_ ;# save cylinder number
    push edi
    mov  edi, [esi+4]
    shl  edi, 2  ;# convert longwords to bytes...
    add  edi, loadaddr ;# ... and then to absolute address
    call unreal_mode
.code16
    call read
    data32 call protected_mode
.code32
    pop  edi
    ;# fall through to common code

;# common code for both reads and writes
readf1:
    drop ;# restore EAX, cylinder number
    inc  eax  ;# next cylinder
    add  dword ptr [esi], 0x1200  ;# move memory pointer up this many longwords
    ret

writef:  ;# write cylinder to floppy disk
;# called with cylinder number in AL, source address, in longwords, in ESI
    mov  cylinder + loadaddr, al
    dup_ ;# save cylinder number
    push esi  ;# save data stack pointer, we need it for memory transfer
    mov  esi, [esi+4]  ;# get memory offset
    shl  esi, 2  ;# convert from longwords to bytes...
    add  esi, loadaddr  ;# ... and then to absolute address
    call unreal_mode
.code16
    call write
    data32 call protected_mode
.code32
    pop  esi  ;# restore stack pointer
    jmp  readf1  ;# join common code

buffer:  ;# return IO buffer address in words, for compatibility reasons
    dup_
    mov  eax, iobuffer >> 2
    ret

off:  ;# return loadaddr expressed in words
;# this is the offset in RAM to where block 0 is loaded
    dup_
    mov  eax, loadaddr >> 2
    ret

setgraphics:
    call unreal_mode
.code16
    addr32 mov byte ptr [es: 0xb8000 | (upper_right - 8)], 'T'
    call graphicsmode
    addr32 mov byte ptr [es: 0xb8000 | (upper_right - 8)], 'G'
    data32 call protected_mode
.code32
    ret

;# these must be defined elsewhere before use
seekf:
cmdf:
readyf:
stop:
    ret
