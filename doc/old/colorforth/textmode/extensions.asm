.intel_syntax
.code32
;# extensions to colorForth better written in assembly language
;# i'm not of the opinion that coding in machine language is a step forward
;# jc.unternet.net

.macro highlevel constant
 ;# make assembly source constants available to high-level routines
 dup_ ;# push anything in EAX onto stack
 mov eax, \constant
 ret ;# return with constant at TOS (top of stack = EAX)
.endm

.macro tenshift register
 ;# shift one decimal point using only binary shifts
 shl \register, 1 ;# this multiplies by 2
 push \register ;# save on stack
 shl \register, 2 ;# multiply by 4 for total of 8
 add [esp], \register ;# add to what we saved is total of 10 times
 pop \register ;# off the stack and back into the register
.endm

hp_: highlevel hp ;# horizontal pixels
vp_: highlevel vp ;# vertical pixels
iw_: highlevel iw ;# icon width including padding
ih_: highlevel ih ;# icon height including padding
hc_: highlevel hc ;# horizontal characters
vc_: highlevel vc ;# vertical characters

vframe: ;# (-a) needed for low-level graphic stuff
 dup_
 mov eax, frame + loadaddr
 ret

vram: ;# (-a) the actual linear RAM of the video board
 dup_
 mov eax, [displ + loadaddr]
 ret

cells: ;# (n-n) return number of bytes in a number of cells
 ;# ONLY WORKS for cell in [2, 4]
 shl eax, (cell / 2)
 ret

.equ nan, 0x80000000 ;# use minimum integer as NaN indicator
nan_: highlevel nan ;# Not a Number for use in fixed-point arithmetic

oneplus:
 inc eax
 ret

oneless:
 dec eax
 ret

;# don't use the following where flags matter
one:
 dup_
 xor eax, eax
 inc eax
 ret

minus1:
 dup_
 xor eax, eax
 dec eax
 ret

wat: ;# word at
 mov edx, eax
 xor eax, eax
 mov ax, [edx]
 ret

wstore: ;# word store
 mov edx, eax
 drop
 mov [edx], ax
 drop
 ret

pwstore: ;# plus word store
 mov edx, eax
 drop
 add [edx], ax
 drop
 ret

zero: ;# na- erase words of RAM
 push edi
 push ecx
 mov edi, eax
 shl edi, 2 ;# convert word address to byte address
 drop
 mov ecx, eax
 xor eax, eax
 rep stosd
 drop
 pop ecx
 pop edi
 ret

herestore: ;# a- directly manipulate the 'here' pointer
 mov h + loadaddr, eax
 drop
 ret

idt:
 .word idt_end - idt0 - 1
 .long loadaddr + idt0
idt0:
 .word divide_error + loadaddr
 .word code32p
 .byte 0
 .byte 0x8e ;# 32-bit Ring 0 interrupt gate
 .word 0
idt_end:

divide_error: ;# assuming 32-bit dividend, fix instruction to return NaN
 xor eax, eax 
 mov edx, [esp] ;# get pointer of instruction, div or idiv
 mov ah, [edx + 1] ;# what we want is the modr/m byte following it
 and ah, 0b11000111 ;# get rid of the nnn bits of the instruction
 mov edx, 0xc30089 ;# "mov r/m, eax; ret"
 or edx, eax ;# set the destination reg or memory, ASSUMING NO SIB
 xor ah, ah ;# clear EAX again
 inc eax  ;# set divisor to 1
 push edx ;# push "subroutine" on stack, won't work from register
 call esp ;# call the 3-byte "subroutine"
 pop edx
 xor edx, edx  ;# clear the high 32-bits of the dividend
 dec edx  ;# now make it an extension of the NaN sign bit
 mov eax, nan ;# "not a number"
 iret ;# return to try division again... endless loop if we don't get it right
