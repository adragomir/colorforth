.intel_syntax ;#colorforth, 2001 jul 22, chuck moore, public domain

;#.486p

.ifdef CM2001 ;# Chuck Moore's 2001 code includes AGP-specific video...
 .equ QUESTIONABLE, 1 ;# ...and other weird stuff found in color.com binary
 .equ AGP, 1  ;# hard-coded for ATI video
 .equ E1_STROBE, 1  ;# see 'debugout' macro below
 .equ AUTO_REFRESH, 1 ;# screen refresh constantly runs
.endif

.macro debugout
/* CM referred to this as "Terry Loveall's e1 strobe" in some online docs, but
 * I can't find anything regarding port 0xe1 anywhere else, and the data
 * sent doesn't make any sense anyway. The boot failure problems mentioned in
 * that online document would more likely be due to the hardcoded millisecond
 * calculations for busy-wait routines than any "strobe" effect.
 * milliseconds should be calulated, with a single loop, using the
 * pc clock chip, like bogoMIPS. or, better yet, the clock should be used for
 * timing rather than busy-wait loops.
 */
.ifdef E1_STROBE
    out 0xe1, al
.endif
.endm

;# can't use loopnz in 32-bit mode
.macro next adr
    dec  ecx
    jnz  \adr
.endm

;# save contents of eax on data stack
;# (eax is already a copy of top of data stack)
.macro dup_
    lea  esi, [esi-4]
    mov  [esi], eax
.endm

;# pop what's at bottom of data stack back into eax
.macro drop
    lodsd
.endm

;# compile Forth words with Huffman coding
.macro packword words:vararg
 .irp word, \words
 .equ packed, 0
 .equ bitcount, 32
 .equ stoppacking, 0
 .irpc letter, "\word"
  .equ savepacked, packed
  .equ huffindex, 0
  .equ huffcode, 0
  .irpc huffman, " rtoeanismcylgfwdvpbhxuq0123456789j-k.z/;:!+@*,?"
   .ifeqs "\letter", "\huffman"
    .equ bitshift, 4 + (huffindex / 8)
    .ifge bitshift - 6
     .equ bitshift, 7
    .endif
    .ifeq stoppacking
     .equ packed, packed | (huffcode << (bitcount - bitshift))
     .equ bitcount, bitcount - bitshift
    .endif
   .else
    .equ huffindex, huffindex + 1
    .equ huffcode, huffcode + 1
    .ifeq huffcode - 0b00001000 ;# going from 4-bit to 5-bit code
     .equ huffcode, 0b00010000
    .endif
    .ifeq huffcode - 0b00011000 ;# going from 5-bit to 7-bit code
     .equ huffcode, 0b01100000
    .endif
   .endif
  .endr
  .ifne packed & 0xf ;# low 4 bits cannot be occupied with packed stuff
   .equ packed, savepacked
   .equ stoppacking, -1
  .endif
 .endr
 .long packed
 .endr
.endm

.ifndef SMALLSCREEN
 .equ hp, 1024 ;# 1024 or 800
 .equ vp, 768 ;# 768 or 600
.if CM2001 - 1 == 0
 .equ vesa, 0x0117 ;# 1024x768 mode
.elseif JF2005 - 1 == 0
 .equ vesa, 0x4118
.else
 .equ vesa, 0x4117 ;# bit 12 sets linear address mode in 0x117 or 0x114
.endif ;# CM2001
.else ;# SMALLSCREEN
 .equ hp, 800
 .equ vp, 600
 .equ vesa, 0x4114
.endif ;# SMALLSCREEN
.equ buffer, 604*256
.include "boot.asm" ;# boot boot0 hard

;#   100000 dictionary
;#    a0000 top of return stack
;#    9f800 top of data stack
;#    9d800 free
;#    97000 floppy buffer
;#     4800 source ;# block 18, first high-level source block (load screen)
.equ icons, 12*256*4 ;# 3000, block 12 start of character maps
;#     7c00 bios boot sector
;#        0 forth

;# register usage:
;#  EAX: TOS, top-of-stack for GODD stack; [ESI] is actually the SECOND element
;#  ESI: data stack pointer (GODD stack)
;#  EDI: pointer to source word, used by compiler and editor
;#       see LOAD and INTER; EDI is incremented after each word is fetched,
;#       which is why you see [-4+EDI*4] a lot, to get the word being compiled

warm: dup_
start1:
.ifdef AGP
    call ati0  ;# access North Bridge chipset to get display RAM address
.else
    pop [displ]  ;# use address determined by VBE2 call in boot.asm
.endif
    call show0 ;# set up 'main' task to draw screen
    mov  dword ptr forths, offset ((forth1-forth0)/4) ;# number of Forth words
    mov  dword ptr macros, offset ((macro1-macro0)/4) ;# number of macros
    mov  eax, 18 ;# load start screen, 18
;# the start screen loads a bunch of definitions, then 'empty' which shows logo
.ifndef cm2001
;# in theory this is a good idea, preventing stack underflow...
;# in practice, other things are messed up so this causes extra item on stack
;#  dup_ ;# so the 'drop' in 'load' doesn't cause stack underflow
.endif
    call load
    jmp  accept ;# wait for keyhit

;# This version of colorforth has cooperative round-robin multi-tasking.
;# the tasks are: God (the forth kernel), and main
;# Each has two grow-down stacks; 's' indicates the
;# return stack, 'd' indicates the data stack.  Thus 'Gods' and 'Godd'
;# are the tops of the return and data stacks, respectively, for the
;# God task.
.equ gods, 0x28000*4 ;# 0xa0000, top of return stack
.equ godd, gods-750*4 ;# 0x9f448, top of data stack
.equ mains, godd-1500*4 ;# 0x9dcd8
.equ maind, mains-750*4 ;# 0x9d120
.align 4
;# 'me' points to the save slot for the current task
me: .long god
screen:
.ifdef CM2001
    .long 0x100f1f ;# matches cm2001 color.com binary
.else
    .long 0 ;# logo
.endif
;# When we switch tasks, we need to switch stacks as well.  We do this
;# by pushing eax (cached top-of-stack) onto the data stack, pushing
;# the data stack pointer onto the return stack, and then saving the
;# return stack pointer into the save slot for the task.
;#
;# these are the save slots - each is followed by code to resume the
;# next task - the last one jumps 'round to the first.
round: call unpause
god:
.ifdef CM2001
    .long 0x9ffe8 ;# found in cm2001 color.com binary
.else
    .long 0 ;# gods-2*4
.endif
    call unpause
main:
.ifdef CM2001
    .long 0x9dcd0
.else
    .long 0 ;# mains-2*4
.endif
    jmp  round

pause: dup_ ;# save cached datum from top of data stack
    push esi ;# save data stack pointer on return stack
    mov  eax, me ;# get current task
    mov  [eax], esp ;# put our stack pointer into [me]
    add  eax, 4 ;# skip storage slot, point to round-robin CALL or JMP
    jmp  eax ;# execute the CALL or JMP

unpause: pop  eax ;# return address is that of 'main' slot above
    mov  esp, [eax] ;# load 'main' task return stack
    mov  me, eax ;# 'main' task becomes 'me', current task
    pop  esi ;# restore my task's data-stack pointer
    drop ;# load previously dup'd datum back into EAX
    ret

act: ;# set currently active task
    mov  edx, maind-4 ;# bottom element of 'main' task data stack
    mov  [edx], eax ;# 0 if called from 'show'
    mov  eax, mains-4 ;# bottom element of 'main' task return stack
    pop  [eax] ;# return of 'god' task now on 'main' stack
    sub  eax, 4 ;# down one slot on 'main' stack
    mov  [eax], edx ;# store 'main' data stack pointer
    mov  main, eax ;# store 'main' return stack pointer in 'main' slot
    drop ;# what was 'dup'd before now into eax
    ret ;# to previous caller, since we already popped 'our' return address

show0: call show
    ret
show: pop  screen ;# pop return address into screen; 'ret' if from show0
    dup_ ;# save whatever is in EAX on data stack
    xor  eax, eax ;# make top stack element 0
    call act ;# make following infinite loop the 'active task'
0:  call graphic ;# just 'ret' in gen.asm
    call [screen] ;# ret if called from show0
    call switch ;# load framebuffer into video, then switch task
    inc  eax ;# why bother? can't see this being used anywhere
    jmp  0b ;# loop eternally

c_: mov  esi, godd+4 ;# causes data stack underflow?!
    ret

mark: ;# save current state so we can recover later with 'empty'
    mov  ecx, macros ;# save number of macros in longword mk
    mov  mk, ecx
    mov  ecx, forths ;# number of forth words in mk+1
    mov  mk+4, ecx
    mov  ecx, h ;# HERE pointer in mk+2
    mov  mk+2*4, ecx
    ret

empty: ;# restore state saved at last 'mark'
    mov  ecx, mk+2*4
    mov  h, ecx ;# HERE pointer restored
    mov  ecx, mk+4
    mov  forths, ecx ;# number of forth words restored
    mov  ecx, mk
    mov  macros, ecx ;# number of macros restored
    mov  dword ptr class, 0 ;# used by C18 compiler, cleared here
    ret

mfind: ;# find pointer to macro code
    mov  ecx, macros ;# number of macros, 1-based
    push edi ;# save destination pointer, we need to use it momentarily
    lea  edi, [macro0-4+ecx*4] ;# point to last macro
    jmp  0f ;# search dictionary

find: ;# locate code of high- or low-level Forth word
    mov  ecx, forths ;# current number of Forth definitions
    push edi ;# save destination pointer so we can use it
    lea  edi, [forth0-4+ecx*4] ;# point it to last packed Forth word
0:  std  ;# search backwards
    repne scasd ;# continue moving until we hit a match
    cld  ;# clear direction flag again
    pop  edi ;# no longer need this, can tell from ECX where match was found
    ret

;# ex1 is the default action of 'aword'; find and execute the word
ex1: dec dword ptr words ;# from keyboard
    jz   0f
    drop
    jmp  ex1
0:  call find
    jnz  abort1
    drop
    jmp  [forth2+ecx*4] ;# jump to low-level code of Forth word or macro

execute: ;# execute source word
    mov dword ptr lit, offset alit
    dup_ ;# save EAX (top of stack) on data stack
    mov  eax, [-4+edi*4]
ex2:
    and  eax, 0xfffffff0 ;# mask tag bits which indicate word type
    call find ;# look for word in the dictionary
    jnz  abort ;# if not found, abort
    drop ;# restore EAX from data stack
    jmp  [forth2+ecx*4] ;# execute the Forth word found

abort: ;# abort load operation
    mov  curs, edi ;# store pointer to last source word encountered
    shr  edi, 10-2 ;# get block number from word pointer
    mov  blk, edi ;# update BLK
    ;# this way, E will begin editing at point of failure
abort1: mov  esp, gods ;# reset return stack pointer
    mov  dword ptr  spaces+3*4, offset forthd ;# reset adefine
    mov  dword ptr  spaces+4*4, offset qcompile
    mov  dword ptr  spaces+5*4, offset cnum
    mov  dword ptr  spaces+6*4, offset cshort
    mov  eax, 057 ;# '?'
    call echo_
    jmp  accept

sdefine: pop adefine
    ret
macro_: call sdefine
macrod: mov  ecx, macros
    inc dword ptr  macros
    lea  ecx, [macro0+ecx*4]
    jmp  0f

forth: call sdefine
forthd:
    mov  ecx, forths ;# current count of Forth words
    inc dword ptr forths ;# make it one more
    lea  ecx, [forth0+ecx*4] ;# point to the slot for the next definition
0:  mov  edx, [-4+edi*4] ;# load the packed word from source block
    and  edx, 0xfffffff0 ;# mask out the tag bits
    mov  [ecx], edx ;# store the "naked" word in the dictionary
    mov  edx, h ;# HERE pointer, place available for new compiled code
    mov  [forth2-forth0+ecx], edx
    lea  edx, [forth2-forth0+ecx]
    shr  edx, 2
    mov  last, edx
    mov  list, esp
    mov dword ptr  lit, offset adup
    ;# for CLASS stuff, search web for moore01a.pdf
    test dword ptr class, -1 ;# is CLASS set for C18 compilation?
    jz   0f ;# return if not
    jmp  [class] ;# otherwise do C18 compilation
0:  ret

cdrop: ;# compile a DROP instruction
    mov  edx, h ;# get HERE pointer to newly compiled code
    mov  list, edx ;# store at LIST for tail optimization
    mov  byte ptr [edx], 0x0ad ;# compile LODSD
    inc  dword ptr h ;# update HERE pointer
    ret

qdup: ;# compile a DUP if necessary (uses tail optimizer)
    mov  edx, h ;# get HERE pointer
    dec  edx ;# point back to last byte compiled
    cmp  list, edx ;# is it subject to tail optimization?
    jnz  cdup ;# if not, go ahead and compile the DUP
    cmp  byte ptr [edx], 0x0ad ;# is it a DROP instruction?
    jnz  cdup ;# if not, compile the DUP
    ;# so it's a DROP instruction, and we were about to compile a DUP.
    ;# DROP followed by DUP is a no-op, so point HERE to before the DROP
    ;# was compiled, and we've eliminated a one-byte DROP, a 5-byte DUP,
    ;# and the wasted runtime they would have consumed
    mov  h, edx ;# move HERE back one byte
    ret
cdup: ;# compile a DUP instruction
    mov  edx, h ;# get HERE pointer
    ;# disassembly of DUP:
    ;# 8d 76 fc    lea    esi,[esi-4]
    ;# 89 06       mov    DWORD PTR [esi],eax
    mov  dword ptr [edx], 0x89fc768d ;# compile the DUP
    mov  byte ptr [4+edx], 06
    add dword ptr  h, 5 ;# adjust HERE accordingly
    ret

adup: dup_
    ret

var1: dup_
    mov  eax, [4+forth0+ecx*4]
    ret
variable: call forthd
    mov dword ptr  [forth2-forth0+ecx], offset var1
    inc dword ptr  forths ;# dummy entry for source address
    mov  [4+ecx], edi ;# EDI is source address containing variable's value
    call macrod
    mov dword ptr  [forth2-forth0+ecx], offset 0f
    inc dword ptr  macros
    mov  [4+ecx], edi
    inc  edi ;# update source word pointer to following word
    ret
0:  call [lit]
    mov  eax, [4+macro0+ecx*4]
    jmp  0f

cnum: call [lit]
    mov  eax, [edi*4]
    inc  edi
    jmp  0f

cshort: call [lit]
    mov  eax, [-4+edi*4]
    sar  eax, 5
0:  call literal
    drop
    ret

alit: mov dword ptr lit, offset adup
literal: call qdup
    mov  edx, list
    mov  list+4, edx
    mov  edx, h
    mov  list, edx
    mov  byte ptr [edx], 0x0b8
    mov  [1+edx], eax
    add dword ptr  h, 5
    ret

qcompile: call [lit]
    mov  eax, [-4+edi*4]
    and  eax, 0xfffffff0 ;# mask out tag bits
    call mfind ;# locate word in macro dictionary
    jnz  0f ;# if failed, try in Forth dictionary
    drop ;# restore EAX
    jmp  [macro2+ecx*4] ;# jmp to macro code
0:  call find ;# try to find the word in the Forth dictionary
    mov  eax, [forth2+ecx*4] ;# load code pointer in case there was a match
0:  jnz  abort ;# abort if no match in dictionary
call_:
    mov  edx, h ;# get HERE pointer to where new compiled code goes
    mov  list, edx
    mov  byte ptr [edx], 0x0e8 ;# x86 "call" instruction
    add  edx, 5
    sub  eax, edx ;# it has to be a 32-bit offset rather than absolute address
    mov  [-4+edx], eax ;# store it after the "call" instruction
    mov  h, edx ;# point HERE to end of just-compiled code
    drop ;# restore EAX from data stack
    ret

compile: call [lit]
    mov  eax, [-4+edi*4]
    and  eax, 0xfffffff0 ;# mask out tag bits
    call mfind
    mov  eax, [macro2+ecx*4]
    jmp  0b

short_: mov dword ptr lit, offset alit
    dup_
    mov  eax, [-4+edi*4]
    sar  eax, 5
    ret

num: mov dword ptr lit, offset alit
    dup_
    mov  eax, [edi*4]
    inc  edi
    ret

comma: mov  ecx, 4
0:  mov  edx, h ;# get HERE pointer to where code is compiled
    mov  [edx], eax ;# move what's at TOS into that location
    mov  eax, [esi] ;# first part of DROP
    lea  edx, [edx+ecx] ;# adjust HERE for number of bytes compiled
    lea  esi, [esi+4] ;# finish DROP, adjusting data stack pointer
    mov  h, edx ;# update HERE
    ret

comma1: mov  ecx, 1 ;# 1-byte compilation
    jmp  0b

comma2: mov  ecx, 2 ;# 2-byte compilation
    jmp  0b

comma3: mov  ecx, 3 ;# 3-byte compilation
    jmp  0b

semi: mov  edx, h ;# get HERE pointer
    sub  edx, 5 ;# peek back 5 bytes
    cmp  list, edx
    jnz  0f
    cmp  byte ptr [edx], 0x0e8 ;# e8=call, e9=jmp
    jnz  0f
    inc  byte ptr [edx] ;# change it to jmp
    ret
0:  mov  byte ptr [5+edx], 0x0c3 ;# ret
    inc  dword ptr h
    ret

then: mov  list, esp
    mov  edx, h
    sub  edx, eax
    mov  [-1+eax], dl
    drop
    ret

begin: mov  list, esp
here: dup_
    mov  eax, h
    ret

qlit: mov  edx, h
    lea  edx, [edx-5]
    cmp  list, edx
    jnz  0f
    cmp  byte ptr [edx], 0x0b8
    jnz  0f
    dup_
    mov  eax, list+4
    mov  list, eax
    mov  eax, [1+edx]
    ;# disassembly of DUP:
    ;# 8d 76 fc    lea    esi,[esi-4]
    ;# 89 06       mov    DWORD PTR [esi],eax
    cmp  dword ptr [edx-5], 0x89fc768d ;# dup
    jz   q1
    mov  h, edx
    jmp  cdrop
q1: add dword ptr  h, -10 ;# flag nz
    ret
0:  xor  edx, edx ;# flag z
    ret

less: cmp  [esi], eax
    js   0f ;# flag nz
    xor  ecx, ecx ;# flag z
0:  ret

qignore: ;# is this a continuation of the previous word? ignore if so
    test dword ptr [-4+edi*4], 0xfffffff0 ;# valid packed word?
    jnz  nul  ;# return if so
    ;# otherwise call it a NUL and terminate the LOAD
    pop  edi ;# pop the RET address from the call to qignore
    pop  edi ;# now pop the saved EDI which was PUSHed by LOAD
nul: ret ;# return to caller (where LOAD was called if NUL)

jump: pop  edx
    add  edx, eax
    lea  edx, [5+eax*4+edx]
    add  edx, [-4+edx]
    drop
    jmp  edx

load: shl  eax, 10-2 ;# multiply by 256 longwords, same as 1024 bytes
    push edi ;# save EDI register in case it's being used by interpreter
    mov  edi, eax
    drop ;# block number from data stack
inter: mov  edx, [edi*4] ;# get next longword from block
    inc  edi ;# then point to the following one
    and  edx, 017 ;# get only low 4 bits, the type tag
    call spaces[edx*4] ;# call the routine appropriate to this type
    jmp  inter ;# loop till "nul" reached, which ends the loop

.align 4
;# these are the compiler actions for the various type tags
spaces: .long qignore, execute, num ;# 0 = extension, 1=execute, 2=executelong
;# 3=define, either in macro dictionary or forth dictionary
adefine: ;# where definitions go, either in macrod (dictionary) or forthd
.ifdef CM2001
    .long forthd ;# as found in CM2001 color.com binary
.else
    .long macrod ;# default, the macro dictionary
.endif
    .long qcompile, cnum, cshort, compile ;# 4=green, 5=long, 6=short, 7=cyan
    .long short_, nul, nul, nul ;# 8=executeshort
    .long variable, nul, nul, nul ;# 12=variable

lit: .long adup
.ifdef CM2001
mk: .long 0x2e, 0x5e, 0x101028
h: .long 0x101137
last: .long 0x59f
class: .long 0 
list: .long 0x101132, 0x10111e
macros: .long 0x2e
forths: .long 0x67
.else
mk: .long 0, 0, 0
h: .long 0x40000*4 ;# HERE pointer: start compiling at 0x100000
last: .long 0
class: .long 0 ;# used by C18 compiler
list: .long 0, 0 ;# used by tail optimizer
macros: .long 0
forths: .long 0
.endif

macro0:
    packword ";", dup, ?dup, drop, then, begin
macro1:
.ifdef CM2001
/* words are defined starting in block 24... their packed representations
/* and longword pointers are stored in these tables */
    packword swap, 0, if, -if, a, a!, 2*, "a," @, !
    packword nip, +, or, binary, and, u+, ?, over, push, pop
    packword -, for, *next, next, 0next, -next, i, *end, end, +!
    packword nop, align, or!, *, */, /mod, /, mod, 2/, time
    packword p@, p!
    ;# following that is some nonsense:
    .long 0xc9800000 ;# hd (a valid packed word but isn't defined anywhere)
    .long 0x00005811 ;# which isn't a valid packed word
.endif
    .rept 128 - ((.-macro1)/4) .long 0; .endr ;# room for new macros
forth0:
    packword boot, warm, pause, macro, forth, c, stop, read, write, nc
    packword command, seek, ready, act, show, load, here, ?lit, "3,", "2,"
    packword "1,", ",", less, jump, accept, pad, erase, copy, mark, empt
    packword emit, digit, 2emit, ., h., h.n, cr, space, down, edit
    packword e, lm, rm, graphic, text, keyboard, debug, at, +at, xy
    packword fov, fifo, box, line, color, octant, sp, last, unpack
.ifdef MANDELBROT
    packword vframe
.endif
forth1:
.ifdef CM2001
;# now we are at address 0xacc
    packword @, !, +, */, *, /, 2/, dup, negate, min
    packword abs, max, v+, writes, reads, oadf, save, block, white, red
    packword green, blue, silver, black, screen, 5*, cf, logo, empty, dump
    packword icons, print, file, north, colors, blks, w/c, buffer, size, set
    packword cyls, put, get, .com, format
;# this brings us to address 0x12cc
.endif
    .rept 512 - ((.-forth1)/4) .long 0; .endr
macro2:
    .long semi, cdup, qdup, cdrop, then, begin
0:
.ifdef CM2001
;# slots filled starting with 0x1008d0 at 0x12e4, to 0x0552 at 0x138c.
    .long 0x1008d0, 0x1008ee, 0x100902, 0x100916, 0x10092a
    .long 0x10093e, 0x10096d, 0x10097c, 0x100985, 0x1009c0
    .long 0x100a2b, 0x100a3a, 0x100a69, 0x100a73, 0x100a99
    .long 0x100aa8, 0x100ad7, 0x100af0, 0x100b04, 0x100b18
    .long 0x100b2c, 0x100b3b, 0x100b45, 0x100b4b, 0x100b55
    .long 0x100b7a, 0x100b89, 0x100b9d, 0x100ba3, 0x100bc3
    .long 0x100c2e, 0x100c3d, 0x100c57, 0x100c7c, 0x100c90
    .long 0x100cb8, 0x100cdb, 0x100ce5, 0x100cef, 0x100cfe
    .long 0x100758, 0x10076c, 0x000552
.endif
    .rept 128 - ((.-0b)/4) .long 0; .endr
forth2:
    .long boot, warm, pause, macro_, forth, c_, stop, readf, writef, nc_
    .long cmdf, seekf, readyf, act, show, load, here, qlit, comma3, comma2
    .long comma1, comma, less, jump, accept, pad, erase, copy, mark, empty
    .long emit, edig, emit2, dot10, hdot, hdotn, cr, space, down, edit
    .long e, lms, rms, graphic, text1, keyboard, debug, at, pat, xy_
    .long fov_, fifof, box, line, color, octant, sps, last_, unpack
.ifdef MANDELBROT
    .long vframe
.endif
0:
.ifdef CM2001
;# in the CM2001 color.com object file, there are 45 entries, starting
;# 45 entries, from 0x100d12 at 0x15d0 to 0x1008c1 at 0x1680.
    .long 0x100d12, 0x100d1a, 0x100d26, 0x100d2c, 0x100d37
    .long 0x100d3e, 0x100d4d, 0x100d50, 0x100d56, 0x100d5e
    .long 0x100d6f, 0x100d79, 0x100d88, 0x100d94, 0x100da6
    .long 0x100db8, 0x100db8, 0x100ddc, 0x100ded, 0x100dfc
    .long 0x100e0b, 0x100e1a, 0x100e29, 0x100e38, 0x100e47
    .long 0x100e74, 0x100e8e, 0x100f1a, 0x100fc4, 0x100fce
    .long 0x100fdd, 0x100fec, 0x100ffb, 0x10100a, 0x101019
    .long 0x101028, 0x101039, 0x101044, 0x10104f, 0x10107d
    .long 0x1010a5, 0x1010d3, 0x1010f5, 0x101119, 0x1008c1
.endif
    .rept 512 - ((.-0b)/4) .long 0; .endr ;# room for new definitions

boot: mov  al, 0x0fe ;# reset
    out  0x64, al
    jmp  .

erase:
    mov  ecx, eax
    shl  ecx, 8
    drop
    push edi
    mov  edi, eax
    shl  edi, 2+8
    xor  eax, eax
    rep stosd
    pop edi
    drop
    ret

;# copy block from blk to block number at top-of-stack (in EAX)
copy: cmp  eax, 12 ;# can't overwrite machine-code blocks...
    jc   abort1 ;# so if we're asked to, abort the operation
    mov  edi, eax ;# get block number into EDI
    shl  edi, 2+8 ;# multiply by 1024 to get physical (byte) address
    push esi  ;# save data stack pointer so we can use it for block move
    mov  esi, blk ;# get current block number from blk
    shl  esi, 2+8 ;# multiply by 1024 to get address
    mov  ecx, 256 ;# 256 longwords = 1024 bytes
    rep movsd ;# move the block from source (ESI) to destination (EDI)
    pop  esi  ;# restore data stack pointer
    mov  blk, eax ;# destination block becomes new current block (blk)
    drop ;# no longer need the block number
    ret

debug: mov dword ptr  xy,  offset (3*0x10000+(vc-2)*ih+3)
    dup_
    mov  eax, god
    push [eax]
    call dot
    dup_
    pop  eax
    call dot
    dup_
    mov  eax, main
    call dot
    dup_
    mov  eax, esi
    jmp  dot

.equ iw, 16+6 ;# icon width, including 6 pixels padding
.equ ih, 24+6 ;# icon height, including padding
.equ hc, hp/iw ;# 46 ;# number of horizontal characters on screen
.equ vc, vp/ih ;# 25 ;# number of vertical characters
;# MASM's 3-byte NOP for alignment is 2e8bc0, cs: mov eax,eax
;# whereas gas's is 8d7600, lea esi,[esi].
.ifdef CM2001
     cs mov eax, eax ;# (just the way MASM pads the alignment)
xy: .long 0x033d02e5
lm: .long 3
rm: .long hc*iw
xycr: .long 3*0x10000+3
fov: .long 10*(2*vp+vp/2) 
.else
.align 4 
xy: .long 3*0x10000+3 ;# 3 pixels padding on each side of icons
lm: .long 3 ;# left margin
rm: .long hc*iw ;# 1012 is right margin on a 1024-pixel screen
xycr: .long 0 ;# this is apparently unused? (jc)
fov: .long 10*(2*vp+vp/2) ;# field of view?
;# nothing that I can see even uses FOV, maybe get rid of it? (jc)
.endif

nc_: dup_
    mov  eax, (offset nc-offset start)/4
    ret

xy_: dup_
    mov  eax, (offset xy-offset start)/4
    ret

fov_: dup_
    mov  eax, (offset fov-offset start)/4
    ret

sps: dup_
    mov  eax, (offset spaces-offset start)/4
    ret

last_: dup_
    mov  eax, (offset last-offset start)/4
    ret

.include "gen.asm" ;# cce.asm pio.asm ati128.asm ati64.asm gen.asm

.equ yellow, 0xffff00 ;# RG0 is yellow
cyan: dup_
    mov  eax, 0x00ffff ;# 0GB is cyan
    jmp  color
magenta: dup_
    mov  eax, 0xff00ff ;# R0B is magenta
    jmp  color
silver: dup_ ;# half-brightness white
    mov  eax, 0xc0c0c0 ;# rgb, 4 bits of each
    jmp  color
blue: dup_
    mov  eax, 0x4040ff ;# a little r and g to make it brighter
    jmp  color
red: dup_
    mov  eax, 0xff0000 ;# pure red
    jmp  color
green: dup_
    mov  eax, 0x8000ff00 ;# what's that 0x80 for?
    jmp  color

history:
.ifdef CM2001
    .byte 0, 0, 0, 0, 0, 0, 0, 37, 10, 3, 9
.else
    .rept 11 .byte 0; .endr
.endif
echo_: push esi
    mov  ecx, 11-1
    lea  edi, history
    lea  esi, [1+edi]
    rep  movsb
    pop  esi
    mov  history+11-1, al
    drop
    ret

right: dup_
    mov  ecx, 11
    lea  edi, history
    xor  eax, eax
    rep  stosb
    drop
    ret

down: dup_
    xor  edx, edx
    mov  ecx, ih
    div  ecx
    mov  eax, edx
    add  edx, 3*0x10000+0x8000-ih+3
    mov  xy, edx
zero: test eax, eax
    mov  eax, 0
    jnz  0f
    inc  eax
0:  ret

blank: ;# clear the screen (black)
    dup_
    xor  eax, eax
    mov  xy, eax
    call color
    dup_
    mov  eax, hp
    dup_
    mov  eax, vp
    jmp  box

top: ;# move to top of screen
    mov  ecx, lm ;# get left margin in pixels
    shl  ecx, 16 ;# shift to X of XY doubleword
    add  ecx, 3 ;# add 3 pixels for top (Y) margin
    mov  xy, ecx ;# update XY
    mov  xycr, ecx ;# update XYCR (unused? [jc])
    ret

qcr: ;# insert a carriage return if at end of line
    mov  cx, word ptr xy+2 ;# get X, the horizontal character pointer
    cmp  cx, word ptr rm ;# at or past right margin?
    js   0f ;# no, return
cr: ;# insert a carriage return (drop to next line)
    mov  ecx, lm ;# set X to left margin
    shl  ecx, 16 ;# move to high 16 bits of XY doubleword...
    mov  cx, word ptr xy ;# now get Y word...
    add  ecx, ih ;# and add height of icon (character) to it
    mov  xy, ecx ;# update XY pointer
0:  ret

lms: ;# set left margin to new pixel amount (high-level FORTH word LM)
    mov  lm, eax ;# top of stack becomes new left margin
    drop
    ret

rms: ;# set right margin to new pixel amount (high-level FORTH word RM)
    mov  rm, eax ;# top of stack becomes new right margin
    drop
    ret

at: ;# set current screen position
    mov  word ptr xy, ax ;# top of stack should be Y position
    drop ;# next on stack should be X position
    mov  word ptr xy+2, ax ;# X goes in high 16 bits of XY
    drop ;# clean X off the stack before returning
    ret

pat: ;# plus-AT -- add to current screen position
    add word ptr xy, ax ;# first add Y update
    drop ;# then get X update into EAX
    add  word ptr xy+2, ax ;# update X
    drop ;# clean X update off the stack before returning
    ret

octant: dup_
    mov  eax, 0x43 ;# poly -last y+ x+ ;0x23 ; last y+ x+
    mov  edx, [4+esi]
    test edx, edx
    jns  0f
    neg  edx
    mov  [4+esi], edx
    xor  al, 1
0:  cmp  edx, [esi]
    jns  0f
    xor  al, 4
0:  ret

;# keyboard
;# display one line of the onscreen keyboard (eight icons)
;# four chars at addr+12, space, four chars at addr.
;# IN: edi = addr-4
;# OUT: edi = addr
eight: add  edi, 12
    call four
    call space
    sub  edi, 16
four: mov  ecx, 4
;# display ECX chars from EDI+4, incrementing EDI with each char.
four1: push ecx
    dup_
    xor  eax, eax
    mov  al, [4+edi]
    inc  edi
    call emit
    pop  ecx
    next four1
    ret

stack: mov  edi, godd-4
0:  mov  edx, god
    cmp  [edx], edi
    jnc  0f
    dup_
    mov  eax, [edi]
    sub  edi, 4
    call qdot
    jmp  0b
0:  ret

keyboard: call text1
    mov  edi, board
    dup_
    mov  eax, keyc
    call color
    mov dword ptr  rm, hc*iw
    mov dword ptr  lm, hp-9*iw+3
    mov dword ptr  xy, (hp-9*iw+3)*0x10000+vp-4*ih+3
    call eight
    call eight
    call eight
    call cr
    add dword ptr  xy, 4*iw*0x10000
    mov  edi, shift
    add  edi, 4*4-4
    mov  ecx, 3
    call four1
    mov dword ptr  lm, 3
    mov  word ptr xy+2, 3
    call stack
    mov  word ptr xy+2, hp-(11+9)*iw+3
    lea  edi, history-4
    mov  ecx, 11
    jmp  four1

alpha: .byte 015, 012,  1 , 014
    .byte 024,  2 ,  6 , 010
    .byte 023, 011, 017, 021
    .byte 022, 013, 016,  7
    .byte  5 ,  3 ,  4 , 026
    .byte 027, 044, 025, 020
graphics: .byte 031, 032, 033, 0 ;# 1 2 3
    .byte 034, 035, 036, 030 ;# 4 5 6 0
    .byte 037, 040, 041, 057 ;# 7 8 9 ?
    .byte 051, 050, 052, 054 ;# : ; ! @
    .byte 046, 042, 045, 056 ;# z j . ,
    .byte 055, 047, 053, 043 ;# * / + -
numbers: .byte 031, 032, 033, 0 ;# 1 2 3
    .byte 034, 035, 036, 030 ;# 4 5 6 0
    .byte 037, 040, 041,  0  ;# 7 8 9 ?
    .byte  0,   0 ,  0 ,  0
    .byte  0,   0 ,  0 ,  0
    .byte  0,   0 ,  0 ,  0
octals: .byte 031, 032, 033, 0 ;# 1 2 3
    .byte 034, 035, 036, 030 ;# 4 5 6 0
    .byte 037, 040, 041,  0  ;# 7 8 9
    .byte  0 ,  5 , 023, 012
    .byte  0 , 020,  4 , 016
    .byte  0 ,  0 ,  0 ,  0
letter:
    cmp  al, 4
    js   0f
    mov  edx, board
    mov  al, [edx][eax]
0:  ret

keys: .byte 16, 17, 18, 19,  0,  0,  4,  5 ;# 20
    .byte  6,  7,  0,  0,  0,  0, 20, 21
    .byte 22, 23,  0,  0,  8,  9, 10, 11 ;# 40
    .byte  0,  0,  0,  0, 24, 25, 26, 27
    .byte  0,  1, 12, 13, 14, 15,  0,  0 ;# 60 n
    .byte  3,  2 ;# alt space

key: ;# loop forever, returning keyhits when available
/* the original CM2001 version uses this as the multitasking loop,
/* by calling 'pause' each time through the loop. this is what enables
/* the constant screen refresh, which causes significant delays in
/* keystroke processing under Bochs. */
    dup_             ;# save copy of return stack pointer(?)
    xor  eax, eax    ;# used as index later, so clear it
0:  call pause       ;# give other task a chance to run
    in   al, 0x64    ;# keyboard status port
    test al, 1       ;# see if there is a byte waiting
    jz   0b          ;# if not, loop
    in   al, 0x60    ;# fetch the scancode
    test al, 0xf0    ;# top row of keyboard generates scancodes < 0x10
    jz   0b          ;# we don't use that row (the numbers row), so ignore it
    cmp  al, 58      ;# 57, space, is the highest scancode we use
    jnc  0b          ;# so if it's over, ignore that too
    /* since we're ignoring scancodes less than 0x10, we subtract that
    /* before indexing into the table, that way the table doesn't have
    /* to have 16 wasted bytes */
    mov  al, [keys-0x10+eax] ;# index into key table
    ret

.align 4
;# layouts for the thumb (shift) keys
;# these sort of go in pairs:
;# foo0 is for the first character of a word
;# foo1 is used for the rest
graph0: .long nul0, nul0, nul0, alph0
    .byte  0 ,  0 ,  5 , 0 ;#     a
graph1: .long word0, x, lj, alph
    .byte 025, 045,  5 , 0 ;# x . a
alpha0: .long nul0, nul0, number, star0
    .byte  0 , 041, 055, 0 ;#   9 *
alpha1: .long word0, x, lj, graph
    .byte 025, 045, 055, 0 ;# x . *
numb0: .long nul0, minus, alphn, octal
    .byte 043,  5 , 016, 0 ;# - a f
numb1: .long number0, xn, endn, number0
    .byte 025, 045,  0 , 0 ;# x .

board: .long alpha-4
.ifdef CM2001
shift: .long alpha1
base: .long 10
current: .long decimal
keyc: .long yellow
chars: .long 5
aword: .long ex1
anumber: .long nul
words: .long 0
.else
shift: .long alpha0
base: .long 10
current: .long decimal
keyc: .long yellow
chars: .long 1
aword: .long ex1
anumber: .long nul
words: .long 1
.endif

nul0: drop
    jmp  0f
accept:
acceptn: mov dword ptr  shift, offset alpha0
    lea  edi, alpha-4
accept1: mov  board, edi
0:  call key
    cmp  al, 4
    jns  first
    mov  edx, shift
    jmp  dword ptr [edx+eax*4]

bits: ;# number of bits available in word for packing more Huffman codes
.ifdef CM2001
   .byte 7 ;# matches CM2001 color.com binary
.else
   .byte 28
.endif
/* for reference, Huffman codes are in groups of 8, the prefixes being:
  0xxx, 10xxx, 1100xxx, 1101xxx, 1110xxx, 1111xxx
  "pack" packs one letter (character code at a time, 
  into a Huffman-coded word at [ESI] (stack item) */
0:  add  eax, 0b01010000 ;# make 0b00010000 into 0b01100000, high 2 bits set
    mov  cl, 7 ;# this is a 7-bit Huffman code
    jmp  0f ;# continue below
pack: cmp  al, 0b00010000 ;# character code greater than 16?
    jnc  0b ;# if so, it's a 7-bitter, see above
    mov  cl, 4 ;# otherwise assume it's a 4-bit code
    test al, 0b00001000 ;# character code more than 7?
    jz   0f ;# if so, it's a 5-bit Huffman code
    inc  ecx ;# make the 4 into a 5
    xor  al, 0b00011000 ;# and change prefix to 10xxx
;# common entry point for 4, 5, and 7-bit Huffman codes
0:  mov  edx, eax ;# copy Huffman code into EDX
    mov  ch, cl ;# copy Huffman-code bitcount into 8-bit CH register
0:  cmp  bits, cl ;# do we have enough bits left in the word?
    jnc  0f  ;# if so, continue on
    shr  al, 1 ;# low bit of character code set?
    jc   full ;# if so, word is full, need to start an extension word instead
    dec  cl  ;# subtract one from bitcount
    jmp  0b ;# keep going; as long as we don't find any set bits, it'll fit
0:  shl  dword ptr [esi], cl ;# shift over just the amount of bits necessary
    xor  [esi], eax ;# 'or' or 'add' would have worked as well (and clearer?)
    sub  bits, cl ;# reduce remaining bitcount by what we just used
    ret

;# left-justification routine packs Huffman codes into the MSBs of the word
lj0: mov  cl, bits ;# bits remaining into CL register
    add  cl, 4 ;# add to that the 4 reserved bits for type tag
    shl  dword ptr [esi], cl ;# shift packed word into MSBs
    ret

;# this is just the high-level entry point to the above routine
lj: call lj0
    drop
    ret

;# the packed word is full, so finish processing
full: call lj0 ;# left-justify the packed word
    inc dword ptr words ;# bump the count
    mov byte ptr bits, 32-4 ;# reset bit count, still saving 4 bits for tag
;# we were processing a character when we found the word full, so add it in
    sub  bits, ch ;# subtract saved bitcount of this Huffman code
    mov  eax, edx ;# restore top-of-stack with partial packed word
    dup_
    ret

x:  call right
    mov  eax, words
    lea  esi, [eax*4+esi]
    drop
    jmp  accept

word_: call right
    mov dword ptr  words, 1
    mov dword ptr  chars, 1
    dup_
    mov  dword ptr [esi], 0
    mov byte ptr bits, 28
word1: call letter
    jns  0f
    mov  edx, shift
    jmp  dword ptr [edx+eax*4]
0:  test al, al
    jz   word0
    dup_
    call echo_
    call pack
    inc dword ptr  chars
word0: drop
    call key
    jmp  word1

decimal: mov dword ptr  base, 10
    mov dword ptr  shift, offset numb0
    mov dword ptr  board, offset numbers-4
    ret

hex: mov dword ptr  base, 16
    mov dword ptr  shift, offset numb0 ;# oct0
    mov dword ptr  board, offset octals-4
    ret

octal: xor dword ptr current, (offset decimal-offset start) ^ (offset hex-offset start)
    xor  byte ptr numb0+18, 041 ^ 016 ;# f vs 9
    call [current]
    jmp  number0

xn: drop
    drop
    jmp  acceptn

digit: .byte 14, 10,  0,  0
    .byte  0,  0, 12,  0,  0,  0, 15,  0
    .byte 13,  0,  0, 11,  0,  0,  0,  0
    .byte  0,  1,  2,  3,  4,  5,  6,  7
    .byte  8,  9
sign: .byte 0
minus:
    mov  sign, al
    jmp  number2

number0: drop
    jmp  number3
number: call [current]
    mov byte ptr sign, 0
    xor  eax, eax
number3: call key
    call letter
    jns  0f
    mov  edx, shift
    jmp  dword ptr [edx+eax*4]
0:  test al, al
    jz   number0
    mov  al, [digit-4+eax]
    test byte ptr sign, 037
    jz   0f
    neg  eax
0:  mov  edx, [esi]
    imul edx, base
    add  edx, eax
0:  mov  [esi], edx
number2: drop
    mov  dword ptr  shift, offset numb1
    jmp  number3

endn: drop
    call [anumber]
    jmp  acceptn

alphn: drop
alph0: mov dword ptr  shift, offset alpha0
    lea  edi, alpha-4
    jmp  0f
star0: mov dword ptr  shift, offset graph0
    lea  edi, graphics-4
0:  drop
    jmp  accept1

alph: mov dword ptr  shift, offset alpha1
    /* variable 'board' holds a pointer to the keyboard currently in use:
    /* alphabetic, numeric, graphic, etc.
    /* since valid key codes start at 1, subtract length of 1 address
    /* (4 bytes) from the start of the table into which we index */
    lea  edi, alpha-4
    jmp  0f
graph: mov dword ptr shift, offset graph1
    lea  edi, graphics-4
0:  mov  board, edi
    jmp  word0

first: add dword ptr shift, 4*4+4
    call word_
    call [aword]
    jmp  accept

hicon: .byte 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f
    .byte 0x20, 0x21,  5 , 0x13, 0xa, 0x10,  4 , 0xe
edig1: dup_
edig: push ecx
    mov  al, hicon[eax]
    call emit
    pop  ecx
    ret

odig: rol  eax, 4
    dup_
    and  eax, 0x0f
    ret

hdotn: mov  edx, eax
    neg  eax
    lea  ecx, [32+eax*4]
    drop
    rol  eax, cl
    mov  ecx, edx
    jmp  0f
hdot: mov  ecx, 8
0:  call odig
    call edig
    next 0b
    drop
    ret

/* '.', 'dot' is the original Forth 'word' for
/* displaying a number off the stack */
dot: mov  ecx, 7
0:  call odig
    jnz  @h
    drop
    next 0b
    inc  ecx
0:  call odig
@h1: call edig
    next 0b
    call space
    drop
    ret
@h: inc  ecx
    jmp  @h1

qdot: cmp dword ptr  base, 10
    jnz  dot
dot10: mov  edx, eax
    test edx, edx
    jns  0f
    neg  edx
    dup_
    mov  eax, 043
    call emit
0:  mov  ecx, 8
0:  mov  eax, edx
    xor  edx, edx
    div dword ptr  tens[ecx*4]
    test eax, eax
    jnz  d_1
    dec  ecx
    jns  0b
    jmp  d_2
0:  mov  eax, edx
    xor  edx, edx
    div dword ptr  tens[ecx*4]
d_1: call edig1
    dec  ecx
    jns  0b
d_2: mov  eax, edx
    call edig1
    call space ;# spcr
    drop
    ret

/* unpack the Huffman-coded characters in a 32-bit word */
unpack: dup_
    test eax, eax
    js   0f
    shl  dword ptr [esi], 4
    rol  eax, 4
    and  eax, 7
    ret
0:  shl  eax, 1
    js   0f
    shl  dword ptr [esi], 5
    rol  eax, 4
    and  eax, 7
    xor  al, 0x8
    ret
0:  shl  dword ptr [esi], 7
    rol  eax, 6
    and  eax, 0x3f
    sub  al, 0x10
    ret

qring: dup_
    inc  dword ptr [esi]
    cmp  curs, edi ;# from abort, insert
    jnz  0f
    mov  curs, eax
0:  cmp  eax, curs
    jz   ring
    jns  0f
    mov  pcad, edi
0:  drop
    ret

ring: ;# show "pacman" cursor at current source word (pointed to by EDI)
    mov  cad, edi ;# save as current cursor address
    sub dword ptr xy, iw*0x10000 ;# backspace one icon width
    dup_
    mov  eax, 0x0e04000 ;# ochre-colored cursor
    call color
    mov  eax, 060 ;# cursor character (shift-space, so to speak, in this code)
    mov  cx, word ptr xy+2
    cmp  cx, word ptr rm
    js   0f
    call emit
    sub dword ptr xy, iw*0x10000 ;# bksp
    ret
0:  jmp  emit

rw: ;# display red word (definition)
    mov  cx, word ptr xy+2 ;# get current X position
    cmp  cx, word ptr lm ;# at left margin?
    jz   0f ;# skip if so
    call cr ;# insert newline before definition
0:  call red ;# set text color to red
    jmp  type_ ;# type the word

gw: ;# display green word
    call green
    jmp  type_

mw: ;# display deferred macro word
    call cyan
    jmp  type_

ww: ;# display "white", executable word (actually yellow)
    ;# my (jc's) guess is that "white" was originally the color, and
    ;# it got changed to yellow when CM decided to use white for comments
    dup_
    mov  eax, yellow
    call color
    jmp  type_

type0: ;# display continuation of previous word
    sub  dword ptr xy, iw*0x10000 ;# call bspcr
    test dword ptr [-4+edi*4], 0xfffffff0 ;# valid packed word?
    jnz  type1 ;# display it if so...
    dec  edi ;# otherwise we've gone past the end of the source, go back one
    mov  lcad, edi ;# "last" cursor address is the last source word of screen
    call space
    call qring
    pop  edx ;# end of block
    drop
    jmp  keyboard

cap: ;# display a capitalized comment word
    call white
    dup_
    mov  eax, [-4+edi*4]
    and  eax, 0xfffffff0 ;# mask out tag bits
    call unpack
    add  al, 48
    call emit
    jmp  type2

caps: ;# display an all-caps comment word
    call white
    dup_
    mov  eax, [-4+edi*4]
    and  eax, 0xfffffff0 ;# mask out tag bits
0:  call unpack
    jz   0f ;# space if it unpacked to nothing
    add  al, 48
    call emit
    jmp  0b

text: call white
type_:
type1: dup_
    mov  eax, [-4+edi*4]
    and  eax, 0xfffffff0 ;# mask out tag bits
type2: call unpack
    jz   0f
    call emit
    jmp  type2
0:  call space
    drop
    drop
    ret

gsw: ;# display a green (compiled) short (27 bits) word
    mov  edx, [-4+edi*4]
    sar  edx, 5 ;# shift into position
    jmp  gnw1

var: ;# within editor, display a variable name
    call magenta
    call type_
;# fall through to next routine to display its value
;# green (compiled) normal (32 bits) word
gnw: mov  edx, [edi*4]
    inc  edi
gnw1: dup_
    mov  eax, 0x0f800 ;# green
    cmp dword ptr bas, offset dot10 ;# is it base 10?
    jz   0f ;# bright green if so
    mov  eax, 0x0c000 ;# else dark green
    jmp  0f

;# short (27 bits) yellow (executable) word
sw: mov  edx, [-4+edi*4]
    sar  edx, 5 ;# shift into position
    jmp  nw1

;# normal (32 bits) yellow (executable) word
nw: mov  edx, [edi*4]
    inc  edi
nw1: dup_
    mov  eax, yellow
    cmp dword ptr bas, offset dot10 ;# is it base 10?
    jz   0f ;# bright yellow if so
    mov  eax, 0x0c0c000 ;# else dark yellow
0:  call color
    dup_
    mov  eax, edx
    jmp  [bas]

refresh: call show
    call blank
    call text1
    dup_            ;# counter
    mov  eax, lcad ;# pointer to end of screen source
    mov  cad, eax ;# for cursor beyond end
    xor  eax, eax
    mov  edi, blk
    shl  edi, 10-2
    mov  pcad, edi ;# for curs=0
ref1: test dword ptr [edi*4], 0x0f
    jz   0f
    call qring
0:  mov  edx, [edi*4]
    inc  edi
    mov dword ptr bas, offset dot10
    test dl, 020
    jz   0f
    mov dword ptr bas, offset dot
0:  and  edx, 017 ;# get typetag bits as index into display vector
    call display[edx*4] ;# call the appropriate display routine
    jmp  ref1 ;# loop

.align 4
display: .long type0, ww, nw, rw
    .long gw, gnw, gsw, mw
    .long sw, text, cap, caps
    .long var, nul, nul, nul
tens: .long 10, 100, 1000, 10000, 100000, 1000000
    .long 10000000, 100000000, 1000000000
bas: .long dot10
blk: .long 18
.ifdef CM2001 ;# match CM2001 color.com binary
curs: .long 2
cad: .long 0x1204
pcad: .long 0x1202
lcad: .long 0x122c
trash: .long buffer*4+12
.else
curs: .long 0
cad: .long 0 ;# cursor (ring) address
pcad: .long 0 ;# "previous" cursor address
lcad: .long 0 ;# "last" cursor address
trash: .long buffer*4
.endif

/* the editor keys and their actions */
ekeys:
;# delete (cut), exit editor, insert (paste)
    .long nul, del, eout, destack ;# n<space><alt>
;# white (yellow), red, green, to shadow block
    .long act1, act3, act4, shadow ;# uiop=yrg*
;# left, up, down, right
    .long mcur, mmcur, ppcur, pcur ;# jkl;=ludr
;# previous block, magenta (variable), cyan (macro), next block
    .long mblk, actv, act7, pblk ;# m,./=-mc+
;# white (comment) all caps, white (comment) capitalized, white (comment)
    .long nul, act11, act10, act9 ;# wer=SCt
    .long nul, nul, nul, nul ;# df=fj (find and jump) not in this version
;# these are the huffman encodings of the characters to display
ekbd0: .long nul, nul, nul, nul
    .byte 21, 37,  7,  0  ;# x  .  i
ekbd:
    .byte 15,  1, 13, 45  ;# w  r  g  *
    .byte 12, 22, 16,  1  ;# l  u  d  r
    .byte 35,  9, 10, 43  ;# -  m  c  +
    .byte  0, 56, 58,  2  ;#    S  C  t
    .byte  0,  0,  0,  0
    .byte  0,  0,  0,  0
;# 1-based array of colors used for various action modes
actc:
    .long yellow, 0, 0x0ff0000, 0x0c000 ;# 1=yellow, 2=none, 3=red, 4=green
    .long 0, 0, 0x0ffff, 0 ;# 7=cyan
    .long 0x0ffffff, 0x0ffffff, 0x0ffffff, 0x8080ff ;# 9-11=white, 12=magenta
vector: .long 0
.ifdef CM2001
action: .byte 10 ;# matches CM2001 color.com binary
.else
action: .byte 1
.endif

act1: mov  al, 1 ;# word execute, yellow
    jmp  0f
act3: mov  al, 3 ;# word define, red
    jmp  0f
act4: mov  al, 4 ;# word compile, green
    jmp  0f
act9: mov  al, 9 ;# word comment, white
    jmp  0f
act10: mov  al, 10 ;# word comment, white Capitalized
    jmp  0f
act11: mov  al, 11 ;# word comment, ALL CAPS
    jmp  0f
act7: mov  al, 7 ;# macro compile, cyan
0:  mov  action, al ;# number of action
    mov  eax, [actc-4+eax*4] ;# load color corresponding to action
    mov dword ptr aword, offset insert ;# "insert" becomes the active word
;# action for number
actn: mov  keyc, eax
    pop  eax
    drop
    jmp  accept
;# after 'm' pressed, change color and prepare to store variable name
actv: mov byte ptr action, 12 ;# variable
    mov  eax, 0x0ff00ff ;# magenta
    mov dword ptr aword, offset 0f
    jmp  actn
;# this is the action performed after the variable name is entered
0:  dup_ ;# save EAX (packed word) on stack
    xor  eax, eax ;# zero out EAX
    inc  dword ptr words ;# add one to count of words
    jmp  insert ;# insert new word into dictionary

mcur: ;# minus cursor
    dec dword ptr curs ;# move left
    jns  0f  ;# just return if it didn't go negative, otherwise undo it...
pcur: ;# plus cursor
    inc dword ptr curs ;# move right
0:  ret

mmcur: ;# move up one "row"...
    sub dword ptr curs, 8 ;# ... actually, only 8 words
    jns  0f  ;# return if it didn't go negative
    mov dword ptr curs, 0  ;# otherwise set to 0
0:  ret
ppcur: ;# move down one "row"...
    add dword ptr curs, 8 ;# ... actually, only 8 words
    ret  ;# guess it's ok to increment beyond end of screen (?)

pblk: add dword ptr  blk, 2 ;# plus one block (+2 since odd are shadows)
    add  dword ptr [esi], 2
    ret
mblk: cmp dword ptr  blk, 20 ;# minus one block unless below 20
    js   0f ;# (18 is first block available for editing)
    sub dword ptr  blk, 2
    sub  dword ptr [esi], 2
0:  ret
;# shadow screens in Forth are documentation for corresponding source screens
shadow: xor dword ptr  blk, 1 ;# switch between shadow and source
    xor  dword ptr [esi], 1 ;# change odd to even and vice versa
    ret

e0: drop
    jmp  0f

/* colorForth editor */
;# when invoked with 'edit', the block number is passed on the stack
edit: mov  blk, eax
    drop
;# when invoked with 'e', uses block number in blk, by default 18
e:  dup_
    mov  eax, blk
    mov dword ptr  anumber, offset format
    mov  byte ptr alpha0+4*4, 045 ;# .
    mov dword ptr alpha0+4, offset e0
    call refresh
0:  mov dword ptr shift, offset ekbd0
    mov dword ptr board, offset ekbd-4
    mov dword ptr keyc, yellow ;# default key color, yellow
;# this is the main loop
0:  call key
    call ekeys[eax*4]
    drop
    jmp  0b

/* exit editor */
eout: pop  eax
    drop
    drop
    mov  dword ptr aword, offset ex1
    mov  dword ptr anumber, offset nul
    mov  byte ptr alpha0+4*4, 0
    mov  dword ptr alpha0+4, offset nul0
    mov  dword ptr keyc, yellow ;# restore key color to yellow
    jmp  accept ;# revert to command-line processing

/* insert, or paste */
destack: mov  edx, trash ;# grab what was left by last "cut" operation
    cmp  edx, buffer*4 ;# anything in there?
    jnz  0f ;# continue if so...
    ret  ;# otherwise, 'insert' is already the default action so nothing to do
0:  sub  edx, 2*4
    mov  ecx, [edx+1*4]
    mov  words, ecx
0:  dup_
    mov  eax, [edx]
    sub  edx, 1*4
    next 0b
    add  edx, 1*4
    mov  trash, edx

insert0: mov  ecx, lcad  ;# room available? get pointer to last source word
    add  ecx, words ;# add to that the number of 32-bit words to be inserted
    xor  ecx, lcad
    and  ecx, -0x100
    jz   insert1
    mov  ecx, words ;# no
0:  drop
    next 0b
    ret
insert1: push esi
    mov  esi, lcad
    mov  ecx, esi
    dec  esi
    mov  edi, esi
    add  edi, words
    shl  edi, 2
    sub  ecx, cad
    js   0f
    shl  esi, 2
    std
    rep movsd
    cld
0:  pop  esi
    shr  edi, 2
    inc  edi
    mov  curs, edi ;# like abort
    mov  ecx, words
0:  dec  edi
    mov  [edi*4], eax
    drop ;# requires cld (see above)
    next 0b
    ret

insert: call insert0
    mov  cl, action
    xor  [edi*4], cl
    jmp  accept

format: test byte ptr action, 012 ;# ignore 3 and 9
    jz   0f
    drop
    ret
0:  mov  edx, eax
    and  edx, 0x0fc000000
    jz   0f
    cmp  edx, 0x0fc000000
    jnz  format2
0:  shl  eax, 5
    xor  al, 2 ;# 6
    cmp  byte ptr  action, 4
    jz   0f
    xor  al, 013 ;# 8
0:  cmp  dword ptr base, 10 ;# base 10?
    jz   0f ;# continue if so...
    xor  al, 0x10 ;# otherwise remove 'hex' bit
0:  mov  dword ptr words, 1
    jmp  insert

format2: dup_
    mov  eax, 1 ;# 5
    cmp  byte ptr  action, 4
    jz   0f
    mov  al, 3 ;# 2
0:  cmp  dword ptr base, 10 ;# base 10?
    jz   0f ;# continue if so...
    xor  al, 0x10 ;# otherwise remove 'hex' bit
0:  xchg eax, [esi]
    mov  dword ptr words, 2
    jmp  insert

del: ;# delete, or cut, current word in editor (to the left of pacman cursor)
    call enstack ;# copy word(s) to be deleted into "trash" buffer
    mov  edi, pcad
    mov  ecx, lcad
    sub  ecx, edi
    shl  edi, 2
    push esi
    mov  esi, cad
    shl  esi, 2
    rep  movsd
    pop  esi
    jmp  mcur

enstack: ;# copy source into "trash" buffer
    dup_ ;# just to save TOS, we don't actually return anything
    mov  eax, cad ;# get current cursor address
    sub  eax, pcad ;# has it changed since previous cursor address?
    jz   ens ;# skip if not; nothing to do
    mov  ecx, eax ;# otherwise, get word count of items to buffer
    xchg eax, edx ;# save the count in EDX
    push esi ;# save data stack pointer, we need it for "string" operations
    mov  esi, cad ;# source address is current cursor address...
    lea  esi, [esi*4-4] ;# convert to a byte address, point to start of word
    mov  edi, trash ;# destination address is "trash" buffer
0:  std  ;# make string addresses work "backwards" in RAM
    lodsd ;# grab a 32-bit word
    cld  ;# now store "forwards" in "trash" buffer
    stosd ;# store a 32-bit word
    next 0b ;# loop for word count
    xchg eax, edx ;# get the count back...
    stosd ;# store it, too, at the end of the trash buffer
    mov  trash, edi ;# update the pointer
    pop  esi ;# restore data stack pointer
ens: drop ;# restore TOS
    ret

;# 'pad' is called by a high-level program to define the keypad, followed by
;# 28 high-level compiled words that define the key vectors (actions), again
;# followed by the Huffman codes for the characters representing the keys
pad: ;# define keypad actions and representations
    pop  edx  ;# keypad data must immediately follow call...
    ;# there are 28 keys total, 12 on each side plus undefined keys (code 0),
    ;# "n", "space" and alt
    ;# we're popping the "return" address which is really the address of
    ;# the vector table
    mov  vector, edx ;# pointer to words for each possible keyhit
    add  edx, 28*5 ;# 5 bytes to each "call" instruction
    ;# just past that is character data, one byte for the Huffman code of each
    ;# character that the physical keyboard represents ("r"=1, "t"=2, etc.)
    ;# but remember that "board" contains 4 less than that address, which means
    ;# that the first 4 must be those for keycode 0 and the "shift" keys
    mov  board, edx
    ;# now, the following makes no sense... subtracting 4*4 from an array of
    ;# 5-byte entries should point in the middle of an instruction (which
    ;# indeed it does). but the "shift" table is ordinarily an array of 4
    ;# longwords followed by the 4 character codes, so there's a method to
    ;# CM's madness
    ;# the 'keyboard' routine uses this address only for the 4 character codes
    ;# at the end of the table
    sub  edx, 4*4  ;# simulate the 4 longwords, can't really use them
    mov  shift, edx ;# this is only to point to the character codes
0:  call key  ;# wait for a keyhit and return the code
    mov  edx, vector ;# load vector table into EDX
    ;# the following 3 instructions point to the appropriate vector for the key
    ;# it amounts to adding eax*5 to the start of the vector table
    add  edx, eax ;# add keyvalue once
    lea  edx, [5+eax*4+edx] ;# add keyvalue 4 more times for total of 5...
    ;# plus an extra 5 bytes, which we explain next...
    ;# remember that a "call" instruction is e8xxxxxxxx, where the xxxxxxxx is
    ;# the offset to the address from the _end_ of the current instruction.
    ;# since by adding that extra 5 bytes we _are_ pointing to the end, adding
    ;# the 4-byte offset just preceding our address should point us to the
    ;# routine to be called. why not just push the address of the "jmp 0b"
    ;# below, then "jmp" to the address of the call instead?
    ;# guess it wouldn't be any clearer.
    add  edx, [-4+edx] ;# point to the address of the routine to be called
    drop  ;# restore EAX from the data stack
    call edx ;# call the routine corresponding to keyhit
    jmp  0b  ;# loop until "accept" code reached, which exits program

.ifdef OBSOLETE_CM2001
 .org (0x1200-1)*4
    .long 0
.else
 .include "chars.asm"
.endif
.ifdef I_HAVE_AT_LEAST_1GB_RAM
 .incbin "newcode.dat"
.else
 .incbin "color.dat"
.endif
.ifdef MANDELBROT
 .incbin "new_mandelbrot.blk"
.endif
.end start
