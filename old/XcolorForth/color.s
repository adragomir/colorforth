#colorForth, 2001 Jul 22, Chuck Moore, Public Domain

.macro next adr
	decl %ecx
	jnz \adr
.endm

.macro DUP_ 
	leal -4(%esi), %esi
	movl %eax, (%esi)
.endm

.macro DROP 
	lodsl 
.endm

#.equ hp, 800
#.equ vp, 600
.equ hp, 1024 
.equ vp, 768 
.equ buffer, 604 * 256 

#   100000 dictionary
#    a0000 top of return stack
#    9f800 top of data stack
#    9d800 free
#    97000 floppy buffer
#     4800 source
.equ icons, 12 * 256 * 4 # 3000
#     7c00 BIOS boot sector
#        0 Forth

.text	
.global _start
_start: pop %eax
	pop frame
	pop sdl_swap
	pop sdl_key
	movl $_start, %ebp
	movl %esp, gods
	lea godd(%ebp), %esi
	movl $_start + 0x40000 * 4, h
	movl $_start + buffer * 4, trash
	call show0
	movl $(forth1 - forth0) / 4, forths
	movl $(macro1 - macro0) / 4, macros
	movl $18, %eax
	call load
	jmp accept

.equ godd, (0x28000 * 4) - 750 * 4 
.equ mains, godd - 1500 * 4 
.equ maind, mains - 750 * 4 
.balign 4
gods:	.int 0
sdl_swap: .int 0
sdl_key: .int 0
me: 	.int god
screen: .int 0# logo

round:	call unpause
god: 	.int 0# Gods-2*4
	call unpause
main: 	.int 0# mains-2*4
	jmp round

pause:	DUP_ 
	pushl %esi
	movl me, %eax
	movl %esp, (%eax)
	addl $4, %eax
	jmpl *%eax

unpause:	popl %eax
	movl (%eax), %esp
	movl %eax, me
	popl %esi
	DROP 
	ret 

act:	movl $_start + (maind - 4), %edx
	movl %eax, (%edx)
	movl $_start + (mains - 4), %eax
	pop (%eax)
	subl $4, %eax
	movl %edx, (%eax)
	movl %eax, main
	DROP 
	ret 

show0:	call show
	ret 
show:	popl screen
	DUP_ 
	xorl %eax, %eax
	call act
0:	call graphic
	calll *screen(, 1)
	call switch
	incl %eax
	jmp 0b

c_:	movl $_start + (godd + 4), %esi
	ret 

mark:	movl macros, %ecx
	movl %ecx, mk
	movl forths, %ecx
	movl %ecx, mk + 4
	movl h, %ecx
	movl %ecx, mk + 2 * 4
	ret 

empty:	movl mk + 2 * 4, %ecx
	movl %ecx, h
	movl mk + 4, %ecx
	movl %ecx, forths
	movl mk, %ecx
	movl %ecx, macros
	movl $0, class
	ret 

mfind:	movl macros, %ecx
	pushl %edi
	leal (macro0 - 4)(, %ecx, 4), %edi
	jmp 0f

find:	movl forths, %ecx
	pushl %edi
	leal (forth0 - 4)(, %ecx, 4), %edi
0:	std 
	repne
	scasl 
	cld 
	popl %edi
	ret 

ex1:	decl words# from keyboard
	jz 0f
	DROP 
	jmp ex1
0:	call find
	jnz abort1
	DROP 
	jmpl *forth2(, %ecx, 4)

execute:movl $alit, lit
	DUP_ 
	movl -4(%ebp, %edi, 4), %eax
ex2:	andl $-020, %eax
	call find
	jnz abort
	DROP 
	jmpl *forth2(, %ecx, 4)

abort:	movl %edi, curs
	shrl $10 - 2, %edi
	movl %edi, blk
abort1:	movl gods, %esp
	movl $forthd, spaces + 3 * 4
	movl $qcompile, spaces + 4 * 4
	movl $cnum, spaces + 5 * 4
	movl $cshort, spaces + 6 * 4
	movl $057, %eax# ?
	call echo_
	jmp accept

sdefine:	popl adefine
	ret 
macro_:	call sdefine
macrod:	movl macros, %ecx
	incl macros
	leal macro0(, %ecx, 4), %ecx
	jmp 0f

forth:	call sdefine
forthd:	movl forths, %ecx
	incl forths
	leal forth0(, %ecx, 4), %ecx
0:	movl -4(%ebp, %edi, 4), %edx
	andl $-020, %edx
	movl %edx, (%ecx)
	movl h, %edx
	movl %edx, (forth2 - forth0)(%ecx)
	leal (forth2 - forth0)(%ecx), %edx
	shrl $2, %edx
	movl %edx, last
	movl %esp, list
	movl $adup, lit
	testl $-1, class
	jz 0f
	jmpl *class(, 1)
0:	ret 

cdrop:	movl h, %edx
	movl %edx, list
	movb $0x0ad, (%edx)# lodsd
	incl h
	ret 

qdup:	movl h, %edx
	decl %edx
	cmpl %edx, list
	jnz cdup
	cmpb $0x0ad, (%edx)
	jnz cdup
	movl %edx, h
	ret 
cdup:	movl h, %edx
	movl $0x89fc768d, (%edx)
	movb $06, 4(%edx)
	addl $5, h
	ret 

adup:	DUP_ 
	ret 

var1:	DUP_ 
	movl (4 + forth0)(, %ecx, 4), %eax
	ret 
variable:call forthd
	movl $var1, (forth2 - forth0)(%ecx)
	incl forths# dummy entry for source address
	lea (%ebp, %edi, 4), %ebx
	shr $2, %ebx
	movl %ebx, 4(%ecx)
	call macrod
	movl $0f, (forth2 - forth0)(%ecx)
	incl macros
	movl %ebx, 4(%ecx)
	incl %edi
	ret 
0:	calll *lit(, 1)
	movl (4 + macro0)(, %ecx, 4), %eax
	jmp 0f

cnum:	calll *lit(, 1)
	movl (%ebp, %edi, 4), %eax
	incl %edi
	jmp 0f

cshort:	calll *lit(, 1)
	movl -4(%ebp, %edi, 4), %eax
	sarl $5, %eax
0:	call literal
	DROP 
	ret 

alit:	movl $adup, lit
literal:call qdup
	movl list, %edx
	movl %edx, list + 4
	movl h, %edx
	movl %edx, list
	movb $0x0b8, (%edx)
	movl %eax, 1(%edx)
	addl $5, h
	ret 

qcompile:calll *lit(, 1)
	movl -4(%ebp, %edi, 4), %eax
	andl $-020, %eax
	call mfind
	jnz 0f
	DROP 
	jmpl *macro2(, %ecx, 4)
0:	call find
	movl forth2(, %ecx, 4), %eax
0:	jnz abort
call_:	movl h, %edx
	movl %edx, list
	movb $0x0e8, (%edx)
	addl $5, %edx
	subl %edx, %eax
	movl %eax, -4(%edx)
	movl %edx, h
	DROP 
	ret 

compile:calll *lit(, 1)
	movl -4(%ebp, %edi, 4), %eax
	andl $-020, %eax
	call mfind
	movl macro2(, %ecx, 4), %eax
	jmp 0b

short_:	movl $alit, lit
	DUP_ 
	movl -4(%ebp, %edi, 4), %eax
	sarl $5, %eax
	ret 

num:	movl $alit, lit
	DUP_ 
	movl (%ebp, %edi, 4), %eax
	incl %edi
	ret 

comma:	movl $4, %ecx
0:	movl h, %edx
	movl %eax, (%edx)
	movl (%esi), %eax# drop
	leal (%edx, %ecx), %edx
	leal 4(%esi), %esi
	movl %edx, h
#    DROP
	ret 

comma1:	movl $1, %ecx
	jmp 0b

comma2:	movl $2, %ecx
	jmp 0b

comma3:	movl $3, %ecx
	jmp 0b

semi:	movl h, %edx
	subl $5, %edx
	cmpl %edx, list
	jnz 0f
	cmpb $0x0e8, (%edx)
	jnz 0f
	incb (%edx)# jmp
	ret 
0:	movb $0x0c3, 5(%edx)# ret
	incl h
	ret 

then:	movl %esp, list
	movl h, %edx
	subl %eax, %edx
	movb %dl, -1(%eax)
	DROP 
	ret 

begin:	movl %esp, list
here:	DUP_ 
	movl h, %eax
	ret 

qlit:	movl h, %edx
	leal -5(%edx), %edx
	cmpl %edx, list
	jnz 0f
	cmpb $0x0b8, (%edx)
	jnz 0f
	DUP_ 
	movl list + 4, %eax
	movl %eax, list
	movl 1(%edx), %eax
	cmpl $0x89fc768d, -5(%edx)# dup
	jz q1
	movl %edx, h
	jmp cdrop
q1:	addl $-10, h# flag nz
	ret 
0:	xorl %edx, %edx# flag z
	ret 

less:	cmpl %eax, (%esi)
	jl 0f# flag nz
	xorl %ecx, %ecx# flag z
0:	ret 

qignore:	testl $-020, -4(%ebp, %edi, 4)
	jnz nul
	popl %edi
	popl %edi
nul:	ret 

jump:	popl %edx
	addl %eax, %edx
	leal 5(%edx, %eax, 4), %edx
	addl -4(%edx), %edx
	DROP 
	jmpl *%edx

load:	shll $10 - 2, %eax
	pushl %edi
	movl %eax, %edi
	DROP 
inter:	movl (%ebp, %edi, 4), %edx
	incl %edi
	andl $017, %edx
	calll *spaces(, %edx, 4)
	jmp inter

.balign 4
spaces: 	.int qignore, execute, num
adefine: 	.int 5 + macro_# offset macrod ?
	.int qcompile, cnum, cshort, compile
	.int short_, nul, nul, nul
	.int variable, nul, nul, nul

lit: 	.int adup
mk: 	.int 0, 0, 0
h: 	.int 0x40000 * 4
last: 	.int 0
class: 	.int 0
list: 	.int 0, 0
macros: .int 0
forths: .int 0
#macro0 dd (3 shl 4+1)shl 24 ; or
#       dd ((5 shl 4+6)shl 7+140o)shl 17 ; and
#       dd 173o shl 25 ; +
macro0: 	.int 0170 << 25# ;
	.int ((0140 << 7 + 0146) << 7 + 0142) << 11# dup
	.int (((0177 << 7 + 0140) << 7 + 0146) << 7 + 0142) << 4# ?dup
	.int (((0140 << 4 + 1) << 4 + 3) << 7 + 0142) << 10# drop
#       dd ((6 shl 4+7)shl 7+142o)shl 17 ; nip
	.int (((2 << 7 + 0144) << 4 + 4) << 4 + 6) << 13# then
	.int ((((0143 << 4 + 4) << 5 + 025) << 4 + 7) << 4 + 6) << 8# begin
macro1: 	.rept 128
	.int 0
	.endr
forth0: .int (((0143 << 4 + 3) << 4 + 3) << 4 + 2) << 13# boot
	.int 0x82e14000 #save
	.int 0xb0ac5000 #frame
	.int ((((0142 << 4 + 5) << 7 + 0146) << 5 + 020) << 4 + 4) << 5# pause
	.int ((((021 << 4 + 5) << 5 + 022) << 4 + 1) << 4 + 3) << 10# MACRO
	.int ((((026 << 4 + 3) << 4 + 1) << 4 + 2) << 7 + 0144) << 8# FORTH
	.int 022 << 27# c
	.int (6 << 5 + 022) << 23# nc
	.int ((5 << 5 + 022) << 4 + 2) << 19# ACT
	.int (((020 << 7 + 0144) << 4 + 3) << 5 + 027) << 11# SHOW
	.int (((024 << 4 + 3) << 4 + 5) << 7 + 0140) << 12# LOAD
	.int (((0144 << 4 + 4) << 4 + 1) << 4 + 4) << 13# here
	.int (((0177 << 5 + 024) << 4 + 7) << 4 + 2) << 12# ?lit
	.int (0153 << 7 + 0176) << 18# 3,
	.int (0152 << 7 + 0176) << 18# 2,
	.int (0151 << 7 + 0176) << 18# 1,
	.int 0176 << 25# ,
	.int (((024 << 4 + 4) << 5 + 020) << 5 + 020) << 13# less
	.int (((0162 << 7 + 0146) << 5 + 021) << 7 + 0142) << 6# jump
	.int (((((5 << 5 + 022) << 5 + 022) << 4 + 4) << 7 + 0142) << 4 + 2) << 3# accept
	.int ((0142 << 4 + 5) << 7 + 0140) << 14# pad
	.int ((((4 << 4 + 1) << 4 + 5) << 5 + 020) << 4 + 4) << 11# erase
	.int (((022 << 4 + 3) << 7 + 0142) << 5 + 023) << 11# copy
	.int (((021 << 4 + 5) << 4 + 1) << 7 + 0164) << 12# mark
	.int (((4 << 5 + 021) << 7 + 0142) << 4 + 2) << 12# empt
	.int (((4 << 5 + 021) << 4 + 7) << 4 + 2) << 15# emit
	.int ((((0140 << 4 + 7) << 5 + 025) << 4 + 7) << 4 + 2) << 8# digit
	.int ((((0152 << 4 + 4) << 5 + 021) << 4 + 7) << 4 + 2) << 8# 2emit
	.int 0165 << 25# .
	.int (0144 << 7 + 0165) << 18# h.
	.int ((0144 << 7 + 0165) << 4 + 6) << 14# h.n
	.int (022 << 4 + 1) << 23# CR
	.int ((((020 << 7 + 0142) << 4 + 5) << 5 + 022) << 4 + 4) << 7# space
	.int (((0140 << 4 + 3) << 5 + 027) << 4 + 6) << 12# DOWN
	.int (((4 << 7 + 0140) << 4 + 7) << 4 + 2) << 13# edit
	.int 4 << 28# E
#       dd (((26o shl 4+3)shl 4+6)shl 4+2)shl 15 ; font
	.int (024 << 5 + 021) << 22# lm
	.int (1 << 5 + 021) << 23# rm
	.int ((((025 << 4 + 1) << 4 + 5) << 7 + 0142) << 7 + 0144) << 5# graph ic
	.int (((2 << 4 + 4) << 7 + 0145) << 4 + 2) << 13# text
#       dd (153o shl 7+140o)shl 18 ; 3d
#       dd (((((1 shl 4+4)shl 4+6)shl 7+140o)shl 4+4)shl 4+1)shl 5 ; render
#       dd ((((141o shl 4+4)shl 4+1)shl 4+2)shl 4+4)shl 9 ; verte x
#       dd ((((26o shl 4+1)shl 4+3)shl 4+6)shl 4+2)shl 11 ; front
#       dd ((2 shl 4+3)shl 7+142o)shl 17 ; top
#       dd (((20o shl 4+7)shl 7+140o)shl 4+4)shl 12 ; side
	.int ((((0164 << 4 + 4) << 5 + 023) << 7 + 0143) << 4 + 3) << 5# keybo ard
	.int (((0140 << 4 + 4) << 7 + 0143) << 7 + 0146) << 7# debu g
	.int (5 << 4 + 2) << 24# at
	.int ((0173 << 4 + 5) << 4 + 2) << 17# +at
	.int (0145 << 5 + 023) << 20# xy
	.int ((026 << 4 + 3) << 7 + 0141) << 16# fov
	.int (((026 << 4 + 7) << 5 + 026) << 4 + 3) << 14# fifo
	.int ((0143 << 4 + 3) << 7 + 0145) << 14# box
	.int (((024 << 4 + 7) << 4 + 6) << 4 + 4) << 15# line
	.int ((((022 << 4 + 3) << 5 + 024) << 4 + 3) << 4 + 1) << 10# color
#       dd (((22o shl 5+24o)shl 4+7)shl 7+142o)shl 11 ; clip
	.int (((((3 << 5 + 022) << 4 + 2) << 4 + 5) << 4 + 6) << 4 + 2) << 7# octant
	.int (020 << 7 + 0142) << 20# sp
	.int (((024 << 4 + 5) << 5 + 020) << 4 + 2) << 14# last
	.int (((((0146 << 4 + 6) << 7 + 0142) << 4 + 5) << 5 + 022)) << 5# unpac k
#       dd (((142o shl 4+5)shl 5+22o)shl 7+164o)shl 9 ; pack
forth1: 	.rept 512
	.int 0
	.endr
#macro2 dd offset cOR
#       dd offset cAND
#       dd offset PLUS
macro2: 	.int semi
	.int cdup
	.int qdup
	.int cdrop
#       dd offset nip
	.int then
	.int begin
	.rept 128
	.int 0
	.endr
forth2: .int boot
	.int save
	.int frame_
	.int pause
	.int macro_
	.int forth
	.int c_
	.int nc_
	.int act
	.int show
	.int load
	.int here
	.int qlit
	.int comma3
	.int comma2
	.int comma1
	.int comma
	.int less
	.int jump
	.int accept
	.int pad
	.int erase
	.int copy
	.int mark
	.int empty
	.int emit
	.int edig
	.int emit2
	.int dot10
	.int hdot
	.int hdotn
	.int cr
	.int space
	.int down
	.int edit
	.int e
#       dd offset font
	.int lms
	.int rms
	.int graphic
	.int text1
#       dd offset set3d
#       dd offset render
#       dd offset vertex
#       dd offset front
#       dd offset top_
#       dd offset side
	.int keyboard
	.int debug
	.int at
	.int pat
	.int xy_
	.int fov_
	.int fifof
	.int box
	.int line
	.int color
#       dd offset clip
	.int octant
	.int sps
	.int last_
	.int unpack
#       dd offset pack
	.rept 512
	.int 0
	.endr

boot:	movl	$0,%ebx		# first argument: exit code
	movl	$1,%eax		# system call number (sys_exit)
	int	$0x80		# call kernel

file:	.asciz "color.com"
save:	DUP_
	movl $5, %eax
	movl $file, %ebx
	movl $1, %ecx
	int	$0x80 #open
	push %eax
	movl %eax, %ebx
	movl $4, %eax
	movl %ebp, %ecx
	movl nc, %edx
	imull $18*2*512, %edx
	int	$0x80 #write
	movl $6, %eax
	popl %ebx
	int	$0x80 #close
	DROP
	ret	

erase:	movl %eax, %ecx
	shll $8, %ecx
	DROP 
	pushl %edi
	movl %eax, %edi
	shll $2 + 8, %edi
	addl %ebp, %edi
	xorl %eax, %eax
	rep
	stosl 
	popl %edi
	DROP 
	ret 

#move: mov  ECX, EAX
#    DROP
#    mov  EDI, EAX
#    shl  EDI, 2
#    DROP
#    push ESI
#     mov  ESI, EAX
#     shl  ESI, 2
#     rep movsd
#    pop  ESI
#    DROP
#    ret

copy:	cmpl $12, %eax
	jc abort1
	movl %eax, %edi
	shll $2 + 8, %edi
	addl %ebp, %edi
	pushl %esi
	movl blk, %esi
	shll $2 + 8, %esi
	addl %ebp, %esi
	movl $256, %ecx
	rep
	movsl 
	popl %esi
	movl %eax, blk
	DROP 
	ret 

debug:	movl $3 * 0x10000 + (vc - 2) * ih + 3, xy
	DUP_ 
	movl god, %eax
	push (%eax)
	call dot
	DUP_ 
	popl %eax
	call dot
	DUP_ 
	movl main, %eax
	call dot
	DUP_ 
	movl %esi, %eax
	jmp dot

.equ iw, 16 + 6 
.equ ih, 24 + 6 
.equ hc, hp / iw # 46
.equ vc, vp / ih # 25
.balign 4
nc:	.int 9
xy: 	.int 3 * 0x10000 + 3
lm: 	.int 3
rm: 	.int hc * iw# 1012
xycr: 	.int 0
fov: 	.int 10 * (2 * vp + vp / 2)

nc_:	DUP_ 
	movl $nc, %eax
	shr $2, %eax
	ret 

xy_:	DUP_ 
	movl $xy, %eax
	shr $2, %eax
	ret 

fov_:	DUP_ 
	movl $fov, %eax
	shr $2, %eax
	ret 

sps:	DUP_ 
	movl $spaces, %eax
	shr $2, %eax
	ret 

last_:	DUP_ 
	movl $last, %eax
	shr $2, %eax
	ret 

###############################.include "gen.s" # cce.asm pio.asm ATI128.asm ATI64.asm gen.asm
#Generic graphics

.balign 4
frame: 	.int 0
fore: 	.int 0x0f7de # white
xc: 	.int 0
yc: 	.int 0

frame_:	DUP_
	movl frame, %eax
	ret

rgb:	rorl $8, %eax
	shrw $2, %ax
	rorl $6, %eax
	shrb $3, %al
	roll $6 + 5, %eax
	andl $0x0f7de, %eax
	ret

white:	DUP_ 
	movl $0x0ffffff, %eax
color:	call rgb
	movl %eax, fore
	DROP 
	ret 

fifof:	DROP 
graphic:ret

switch:	push %eax
	movl %esp, %eax
	movl god, %esp
	push %eax
	calll *sdl_swap(, 1)
	pop %esp
	pop %eax
	jmp pause

clip:	movl xy, %edi
	movl %edi, %ecx
	testw %cx, %cx
	jns 0f
	xorl %ecx, %ecx
0:	andl $0x0ffff, %ecx
	movl %ecx, yc
	imull $hp * 2, %ecx
#    shl  ECX, 10+1
	sarl $16, %edi
	jns 0f
	xorl %edi, %edi
0:	movl %edi, xc
	leal (%ecx, %edi, 2), %edi
	addl frame, %edi
	ret 

bit16:	lodsw 
	xchgb %ah, %al
	movl $16, %ecx
b16:	shlw $1, %ax
	jnc 0f
	movw %dx, (%edi)
0:	addl $2, %edi
	next b16
	ret 

bit32:	lodsw 
	xchgb %ah, %al
	movl $16, %ecx
b32:	shll $1, %eax
	jnc 0f
	movw %dx, (%edi)
	movw %dx, 2(%edi)
	movw %dx, (hp * 2)(%edi)
	movw %dx, (hp * 2 + 2)(%edi)
0:	addl $4, %edi
	next b32
	ret 

emit:	call qcr
	pushl %esi
	pushl %edi
	pushl %edx
	imull $16 * 24 / 8, %eax
	leal icons(%ebp, %eax), %esi
	call clip
	movl fore, %edx
	cmpl $hp, xc
	jge 1f
	cmpl $vp-1, yc
	jge 1f
	movl $vp-1, %ecx
	subl yc, %ecx
	cmpl $24, %ecx
	jle 0f
	movl $24, %ecx
0:	pushl %ecx
	call bit16
	addl $(hp - 16) * 2, %edi
	popl %ecx
	next 0b
1:	popl %edx
	popl %edi
	popl %esi
bl_:	DROP 
space:	addl $iw * 0x10000, xy
	ret 

emit2:	pushl %esi
	pushl %edi
	pushl %edx
	imull $16 * 24 / 8, %eax
	leal icons(%ebp, %eax), %esi
	call clip
	movl fore, %edx
	movl $24, %ecx
0:	pushl %ecx
	call bit32
	addl $(2 * hp - 16 * 2) * 2, %edi
	popl %ecx
	next 0b
	popl %edx
	popl %edi
	popl %esi
	addl $iw * 0x10000 * 2, xy
	DROP 
	ret 

text1:	call white
	movl $3, lm
	movl $hc * iw, rm
	jmp top

line:	push %edi
	call clip
	movl (%esi), %ecx
	shll $1, %ecx
	subl %ecx, %edi
	movl %eax, %ecx
	movl fore, %eax
	rep
	stosw 
	incl xy
	DROP 
	DROP
	pop %edi
	ret
 
box:	call clip
	cmpl $vp + 1, %eax
	js 0f
	movl $vp, %eax
0:	movl %eax, %ecx
	subl yc, %ecx
	jng no
	cmpl $hp + 1, (%esi)
	js 0f
	movl $hp, (%esi)
0:	movl xc, %eax
	subl %eax, (%esi)
	jng no
	movl $hp, %edx
	subl (%esi), %edx
	shll $1, %edx
	movl fore, %eax
0:	pushl %ecx
	movl (%esi), %ecx
	rep
	stosw 
	addl %edx, %edi
	popl %ecx
	next 0b
no:	DROP 
	DROP 
	ret 

###############################.include "gen.s" # cce.asm pio.asm ATI128.asm ATI64.asm gen.asm


.equ yellow, 0x0ffff00 
cyan:	DUP_ 
	movl $0x0ffff, %eax
	jmp color
magenta:	DUP_ 
	movl $0x0ff00ff, %eax
	jmp color
silver:	DUP_ 
	movl $0x0c0c0c0, %eax
	jmp color
blue:	DUP_ 
	movl $0x4040ff, %eax
	jmp color
red:	DUP_ 
	movl $0x0ff0000, %eax
	jmp color
green:	DUP_ 
	movl $0x8000ff00, %eax
	jmp color

history: 	.rept 11
	.byte 0
	.endr
echo_:	pushl %esi
	movl $11 - 1, %ecx
	lea history, %edi
	leal 1(%edi), %esi
	rep
	movsb 
	popl %esi
	movb %al, history + 11 - 1
	DROP 
	ret 

right:	DUP_ 
	movl $11, %ecx
	lea history, %edi
	xorl %eax, %eax
	rep
	stosb 
	DROP 
	ret 

down:	DUP_ 
	xorl %edx, %edx
	movl $ih, %ecx
	divl %ecx
	movl %edx, %eax
	addl $3 * 0x10000 + 0x8000 - ih + 3, %edx
	movl %edx, xy
zero:	testl %eax, %eax
	movl $0, %eax
	jnz 0f
	incl %eax
0:	ret 

blank:	DUP_ 
	xorl %eax, %eax
	movl %eax, xy
	call color
	DUP_ 
	movl $hp, %eax
	DUP_ 
	movl $vp, %eax
	jmp box

top:	movl lm, %ecx
	shll $16, %ecx
	addl $3, %ecx
	movl %ecx, xy
	movl %ecx, xycr
	ret 

qcr:	movw xy + 2, %cx
	cmpw rm, %cx
	js 0f
cr:	movl lm, %ecx
	shll $16, %ecx
	movw xy, %cx
	addl $ih, %ecx
	movl %ecx, xy
0:	ret 

lms:	movl %eax, lm
	DROP 
	ret 

rms:	movl %eax, rm
	DROP 
	ret 

at:	movw %ax, xy
	DROP 
	movw %ax, xy + 2
	DROP 
	ret 

pat:	addw %ax, xy
	DROP 
	addw %ax, xy + 2
	DROP 
	ret 

#cl1: xor  EAX, EAX
#    mov  [ESI], EAX
#    ret
#clip: movsx EDX, word ptr xy
#    cmp  EDX, vp
#    jns  cl1
#    add  EAX, EDX
#    js   cl1
#    test EDX, EDX
#    jns  @f
#        xor  EDX, EDX
#@@: cmp  EAX, vp
#    js   @f
#        mov  EAX, vp
#@@: sub  EAX, EDX
#    mov  word ptr xy, DX
#    movsx EDX, word ptr xy+2
#    cmp  EDX, hp
#    jns  cl1
#    add  [ESI], EDX
#    js   cl1
#    test EDX, EDX
#    jns  @f
#        xor  EDX, EDX
#@@: cmp  dword ptr [ESI], hp
#    js   @f
#        mov  dword ptr [ESI], hp
#@@: sub  [ESI], EDX
#    mov  word ptr xy+2, DX
#    ret

octant:	DUP_ 
	movl $0x43, %eax# poly -last y+ x+ ;23h ; last y+ x+
	movl 4(%esi), %edx
	testl %edx, %edx
	jns 0f
	negl %edx
	movl %edx, 4(%esi)
	xorb $1, %al
0:	cmpl (%esi), %edx
	jns 0f
	xorb $4, %al
0:	ret 

# Keyboard
eight:	addl $12, %edi
	call four
	call space
	subl $16, %edi
four:	movl $4, %ecx
four1:	pushl %ecx
	DUP_ 
	xorl %eax, %eax
	movb 4(%edi), %al
	incl %edi
	call emit
	popl %ecx
	next four1
	ret 

stack:	movl $_start + (godd - 4), %edi
0:	movl god, %edx
	cmpl %edi, (%edx)
	jnc 0f
	DUP_ 
	movl (%edi), %eax
	subl $4, %edi
	call qdot
	jmp 0b
0:	ret 

keyboard:call text1
	movl board, %edi
	DUP_ 
	movl keyc, %eax
	call color
	movl $hc * iw, rm
	movl $hp - 9 * iw + 3, lm
	movl $(hp - 9 * iw + 3) * 0x10000 + vp - 4 * ih + 3, xy
	call eight
	call eight
	call eight
	call cr
	addl $4 * iw * 0x10000, xy
	movl shift, %edi
	addl $4 * 4 - 4, %edi
	movl $3, %ecx
	call four1
	movl $3, lm
	movw $3, xy + 2
	call stack
	movw $hp - (11 + 9) * iw + 3, xy + 2
	lea history - 4, %edi
	movl $11, %ecx
	jmp four1

alpha: 	.byte 015, 012, 1, 014
	.byte 024, 2, 6, 010
	.byte 023, 011, 017, 021
	.byte 022, 013, 016, 7
	.byte 5, 3, 4, 026
	.byte 027, 044, 025, 020
graphics: 	.byte 031, 032, 033, 0
	.byte 034, 035, 036, 030
	.byte 037, 040, 041, 057
	.byte 051, 050, 052, 054# : ; ! @
	.byte 046, 042, 045, 056# Z J . ,
	.byte 055, 047, 053, 043# * / + -
numbers: 	.byte 031, 032, 033, 0
	.byte 034, 035, 036, 030
	.byte 037, 040, 041, 0
	.byte 0, 0, 0, 0
	.byte 0, 0, 0, 0
	.byte 0, 0, 0, 0
octals: 	.byte 031, 032, 033, 0
	.byte 034, 035, 036, 030
	.byte 037, 040, 041, 0
	.byte 0, 5, 023, 012
	.byte 0, 020, 4, 016
	.byte 0, 0, 0, 0
letter:	cmpb $4, %al
	js 0f
	movl board, %edx
	movb (%edx, %eax), %al
0:	ret 

keys: 	.byte 16, 17, 18, 19, 0, 0, 4, 5# 20
	.byte 6, 7, 0, 0, 0, 0, 20, 21
	.byte 22, 23, 0, 0, 8, 9, 10, 11# 40
	.byte 0, 0, 0, 0, 24, 25, 26, 27
	.byte 0, 1, 12, 13, 14, 15, 0, 0# 60 N
	.byte 3, 2# alt space
key:	DUP_ 
0:	call pause
	calll *sdl_key(, 1)
	movb (keys)(%eax), %al
	ret
	
.balign 4
graph0: 	.int nul0, nul0, nul0, alph0
	.byte 0, 0, 5, 0#     a
graph1: 	.int word0, x, lj, alph
	.byte 025, 045, 5, 0# x . a
alpha0: 	.int nul0, nul0, number, star0
	.byte 0, 041, 055, 0#   9 *
alpha1: 	.int word0, x, lj, graph
	.byte 025, 045, 055, 0# x . *
numb0: 	.int number0, minus, alphn, octal
	.byte 043, 5, 016, 0# - a f
numb1: 	.int number0, xn, endn, number0
	.byte 025, 045, 0, 0# x .

board: 	.int alpha - 4
shift: 	.int alpha0
base: 	.int 10
current: 	.int decimal
keyc: 	.int yellow
chars: 	.int 1
aword: 	.int ex1
anumber: 	.int nul
words: 	.int 1

nul0:	DROP 
	jmp 0f
accept:
acceptn:	movl $alpha0, shift
	lea alpha - 4, %edi
accept1:	movl %edi, board
0:	call key
	cmpb $4, %al
	jns first
	movl shift, %edx
	jmpl *(%edx, %eax, 4)

bits: 	.byte 28
0:	addl $0120, %eax
	movb $7, %cl
	jmp 0f
pack:	cmpb $020, %al
	jnc 0b
	movb $4, %cl
	testb $010, %al
	jz 0f
	incl %ecx
	xorb $030, %al
0:	movl %eax, %edx
	movb %cl, %ch
0:	cmpb %cl, bits
	jnc 0f
	shrb $1, %al
	jc full
	decb %cl
	jmp 0b
0:	shll %cl, (%esi)
	xorl %eax, (%esi)
	subb %cl, bits
	ret 

lj0:	movb bits, %cl
	addb $4, %cl
	shll %cl, (%esi)
	ret 

lj:	call lj0
	DROP 
	ret 

full:	call lj0
	incl words
	movb $28, bits
	subb %ch, bits
	movl %edx, %eax
	DUP_ 
	ret 

x:	call right
	movl words, %eax
	leal (%esi, %eax, 4), %esi
	DROP 
	jmp accept

word_:	call right
	movl $1, words
	movl $1, chars
	DUP_ 
	movl $0, (%esi)
	movb $28, bits
word1:	call letter
	jns 0f
	movl shift, %edx
	jmpl *(%edx, %eax, 4)
0:	testb %al, %al
	jz word0
	DUP_ 
	call echo_
	call pack
	incl chars
word0:	DROP 
	call key
	jmp word1

decimal:	movl $10, base
	movl $numb0, shift
	movl $numbers - 4, board
	ret 

hex:	movl $16, base
	movl $numb0, shift# oct0
	movl $octals - 4, board
	ret 

octal:	xorl $(decimal - _start) ^ (hex - _start), current
	xorb $041 ^ 016, numb0 + 18# f vs 9
	calll *current
	jmp number0

xn:	DROP 
	DROP 
	jmp acceptn

#      db  0,  0,  0,  0
digit: 	.byte 14, 10, 0, 0
	.byte 0, 0, 12, 0, 0, 0, 15, 0
	.byte 13, 0, 0, 11, 0, 0, 0, 0
	.byte 0, 1, 2, 3, 4, 5, 6, 7
	.byte 8, 9
sign: 	.byte 0
minus:# mov  AL, 43o ; -
	movb %al, sign
	jmp number2

number0:	DROP 
	jmp number3
number:	calll *current
	movb $0, sign
	xorl %eax, %eax
number3:	call key
	call letter
	jns 0f
	movl shift, %edx
	jmpl *(%edx, %eax, 4)
0:	testb %al, %al
	jz number0
	movb (digit - 4)(%eax), %al
	testb $037, sign
	jz 0f
	negl %eax
0:	movl (%esi), %edx
	imull base, %edx
	addl %eax, %edx
0:	movl %edx, (%esi)
number2:	DROP 
	movl $numb1, shift
	jmp number3

endn:	DROP 
	calll *anumber(, 1)
	jmp acceptn

alphn:	DROP 
alph0:	movl $alpha0, shift
	lea alpha - 4, %edi
	jmp 0f
star0:	movl $graph0, shift
	lea graphics - 4, %edi
0:	DROP 
	jmp accept1

alph:	movl $alpha1, shift
	lea alpha - 4, %edi
	jmp 0f
graph:	movl $graph1, shift
	lea graphics - 4, %edi
0:	movl %edi, board
	jmp word0

first:	addl $4 * 4 + 4, shift
	call word_
	calll *aword(, 1)
	jmp accept

hicon: 	.byte 030, 031, 032, 033, 034, 035, 036, 037
	.byte 040, 041, 5, 023, 012, 020, 4, 016
edig1:	DUP_ 
edig:	pushl %ecx
	movb hicon(%eax), %al
	call emit
	popl %ecx
	ret 

odig:	roll $4, %eax
	DUP_ 
	andl $0x0F, %eax
	ret 

hdotn:	movl %eax, %edx
	negl %eax
	leal 32(, %eax, 4), %ecx
	DROP 
	rol %cl, %eax
	movl %edx, %ecx
	jmp 0f
hdot:	movl $8, %ecx
0:	call odig
	call edig
	next 0b
	DROP 
	ret 

dot:	movl $7, %ecx
0:	call odig
	jnz _h
	DROP 
	next 0b
	incl %ecx
0:	call odig
_h1:	call edig
	next 0b
	call space
	DROP 
	ret 
_h:	incl %ecx
	jmp _h1

qdot:	cmpl $10, base
	jnz dot
dot10:	movl %eax, %edx
	testl %edx, %edx
	jns 0f
	negl %edx
	DUP_ 
	movl $043, %eax
	call emit
0:	movl $8, %ecx
0:	movl %edx, %eax
	xorl %edx, %edx
	divl tens(, %ecx, 4)
	testl %eax, %eax
	jnz d_1
	decl %ecx
	jns 0b
	jmp d_2
0:	movl %edx, %eax
	xorl %edx, %edx
	divl tens(, %ecx, 4)
d_1:	call edig1
	decl %ecx
	jns 0b
d_2:	movl %edx, %eax
	call edig1
	call space# spcr
	DROP 
	ret 

unpack:	DUP_ 
	testl %eax, %eax
	js 0f
	shll $4, (%esi)
	roll $4, %eax
	andl $7, %eax
	ret 
0:	shll $1, %eax
	js 0f
	shll $5, (%esi)
	roll $4, %eax
	andl $7, %eax
	xorb $010, %al
	ret 
0:	shll $7, (%esi)
	roll $6, %eax
	andl $077, %eax
	subb $020, %al
	ret 

qring:	DUP_ 
	incl (%esi)
	cmpl %edi, curs# from abort, insert
	jnz 0f
	movl %eax, curs
0:	cmpl curs, %eax
	jz ring
	jns 0f
	movl %edi, pcad
0:	DROP 
	ret 

ring:	movl %edi, cad
	subl $iw * 0x10000, xy# bksp
	DUP_ 
	movl $0x0e04000, %eax
	call color
	movl $060, %eax
	movw xy + 2, %cx
	cmpw rm, %cx
	js 0f
	call emit
	subl $iw * 0x10000, xy# bksp
	ret 
0:	jmp emit

rw:	movw xy + 2, %cx
	cmpw lm, %cx
	jz 0f
	call cr
0:	call red
	jmp type_

gw:	call green
	jmp type_
mw:	call cyan
	jmp type_
ww:	DUP_ 
	movl $yellow, %eax
	call color
	jmp type_

type0:	subl $iw * 0x10000, xy# call bspcr
	testl $-020, -4(%ebp, %edi, 4)
	jnz type1
	decl %edi
	movl %edi, lcad
	call space
	call qring
	popl %edx# End of block
	DROP 
	jmp keyboard

cap:	call white
	DUP_ 
	movl -4(%ebp, %edi, 4), %eax
	andl $-020, %eax
	call unpack
	addb $48, %al
	call emit
	jmp type2

caps:	call white
	DUP_ 
	movl -4(%ebp, %edi, 4), %eax
	andl $-020, %eax
0:	call unpack
	jz 0f
	addb $48, %al
	call emit
	jmp 0b

text:	call white
type_:
type1:	DUP_ 
	movl -4(%ebp, %edi, 4), %eax
	andl $-020, %eax
type2:	call unpack
	jz 0f
	call emit
	jmp type2
0:	call space
	DROP 
	DROP 
	ret 

gsw:	movl -4(%ebp, %edi, 4), %edx
	sarl $5, %edx
	jmp gnw1

var:	call magenta
	call type_
gnw:	movl (%ebp, %edi, 4), %edx
	incl %edi
gnw1:	DUP_ 
	movl $0x0f800, %eax# Green
	cmpl $dot10, bas
	jz 0f
	movl $0x0c000, %eax# dark green
	jmp 0f

sw:	movl -4(%ebp, %edi, 4), %edx
	sarl $5, %edx
	jmp nw1

nw:	movl (%ebp, %edi, 4), %edx
	incl %edi
nw1:	DUP_ 
	movl $yellow, %eax
	cmpl $dot10, bas
	jz 0f
	movl $0x0c0c000, %eax# dark yellow
0:	call color
	DUP_ 
	movl %edx, %eax
	jmpl *bas(, 1)

refresh:	call show
	call blank
	call text1
	DUP_ # Counter
	movl lcad, %eax
	movl %eax, cad# for curs beyond end
	xorl %eax, %eax
	movl blk, %edi
	shll $10 - 2, %edi
	movl %edi, pcad# for curs=0
ref1:	testl $0x0f, (%ebp, %edi, 4)
	jz 0f
	call qring
0:	movl (%ebp, %edi, 4), %edx
	incl %edi
	movl $dot10, bas
	testb $020, %dl
	jz 0f
	movl $dot, bas
0:	andl $017, %edx
	calll *display(, %edx, 4)
	jmp ref1

.balign 4
display: 	.int type0, ww, nw, rw
	.int gw, gnw, gsw, mw
	.int sw, text, cap, caps
	.int var, nul, nul, nul
tens: 	.int 10, 100, 1000, 10000, 100000, 1000000
	.int 10000000, 100000000, 1000000000
bas: 	.int dot10
blk: 	.int 18
curs: 	.int 0
cad: 	.int 0
pcad: 	.int 0
lcad: 	.int 0
trash: 	.int buffer * 4
ekeys: 	.int nul, del, eout, destack
	.int act1, act3, act4, shadow
	.int mcur, mmcur, ppcur, pcur
	.int mblk, actv, act7, pblk
	.int nul, act11, act10, act9
	.int nul, nul, nul, nul
ekbd0: 	.int nul, nul, nul, nul
	.byte 025, 045, 7, 0# x  .  i
ekbd: 	.byte 017, 1, 015, 055# w  r  g  *
	.byte 014, 026, 020, 1# l  u  d  r
	.byte 043, 011, 012, 053# -  m  c  +
	.byte 0, 070, 072, 2#    S  C  t
	.byte 0, 0, 0, 0
	.byte 0, 0, 0, 0
actc: 	.int yellow, 0, 0x0ff0000, 0x0c000, 0, 0, 0x0ffff
	.int 0, 0x0ffffff, 0x0ffffff, 0x0ffffff, 0x8080ff
vector: 	.int 0
action: 	.byte 1

act1:	movb $1, %al
	jmp 0f
act3:	movb $3, %al
	jmp 0f
act4:	movb $4, %al
	jmp 0f
act9:	movb $9, %al
	jmp 0f
act10:	movb $10, %al
	jmp 0f
act11:	movb $11, %al
	jmp 0f
act7:	movb $7, %al
0:	movb %al, action
	movl (actc - 4)(, %eax, 4), %eax
	movl $insert, aword
actn:	movl %eax, keyc
	popl %eax
	DROP 
	jmp accept

actv:	movb $12, action
	movl $0x0ff00ff, %eax# Magenta
	movl $0f, aword
	jmp actn

0:	DUP_ 
	xorl %eax, %eax
	incl words
	jmp insert

mcur:	decl curs
	jns 0f
pcur:	incl curs
0:	ret 

mmcur:	subl $8, curs
	jns 0f
	movl $0, curs
0:	ret 
ppcur:	addl $8, curs
	ret 

pblk:	addl $2, blk
	addl $2, (%esi)
	ret 
mblk:	cmpl $20, blk
	js 0f
	subl $2, blk
	subl $2, (%esi)
0:	ret 

shadow:	xorl $1, blk
	xorl $1, (%esi)
	ret 

e0:	DROP 
	jmp 0f

edit:	movl %eax, blk
	DROP 
e:	DUP_ 
	movl blk, %eax
	movl $format, anumber
	movb $045, alpha0 + 4 * 4# .
	movl $e0, alpha0 + 4
	call refresh
0:	movl $ekbd0, shift
	movl $ekbd - 4, board
	movl $yellow, keyc
0:	call key
	calll *ekeys(, %eax, 4)
	DROP 
	jmp 0b

eout:	popl %eax
	DROP 
	DROP 
	movl $ex1, aword
	movl $nul, anumber
	movb $0, alpha0 + 4 * 4
	movl $nul0, alpha0 + 4
	movl $yellow, keyc
	jmp accept

destack:movl trash, %edx
	lea buffer * 4(%ebp), %ebx
	cmpl %ebx, %edx
	jnz 0f
	ret 
0:	subl $2 * 4, %edx
	movl (1 * 4)(%edx), %ecx
	movl %ecx, words
0:	DUP_ 
	movl (%edx), %eax
	subl $1 * 4, %edx
	next 0b
	addl $1 * 4, %edx
	movl %edx, trash

insert0:movl lcad, %ecx# room available?
	addl words, %ecx
	xorl lcad, %ecx
	andl $-0x100, %ecx
	jz insert1
	movl words, %ecx# no
0:	DROP 
	next 0b
	ret 
insert1:pushl %esi
	movl lcad, %esi
	movl %esi, %ecx
	decl %esi
	movl %esi, %edi
	addl words, %edi
	shll $2, %edi
	addl %ebp, %edi
	subl cad, %ecx
	js 0f
	shll $2, %esi
	addl %ebp, %esi
	std 
	rep
	movsl 
	cld 
0:	popl %esi
	subl %ebp, %edi
	shrl $2, %edi
	incl %edi
	movl %edi, curs# like abort
	movl words, %ecx
0:	decl %edi
	movl %eax, (%ebp, %edi, 4)
	DROP # requires cld
	next 0b
	ret 

insert:	call insert0
	movb action, %cl
	xorb %cl, (%ebp, %edi, 4)
	jmp accept

format:	testb $012, action# ignore 3 and 9
	jz 0f
	DROP 
	ret 
0:	movl %eax, %edx
	andl $0x0FC000000, %edx
	jz 0f
	cmpl $0x0FC000000, %edx
	jnz format2
0:	shll $5, %eax
	xorb $2, %al# 6
	cmpb $4, action
	jz 0f
	xorb $013, %al# 8
0:	cmpl $10, base
	jz 0f
	xorb $020, %al
0:	movl $1, words
	jmp insert

format2:	DUP_ 
	movl $1, %eax# 5
	cmpb $4, action
	jz 0f
	movb $3, %al# 2
0:	cmpl $10, base
	jz 0f
	xorb $020, %al
0:	xchgl (%esi), %eax
	movl $2, words
	jmp insert

del:	call enstack
	movl pcad, %edi
	movl lcad, %ecx
	subl %edi, %ecx
	shll $2, %edi
	addl %ebp, %edi
	pushl %esi
	movl cad, %esi
	shll $2, %esi
	addl %ebp, %esi
	rep
	movsl 
	popl %esi
	jmp mcur

enstack:DUP_ 
	movl cad, %eax
	subl pcad, %eax
	jz ens
	movl %eax, %ecx
	xchgl %edx, %eax
	pushl %esi
	movl cad, %esi
	leal -4(%ebp, %esi, 4), %esi
	movl trash, %edi
0:	std 
	lodsl 
	cld 
	stosl 
	next 0b
	xchgl %edx, %eax
	stosl 
	movl %edi, trash
	popl %esi
ens:	DROP 
	ret 

pad:	popl %edx
	movl %edx, vector
	addl $28 * 5, %edx
	movl %edx, board
	subl $4 * 4, %edx
	movl %edx, shift
0:	call key
	movl vector, %edx
	addl %eax, %edx
	leal 5(%edx, %eax, 4), %edx
	addl -4(%edx), %edx
	DROP 
	calll *%edx
	jmp 0b
