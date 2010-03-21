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
