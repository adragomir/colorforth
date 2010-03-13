#Generic graphics

white:	DUP_ 
	movl $0x0ffffff, %eax
color:	push %edx
	push %ecx
	push %eax
	calll *gl_color(, 1)
	pop %eax
	pop %ecx
	pop %edx
	DROP 
	ret 

switch:	push %eax
	calll *gl_swap(, 1)
	pop %eax
	jmp pause

emit:	call qcr
	push %edx
	push %ecx
	push %eax
	movl xy, %eax
	xor %ebx, %ebx
	movw %ax, %bx
	push %ebx
	shr $16, %eax
	push %eax
	calll *gl_emit(, 1)
	lea 12(%esp), %esp
	pop %ecx
	pop %edx
bl_:	DROP 
space:	addl $iw * 0x10000, xy
	ret 

emit2:	call qcr
	push %eax
	movl xy, %eax
	xor %ecx, %ecx
	movw %ax, %cx
	push %ecx
	shr $16, %eax
	push %eax
	calll *gl_emit2(, 1)
	lea 12(%esp), %esp
	addl $iw * 0x10000 * 2, xy
	DROP 
	ret 

text1:	call white
	movl $3, lm
	movl $hc * iw, rm
	jmp top

line:	push %eax
	movl xy, %eax
	xor %ecx, %ecx
	movw %ax, %cx
	push %ecx
	shr $16, %eax
	subl (%esi), %eax
	push %eax
	calll *gl_line(, 1)
	lea 12(%esp), %esp
	incl xy
	DROP 
	DROP
	ret
 
box:	push %eax
	DROP
	push %eax
	movl xy, %eax
	xor %ecx, %ecx
	movw %ax, %cx
	push %ecx
	shr $16, %eax
	push %eax
	calll *gl_box(, 1)
	lea 16(%esp), %esp
	DROP 
	ret 
