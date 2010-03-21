	align 4

frame:	dd 0x00740000
	dd 0x00740000
displ:	dd 0xE8000000
fore:	dd _yellow
xc:	dd 0
yc:	dd 0

white:
	DUP_
	mov eax, _white
color:	mov [fore], eax
	DROP
	ret

north:
	mov edx, 0xcf8
	out dx, eax
	lea edx, [edx+4]
	in eax, dx
	ret

dev:
	DUP_
	mov eax, 0x80010008
	mov ecx, 30
.0:	DUP_
	call north
	and eax, 0xff000000
	xor eax, [esi+4]
	DROP
	jz .1
	sub eax, 0x800
	NEXT .0
.1:	lea esi, [esi+4]
	lea eax, [eax-8]
	ret

ati0:
	mov eax, 0x03000000
	call dev
	lea eax, [eax+16]
	mov cl, 6
.0:	DUP_
	call north
	xor al, 8
	jz .1
	DROP
	lea eax, [eax+4]
	NEXT .0
	lea eax, [eax-24]
	DUP_
	call north
	and al, 0xf0
.1:	mov [displ], eax
	DROP
	ret

fifof:
	DROP
graphic:
	ret

switch_:
	push esi
	mov esi, [frame+4]
	push edi
	mov edi, [displ]
	mov ecx, hp*vp*bpp/4
	rep movsd
	pop edi
	pop esi
	ret

switch:
	call switch_
	jmp dopause

# set EDI, xc, and yc from xy.
# ensure that neither xc nor yc is negative.
# ( -- )
pen_addr:
	mov edi, [xy]
	mov ecx, edi
	test cx, cx
	jns .0
	xor ecx, ecx
.0:	and ecx, 0xffff
	mov [yc], ecx
	imul ecx, hp*bpp
	sar edi, 16
	jns .1
	xor edi, edi
.1:	mov [xc], edi
	lea edi, [ecx+edi*bpp]
	add edi, [frame]
	ret

; display 16 pixels
bit16:
	lodsw
	xchg al, ah
	mov ecx, iw
.0:	shl ax, 1
	jnc .1
	PUTPIX
.1:	add edi, byte bpp
	NEXT .0
	ret

; display 16 doubled pixels
bit32:
	lodsw
	xchg al, ah
	mov ecx, iw
.0:	shl eax, 1
	jnc .1
	PUT4PIX
.1:	add edi, byte 2*bpp
	NEXT .0
	ret

; : emit ( c -- )
emit:
	call qcr
	push esi
	push edi
	push edx
	imul eax, byte iw*ih/8
	lea esi, [icons+eax]
	call pen_addr
	mov edx, [fore]
	mov ecx, ih
.0:	push ecx
	call bit16
	add edi, (hp-iw)*bpp
	pop ecx
	NEXT .0
	pop edx
	pop edi
	pop esi
bl_:	DROP
space:	add dword [xy], ch_w*0x10000
	ret

; display a double-size character.
; : 2emit ( c -- )
emit2:
	push esi
	push edi
	push edx
	imul eax, byte iw*ih/8
	lea esi, [icons+eax]
	call pen_addr
	mov edx, [fore]
	mov ecx, ih
.0:	push ecx
	call bit32
	add edi, 2*(hp-iw)*bpp
	pop ecx
	NEXT .0
	pop edx
	pop edi
	pop esi
	add dword [xy], 2*ch_w*0x10000
	DROP
	ret

text1:
	call white
	mov dword [lm], ch_pad
	mov dword [rm], hc*ch_w
	jmp top

# ( x len -- )
# draw a horizontal line LEN pixels long, starting
# X pixels to the left of the current pen position.
line:
	call pen_addr
	mov ecx, [esi]
	shl ecx, bpplog2	; ecx = ecx*bpp
	sub edi, ecx
	mov ecx, eax
	mov eax, [fore]
	REP_STOPIX
	inc dword [xy]
	DROP
	DROP
	ret

# ( width height -- )
box:
	call pen_addr
	cmp eax, vp+1
	js .0
	mov eax, vp
.0:	mov ecx, eax
	sub ecx, [yc]
	jle .9
	cmp dword [esi], hp+1
	js .1
	mov dword [esi], hp
.1:	mov eax, [xc]
	sub [esi], eax
	jle .9
	mov edx, hp
	sub edx, [esi]
	shl edx, bpplog2	; edx = edx*bpp
	mov eax, [fore]
.2:	push ecx
	mov ecx, [esi]
	REP_STOPIX
	add edi, edx
	pop ecx
	NEXT .2
.9:	DROP
	DROP
	ret

; vim:ts=8:sw=8
