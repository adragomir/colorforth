;==========================================================================
; REALEXP.ASM (REAL-MODE EXPERIMENTAL CODE)
; Any experimental or test code to be run in real mode goes here.
; (Once the protected-mode memory-dump code works, this file will be
; obsolete.)
;==========================================================================

		call	clear_screen

; ------ Set up data stack. (Return stack runs from 0x0800 downward.)
		mov	esi, 0xFFFC

; ------ TEST CODE GOES HERE.
		_lit	' END'
		;call	tapp4
		mov	ebx, [scr$.toff]
		mov	[scr$.t + ebx], eax
		_drop

		mov	bx, scr$.t
		call	dump_16
		call	dump_16
		call	dump_16
		call	dump_16
		call	dump_16
		call	dump_16

; ------ Halt computer.
		jmp	short $

; ROUTINES to print to the screen.
; ------ [http://www.karig.net/0008.html]

clear_screen:
		mov	ax, 3
		int	0x10

		xor	dx, dx
		jmp	set_pos

scroll_up:
		mov	bh, 7
		xor	cx, cx
		mov	dx, (24*0x100) + 79
		mov	ax, (0x0600 + 23)
		int	0x10
		ret

; ROUTINES needed to support hex dumps.
; ------ [http://www.karig.net/0009.html]

byte_to_hex:
		xor	bh, bh
		mov	bl, al
		and	bl, 0x0F
		mov	ah, [.digits + bx]
		mov	bl, al
		shr	bl, 4
		mov	al, [.digits + bx]
		ret

	.digits:
		db	"0123456789ABCDEF"

print_colon:
		mov	al, ':'
		jmp	print_char
print_space:
		mov	al, ' '
		jmp	print_char
print_vbar:
		mov	al, '|'
		jmp	print_char

print_word:
		; pass word in AX
		push	ax
		mov	al, ah
		call	print_byte
		pop	ax
print_byte:
		; pass byte in AL
		call	byte_to_hex
		push	ax
		call	print_char
		pop	ax
		mov	al, ah
		;jmp	print_char

print_char:
		xor	bh, bh
		xor	cx, cx
		inc	cx
		mov	ah, 0x0A
		int	0x10
get_pos:
		xor	bh, bh
		mov	ah, 3
		int	0x10
next_column:
		inc	dl
		cmp	dl, 80
		jb	set_pos
next_row:
		xor	dl, dl
		inc	dh
		cmp	dh, 25
		jb	set_pos

		dec	dh
		push	dx
		call	scroll_up
		pop	dx
set_pos:
		xor	bh, bh
		mov	ah, 2
		int	0x10
		ret

; ROUTINE to dump bytes to the screen.
; ------ [http://www.karig.net/0009.html]

dump_16:
		push	bx
		mov	ax, fs
		call	print_word
		call	print_colon
		pop	ax
		push	ax
		call	print_word
		call	print_colon
		call	print_space
		pop	bx
		xor	si, si

	.1:	mov	al, [fs:bx+si]
		push	si
		push	bx
		call	print_byte
		call	print_space
		pop	bx
		pop	si
		inc	si
		cmp	si, 16
		jb	.1

		push	bx
		call	print_vbar
		call	print_space
		pop	bx

		xor	si, si

	.2:	mov	al, [fs:bx+si]
		push	si
		push	bx
		call	print_char
		pop	bx
		pop	si
		inc	si
		cmp	si, 16
		jb	.2

		push	bx
		call	get_pos
		call	next_row
		pop	bx
		add	bx, 16
		ret

;==========================================================================
