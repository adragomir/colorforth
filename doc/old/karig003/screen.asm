;==========================================================================
; SCREEN.ASM
; Code for drawing text and graphics on the screen.
;==========================================================================

%define rowsize 80
%define rowcount 30

cls:
; ( -- ) Wipe the text screen clean (fill it with white pixels).
		_dup
		push	edi
		mov	eax, 0xFFFFFFFF ; white
		mov	edi, [scr$.vb]
		mov	ecx, [scr$.vz]
		rep	stosd
		pop	edi
		_drop
		ret

refresh:
; ( -- ) Copy video buffer into video RAM (display video buffer contents).
		push	esi
		push	edi
		mov	esi, [scr$.vb]
		mov	edi, [scr$.vr]
		mov	ecx, [scr$.vz]
		rep	movsd
		pop	edi
		pop	esi
		ret

;--------------------------------------------------------------------------
; "T" (TEXT REGISTER) ROUTINES
;--------------------------------------------------------------------------

; Text is printed to the screen via "T", the text register, which is large
; enough to contain a single row of characters (80 bytes). Text is appended
; to T, then T as a whole is printed to a specific row on the screen.

tsize:
; ( -- 80 ) Return capacity of T (and size of row).
		_dup
		mov	eax, rowsize
		ret

tcount:
; ( -- o ) Return number of characters in T.
		_dup
		mov	eax, [scr$.toff]
		ret

tclear:
; ( -- ) Empty T (fill it with spaces).
		xor	ebx, ebx
		mov	[scr$.toff], ebx

		; fill with spaces
		call	tsize
		mov	ecx, eax
		mov	al, ' '
		push	edi
		mov	edi, scr$.t
		rep	stosb
		pop	edi
		_drop
		ret

tapps:
; ( p s -- ) Append string (at address p, of length s) to T.
;	RETURNS ERROR and appends nothing if not all characters will fit.
		_dup
		call	tfit
		jc	.x
		push	esi
		push	edi

			mov	edi, [scr$.toff]
			add	edi, scr$.t
			mov	esi, [esi]
			mov	ecx, eax
			rep	movsb

		pop	edi
		pop	esi
	.x:	_drop
		_drop
		ret

tapp1:
; ( c -- ) Append one character (in AL) to T.
;	RETURNS ERROR and appends nothing if character won't fit.
		_lit	1
		jmp	tapp4.do

tapp2:
; ( c -- ) Append two characters (in AX) to T.
;	RETURNS ERROR and appends nothing if not all characters will fit.
		_lit	2
		jmp	tapp4.do

tapp4:
; ( c -- ) Append four characters (in EAX) to T.
;	RETURNS ERROR and appends nothing if not all characters will fit.
		_lit	4
	.do:	_dup
		call	tfit
		mov	ecx, eax
		_drop
		jc	.x

		mov	ebx, [scr$.toff]
		mov	[scr$.t + ebx], eax
		add	ebx, ecx
		mov	[scr$.toff], ebx
	.x:	_drop
		ret

tprint:
; ( r -- ) Print contents of T to the row given.
;	Does not refresh the screen; call "refresh" to do this.
;	RETURNS ERROR and prints nothing if row number is too high.

		; CHECK ROW: If "r" >= rowcount, just exit with carry set.
		mov	ebx, rowcount-1
		cmp	ebx, eax
		jc	.x

		; PRESERVE VM REGISTERS AND ROW NUMBER
		push	esi
		push	edi
		push	eax

		; COPY TEXT IN T INTO TEXT BUFFER
		mov	ebx, rowsize
		mul	ebx
		add	eax, [scr$.tb]
		mov	edi, eax
		mov	esi, scr$.t
		mov	ecx, rowsize/4
		rep	movsd

		; PRINT TEXT IN T INTO VIDEO BUFFER
		pop	eax
		call	scr$.calcvbo
		mov	edi, eax
		xor	ecx, ecx
	.1:	mov	al, [scr$.t + ecx]
		call	scr$.glyph
		inc	ecx
		cmp	ecx, rowsize
		jl	.1

		; RESTORE VM REGISTERS AND EXIT "OK"
		pop	edi
		pop	esi
		clc
	.x:	_drop
		ret

tfit:
; ( s -- ) Return error if there isn't room in T for "s" more characters.
		call	scr$.trem  ; ( s rem )
		cmp	eax, [esi] ; set carry if rem < s
		_drop
		_drop
		ret

;==========================================================================
scr$: ; INTERNAL ROUTINES
;==========================================================================

.trem:
; ( -- s ) Get number of characters that can still be added to T.
;	RETURNS ZERO FLAG SET if number is zero.
		call	tsize      ; ( 80 )
		call	tcount     ; ( 80 toff )
		sub	[esi], eax ; ( rem toff )
		_drop	           ; ( rem )
		ret

.calcvbo:
; ( r -- p ) Convert row to video buffer offset.
		mov	ebx, [.rz]
		mul	ebx
		add	eax, [.vb]
		ret

;--------------------------------------------------------------------------
; CHARACTER-DRAWING ROUTINES
;--------------------------------------------------------------------------

.2_pixels:
; Converts low 2 bits in EAX to 2 pixels on screen
; PASS eax = bits from font, edi = pointer into screen buffer
		mov	ebx, eax
		and	ebx, 0x03 ; leave only bits 0..1 (value 0..3)
		mov	ebx, [.colors+ebx*4]
		shr	eax, 2
		mov	[edi], ebx
		lea	edi, [edi+4]
		ret

.scanline:
; Converts low 8 bits in EAX to 8 pixels on screen
; PASS eax = bits from font, edi = pointer into screen buffer
		call	.2_pixels
		call	.2_pixels
		call	.2_pixels
		call	.2_pixels
		add	edi, [.scz]
		ret

.4_scanlines:
; Converts 32 bits in EAX to 4 rows of 8 pixels on screen
; PASS esi = pointer into font, edi = pointer into screen buffer
		mov	eax, [esi]   ; grab 4 bytes from font
		call	.scanline
		call	.scanline
		call	.scanline
		call	.scanline
		lea	esi, [esi+4] ; move to next 4 bytes in font
		ret

.glyph:
; Prints character whose ASCII value is passed in AL (EAX)
; PASS eax = character, edi = pointer into screen buffer
		push	edi
		and	eax, 0xFF
		shl	eax, 4     ; multiply by 16 (bytes/char in font)
		add	eax, .font ; create offset into font
		mov	esi, eax
		call	.4_scanlines
		call	.4_scanlines
		call	.4_scanlines
		call	.4_scanlines
		pop	edi
		add	edi, 16    ; width in bytes of one character
		ret

; -------------------------------------------------------------------------
; SCREEN-RELATED VARIABLES AND DATA
; -------------------------------------------------------------------------

align 4
.t:	db	"This is the great big long message that Karig will display when it boots up. "
.t2:	times	rowsize+4 - ($-.t) db ' '
.toff:	dd	.t2 - .t

.tb:	dd	0x996A0    ; pointer to text buffer (stacks - (80*30))
.vb:	dd	0x200000   ; pointer to video buffer
.vr:	dd	0xE0000000 ; pointer to video RAM
.vz:	dd	640*480/2  ; dwords in video buffer
.rz:	dd	640*2*16   ; bytes per 16-scanline row
.scz:	dd	640*2-16   ; bytes per scanline, minus character width

black equ 0x0000
white equ 0xFFFF

; We take pixels two at a time and print both at once. Bit 0 becomes the
; first pixel printed; bit 1, the second. If the two bits are 01 (bit 0 = 1
; and bit 1 = 0), we want bit 0 done first, so we need to lay color 1 first,
; thus offset 01 leads to the "white, black" entry.

; To use different colors, change the contents of this table. This should
; need to be done more than two or three times while refreshing the text
; screen.

.colors:
		dw	black, black ; bits 00
		dw	white, black ; bits 10 -> 01
		dw	black, white ; bits 01 -> 10
		dw	white, white ; bits 11

.font:	incbin "font.bin"

