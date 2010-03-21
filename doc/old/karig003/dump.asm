;==========================================================================
; DUMP.ASM
; Code for dumping memory to the screen.
;==========================================================================

dump16:
; ( r p -- r+1 p+16 ) Dump the contents of 16 bytes of memory (from address
;	"p") onto the screen (at row "r"). Increment r and add 16 to p so
;	that dump16 can be called repeatedly until the screen fills up.
;	DOES NOT VERIFY THAT "r" IS A GOOD ROW NUMBER (0..29).

		call	tclear           ; ( r p )
		call	dump$.tappdword  ; ( r p )
		_lit	": "             ; ( r p .. )
		call	tapp2            ; ( r p )

		_dup	                 ; ( r p p )
		push	edi

		; Dump 16 bytes as hex numbers
		mov	edi, eax
		call	dump$.16bytes    ; ( r p x )

		_lit	"| "
		call	tapp2

		; Dump 16 bytes as ASCII
		mov	edi, [esi]
		call	dump$.16ascii

		pop	edi
		_drop	                 ; ( r p )

		add	eax, 16          ; ( r p+16 )
		_push	                 ; ( r )
		_dup	                 ; ( r r )
		call	tprint           ; ( r )
		inc	eax              ; ( r+1 )
		_pop	                 ; ( r+1 p+16 )
		ret

;==========================================================================
dump$: ; INTERNAL ROUTINES
;==========================================================================

.16ascii:
		call	.8ascii
		;jmp	.8ascii

.8ascii:
		call	.4ascii
		;jmp	.4ascii

.4ascii:
; (  --  ) Loads EAX from pointer in EDI, appends EAX to T as is, adds four
;	to EDI.
		_dup
		mov	eax, [edi]
		call	tapp4
		add	edi, 4
		ret

;--------------------------------------------------------------------------

.16bytes:
		call	.8bytes
		;jmp	.8bytes

.8bytes:
		call	.4bytes
		;jmp	.4bytes

.4bytes:
		call	.2bytes
		;jmp	.2bytes

.2bytes:
		call	.1byte
		;jmp	.1byte

.1byte:
; ( x -- x ) (Assumes TOS is junk and can be overwritten)
;	Loads AL from pointer in EDI, converts AL to hex number, appends
;	hex digits to T (followed by space), increments EDI.
		mov	al, [edi]
		call	dump$.tappbyte
		_lit	" "
		call	tapp1 ; assumes tapp1 doesn't alter EDI
		inc	edi
		ret

;--------------------------------------------------------------------------

.tappdword:
; ( n -- n ) Convert number into eight-hex-digit string and append it to T.
;	RETURN ERROR if T doesn't have room for eight more characters.

		_lit	8
		call	tfit
		jc	.x1

		call	.rollhex
		call	.rollhex
		call	.rollhex
		call	.rollhex
		call	.rollhex
		call	.rollhex
		call	.rollhex
		call	.rollhex

	.x1:	ret

.tappbyte:
; ( n -- n ) Convert number in AL into two-hex-digit string and append it
;	to T.
;	RETURN ERROR if T doesn't have room for two more characters.

		_lit	2
		call	tfit
		jc	.x1

		ror	eax, 4
		call	.tappnybble
		; jmp	.rollhex

.rollhex:
; ( v -- v2 ) Convert top 4 bits in value "v" to a character, and append
;	the character to T. Return "v" rotated left four bits (so that
;	0x01234567 comes out as 0x12345670).

		rol	eax, 4
		;jmp	.tappnybble

.tappnybble:
; ( v -- v ) Convert bottom 4 bits in value "v" to a character, and append
;	the character to T. Return "v" unchanged.

		_dup
		and	eax, byte 0x0F
		mov	al, [.hexdigits + eax]
		jmp	tapp1

.hexdigits: db "0123456789ABCDEF"

