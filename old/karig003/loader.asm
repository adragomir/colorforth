;==========================================================================
; LOADER.ASM
; Code loaded by the boot sector
;==========================================================================

; ------ PUT CODE HERE.
		call	cls
		_lit	0
		_lit	sampletext
		call	dump16
		call	dump16
		call	dump16
		call	dump16
		call	dump16
		call	dump16
		call	dump16
		call	dump16
		call	refresh

; ------ Halt computer.
		jmp	short $

sampletext:
		db	"This is a sample of text. "
		db	"The quick brown fox jumps over the lazy dog. "
		db	"PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS. "

		dd	0x01234567, 0x89ABCDEF
		db	0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF

;==========================================================================
