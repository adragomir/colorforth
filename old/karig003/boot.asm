;==========================================================================
; BOOT.ASM
; This code prepares the Korth virtual machine. Code in other files expects
; data to be passed and returned via the Korth data stack.
;==========================================================================

%define BOOTING_FROM_FLOPPY     1
%define STARTING_PROTECTED_MODE 1

[ORG 0x0800]
; (Boot sector is loaded at 0x7C00 but moves itself.)

[BITS 16]

boot_stage_1: ; CODE EXECUTED FROM ADDRESS 0x7C00

; ------ Set up real-mode segment registers.
		xor	ax, ax
		mov	ds, ax
		mov	es, ax
		mov	fs, ax
		mov	gs, ax

; ------ Set up real-mode call stack.
		cli
		mov	ss, ax
		mov	sp, 0x0800
		mov	di, sp
		sti

; ------ Move this boot sector lower in memory.
		cld
		mov	cx, 256
		mov	si, 0x7C00
		rep	movsw

; ------ Jump to new location.
		jmp	0:0x0800 + (boot_stage_2 - boot_stage_1)

boot_stage_2: ; CODE EXECUTED FROM ADDRESS 0x0800

; System loader is fixed at 9KB (18 sectors), the size of a single track on
; a floppy disk.

%if BOOTING_FROM_FLOPPY

		mov	ax, 0x0200 + 17  ; function 2 -- read 17 sectors
		mov	bx, 0x0800 + 512 ; buffer follows boot sector
		mov	cx, 2            ; cylinder 0 (CH), sector 2 (CL)
		xor	dx, dx           ; head 0 (DH), drive 0 (DL)
		int	0x13
		; Check for errors.

%else

		; Get partition data first!

%endif

%if STARTING_PROTECTED_MODE
		%include "protmode.asm"
%else
		%include "realexp.asm"
%endif

; ------ (Required to make this a boot sector.)
		times	508 - ($-$$) db 0
		jmp	short $+4
		db	0x55, 0xAA

;==========================================================================
