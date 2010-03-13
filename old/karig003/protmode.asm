;==========================================================================
; PROTMODE.ASM
; Code to prepare for and start protected mode/VESA graphic screen.
; Included into the boot sector. Can be left out to conduct experiments in
; real mode. (Once REALEXP.ASM is obsolete, this code can be moved back
; into BOOT.ASM.)
;==========================================================================

; ------ Delay for three seconds.
;        (Gives floppy controller time to finish BEFORE
;        we clear interrupts and enter protected mode.)
		xor	cx, cx
		xor	dx, dx
		mov	ah, 1
		int	0x1A     ; set system timer to zero

	.zz:	xor	ah, ah
		int	0x1A
		cmp	dx, 18*3 ; Timer ticks 18.2 times a second.
		jl	.zz

; ------ Enable graphic screen: 800x600, 64K colors
		mov	bx, 0x4111 ; mode 0x111, linear, clear memory
		mov	ax, 0x4F02
		int	0x10

; ------ Enable A20 line. (Method used in ColorForth)
		cli
		in	al, 0x70
		or	al, 0x80
		out	0x70, al

		mov	al, 0xD1
		out	0x64, al
	.20:	in	al, 0x64
		and	al, 2
		jnz	.20
		mov	al, 0x4B
		out	0x60, al
		; No "sti" here.

; ------ Load GDT and enter protected mode.
		lgdt	[gdt]
		mov	eax, cr0
		or	al, 1
		mov	cr0, eax
		jmp	dword 8:pmstart
[BITS 32]
pmstart:
		mov	eax, dseg-gdt
		mov	ds, eax
		mov	es, eax
		mov	fs, eax
		mov	gs, eax
		mov	ss, eax

; ------ Set up Korth machine: two call stacks, two data stacks.
		mov	esp, gods
		mov	esi, godd

; ------ Finish setting up the system.
		jmp	loader ; defined in memmap.asm

;--------------------------------------------------------------------------
; DATA
;--------------------------------------------------------------------------

gdt:		dw	0x17 ; GDT limit (entry size * 3 - 1)
		dd	gdt
		dw	0
cseg:		dw	0xFFFF, 0, 0x9A00, 0x00CF ; code
dseg:		dw	0xFFFF, 0, 0x9200, 0x00CF ; data

;==========================================================================
