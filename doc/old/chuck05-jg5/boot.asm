[ORG 0]						; actually 7c00
[BITS 16]
start:
	jmp short start0
	nop

	db 'cmcf 1.0'
	dw 512					; bytes/sector
	db 1					; sector/cluster
	dw 1					; sector reserved
	db 2					; FATs
	dw 16*14				; root directory entries
	dw 80*2*18				; sectors
	db 0xf0					; media (3.5 inch, double-sided)
	dw 9					; sectors/FAT
	dw 18					; sectors/track
	dw 2					; heads
	dd 0					; hidden sectors
	dd 80*2*18				; sectors again
	db 0					; drive

command:
	db 0
	db 0					; head << 2 + drive
cylinder:
	db 0
	db 0					; head
	db 1					; sector (1 based)
	db 2					; 512 bytes/sector (2^(7+N) bytes)
	db 18					; sectors/track
	db 0x1b					; gap (not used, but 0x1b is standard value)
	db 0xff					; length (also not used)

; address 0000002E


gdt:
	dw 0x17
	dd gdt0

	align 4	; should already be aligned.
		; but nc is a variable, so make sure.
; number of cylinders to load.
; A cylinder is 18 KB (512 bytes * 18 sectors * 2 sides)
nc:	dd 4	; load 4*18 = 72 blocks

	align 8
gdt0:
	dw 0, 0, 0 ,0
code	equ $-gdt0
	dw 0xffff, 0, 0x9a00, 0x0cf	; 0 to 4GB, read/execute
data	equ $-gdt0
	dw 0xffff, 0, 0x9200, 0x0cf	; 0 to 4GB, read/write


start0:
	mov ax, 0x4f02				; service (set video mode)
	mov bx, vesa				; mode number
	int 0x10
	cli
	; Move the bootsector down from the load address to 0
	xor eax, eax
	mov ebx, cs
	mov ds, bx
	mov es, ax
	mov edi, eax
	mov esi, eax
	call .0					; compute load address
.0:	pop si
	sub si, .0-start
	mov cx, 512/4
	rep movsd
	jmp 0:relocated

relocated:
	lgdt [es:gdt]
	mov al, 1
	mov cr0, eax
	jmp code:protected

[BITS 32]
protected:
	mov al, data
	mov ds, eax
	mov es, eax
	mov ss, eax
	mov esp, mains
	xor ecx, ecx

A20:
	mov al, 0xd1
	out 0x64, al
.0:	in al, 0x64
	and al, 2
	jnz .0
	mov al, 0x4b
	out 0x60, al

	mov eax, 0x47ff
	call dma
	; This code (down to cold) is unnecessary unless you want
	; to run this as a .com file under DOS.
	shl ebx, 4				; bx = old cs
	add esi, ebx
	cmp dword [esi], 0x44444444		; Were we run as a .COM file?
	jne cold
	mov cx, (63*1024 - 512)/4		; Yes, copy the rest of it down from
	rep movsd				; wherever we were loaded.
	jmp short start2
	; No, we were booted from a floppy, load rest of cf from there.
cold:
	xor edi, edi				; start loading at address 0
				; if your source blocks are shifted down,
	call read		; you need to comment out this line,
	inc byte [cylinder]	; this line,
	mov cl, [nc]
	dec cl			; and this line
.0:	push ecx
	call read
	pop ecx
	loop .0
start2:	call stop
	mov esi, maind
	jmp start1

us:	equ 900/6
ms:	equ 1000*us

spinup:
	mov al, DRIVE0 + MOTOR0_ON
	call motor
	mov ecx, 500*ms
.0:	loop .0
	mov al, CMD_CALIBRATE
	mov cl, LEN_CALIBRATE
	jmp short cmd_wait

; IN: none
; OUT: dx = port number for commands/data
ready:
	mov dx, FDC_STATUS
.0:	in  al, dx
	DELAY
	shl al, 1    ; check high bit (1 = data ready)
	jnc .0
	inc edx      ; set dx to FDC_DATA
	test al, al  ; check next bit (transfer direction: 1 = in to computer)
	ret

; IN: al = command
cmd:
	lea edx, [command]
	mov [edx], al
cmd0:
	push esi
	mov esi, edx
.0:	call ready
	jns .1		; it wants to send us data, read until it switches directions.
	in  al, dx
	DELAY
	jmp .0
.1:	lodsb
	mov ah, 0x1e
	out dx, al
.2:	DELAY
	dec ah
	jnz .2
	loop .0
	pop esi
	ret

; IN: none
; OUT: Z = all clear, NZ = active interrupt.
idle:
	mov al, CMD_SENSE_INT
	mov cl, LEN_SENSE_INT
	call cmd
	call ready
	in  al, dx
	DELAY
	cmp al, NO_INTERRUPT
	ret

; IN: al = DMA mode
seek:
	out DMA_MODE, al
.0:	call idle		; wait until idle, then send SEEK command
	jne .0
	mov al, CMD_SEEK
	mov cl, LEN_SEEK
cmd_wait:
	call cmd
.0:	call idle		; wait for an interrupt (signaling completion)
	je .0
	ret

stop:
	mov dword [trash], buffer
	mov al, DRIVE0 + MOTOR0_OFF

motor:
	mov dx, FDC_DRIVES
	mov ah, 15
	out dx, al
.0:	DELAY
	dec ah
	jnz .0
	ret

; IN: ax = byte count for DMA channel 2
dma:
	out 5, al		; set byte count
	mov al, ah
	out 5, al
	mov eax, buffer		; set address
	out 4, al
	mov al, ah
	out 4, al
	shr eax, 16
	out 0x81, al
	mov al, 0xb
	out 0xf, al		; DMA write mask register??
	; set floppy drive timings
	mov word [command+1], 0x2a1	; l2 s6 u16 ms
	mov al, CMD_SET_TIMINGS
	mov cl, LEN_SET_TIMINGS
	call cmd
	mov word [command+1], 0		; reset command buffer.
	ret
 
transfer:
	mov cl, LEN_RD_WR
	call cmd
	inc byte [cylinder]
.0:	call ready	; wait until its ready to send command response
	jns .0
	ret

; read a cylinder (18KB) from the disk to EDI.
read:
	mov al, DMA_IN
	call seek
	mov al, CMD_READ
	call transfer
	push esi
	mov esi, buffer
	mov ecx, bufsize/4
	rep movsd
	pop esi
	ret

	times (512-2) - ($-$$) db 0
	dw 0xaa55

; END OF BOOT SECTOR

	dd 0x44444444

; write a cylinder (18KB) at ESI to the disk.
write:
	mov edi, buffer
	mov ecx, bufsize/4
	rep movsd
	mov al, DMA_OUT
	call seek
	mov al, CMD_WRITE
	jmp transfer

flop:	; ( cyl - cyl scratch )
	mov [cylinder], al
	DUP_
	mov dx, FDC_DRIVES
	in al, dx
	DELAY
	test al, MOTOR0_ON
	jnz .9
	jmp spinup
.9:	ret

readf:
	call flop				; ac-ac
	push edi
	mov edi, [esi+4]
	shl edi, 2
	call read
	pop edi
nextcyl:
	DROP				; drop scratch value
	inc eax				; increment cylinder number
	add dword [esi], bufsize/4	; and data address
	ret

writef:
	call flop				; ac-ac
	push esi
	mov esi, [esi+4]
	shl esi, 2
	call write
	pop esi
	jmp nextcyl

stopf:
	DUP_
	call stop
	DROP
	ret

formatf_:
	mov al, DMA_OUT
	call seek
	mov edx, esi
	mov cl, 6
	call cmd0
.0:	call ready
	jns .0
	ret

formatf:
	push eax
	mov eax, 0x47
	call dma
	pop eax
	call flop
	push esi
	mov esi, [esi+4]
	shl esi, 2
	call formatf_
	pop esi
	mov eax, 0x47ff
	call dma
	DROP
	ret

; vim:ts=8:sw=8
