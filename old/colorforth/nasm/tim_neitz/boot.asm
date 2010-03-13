[ORG 0]
[BITS 16]
start: 	jmp  short start0
    	nop

    	db 'cmcf 1.0'
    	dw 512     				; bytes/sector
    	db 1       				; sector/cluster
    	dw 1       				; sector reserved
    	db 2       				; FATs
    	dw 16*14   				; root directory entries
    	dw 80*2*18 				; sectors
    	db 0x0F0    				; media
    	dw 9       				; sectors/FAT
    	dw 18      				; sectors/track
    	dw 2       				; heads
    	dd 0       				; hidden sectors
    	dd 80*2*18 				; sectors again
    	db 0       				; drive
command:
	db 0
      db 0   					; head, drive
cylinder: 
	db 0
      db 0   					; head
      db 1   					; sector
      db 2   					; 512 bytes/sector
      db 18  					; sectors/track
      db 0x1b 					; gap
      db 0x0ff
gdt:
	dw 0x17
    	dd gdt0
	
	align 4
nc:	dd 9 					; Forth+Icons+blocks 24-161
	align 8
gdt0:
	dw 0, 0, 0, 0
    	dw 0x0FFFF, 0, 0x9A00, 0x0CF 		; code
    	dw 0x0FFFF, 0, 0x9200, 0x0CF 		; data
; ------------------------------------------------------
start0:
	mov ax, 0x7c20				; executed from 7c00
	mov es, ax
	xor di, di					; at the end of sector
	mov ax, 0x4f01
	mov cx, vesa+0x4000			; ask for linear buffer
	int 0x10

	mov ax, 0x4F02 				; Video mode
    	mov bx, vesa+0x4000			; hp*vp rgb: 565
    	int 0x10
	mov ebp, [es:di+40]; this?
; -------------------
;	mov ax, 0x4F02 				; Video mode
;    	mov bx, vesa+0x4000			; hp*vp rgb: 565
;    	int 0x10
	;mov ebp, [es:di+40] ; or this?
; ------------------
    	cli
    	xor eax,eax					; Move code to 0
	mov ebx,eax
	mov bx,cs
	mov ds,bx
	mov es,ax
	mov edi,eax
	mov esi,eax
    	call $+3 				; Where are we? IP+4*CS
loc: 
	pop si
    	sub si, loc-start
    	mov cx, 512/4
    	rep movsd
	jmp 0x0:relocate

relocate:
	mov ds,ax
	lgdt [gdt]
	mov al,0x1
	mov cr0,eax
	jmp 0x8:protected

[BITS 32]
protected:
	mov al, 0x10
	mov ds, eax
	mov es, eax
	mov ss, eax
	mov esp, Gods
	xor ecx, ecx

A20:
	mov al, 0xd1
	out 0x64, al
.a20:	in al, 0x64
	and al, 2
	jnz .a20
	mov al, 0x4b
	out 0x60, al

	call dma
	shl ebx, 4
	add esi, ebx
	cmp word [esi], 0x4444			; boot?
	jnz cold

      mov cx, 63*0x100-0x80			; No
	rep movsd
	mov esi, Godd
	jmp short start2






cold:
	call sense_
	jns cold
	mov esi, Godd
	xor edi, edi				; Cylinder 0 on addr 0
	mov cl, byte [nc]
.cold:  push ecx
	call read
	inc byte [cylinder]
	pop ecx
	loop .cold
start2: call stop
	jmp start1

us:	equ 1000/6
ms:	equ 1000*us

spin:
	mov cl, 0x1c
	call onoff
.spin:  call sense_
	jns .spin
	mov byte [cylinder], 0			; calibrate
	mov al, 7
	mov cl, 2
	call cmd
	mov ecx, 500*ms
.sp:	loop .sp
cmdi:	call sense_
	js cmdi
	ret

ready:
	mov dx, 0x3f4
.re:	in al, dx
	out 0xe1, al
	shl al, 1
	jnc .re
	lea edx, [edx+1]
	ret

transfer:
	mov cl, 9
cmd:
	lea edx, [command]
	mov [edx], al
cmd0:
	push esi
	mov esi, edx
.cm1:	call ready
	jns .cm2
	   in al, dx
	   jmp .cm1
.cm2:	lodsb
	out dx, al
	out 0xe1, al
	loop .cm1
	pop esi
	ret

sense_:
	mov al, 8
	mov ecx, 1
	call cmd
.se:	call ready
	jns .se
	in al, dx
	out 0xe1, al
	and al, al
	ret

seek:
	call sense_
	jns seek
	ret

stop:						; Motor off
	mov cl, 0xc
onoff:
	DUP_
	mov al, cl
	mov dx, 0x3f2
	out dx, al
	out 0xe1, al
	DROP
	ret

dma:
	mov word [command+1], 0x3a2		; 12 s6 u32 ms
	mov al, 3				; timing
	mov cl, 3
	call cmd
	mov word [command+1], 0x7000		; +seek -fifo -poll
	mov al, 0x13				; configure
	mov cl, 4
	call cmd
	mov dword [command], ecx
	ret

read:
	call seek
	mov al, 0xe6				; read normal data
	call transfer
	mov cx, 18*2*512
.rea:	call ready
	in al, dx
	out 0xe1, al
	stosb
	dec ecx
	jnz .rea
	ret

write:
	call seek
	mov al, 0xc5				; write data
	call transfer
	mov cx, 18*2*512
.wri:	call ready
	lodsb
	out dx, al
	out 0xe1, al
	dec ecx
	jnz .wri
	ret

	times 512-2-($-$$) db 0
	dw 0xaa55
	dd 0x44444444

flop:
	mov byte [cylinder], al
	DUP_
	mov dx, 0x3f2
	in al, dx
	out 0xe1, al
	test al, 0x10
	jnz .flo
	jmp spin
.flo	ret

readf:
	call flop				; ac-ac
	push edi
	mov edi, [esi+4]
	shl edi, 2
	call read
	pop edi
readf1: DROP
	inc eax
	add dword [esi], 0x1200
	ret

writef:
	call flop				; ac-ac
	push esi
	mov esi, [esi+4]
	shl esi, 2
	call write
	pop esi
	jmp short readf1

seekf:
	call flop				; c-c
	call seek
	mov al, 0xf
	mov cl, 3
	call cmd
	call cmdi
	DROP
	ret

cmdf:
	mov ecx, eax				; an
	DROP
	lea edx, [eax*4]
	call cmd0
	DROP
	ret

readyf:
	DUP_
	call ready
	DROP
	ret