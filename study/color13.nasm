; color13.asm		28Oct07
; colorforth assembled with nasm
;
; mods by Mark Tillotson:
;	800x600
;	to allow variable sized icons:
;		idw & idh instead of fixed sizes of 16 & 24
;	added bits8
;	std kbd from 'NASM (std kbd) 800x600' color.asm
; Rev 1.0  - note arbitrary start for version control
;	   - added comments only? may have moved some things to/from gen.asm
; Rev 1.1 - moved all equates and data together at begining.
;	  - changed flow of 'letter' & 'getkey'
;	  - included boot and video code in this file
;	  - modified keyscan so that if a key is to be ignored, it checks for
;		another key before calling pause.
; Rev 1.2 - started process of moving all words above minimum into colorForth
;		source code.
; Rev 1.3 - continue process, add edjmp and fix display of command keys
;------------------------------------------------------
;
; Register usage:
; 0 EAX: stack (1st number on Data stack): TOS
; 1 ECX: string counter, scratch
; 2 EDX: address register A, I/O port, scratch
; 3 EBX: unused
; 4 ESP: byte pointer to top of Return stack
; 5 EBP: unused
; 6 ESI: byte pointer to 2nd number on Data stack: NOS
; 7 EDI: dword pointer to next word to be interpreted
;

%macro	NEXT 1
	dec ecx
	jnz %1
%endmacro

%macro	DUP_	0
	lea esi,[esi-4]
	mov [esi], eax
%endmacro

%macro	DROP	0
	lodsd
%endmacro

; ----------------- System Equates --------------------

cpl	equ 5			; cycles per loop for 'loop' instruction (3 cycles on a 2GHz Athlon)
cpuclk	equ 166			; CPU clock in MHz
ms:	equ cpuclk*1000/cpl	; initial count per ms for loop

hp	equ 800			; screen horizontal pixels
vp	equ 600			; screen vertical pixels
bpp	equ 2			; bytes per pixel
vesa 	equ 0x114+0x4000	; 0x4000, bit 14 set is: linear buffer, 0x114 is: 800x600 16 bit rgb color: 5 bits red,6 green,5 blue
				; 0x4117 is 1024x768 16 bit rgb color: 5 bits red,6 green,5 blue with linear buffer

; Memory map:
;
vidpage	equ 0xc0000000		; (in C&T 65554)
framest	equ 0x1000000-hp*vp*bpp	; 0xf15a00 assume 16 mb (would be 0x2000000-hp*vp*bpp if 32 mb)
inith	equ 0x40000*4		; 0x100000 (compiled code start)
gods	equ 0x28000*4		; 0x0a0000 god (graphic output display) stack
godd	equ gods-750*4		; 0x09f448 god data
mains	equ godd-1500*4		; 0x09e890 main stack
maind	equ mains-750*4		; 0x09dcd8 main data
buffer	equ 604*256		; 0x097000 (after *4 before use) leaves room for 604 blocks
				; 0x004800 block 18 (source code)
				; 0x003000 icons data
				; 0 kernel code starts from 0

idw	equ 8			; icon data width
idh	equ 12			; icon data height
hs	equ 1			; horizontal spacing
vs	equ 1			; vertical spacing
iw	equ idw+hs		; =9 icon width with spacing
ih	equ idh+vs		; =13 icon height with spacing
hc	equ (hp-2*hs)/iw	; =88 horizontal characters per line
vc	equ (vp-2*vs)/ih	; =46 vertical characters rows
xwrd	equ 0x10000		; multiply by this to put in x word

_white	equ 0xffffff
_red	equ 0xff0000
_green	equ 0x00ff00
_blue	equ 0x0000ff
_yellow	equ 0xffff00
_magenta equ 0xff00ff
_cyan	equ 0x00ffff
_silver	equ 0xc0c0c0
_dgreen equ 0x00c000
_dyellow equ 0xc0c000

op_drop		equ 0xad		; lodsd
op03_dup	equ 0x89fc768d		; lea esi, [esi-4]	mov [esi], eax (with op4_dup byte)
op4_dup		equ 0x06		; (last byte of dup opcode sequence from above)
op_mov_eax_n	equ 0xb8		; mov eax,nnnn (32 bit immediate)
op_call		equ 0xe8		; call xxxx (32 bit address)
op_ret		equ 0xc3		; ret

; ------------------- boot code ------------------------
;
; Floppy boot segment
;

[ORG 0]				 		; actually 7c00
[BITS 16]
start: 	jmp  short start0
    	nop

    	db 'cmcf 1.3'
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
        db 0x1b 				; gap
        db 0x0ff

gdt:
	dw 0x17
    	dd gdt0
	
	align 4					; should already be aligned
nc:
	dd 9 					; Forth+Icons+blocks 24-161
	
	align 8					; should already be aligned
gdt0:
	dw 0, 0, 0, 0
    	dw 0x0FFFF, 0, 0x9A00, 0x0CF 		; code
    	dw 0x0FFFF, 0, 0x9200, 0x0CF 		; data

start0:
	mov ax, 0x7c20				; executed from 7c00
	mov es, ax
	xor di, di				; at the end of sector
	mov ax, 0x4f01				; 0x4f01 is: vesa mode_info
	mov cx, vesa				; vesa mode
	int 0x10				; vesa call
	mov ax, 0x4f02				; 0x4f02 is: vesa set_mode
    	mov bx, vesa				; vesa mode
    	int 0x10				; vesa call
	mov ebp, [es:di+40]			; PhysBasePtr is 40th byte in returned mode_info block
    	cli					; disable interupts
    	xor eax,eax				; Move code to 0
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
	mov al, 0x10				; now running in relocated code
	mov ds, eax
	mov es, eax
	mov ss, eax
	mov esp, gods
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
	mov esi, godd
	jmp short start2
	  
cold:
	call sense_
	jns cold
	mov esi, godd
	xor edi, edi				; Cylinder 0 on addr 0
	mov cl, byte [nc]
.cold:  push ecx
	call read
	inc byte [cylinder]
	pop ecx
	loop .cold
start2: call stop
	jmp start1

spin:
	mov cl, 0x1c				; turn motor on
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

; wait for the drive to be ready
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
	mov [edx], al				; set direction of data transfer (read or write)
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

stop:						; turn motor off
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

; read one cylinder
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

;write one cylinder
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

	times 512-2-($-$$) db 0			; ends up not doing anything because boot sector is full 
	dw 0xaa55
; --------------------- End Boot Sector ----------------------------
; --------------------- Start Data Section --------------------------
	ALIGN 4
	dd 0x44444444
me:	dd god
screen:	dd 0					; routine to paint screen
xy:	dd hs*xwrd+vs
lm:	dd hs
rm:	dd hc*iw
xycr:	dd 0
fov:	dd 10*(2*vp+vp/2)			; 25*vp ???
lit:	dd adup
mk:	dd 0, 0, 0
h:	dd inith				; compiled code starts at 1mb
last:	dd 0
class:	dd 0
list:	dd 0, 0
macros: dd 0
forths: dd 0
board:	dd 0					; 16 characters to display above command line
shift:	dd alpha0				; 3 characters to display at right end of command line
						;   also used to determine input mode
base:	dd 10
current:
	dd decimal
keyc:	dd _yellow
chars:	dd 1
aword:  dd ex1
anumber:
	dd nul
words:	dd 1
shifted:
	dd 0
bas:	dd dot10
blk:	dd 18,18
curs:	dd 0
cad:	dd 0
pcad:	dd 0
lcad:	dd 0
trash:	dd buffer*4
frame:	dd framest
displ:	dd vidpage
fore:	dd 0xf7de
xc:	dd 0
yc:	dd 0

macro0:
	dd (170q<<25)						; ';'
	dd ((((140q<<7)+146q)<<7)+142q)<<11			; dup
	dd ((((((177q<<7)+140q)<<7)+146q)<<7)+142q)<<4		; ?dup
	dd ((((((140q<<4)+1)<<4)+3)<<7)+142q)<<10		; drop
	dd ((((((2<<7)+144q)<<4)+4)<<4)+6)<<13			; then
	dd ((((((((143q<<4)+4)<<5)+25q)<<4)+7)<<4)+6)<<8	; begin
macro1:
	times 128 dd 0

forth0:
	dd ((((((143q<<4)+3)<<4)+3)<<4)+2)<<13			; boot
	dd ((((((27q<<4)+5)<<4)+1)<<5)+21q)<<14			; warm
	dd ((((((((142q<<4)+5)<<7)+146q)<<5)+20q)<<4)+4)<<5	; pause
	dd ((((((((21q<<4)+5)<<5)+22q)<<4)+1)<<4)+3)<<10	; MACRO
	dd ((((((((26q<<4)+3)<<4)+1)<<4)+2)<<7)+144q)<<8	; FORTH
	dd (22q << 27)						; c
	dd ((((((20q << 4)+2)<< 4)+3)<< 7)+142q)<< 12		; stop
	dd ((((((1 << 4)+4)<< 4)+5)<< 7)+140q)<< 13		; read
	dd ((((((((27q << 4)+1)<< 4)+7)<< 4)+2)<< 4)+4)<< 11	; write
	dd ((6 << 5)+22q)<< 23					; nc
	dd ((((((((((22q<<4)+3)<<5)+21q)<<5)+21q)<<4)+5)<<4)+6)<<5
								; comman(d)
	dd ((((((20q<<4)+4)<<4)+4)<<7)+164q)<<12		; seek
	dd ((((((((1<<4)+4)<<4)+5)<<7)+140q)<<5)+23q)<<8	; ready
	dd ((((5<<5)+22q)<<4)+2)<<19				; act
	dd ((((((20q<<7)+144q)<<4)+3)<<5)+27q)<<11		; show
	dd ((((((24q<<4)+3)<<4)+5)<<7)+140q)<<12		; load
	dd ((((((144q<<4)+4)<<4)+1)<<4)+4)<<13			; here
	dd ((((((177q<<5)+24q)<<4)+7)<<4)+2)<<12		; ?lit
	dd ((153q<<7)+176q)<<18					; 3,
	dd ((152q<<7)+176q)<<18					; 2,
	dd ((151q<<7)+176q)<<18					; 1,
	dd (176q<<25)						; ,
	dd ((((((24q<<4)+4)<<5)+20q)<<5)+20q)<<13		; less
	dd ((((((162q<<7)+146q)<<5)+21q)<<7)+142q)<<6		; jump
	dd ((((((((((5<<5)+22q)<<5)+22q)<<4)+4)<<7)+142q)<<4)+2)<<3
								; accept
	dd ((((142q<<4)+5)<<7)+140q)<<14			; pad
	dd ((((((((4<<4)+1)<<4)+5)<<5)+20q)<<4)+4)<<11		; erase
	dd ((((((22q<<4)+3)<<7)+142q)<<5)+23q)<<11		; copy
	dd ((((((21q<<4)+5)<<4)+1)<<7)+164q)<<12		; mark
	dd ((((((4<<5)+21q)<<7)+142q)<<4)+2)<<12		; empt
	dd ((((((4<<5)+21q)<<4)+7)<<4)+2)<<15			; emit
	dd ((((((((140q<<4)+7)<<5)+25q)<<4)+7)<<4)+2)<<8	; digit
	dd ((((((((152q<<4)+4)<<5)+21q)<<4)+7)<<4)+2)<<8	; 2emit
	dd (165q<<25)						; .
	dd ((144q<<7)+165q)<<18					; h.
	dd ((((144q<<7)+165q)<<4)+6)<<14			; h.n
	dd ((22q<<4)+1)<<23					; cr
	dd ((((((((20q<<7)+142q)<<4)+5)<<5)+22q)<<4)+4)<<7	; space
;	dd ((((((140q<<4)+3)<<5)+27q)<<4)+6)<<12		; down
	dd ((((((4<<7)+140q)<<4)+7)<<4)+2)<<13			; edit
	dd (4<<28)						; e
	dd ((24q <<5)+21q)<<22					; lm
	dd ((1<<5)+21q)<<23					; rm
	dd ((((((((25q<<4)+1)<<4)+5)<<7)+142q)<<7)+144q)<<5	; graph(ic)
	dd ((((((2<<4)+4)<<7)+145q)<<4)+2)<<13			; text
	dd ((((((((164q<<4)+4)<<5)+23q)<<7)+143q)<<4)+3)<<5	; keybo(ard)
	dd ((((((140q<<4)+4)<<7)+143q)<<7)+146q)<<7		; debu(g)
	dd ((5<<4)+2)<<24					; at
	dd ((((173q<<4)+5)<<4)+2)<<17				; +at
	dd ((145q<<5)+23q)<<20					; xy
	dd ((((26q<<4)+3)<<7)+141q)<<16				; fov
	dd ((((((26q<<4)+7)<<5)+26q)<<4)+3)<<14			; fifo
	dd ((((143q<<4)+3)<<7)+145q)<<14			; box
	dd ((((((24q<<4)+7)<<4)+6)<<4)+4)<<15			; line
	dd ((((((((22q<<4)+3)<<5)+24q)<<4)+3)<<4)+1)<<10	; color
;	dd ((((((((((3<<5)+22q)<<4)+2)<<4)+5)<<4)+6)<<4)+2)<<7	; octant
	dd ((20q<<7)+142q)<<20					; sp
	dd ((((((24q<<4)+5)<<5)+20q)<<4)+2)<<14			; last
	dd (((((((((146q<<4)+6)<<7)+142q)<<4)+5)<<5)+22q))<<5	; unpac(k)
forth1:
	times 512 dd 0

macro2:
	dd semi
	dd cdup
	dd qdup
	dd cdrop
	dd then
	dd begin

	times 128 dd 0

forth2:
	dd boot
	dd warm
	dd dopause
	dd macro_
	dd forth
	dd c_
	dd stop
	dd readf
	dd writef
	dd nc_
	dd cmdf
	dd seekf
	dd readyf
	dd act
	dd show
	dd load
	dd here
	dd qlit
	dd comma3
	dd comma2
	dd comma1
	dd comma
	dd less
	dd jump
	dd accept
	dd pad
	dd erase
	dd copy
	dd mark
	dd empty
	dd emit
	dd edig
	dd emit2
	dd dot10
	dd hdot
	dd hdotn
	dd cr
	dd space
;	dd down
 	dd edit
	dd e
	dd lms
	dd rms
	dd graphic
	dd text1
	dd keyboard
	dd debug
	dd at_
	dd pat
	dd xy_
	dd fov_
	dd fifof
	dd box
	dd line
	dd color
;	dd octant
	dd sps
	dd last_
	dd unpack

	times 512 dd 0

; command key (Esc=0,backspace=1,spacebar=2,Alt=3) routines and display characters when entering:
;   first letter entered
alpha0:
	dd nul0, nul0, number, nul0
	db  0, 41q, 0, 0			;   9
						; note that there are 4 db per line, so it stays word aligned
; succesive letters
alpha1:
	dd word0,   x,  lj, nul0
	db  25q, 41q, 0, 0			; x 9 
; 1st number digit
numb0:
	dd nul0, minus, alphn, octal
	db  43q, 65q,  16q , 0			; - A f
; succesive number digits
numb1:
	dd number0,  xn, endn, number0
	db   25q, 65q, 76q, 0			; x A F

; spaces is 16 routines for color bits to index into (adefine is the define word and gets changed between macrod and forthd)
spaces:	dd qignore, execute, num		; extension word, yellow word, yellow number
adefine:
	dd macrod				; red word
	dd qcompile, cnum, cshort, compile	; green word, green number, green short number, cyan word
	dd short_, nul, nul, nul		; yellow short number, white comment, white Comment, white COMMENT
	dd variable, nul, nul, nul		; magenta word, nul, nul, nul

; display is 16 routines for color bits to index into
display:
	dd type0, ww, nw, rw
	dd gw, gnw, gsw, mw
	dd sw, text, cap, caps
	dd var, nul, nul, nul

; powers of ten for output of decimal numbers
tens:
	dd 10, 100, 1000, 10000, 100000, 1000000
	dd 10000000, 100000000, 1000000000

; initial key functions in editor
ekeys:	dd nul, eout, shadow, act3		; 0-3
	dd act4, act1, actv, act7		; 4-7
	dd act9, act10, act11, edjmp		; 8-11		; F8 should now be 'jump'
	dd nul, nul, nul, hcur			; 12-15
	dd mmcur, mblk, nul, mcur		; 16-19
	dd nul, pcur, nul, shadow		; 20-23
ekbd0:	dd ppcur, pblk, destack, del		; 24-28 ; this row is used as a dummy row for ekbd0
	db  0 , 64q, 10q,  0			; note that there are 4 db per line, so it stays word aligned

; characters to display in the key map (keyboard only displays the 12 function keys)
ekbd:	db  1 , 15q, 17q, 11q
	db 12q,  2 , 72q, 70q
	db  0 ,  0 ,  0 ,  0

; 12 colors
actc:	dd _yellow, 0, _red, _dgreen, 0, 0, _cyan
	dd 0, _white, _white, _white, _blue

; pointer to data structure of 28 routines and 28 characters (bytes) for pad
vector: dd 0

; scan code to colorforth character codes
; the codes are huffman compressed etc...
; -1 for backspace/esc, -2 for return/space
; and -3 for alt.
keys:
	dw 0x0000				;  0
	dw 0xffff, 0x2a19, 0x2c1a, 0x001b	;  1  esc !1 @2 #3
	dw 0x001c, 0x001d, 0x001e, 0x001f	;  5  $4 %5 ^6 &7
	dw 0x2d20, 0x0021, 0x0018, 0x0023	;  9  *8 (9 )0 _-
	dw 0x2b00, 0xffff, 0x0000, 0x1717	;  d  += bs tab Qq
	dw 0x0f0f, 0x0404, 0x0101, 0x0202	; 11  Ww Ee Rr Tt
	dw 0x0b0b, 0x1616, 0x0707, 0x0303	; 15  Yy Uu Ii Oo
	dw 0x1212, 0x0000, 0x0000, 0xfefe	; 19  Pp {[ }] enter
	dw 0x0000, 0x0505, 0x0808, 0x1010	; 1d  Lctrl Aa Ss Dd
	dw 0x0e0e, 0x0d0d, 0x1414, 0x2222	; 21  Ff Gg Hh Jj
	dw 0x2424, 0x0c0c, 0x2928, 0x0000	; 25  Kk Ll :; "'
	dw 0x0000, 0x0000, 0x0000, 0x2626	; 29  ~` Lshift |\ Zz
	dw 0x1515, 0x0a0a, 0x1111, 0x1313	; 2d  Xx Cc Vv Bb
	dw 0x0606, 0x0909, 0x002e, 0x0025	; 31  Nn Mm <, >.
	dw 0x2f27, 0x0000, 0x2d2d, 0xfdfd	; 35  ?/ Rshift * Lalt
	dw 0xfefe				; 39  space

; programmable keys. Scan code to colorforth character codes
; 0x00:  . esc 1 2 3 4 5 6
; 0x08:  7 8 9 0 - = bs tab
; 0x10:  Q W E R T Y U I
; 0x18:  O P [ ] ret Lctrl A S
; 0x20:  D F G H J K L ;
; 0x28:  ' ` Lshift \ Z X C V
; 0x30:  B N M , . / Rshift *
; 0x38:  Lalt space.F1|F2|F3|F4|F5
; 0x40:  F6|F7|F8|F9|F10..KP7
; 0x48:  KP8|KP9|KP-|KP4|KP5|KP6|KP+|KP1
; 0x50:  KP2|KP3|KP0|KP.|...F11
; 0x58:  F12
pkeys:
	db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 	;  0  ........
	db 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0	;  8  ......bs.
	db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 	; 10  ........
	db 0 , 0 , 0 , 0 , 2 , 0 , 0 , 0	; 18  ....enter...
	db 0 , 0 , 0 , 0 , 20, 17, 25, 22	; 20  ....jkl;
	db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0	; 28  ........
	db 0 , 0 , 18, 0 , 0 , 26, 0 , 0 	; 30  ..M../..
	db 3 , 2 , 0 , 4 , 5 , 6 , 7 , 8	; 38  Lalt|space.F1|F2|F3|F4|F5
	db 9 , 10, 11, 12, 13, 0 , 0 , 16	; 40  F6|F7|F8|F9|F10..KP7
	db 17, 18, 19, 20, 21, 22, 23, 24	; 48  KP8|KP9|KP-|KP4|KP5|KP6|KP+|KP1
	db 25, 26, 27, 28, 0 , 0 , 0 , 14	; 50  KP2|KP3|KP0|KP.|...F11
	db 15								; 58  F12

; colorforth character codes to hex digits
; starts at digit-4
digit:
	db 14, 10,  0,  0			; EA..
	db  0,  0, 12,  0,  0,  0, 15,  0	; ..C...F.
	db 13,  0,  0, 11,  0,  0,  0,  0	; D..B....
	db  0,  1,  2,  3,  4,  5,  6,  7	; 01234567
	db  8,  9				; 89
	
; hex digits to colorforth character codes
hicon:
	db 30q, 31q, 32q, 33q, 34q, 35q, 36q, 37q ; 01234567
	db 40q, 41q,  5 , 23q, 12q, 20q,  4 , 16q ; 89abcdef 

; flag for a negative number
sign:	db 0

; bits used in parsing a word
bits_:	db 28

; edit action???
action: db 1

; list of keys typed from command line (for display at bottom right of screen)
history:
	times 11 db 0
; --------------------- End Data Segment --------------------------

; tasks

round:	call unpause
god:	dd 0					; gods-2*4
	call unpause
main:	dd 0					; mains-2*4
	jmp round

dopause:
	DUP_
	push esi
	mov eax, [me]		; initially god, but then should alternate between god and main
	mov [eax], esp		; save SP
	add eax, byte +4	; point past it
	jmp eax			; go there (call unpause)

unpause:
	pop eax			; either god or main (from return)
	mov esp, [eax]		; pick up return SP from there
	mov [me], eax		; save ret address in me
	pop esi			; get data SP from new return stack
	DROP			; get TOS off of new data stack
	ret			; ret goes to routine from new return stack

; reset data (TOS on stack) & return stacks for main and save SP at main in round
act:
	mov edx, maind-4
	mov [edx], eax
	mov eax, mains-4	; pointer to stack pointer storage
	pop dword [eax]		; put return address there
	sub eax, byte +4
	mov [eax], edx		; 
	mov dword [main], eax
	DROP
	ret

; set god task to nothing (just a return)
show0:
	call show
	ret

show:
	pop dword [screen]	; save pointer to routine to paint the screen (initially just a return)
	DUP_			;  but becomes: black screen ... keyboard ; (logo) after 18 load
	xor eax, eax
	call act		; act returns to caller's caller, so doesn't come back
.show:	call graphic		; '.show' is set by act to be the 'main' routine.
	call [screen]
	call switch		; 
	inc eax
	jmp .show

; interpret/compile

; find in macro dictionary
mfind:
	mov ecx, [macros]
	push edi
	lea edi, [macro0-4+ecx*4]
	jmp short ffind

; find in forth dictionary
find:
	mov ecx, [forths]
	push edi
	lea edi, [forth0-4+ecx*4]
ffind:	std
	repne scasd
	cld
	pop edi
	ret

; execute word(s) entered at command line
ex1:
	dec dword [words]			; from keyboard
	jz .ex
	DROP					; drop all extended words
	jmp ex1
.ex:	call find				; look for word in forth
	jnz abort1				; abort if not found
	DROP
	jmp [ecx*4+forth2]			; go execute word

abort:
	mov [curs], edi
	shr edi, 10-2
	mov [blk], edi
abort1:	mov esp, gods
	mov dword [spaces+3*4], forthd
	mov dword [spaces+4*4], qcompile
	mov dword [spaces+5*4], cnum
	mov dword [spaces+6*4], cshort
	mov eax, 57q
	call echo_
	jmp accept

; routine executed when color bits=1 (yellow word)
execute:
	mov dword [lit], alit
	DUP_
	mov eax, [-4+edi*4]			; get next word
	and eax, byte -0x10			; mask off color bits
	call find
	jnz abort
	DROP
	jmp [forth2+ecx*4]

sdefine:
	pop dword [adefine]
	ret

; routine to set macro as the current dictionary
macro_:	
	call sdefine

; routine to start a macro definition (red word)
macrod:
	mov ecx, [macros]
	inc dword [macros]
	lea ecx, [macro0+ecx*4]
	jmp short forthdd

; routine to set forth as the current dictionary
forth:
	call sdefine

; routine to start a forth definition (red word)
forthd:
	mov ecx, [forths]
	inc dword [forths]
	lea ecx, [forth0+ecx*4]
forthdd:
	mov edx, [-4+edi*4]
	and edx, byte -20q
	mov [ecx], edx
	mov edx, [h]
	mov [forth2-forth0+ecx], edx
	lea edx, [forth2-forth0+ecx]
	shr edx, 2
	mov [last], edx
	mov [list], esp
	mov dword [lit], adup
	test dword [class], -1
	jz .fthd
	jmp [class]
.fthd:	ret

; compile a drop
cdrop:
	mov edx, [h]
	mov [list], edx
	mov byte [edx], op_drop			; drop opcode
	inc dword [h]
	ret

; optimized dup (if previous word ended in a drop, no drop or dup)
qdup:
	mov edx, [h]
	dec edx
	cmp dword [list], edx
	jnz cdup
	cmp byte [edx], 0xad
	jnz cdup
	mov [h], edx
	ret

; compile a dup
cdup:	mov edx, [h]
	mov dword [edx], op03_dup
	mov byte [edx+4], op4_dup
	add dword [h], byte +5
	ret

; called dup
adup:
	DUP_
	ret

; return value of variable
var1:
	DUP_
	mov eax, [4+forth0+ecx*4]
	ret

; define a variable (magenta word)
variable:
	call forthd
	mov dword [forth2-forth0+ecx], var1
	inc dword [forths]			; dummy entry for source address
	mov [4+ecx], edi
	call macrod
	mov dword [forth2-forth0+ecx], .var
	inc dword [macros]
	mov [4+ecx], edi
	inc edi
	ret

; if variable is invoked as a macro
.var:	call [lit]
	mov eax, [4+macro0+ecx*4]
	jmp short cshrt

; compile a number (green)
cnum:
	call [lit]
	mov eax, [edi*4]
	inc edi
	jmp short cshrt

; compile a short number (green)
cshort:
	call [lit]
	mov eax, [-4+edi*4]
	sar eax, 5
cshrt:  call literal
	DROP
	ret

alit:	mov dword [lit], adup

; compile TOS as a 32 bit number
literal:
	call qdup
	mov edx, [list]
	mov [list+4], edx
	mov edx, [h]
	mov [list], edx
	mov byte [edx], op_mov_eax_n
	mov dword [1+edx], eax
	add dword [h], byte +5
	ret

; handle green words that could be in either the macro or the forth dictionary
qcompile:
	call [lit]
	mov eax, [-4+edi*4]
	and eax, byte -20q
	call mfind
	jnz .qcomp
	DROP
	jmp [macro2+ecx*4]
.qcomp: call find
	mov eax, [forth2+ecx*4]
qcom1:  jnz abort
	mov edx, [h]			; was originaly labeled: call_:, but was unused.
	mov dword [list], edx
	mov byte [edx], op_call		; 'call' opcode
	add edx, byte +5
	sub eax, edx
	mov dword [-4+edx], eax
	mov [h], edx
	DROP
	ret

; compile a call to a macro word (cyan)
compile:
	call [lit]
	mov eax, [-4+edi*4]
	and eax, byte -20q
	call mfind
	mov eax, [macro2 + ecx * 4]
	jmp short qcom1	

; compile a 27-bit number
short_:
	mov dword [lit], alit
	DUP_
	mov eax, [-4+edi*4]
	sar eax, 5
	ret

; compile a 32-bit number
; routine executed when color bits=2 (yellow 32 bit number)
num:
	mov dword [lit], alit
	DUP_
	mov eax, [edi*4]
	inc edi
	ret

; compile code for: , ,1 ,2 & ,3
comma:
	mov ecx, 4
dcomma:	
    mov edx, [h]
	mov [edx], eax
	lea edx, [ecx+edx]
	mov [h], edx
	DROP
	ret

comma1:
	mov ecx, 1
	jmp short dcomma

comma2:
	mov ecx, 2
	jmp short dcomma

comma3:
	mov ecx, 3
	jmp short dcomma

; compile code for a semi-colon
semi:
	mov edx, [h]
	sub edx, byte 5
	cmp dword [list], edx
	jnz .semi
	cmp byte [edx], op_call			; 'call' opcode
	jnz .semi
	inc byte [edx]				; change 'call' to 'jmp' (0xe8 to 0xe9)
	ret
.semi:  mov byte [5+edx], op_ret		; 'ret' opcode
	inc dword [h]
	ret

; routine executed when color bits=0 (extension word) 9,10,11,13,14 or 15 (comments or undefined)
qignore:
	test dword [-4+edi*4], -20q		; check for 0 in color bits of next word
	jnz nul					; just return if not 0
	pop edi					; pop 2 return addresses to finish loading source code
	pop edi					; and return to the command line interpreter
nul:	ret

; load and compile a source code block
load:
	shl eax, 10-2				; multiply block number by 1024/4=256
	push edi
	mov edi, eax
	DROP
inter:
	mov edx, [edi*4]			
	inc edi
	and edx, byte 17q			; mask down to bottom 4 (color) bits
	call [spaces+edx*4]			; index into 1 of 16 routines
	jmp inter				; loop exited by qignore on 0 word

; system variables access

nc_:
	DUP_
	mov eax, (nc-start)/4
	ret

xy_:
	DUP_
	mov eax, (xy-start)/4
	ret

fov_:
	DUP_
	mov eax, (fov-start)/4
	ret

sps:
	DUP_
	mov eax, (spaces-start)/4
	ret

last_:
	DUP_
	mov eax, (last-start)/4
	ret

;----------------------- Start of Video ----------------------

rgb:
	ror eax, 8
	shr ax, 2
	ror eax, 6
	shr al, 3
	rol eax, 6+5
	and eax, 0xffff		; was originally 0xf7de
	ret

cyan:
	DUP_
	mov eax, _cyan
	jmp short color

magenta:
	DUP_
	mov eax, _magenta
	jmp short color

silver:
	DUP_
	mov eax, _silver
	jmp short color

blue:
	DUP_
	mov eax, _blue
	jmp short color

red:
	DUP_
	mov eax, _red
	jmp short color

green:
	DUP_
	mov eax, _green
	jmp short color

white:
	DUP_
	mov eax, _white			; fall through into color...
color:
	call rgb
	mov dword [fore], eax
	DROP
	ret

fifof:
	DROP				; fall through into graphic...
graphic:
	ret

; move image from 'frame' to 'displ' and pause

switch:
	push esi
	mov esi, dword [frame]
	push edi
	mov edi, dword [displ]
	mov ecx, hp*vp/2
	rep movsd
	pop edi
	pop esi
	jmp dopause

; convert xy to pointer (edi) into 'frame' clipped to screen width/height

clip:
	mov edi, [xy]
	mov ecx, edi
	test cx, cx
	jns .cl1
	  xor ecx, ecx
.cl1:	and ecx, 0xffff
	mov dword [yc], ecx
	imul ecx, hp*2
	sar edi, 16
	jns .cl2
	  xor edi, edi
.cl2:	mov dword [xc], edi
	lea edi, [edi*2+ecx]
	add edi, [frame]
	ret

; display byte as 8 pixels
; edi=>location in frame
; edx=color

bit8:
	lodsb
	mov ecx, 8
.bb5:	shl al, 1			; shift out bits of data to write to screen
	jnc .bb6			; skip if 0
	  mov word [edi], dx		; if 1, set screen bit
.bb6:	add edi, byte +2		; point to next screen bit
	NEXT .bb5			; repeat 8 times
	ret

; display word as 16 pixels
; edi=>location in frame
; edx=color
; currently unused!!!

;bit16:
;	lodsw
;	xchg al, ah
;	mov ecx, 16
;.bb1:	shl ax, 1			; shift out bits of data to write to screen
;	jnc .bb2			; skip if 0
;	  mov word [edi], dx		; if 1, set screen bit
;.bb2:	add edi, byte +2		; point to next screen bit
;	NEXT .bb1			; repeat 16 times
;	ret

; display byte as 32 pixels
; edi=>location in frame
; edx=color

bit32:
	lodsb
	mov ecx, 8
.bb3:	shl al, 1
	jnc .bb4
	  mov word [edi], dx
	  mov word [edi+2], dx
	  mov word [edi+hp*2], dx
	  mov word [edi+hp*2+2], dx
.bb4:	add edi, byte +4
	NEXT .bb3
	ret

; display a 'fore' color colorforth character code icon on screen
emit:
	call qcr
	push esi
	push edi
	push edx
	imul eax, byte idw*idh/8	; 8*12/8=12
	lea esi, [icons+eax]
	call clip
	mov edx, [fore]
	mov ecx, idh
.em:	push ecx
	call bit8
	add edi, (hp-idw)*2
	pop ecx
	NEXT .em
	pop edx
	pop edi
	pop esi
bl_:	DROP
space:	add dword [xy], iw*xwrd
	ret

; display a 'fore' colored colorforth character code icon double size on screen
emit2:
	push esi
	push edi
	push edx
	imul eax, byte idw*idh/8
	lea esi, [icons+eax]
	call clip
	mov edx, [fore]
	mov ecx, idh
.em2:   push ecx
	call bit32
	add edi, (2*hp-idw*2)*2
	pop ecx
	NEXT .em2
	pop edx
	pop edi
	pop esi
	add dword [xy], iw*2*xwrd
	DROP
	ret

; prepare to display white text starting at the top of the screen
text1:
	call white
	mov dword [lm], 3
	mov dword [rm], hc*iw		; fall through into top...

top:
	mov ecx, dword [lm]
	shl ecx, 16
	add ecx, byte vs
	mov dword [xy], ecx
	mov dword [xycr], ecx
	ret

; clear screen

blank:
	DUP_
	xor eax, eax
	mov dword [xy], eax
	call color
	DUP_
	mov eax, hp
	DUP_
	mov eax, vp			; fall through into box...

; display 'fore' colored box

box:
	call clip
	cmp eax, vp+1
	js .bo1
	  mov eax, vp
.bo1	mov ecx, eax
	sub ecx, dword [yc]
	jng nob
	cmp dword [esi], hp+1
	js .bo2
	  mov dword [esi], hp
.bo2	mov eax, [xc]
	sub [esi], eax
	jng nob
	mov edx, hp
	sub edx, [esi]
	shl edx, 1
	mov eax, [fore]
.bo3	push ecx
	mov ecx, [esi]
	rep stosw
	add edi, edx
	pop ecx
	NEXT .bo3
nob:	DROP
	DROP
	ret

; display a 'fore' colored line

line:
	call clip
	mov ecx, [esi]
	shl ecx, 1
	sub edi, ecx
	mov ecx, eax
	mov eax, [fore]
	rep stosw
	inc dword [xy]
	DROP
	DROP
	ret

qcr:
	mov cx, word [xy+2]
	cmp cx, word [rm]
	js crr
cr:
	mov ecx, [lm]
	shl ecx, 16
	mov cx, word [xy]
	add ecx, byte ih
	mov dword [xy], ecx
crr:	ret

; display key map characters 13 to 16
eight:
	add edi, byte 12
	call four
	call space
	sub edi, byte 16		; go back and and display key map characters 1 through 4	; fall through into four ...
; display key map characters 1 to 4
four:
	mov ecx, 4
four1:
	push ecx
	DUP_
	xor eax, eax
	mov al, [4+edi]
	inc edi
	call emit
	pop ecx
	NEXT four1
	ret

;---------------------- End of video --------------------------

echo_:
	push esi
	mov ecx, 11-1
	lea edi, [history]
	lea esi, [edi+1]
	rep movsb
	pop esi
	mov byte [history+11-1], al
	DROP
	ret

; clear history
right:
	DUP_
	mov ecx, 11
	lea edi, [history]
	xor eax, eax
	rep stosb
	DROP
	ret

keyboard:
	call text1
	mov edi, [board]
	DUP_
	mov eax, [keyc]
	call color
	mov dword [rm], hc*iw
	mov dword [lm], hp-30*iw+hs
	mov dword [xy], (hp-30*iw+hs)*xwrd+vp-2*ih-vs
	test edi, edi			; if board is 0, don't display a key map
	jz .kb1
	   call eight			; display 13-16 and 1-4
	   call space
	   call four			; display 5-8
.kb1:	call cr
	add dword [xy], 25*iw*xwrd
	mov edi, [shift]
	add edi, byte 4*4-4
	mov ecx, 3			; display 1st 3 chars of the key map, the command keys
	call four1
	mov dword [lm], hs
	mov word [xy+2], hs		; set to the left side of the command line
	call stack			; and display the stack contents
	mov word [xy+2], hp-(11+9)*iw-hs
	lea edi, [history-4]
	mov ecx, 11			; display 11 characters from history (keyboard input)
	jmp four1

; display the contents of the stack
stack:
	mov edi, godd-4
.st1:	mov edx, [god]
	cmp [edx], edi
	jnc .st2
	  DUP_
	  mov eax, [edi]
	  sub edi, byte +4
	  call qdot
	  jmp short .st1
.st2:	ret

; filters 0..9 and a..f for numeric, and returns flags according to al
letter:
	and al, al
	js .letx				; go to exit if negative (a control key)
	cmp dword [shift], numb0		; number entry mode?
	jc .letf				; exit if not
	cmp dword [current], decimal		; decimal mode?
	jz .letd				; if so, go check for decimal digit
	cmp al, 0x04				; check for hex digit: a?
	jz .letf				; if so return digit
	cmp al, 0x05				; e?
	jz .letf
	cmp al, 0x0a				; c?
	jz .letf
	cmp al, 0x0e				; f?
	jz .letf
	cmp al, 0x10				; d?
	jz .letf
	cmp al, 0x13				; b?
	jz .letf				; if not, fall through into decimal number check
.letd:	cmp al, 0x18				; =<24 (is it a numeric digit?)
	jc .let0				; if so, return 0
	cmp al, 0x22				; =<34?
	jc .letf				; if so, return digit 
.let0:	xor eax, eax				; else return 0
.letf:	and al, al				; set flag
.letx:	ret

; single check for key
getkey:
	in al, 0x64
	test al, 1
	jz .gkx
.gk:	in al, 0x60
.gkx:	ret
	
; return scan code. Takes care of shift keys.
kscan:
	DUP_
	xor eax, eax
ksc1:	call dopause
ksc8:	call getkey
	jz ksc1
	cmp al, 0xe0
	jnz .ksc3
.ksc2:	call getkey				; extended key code 0xe0
	jz .ksc2
	cmp al, 0x2a				; ignore e0,2a
	jz ksc8
	cmp al, 0xaa				; ignore e0,aa
	jz ksc8
.ksc3:	cmp al, 0xaa				; shift up?
	jz unshiftk
	cmp al, 0xb6
	jz unshiftk
	test al, 0x80				; ignore key releases
	jnz ksc8
	cmp al, 0x2a				; shift down?
	jz shiftk
	cmp al, 0x36
	jz shiftk
	ret
unshiftk:
	xor eax, eax
	jmp short shifty
shiftk:
	mov al, 1
shifty:
	mov dword [shifted], eax
	jmp short ksc8

; returns huffman coded chars or -1 -2 -3 for control
; zero is filtered
; sets flags according to al
key0:	DROP
key:
	call kscan
	cmp al, 0x3a				; highest acceptable key is 0x39 (space)
	jnc key0
	add eax, eax
	add eax, [shifted]
	mov al, [keys+eax]
	and al, al
	jz key0
	ret

; key handler for pad. Returns 0..11 for
; the 12 function keys.
pkey0:	DROP
pkey:
	call kscan
	cmp al, 0x59
	jnc pkey0
	mov al, [pkeys+eax]
	and al, al
	jz pkey0
	dec al
	ret

warm:
	DUP_
	jmp short strtx1
start1:
	mov dword [displ], ebp				; pointer to linear frame buffer from VESA call
strtx1:	call show0					; god task => ret, main => graphic (ret), screen, switch & loop
	mov dword [forths], ((forth1-forth0)/4)		; empty forth dictionary
	mov dword [macros], ((macro1-macro0)/4)		; empty macro dictionary
	mov eax, 18
	call load					; go load block 18 and fall through into accept...

; accept words typed on command line
accept:
	mov dword [shift], alpha0
	xor edi, edi					; edi=0 is flag to not display full key map
accept1:
	mov dword [board], edi
accept2:
	call key
	js near acmdk					; go if a command key
	add dword [shift], byte 4*4+4			; shift: alpha0 or numb0 to alpha1 or numb1
	call word_
	call [aword]					; call ex1 normally
	jmp short accept
acmdk:	neg al						; make command key code positive
	mov edx, [shift]
	jmp [edx+eax*4]					; index into shift
nul0:
	DROP
	jmp short accept2

; TOS is converted into a compressed bit pattern, and added to NOS
; if NOS is full already, a new word is started.
pack:
	cmp al, 20q		; <16?
	jc pack0		; go if so
	add eax, byte 120q	; +80
	mov cl, 7		; 7 bit char
	jmp short pack1		; go pack it
pack0:	mov cl, 4		; assume 4 bit char
	test al, 10q		; <8?
	jz short pack1		; if so, go pack it
	inc ecx			; must be 5 bit char
	xor al, 30q		; 01xxx=>10xxx
pack1:	mov edx, eax		; save packed char
	mov ch, cl		; save bit length
.pa2:	cmp byte [bits_], cl	; will it fit in current word?
	jnc .pa3		; go if so
	shr al, 1		; is last bit of packed character a 0?
	dec cl
	jnc .pa2
	inc cl			; didn't fit into current word
	call lj0
	inc dword [words]
	mov byte [bits_], 28	; reinitialize remaining bits count
	sub byte [bits_], ch	; subtract length from bits
	mov eax, edx		; get back packed char
	DUP_			; 
	ret

.pa3:	shl dword [esi], cl	; shift by char length
	or [esi], eax		; merge packed char
	sub byte [bits_], cl	; subtract length from bits
	ret

lj0:
	mov cl, byte [bits_]
	add cl, 4
	shl dword [esi], cl
	ret

; delete the currently being packed word
lj:
	call lj0
	DROP
	ret

x:
	call right
	mov eax, [words]
	lea esi, [eax*4+esi]
	DROP
	jmp accept

word_:
	call right
	mov dword [words], 1
	mov dword [chars], 1
	DUP_
	mov dword [esi], 0
	mov byte [bits_], 28
word1:
	call letter
	jns .wo
	neg al
	mov edx, [shift]
	jmp dword [edx+eax*4]
.wo:	test al, al
	jz word0
	DUP_
	call echo_
	call pack
	inc dword [chars]
word0: 	DROP
	call key
	jmp word1

decimal:
	mov dword [base], 10
	mov dword [shift], numb0
	ret

hex:
	mov dword [base], 16
	mov dword [shift], numb0
	ret

; toggle between decimal and hexadecimal (14 & 33 = characters f & 9)
octal:
	xor dword [current], (decimal-start) ^ (hex-start)
	lea edx, [numb0+18]			; point to character for alt key in numb0
	xor byte [edx], 14 ^ 33			; toggle between f & 9
	xor byte [edx+20], 14 ^ 33		; change it in numb1 as well
;	xor byte [edx+40], 14 ^ 33		; and numb0
;	xor byte [edx+60], 14 ^ 33		; and numb1
	call [current]				; set to other mode
	jmp short number0

; delete number
xn:
	DROP
	DROP
	jmp accept

; enter negative number
minus:
	mov byte [sign], al	; al is Alt key (1=negative)
	jmp short number2

number0:
	DROP
	jmp short number3
number:
	call [current]
	mov byte [sign], 0	; set sign to positive
	xor eax, eax
number3:
	call key
	call letter
	jns .nu1		; go if not a control key (negative)
	neg al			; make it positive
	mov edx, [shift]
	jmp dword [edx+eax*4]	; go execute control function
.nu1:	test al, al
	jz number0
	mov al, [digit-4+eax]
	test byte [sign], 37q	; negative?
	jz .nu2			; go if not
	neg eax			; make digit negative
.nu2:	mov edx, [esi]		; get rest of number from stack
	imul edx, dword [base]	; multiply it by the base (10 or 16)
	add edx, eax		; add the new digit
	mov [esi], edx		; back on stack
number2:
	DROP			; drop digit
	mov dword [shift], numb1 ; change from numb0 to numb1
	jmp number3		; loop

endn:
	DROP
	call [anumber]
	jmp accept

alphn:
	DROP
	DROP
	jmp accept

edig1:
	DUP_
edig:
	push ecx
	mov al, [eax+hicon]
	call emit
	pop ecx
	ret

odig:
	rol eax, 4
	DUP_
	and eax, byte 0xf
	ret

; display a 32 bit value in hexadecimal
dot:
	mov ecx, 7
.do1:	call odig
	jnz .do4
	DROP
	NEXT .do1
	inc ecx
.do2:	call odig
.do3:	call edig
	NEXT .do2
	call space
	DROP
	ret
.do4:	inc ecx
	jmp short .do3

; display a 32 bit value in hex or decimal according to 'base'
qdot:
	cmp dword [base], byte 10
	jnz dot
; display a 32 bit value in decimal
dot10:
	mov edx, eax
	test edx, edx
	jns .dt1
	  neg edx				; if negative, make positive
	  DUP_
	  mov eax, 43q				; display a '-'
	  call emit
.dt1:	mov ecx, 8
.dt2:	mov eax, edx
	xor edx, edx
	div dword [ecx*4+tens]
	test eax, eax
	jnz d_1
	dec ecx
	jns .dt2
	jmp short d_2
d_0:	mov eax, edx
	xor edx, edx
	div dword [ecx*4+tens]
d_1:	call edig1
	dec ecx
	jns d_0
d_2:	mov eax, edx
	call edig1
	call space
	DROP
	ret

unpack:
	DUP_
	test eax, eax
	js .up1
	shl dword [esi], 4
	rol eax, 4
	and eax, byte 7
	ret
.up1:	shl eax, 1
	js  .up2
	shl dword [esi], 5
	rol eax, 4
	and eax, byte 7
	xor al, 10q
	ret
.up2:	shl dword [esi], 7
	rol eax, 6
	and eax, byte 77q
	sub al, 20q
	ret

qring:
	DUP_
	inc dword [esi]
	cmp dword [curs], edi
	jnz .ri1
	  mov dword [curs], eax
.ri1:	cmp eax, dword [curs]
	jz ring
	jns .ri2
	  mov dword [pcad], edi
.ri2:	DROP
	ret

ring:
	mov dword [cad], edi
	sub dword [xy], iw*xwrd		; bksp
	DUP_
	mov eax, 0x0e04000
	call color
	mov eax, 60q
	mov cx, word [xy+2]
	cmp cx, word [rm]
	js .ri1
	  call emit
	  sub dword [xy], iw*xwrd
	  ret
.ri1:	jmp emit

rw:
	mov cx, word [xy+2]
	cmp cx, word [lm]
	jz .rw1
	 call cr
.rw1:	call red
	jmp type_

gw:
	call green
	jmp type_

mw:
	call cyan
	jmp type_

ww:
	DUP_
	mov eax, _yellow
	call color
	jmp short type_

type0:
	sub dword [xy], iw*xwrd		; call bspcr
	test dword [-4+edi*4], -20q
	jnz type_
	  dec edi
	  mov dword [lcad], edi
	  call space
	  call qring
	  pop edx				; end of block
	  DROP
	  jmp keyboard

cap:
	call white
	DUP_
	mov eax, [-4+edi*4]
	and eax, byte -0x10
	call unpack
	add al, 48
	call emit
	jmp short type2

caps:
	call white
	DUP_
	mov eax, [-4+edi*4]
	and eax, byte -0x10
.0	call unpack
	  jz type3
	  add al, 48
	  call emit
	jmp .0

text:
	call white
type_:
	DUP_
	mov eax, [-4+edi*4]
	and eax, byte -0x10
type2:	call unpack
	jz type3
	call emit
	jmp short type2
type3:	call space
	DROP
	DROP
	ret

gsw:
	mov edx, [-4+edi*4]
	sar edx, 5
	jmp short gnw1

var:
	call magenta
	call type_
gnw:	mov edx, [edi*4]
	inc edi
gnw1:	DUP_
	mov eax, 0x0f800			; green
	cmp dword [bas], dot10
	jz nw2
	mov eax, 0xc000				; dark green
	jmp short nw2

sw:
	mov edx, [-4+edi*4]
	sar edx, 5
	jmp short nw1

nw:
	mov edx, [edi*4]
	inc edi
nw1:	DUP_
	mov eax, _yellow
	cmp dword [bas], dot10
	jz nw2
	mov eax, _dyellow			; dark yellow
nw2:	call color
	DUP_
	mov eax, edx
	jmp [bas]

refresh:
	call show
	call blank
	call text1
	DUP_					; counter
	mov eax, [lcad]
	mov dword [cad], eax			; for curs beyond end
	xor eax, eax
	mov edi, [blk]
	shl edi, 10-2
	mov dword [pcad], edi			; for curs=0
ref1:	test dword [edi*4], 0xf
	jz .re1
	call qring
.re1:	mov edx, [edi*4]
	inc edi
	mov dword [bas], dot10
	test dl, 20q
	jz .re2
	mov dword [bas], dot
.re2:	and edx, byte 0xf
	call [edx*4+display]
	jmp ref1

act1:	mov al, 1
	jmp short actt
act3:	mov al, 3
	jmp short actt
act4:	mov al, 4
	jmp short actt
act9:	mov al, 9
	jmp short actt
act10:	mov al, 10
	jmp short actt
act11:	mov al, 11
	jmp short actt
act7:	mov al, 7
actt:	mov byte [action], al
	mov eax, [actc-4+eax*4]
	mov dword [aword], insert
actn:	mov dword [keyc], eax
	pop eax
	DROP
	jmp accept

actv:	mov byte [action], 12
	mov eax, 0xff00ff			; magenta
	mov dword [aword], .act1
	jmp actn

.act1:	DUP_
	xor eax, eax
	inc dword [words]
	jmp near insert

; minus cursor

mcur:	dec dword [curs]
	jns pcr1		; fall through into pcur ...

; plus cursor

pcur:	inc dword [curs]
pcr1:	ret

; minus minus cursor (back 8 words)
; restrict to 0
mmcur:	sub dword [curs], byte 8
	jns mmc1
hcur:	mov dword [curs], 0
mmc1:	ret

; plus plus cursor (forward 8 words)

ppcur:	add dword [curs], byte 8
	ret

; plus block - move editor to next source (or shadow) block
pblk:	add dword [blk], byte 2
	add dword [esi], byte 2
	ret

; minus block - move editor to previous source (or shadow) block
; restrict to ...

mblk:	cmp dword [blk], byte 20	;???
	js .mb
	sub dword [blk], byte 2
	sub dword [esi], byte 2
.mb:	ret

; Jump to previous block edited with 'edit'
; [blk], N <= [blk+4] <= N
edjmp:
	mov ecx, [esi]
	xchg ecx, [blk+4]
	mov [blk], ecx
	mov [esi], ecx
	ret

; toggle between sorce block and shadow block

shadow:
	xor dword [blk], byte 1
	xor dword [esi], byte 1
	ret

e0:	DROP
	jmp short e_1


edit:	mov ecx, [blk]
	mov [blk+4], ecx
	mov dword [blk], eax
	DROP
e:	DUP_
	mov eax, [blk]
	mov dword [anumber], format
	mov byte [alpha0+4*4], 45q		; .
	mov dword [alpha0+4], e0
	call refresh
e_1:	mov dword [shift], ekbd0
	mov dword [board], ekbd
	mov dword [keyc], _yellow
.eed:	call pkey
	call [eax*4+ekeys]
	DROP
	jmp .eed

; exit the editor
eout:	pop eax
	DROP
	DROP
	mov dword [aword], ex1
	mov dword [anumber], nul
	mov byte [alpha0+4*4], 0
	mov dword [alpha0+4], nul0
	mov dword [keyc], _yellow
	jmp accept

; re-insert a deleted word
destack:
	mov edx, [trash]
	cmp edx, buffer*4
	jnz .de1
	ret
.de1:	sub edx, byte 2*4
	mov ecx, [edx+1*4]
	mov dword [words], ecx
.de2:	DUP_
	mov eax, [edx]
	sub edx, byte 1*4
	NEXT .de2
	add edx, byte 1*4
	mov dword [trash], edx			; fall through into insert0

; move source words to make room for an inserted word and its extension words if there is room
insert0:
	mov ecx, [lcad]				; room available?
	add ecx, [words]
	xor ecx, [lcad]
	and ecx, -0x100
	jz insert1
	mov ecx, [words]			; if not, drop word and any extension words on stack
.in1:	DROP
	NEXT .in1
	ret
insert1:
	push esi
	mov esi, [lcad]
	mov ecx, esi
	dec esi
	mov edi, esi
	add edi, [words]
	shl edi, 2
	sub ecx, [cad]
	js .in2
	shl esi, 2				; /4
	std 
	rep movsd				; move
	cld
.in2:	pop esi
	shr edi, 2
	inc edi
	mov dword [curs], edi			; like abort
	mov ecx, [words]
.in3:	dec edi
	mov dword [edi*4], eax			; insert word
	DROP					; requires cld
	NEXT .in3
	ret

insert:
	call insert0
	mov cl, [action]
	xor byte [edi*4], cl
	jmp accept

format:
	test byte [action], 12q			; ignore 3 and 9
	jz .fo1
	DROP
	ret
.fo1:	mov edx, eax
	and edx, 0xfc000000
	jz .fo2
	cmp edx, 0xfc000000
	jnz format2
.fo2:	shl eax, 5
	xor al, 2				; 6
	cmp byte [action], 4
	jz .fo3
	xor al, 13q				; 8
.fo3:	cmp dword [base], byte 10
	jz .fo4
	xor al, 20q
.fo4:	mov dword [words], 1
	jmp insert

format2:
	DUP_
	mov eax, 1				; 5
	cmp byte [action], 4
	jz .fo5
	mov al, 3				; 2
.fo5:	cmp dword [base], byte 10
	jz .fo6
	xor al, 20q
.fo6:	xchg eax, [esi]
	mov dword [words], 2
	jmp insert

del:
	call enstack
	mov edi, [pcad]
	mov ecx, [lcad]
	sub ecx, edi
	shl edi, 2
	push esi
	mov esi, [cad]
	shl esi, 2
	rep movsd
	pop esi
	jmp mcur

enstack:
	DUP_
	mov eax, [cad]
	sub eax, [pcad]
	jz ens
	mov ecx, eax
	xchg eax, edx
	push esi
	mov esi, [cad]
	lea esi, [esi*4-4]
	mov edi, [trash]
.en:	std
	lodsd
	cld
	stosd
	NEXT .en
	xchg eax, edx
	stosd
	mov dword [trash], edi
	pop esi
ens:	DROP
	ret

; -------------------------------------------------------
; words from here down should be re-defined in colorForth
; -------------------------------------------------------


; usage: pad word_0 ... word_27 [28 words executed when a corresponding key is pressed]
;	     byte_0 ... byte_27[28 key map display bytes]
;		(becomes a 'main' loop.  Use 'accept' as one of the ref'ed words
;               to toggle back to normal key functioning)
; programmable keys:
; bs=0
; enter(or space)=1
; alt=2
; F1=3, F2=4 ... F12=14
; 15...=KP7(home)|KP8|KP9(pgUp)|KP-|KP4|KP5|KP6|KP+|KP1(end)
; 24...=KP2|KP3(pgDn)|KP0(ins)|KP.(del)
pad:
	pop edx
	mov dword [vector], edx
	add edx, 28*5+4
	mov dword [board], edx
	sub edx, byte 4*4+4
	mov dword [shift], edx
.pa1:	call pkey
	mov edx, [vector]
	add edx, eax
	lea edx, [5+eax*4+edx]
	add edx, [-4+edx]
	DROP
	call edx
	jmp .pa1

; display TOS in hex with n characters
hdotn:
	mov edx, eax
	neg eax
	lea ecx, [32+eax*4]
	DROP
	rol eax, cl
	mov ecx, edx
	jmp short hdot1
hdot:
	mov ecx, 8
hdot1:	call odig
	call edig
	NEXT hdot1
	DROP
	ret

; clear the stack
c_:
	mov esi, godd+4
	ret

; mark a point in the dictionaries to revert to later with empty
mark:
	mov ecx, [macros]
	mov [mk], ecx
	mov ecx, [forths]
	mov [mk+4], ecx
	mov ecx, [h]
	mov [mk+2*4], ecx
	ret

; revert to a point in the dictionaries marked earlier with mark
empty:
	mov ecx, [mk+2*4]
	mov [h], ecx
	mov ecx, [mk+4]
	mov [forths], ecx
	mov ecx, [mk]
	mov [macros], ecx
	mov dword [class], 0
	ret

then:
	mov [list], esp		
	mov edx, [h]
	sub edx, eax
	mov [-1+eax], dl
	DROP
	ret

begin:
	mov [list], esp
here:
	DUP_
	mov eax, [h]
	ret

qlit:
	mov edx, [h]			; get HERE
	lea edx, [edx-5]		; point back 5 bytes
	cmp dword [list], edx		; same as pointed to by LIST?
	jnz .ql2			; if not, return Z
	cmp byte [edx], op_mov_eax_n	; was instruction a literal? ('mov eax,nn')
	jnz .ql2			; if not, return NZ
	DUP_				; dup
	mov eax, [list+4]		; get previous LIST
	mov [list], eax			; set LIST
	mov eax, [1+edx]		; TOS=literal data
	cmp dword [edx-5], 0x89fc768d	; was instruction prior to literal a DUP?
	jz .ql1				; if so, return NZ
	mov [h], edx			; set HERE
	jmp cdrop			; go compile a drop 
.ql1:	add dword [h], byte -10		; flag nz
	ret
.ql2:   xor edx, edx  			; flag z
	ret

less:
	cmp [esi], eax
	jl .less			; flag nz
	xor ecx, ecx   			; flag z
.less:	ret

jump:
	pop edx
	add edx, eax
	lea edx, [5+eax*4+edx]
	add edx, [-4+edx]
	DROP
	jmp edx	

boot:
	mov al, 0xfe				; Reset
	out 0x64, al
	jmp $

erase:
	mov ecx, eax
	shl ecx, 8
	DROP
	push edi
	mov edi, eax
	shl edi, 2+8
	xor eax, eax
	rep stosd
	pop edi
	DROP
	ret

copy:
	cmp eax, byte 12
	jc abort1
	mov edi, eax
	shl edi, 2+8
	push esi
	mov esi, [blk]
	shl esi, 2+8
	mov ecx, 256
	rep movsd
	pop esi
	mov [blk], eax
	DROP
	ret

debug:
	mov dword [xy], hs*xwrd+(vc-2)*ih+vs	;x=3, y=573
	DUP_
	mov eax, [god]
	push dword [eax]
	call dot
	DUP_
	pop eax
	call dot
	DUP_
	mov eax, [main]
	call dot
	DUP_
	mov eax, esi
	jmp dot

lms:
	mov dword [lm], eax
	DROP
	ret

rms:
	mov dword [rm], eax
	DROP
	ret

at_:
	mov word [xy], ax
	DROP
	mov word [xy+2], ax
	DROP
	ret

pat:
	add word [xy], ax
	DROP
	add word [xy+2], ax
	DROP
	ret

; something is wrong with this word!!! It leaves y in xy at 32,756
;down:
;	DUP_
;	xor edx, edx				;
;	mov ecx, ih
;	div ecx
;	mov eax, edx
;	add edx, hs*xwrd+0x8000-ih+vs		;x=x+hs, y=y+ WTF????
;	mov dword [xy], edx
;zero:	test eax, eax
;	mov eax, 0
;	jnz .dw
;	  inc eax
;.dw:	ret

; (nn-nnf) I don't know what this word is for!!!
; octant:
;	DUP_
;	mov eax, 0x43		; TOS=0x43
;	mov edx, [4+esi]	; get entry NOS
;	test edx, edx		; test it
;	jns .oc1		; go if positive
;	neg edx			; make it positive
;	mov [4+esi], edx	; change it on stack
;	xor al, 1		; change TOS to 0x42
;.oc1:	cmp edx, [esi]		; compare it to entry TOS
;	jns .oc2		; go if entry NOS =< entry TOS
;	xor al, 4		; change TOS to 0x47 or 0x46
;.oc2:	ret

; floppy disk i/o
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

	times 0x3000-($-$$) db 0 

%include 'icons.asm'

	times 0x4800-($-$$) db 0

INCBIN	 'data200.bin'
