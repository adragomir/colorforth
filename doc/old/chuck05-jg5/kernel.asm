; eax: T
; esi: Data Stack pointer
; esp: Return Stack pointer
; edx: A
; edi: word pointer

%include 'hardware.inc'
%include 'huffman.inc'
%include 'icons.inc'

; the Pentium manual recommends not using "complex instructions"
; like LOOP.  However, it IS used in the boot sector where space
; is at a premium.
%macro NEXT 1
	dec ecx
	jnz %1
%endmacro

%macro DUP_ 0
	lea esi, [esi-4]
	mov [esi], eax
%endmacro

%macro DROP 0
	lodsd
%endmacro

op_drop		equ 0xad
op03_dup	equ 0x89fc768d
op4_dup		equ 0x06
op_mov_eax_n	equ 0xb8
op_call		equ 0xe8
op_ret		equ 0xc3

; width, height, BYTES per pixel
screen_width	equ 1024
screen_height	equ 768
bpp	equ 4
bpplog2	equ 2

_yellow	equ 0xffff00
_dkylw	equ 0xc0c000
_orange	equ 0xe04000

_green	equ 0x00ff00
_dkgrn	equ 0x00c000

_red	equ 0xff0000
_cyan	equ 0x00ffff
_mag	equ 0xff00ff

_black	equ 0x000000
_silver	equ 0xc0c0c0
_white	equ 0xffffff

_blue	equ 0x8080ff
_dkblue	equ 0x4040ff

; icons are 16x24 pixels
icon_width	equ 16
icon_height	equ 24
; a character is an icon with 3 pixels of padding all round.
char_padding	equ 3
; character width/height
char_width	equ char_padding + icon_width + char_padding			; 22
char_height	equ char_padding + icon_height + char_padding			; 30
; horizontal/vertical characters (screen size)
horizontal_chars	equ screen_width / char_width				; 1024 / 22 = 46 (remainder 12)
vertical_chars	equ screen_height / char_height				; 768 / 30 = 25 (remainder 18)

; This version of colorforth has three tasks; main (the accept loop),
; draw (user defined), and serve (also user defined).  Each has two
; grows-down stacks.  A suffix of 's' indicates the return stack, 'd'
; indicates the data stack.  Thus 'draws' and 'drawd' are the tops of
; the return and data stacks, respectively, for the draw task.

rsize	equ 256 * 4 * 3   ; size of return stacks (3 blocks)
dsize	equ 256 * 4 * 6 ; size of data stacks (6 blocks)

fsize	equ 2048 * 4  ; size of forth dictionary (2048 words, 8 blocks)
                    ; we have two arrays like this: names and addrs.

bufsize	equ 18 * 1024 ; size of floppy buffer.

;   100000 dictionary
;    a0000 return stack (main)
;    9f400 data stack
;    9dc00 return stack (draw)
;    9d000 data stack
;    9b800 return stack (serve)
;    9ac00 data stack
;    99400 divider (bottom of rstack, top of floppy buffer)
;    94c00 floppy buffer
;    92c00 forth dictionary addrs (room for 2048 entries)
;    90c00 forth dictionary names
;     7c00 BIOS loads boot sector here; we immediately move it to 0
;     4800 source
;     3000 icons
;        0 the colorforth kernel

dict	equ 0x100000
mains	equ 0xa0000
maind	equ mains - rsize
draws	equ maind - dsize
drawd	equ draws - rsize
servs	equ drawd - dsize
servd	equ servs - rsize
estacks	equ servd - dsize	; end of stacks
buffer	equ estacks - bufsize
faddrs	equ buffer - fsize
fnames	equ faddrs - fsize
; ...
source	equ 18 * 1024
icons	equ 12 * 1024


%include 'boot.asm'

warm:
	DUP_
	jmp short start1.0
start1:
	call ati0
.0:	call noshow
	call noserve
	mov dword [forths], kforths
	mov dword [macros], kmacros
	mov dword [trash], buffer
	; copy initial forth dictionary up to its real address
	push esi
	lea esi, [fnames_]
	mov edi, fnames
	mov ecx, [forths]
	rep movsd
	lea esi, [faddrs_]
	mov edi, faddrs
	mov ecx, [forths]
	rep movsd
	pop esi
	; load block 18
	mov eax, 18
	call load
	jmp accept

	align 4

; When we switch tasks, we need to switch stacks as well.  We do this
; by pushing eax (cached top-of-stack) onto the data stack, pushing
; the data stack pointer onto the return stack, and then saving the
; return stack pointer into the save slot for the task.

; 'me' points to the save slot for the current task
me:	dd main

; user routines for draw and serve.
udraw:	dd 0
userve:	dd 0

; these are the save slots - each is followed by code to resume the
; next task - the last one jumps 'round to the first.
round:	call resume
main:	dd 0
	call resume
draw:	dd 0
	call resume
serv:	dd 0
	jmp round

; pause this task and switch to the next one.
dopause:
	DUP_              ; save cached top-of-stack to memory.
	push esi          ; save dstack pointer to rstack
	mov eax, [me]     ; save rstack pointer to save slot.
	mov [eax], esp
	add eax, byte 4   ; jump to addr+4 to resume the next task.
	jmp eax

; resume a task.
; IN: return address points to the save slot.
resume:
	pop eax         ; get address of save slot
	mov esp, [eax]  ; restore rstack pointer
	mov [me], eax   ; and set 'me' to the current task
	pop esi         ; restore dstack pointer
	DROP            ; and cache the top element in EAX
	ret

; set draw task to code following 'call to_draw' (with empty stacks).
to_draw:
	mov edx, drawd-4	; data stack is empty.
	mov ecx, draws-4	; return stack contains our return address
	pop dword [ecx]
	lea ecx, [ecx-4]	; and the data stack pointer
	mov [ecx], edx
	mov [draw], ecx		; store it in the draw slot
	ret			; and return twice (to our caller's caller).

; ditto, for serve task
to_serve:
	mov edx, servd-4
	mov ecx, servs-4
	pop dword [ecx]
	lea ecx, [ecx-4]
	mov [ecx], edx
	mov [serv], ecx
	ret

noshow:
	call show
	; an empty "drawing" routine
	ret

; set user drawing routine to code following 'call show'.
show:
	pop dword [udraw]
	call to_draw
.0:	; the drawing loop.
	call [udraw]
	call switch	; copy frame to screen, then pause.
	jmp .0

freeze:
	pop dword [udraw]
	call to_draw
.0:	call [udraw]
	jmp .0

noserve:
	call serve
	; an empty "server" routine
	ret

; set the user serve routine to code following 'call serve'.
serve:
	pop dword [userve]
	call to_serve
.0:	; the server loop
	call dopause
	call [userve]
	jmp .0

; set data stack pointer to highest cell of rstack.
c_:
	mov esi, mains - rsize + 4
	ret

mark:
	mov ecx, [macros]
	mov [mk], ecx
	mov ecx, [forths]
	mov [mk+4], ecx
	mov ecx, [H]
	mov [mk+8], ecx
	ret

empty:
	mov ecx, [mk+8]
	mov [H], ecx
	mov ecx, [mk+4]
	mov [forths], ecx
	mov ecx, [mk]
	mov [macros], ecx
	mov dword [class], 0
	ret

mfind:
	mov ecx, [macros]
	push edi
	lea edi, [mnames-4+ecx*4]
	jmp short ffind

find:
	mov ecx, [forths]
	push edi
	lea edi, [fnames-4+ecx*4]
ffind:
	std
	repne scasd
	cld
	pop edi
	ret

; execute word (full name on top of stack).
exword:
	dec dword [words]
	jz execute.0
	DROP
	jmp exword

execute:
	mov dword [lit], alit
	DUP_
	mov eax,[-4+edi*4]
	and eax, byte -16
.0:	call find
	jnz abort
	DROP
	jmp [faddrs+ecx*4]

abort:
	cmp edi,0x1200
	js abort1
	mov [curs], edi
	shr edi, 10-2
	mov [blk], edi
abort1:	mov esp, mains
	mov dword [adefine], forthd
	mov dword [adefine+4], qcompile
	mov dword [adefine+8], cnum
	mov dword [adefine+12], cshort
	mov eax, Iquestion
	call echo
	jmp accept

sdefine:
	pop dword [adefine]
	ret

macro:
	call sdefine
macrod:
	push eax
	mov ecx, [macros]
	inc dword [macros]
	lea ecx, [mnames+ecx*4]
	mov eax, maddrs-mnames
	jmp short forthdd

forth:
	call sdefine
forthd:
	push eax
	mov ecx, [forths]
	inc dword [forths]
	lea ecx, [fnames+ecx*4]
	mov eax, faddrs-fnames
forthdd:
	mov edx, [-4+edi*4]
	and edx, byte -16
	mov [ecx], edx
	mov edx, [H]
	mov [ecx+eax], edx
	lea edx, [ecx+eax]
	shr edx, 2
	mov [last], edx
	pop eax
	mov [list], esp
	mov dword [lit], adup
	test dword [class], -1
	jz .9
	jmp [class]
.9:	ret

cdrop:
	mov edx, [H]
	mov [list], edx
	mov byte [edx], op_drop
	inc dword [H]
	ret

qdup:
	mov edx, [H]
	dec edx
	cmp [list], edx
	jne cdup
	cmp byte [edx], op_drop
	jne cdup
	mov [H], edx
	ret
cdup:
	mov edx, [H]
	mov dword [edx], op03_dup
	mov byte [edx+4], op4_dup
	add dword [H], byte 5
	ret

adup:
	DUP_
	ret

var1:
	DUP_
	mov eax, [fnames+4+ecx*4]
	ret

variable:
	call forthd
	mov dword [faddrs-fnames+ecx], var1
	inc dword [forths]
	mov [4+ecx], edi
	call macrod
	mov dword [maddrs-mnames+ecx], .var
	inc dword [macros]
	mov [4+ecx], edi
	inc edi
	ret

.var:
	call [lit]
	mov eax, [mnames+4+ecx*4]
	jmp short cshort_

cnum:
	call [lit]
	mov eax, [edi*4]
	inc edi
	jmp short cshort_

cshort:
	call [lit]
	mov eax, [-4+edi*4]
	sar eax, 5
cshort_:
	call literal
	DROP
	ret

alit:
	mov dword [lit], adup
literal:
	call qdup
	mov edx, [list]
	mov [list+4], edx
	mov edx, [H]
	mov [list], edx
	mov byte [edx], op_mov_eax_n
	mov [edx+1], eax
	add dword [H], byte 5
	ret

qcompile:
	call [lit]
	mov eax, [-4+edi*4]
	and eax, byte -16
	call mfind
	jnz .0
	DROP
	jmp [maddrs+ecx*4]
.0:	call find
	mov eax, [faddrs+ecx*4]
qcom1:	jnz abort
	mov edx, [H]
	mov [list], edx
	mov byte [edx], op_call
	add edx, byte 5
	sub eax, edx
	mov [edx-4], eax
	mov [H], edx
	DROP
	ret

compile:
	call [lit]
	mov eax, [-4+edi*4]
	and eax, byte -16
	call mfind
	mov eax, [maddrs+ecx*4]
	jmp qcom1

short_:
	mov dword [lit], alit
	DUP_
	mov eax, [-4+edi*4]
	sar eax, 5
	ret

num:
	mov dword [lit], alit
	DUP_
	mov eax, [edi*4]
	inc edi
	ret

comma:
	mov ecx, 4
dcomma:
	mov edx, [H]
	mov [edx], eax
	DROP
	lea edx, [ecx+edx]
	mov [H], edx
	ret

comma1:
	mov ecx, 1
	jmp dcomma

comma2:
	mov ecx, 2
	jmp dcomma

comma3:
	mov ecx, 3
	jmp dcomma

semi:
	mov edx, [H]
	sub edx, byte 5
	cmp [list], edx
	jne .0
	cmp byte [edx], op_call
	jne .0
	inc byte [edx]				; convert call to jmp
	ret
.0:	mov byte [edx+5], op_ret
	inc dword [H]
	ret

then:
	mov [list], esp
	mov edx, [H]
	sub edx, eax
	mov [eax-1], dl
	DROP
	ret

begin:
	mov [list], esp
here:
	DUP_
	mov eax, [H]
	ret

qlit:
	mov edx, [H]
	lea edx, [edx-5]
	cmp [list], edx
	jne .1
	cmp byte [edx], op_mov_eax_n
	jne .1
	DUP_
	mov eax, [list+4]
	mov [list], eax
	mov eax, [edx+1]
	cmp dword [edx-5], op03_dup
	je .0
	mov [H], edx
	jmp cdrop
.0:	add dword [H], byte -10
	ret
.1:	xor edx, edx
	ret

less:
	cmp [esi], eax
	js .9
	xor ecx, ecx
.9:	ret

qignore:
	test dword [-4+edi*4], -16
	jnz nul
	pop edi
	pop edi
nul:	ret

jump:
	pop edx
	add edx, eax
	lea edx, [5+edx+eax*4]
	add edx, [edx-4]
	DROP
	jmp edx

load:
	shl eax, 10-2
	push edi
	mov edi, eax
	DROP
inter:
	mov edx, [edi*4]
	inc edi
	and edx, byte 15
	call [spaces+edx*4]
	jmp inter

spaces:
	dd qignore, execute, num
adefine:
	dd forthd
	dd qcompile, cnum, cshort, compile
	dd short_, nul, nul, nul
	dd variable, nul, nul, nul

lit:	dd adup
mk:	dd 0, 0, 0				; macros, forths, H
H:	dd dict
last:	dd 0
class:	dd 0
list:	dd 0, 0
forths:	dd 0
macros:	dd 0
mnames:	
	dd _semi<<25                              ; ;
	dd ((_d<<7|_u)<<7|_p)<<11                 ; dup
	dd (((_question<<7|_d)<<7|_u)<<7|_p)<<4   ; ?dup
	dd (((_d<<4|_r)<<4|_o)<<7|_p)<<10         ; drop
	dd (((_t<<7|_h)<<4|_e)<<4|_n)<<13         ; then
	dd ((((_b<<4|_e)<<5|_g)<<4|_i)<<4|_n)<<8  ; begin
kmacros	equ ($-mnames)/4  ; number of macros in the kernel
	times 128 dd 0
maddrs:
	dd semi
	dd cdup
	dd qdup
	dd cdrop
	dd then
	dd begin
	times 128 dd 0

fnames_:
	dd (((_b<<4|_o)<<4|_o)<<4|_t)<<13                 ; boot
	dd (((_w<<4|_a)<<4|_r)<<5|_m)<<14                 ; warm
	dd ((((_p<<4|_a)<<7|_u)<<5|_s)<<4|_e)<<5          ; pause
	dd ((((_m<<4|_a)<<5|_c)<<4|_r)<<4|_o)<<10         ; macro
	dd ((((_f<<4|_o)<<4|_r)<<4|_t)<<7|_h)<<8          ; forth
	dd _c<<27                                         ; c
	dd (((_s<<4|_t)<<4|_o)<<7|_p)<<12                 ; stop
	dd (((_r<<4|_e)<<4|_a)<<7|_d)<<13                 ; read
	dd ((((_w<<4|_r)<<4|_i)<<4|_t)<<4|_e)<<11         ; write
	dd (_n<<5|_c)<<23                                 ; nc
	dd (((((_f<<4|_o)<<4|_r)<<5|_m)<<4|_a)<<4|_t)<<6  ; format
	dd (((_s<<7|_h)<<4|_o)<<5|_w)<<11                 ; show
	dd ((((_s<<4|_e)<<4|_r)<<7|_v)<<4|_e)<<8          ; serve
	dd (((_l<<4|_o)<<4|_a)<<7|_d)<<12                 ; load
	dd (((_h<<4|_e)<<4|_r)<<4|_e)<<13                 ; here
	dd (((_question<<5|_l)<<4|_i)<<4|_t)<<12          ; ?lit
	dd (_3<<7|_comma)<<18                             ; 3,
	dd (_2<<7|_comma)<<18                             ; 2,
	dd (_1<<7|_comma)<<18                             ; 1,
	dd _comma<<25                                     ; ,
	dd (((_l<<4|_e)<<5|_s)<<5|_s)<<13                 ; less
	dd (((_j<<7|_u)<<5|_m)<<7|_p)<<6                  ; jump
	dd ((_p<<5|_c)<<4|_i)<<16                         ; pci
	dd ((((_d<<4|_e)<<7|_v)<<4|_i)<<5|_c)<<5          ; devic(e)
	dd (((((_a<<5|_c)<<5|_c)<<4|_e)<<7|_p)<<4|_t)<<3  ; accept
	dd ((_p<<4|_a)<<7|_d)<<14                         ; pad
	dd ((((_e<<4|_r)<<4|_a)<<5|_s)<<4|_e)<<11         ; erase
	dd (((_c<<4|_o)<<7|_p)<<5|_y)<<11                 ; copy
	dd (((_m<<4|_a)<<4|_r)<<7|_k)<<12                 ; mark
	dd (((_e<<5|_m)<<7|_p)<<4|_t)<<12                 ; empt(y)
	dd (((_e<<5|_m)<<4|_i)<<4|_t)<<15                 ; emit
	dd ((((_d<<4|_i)<<5|_g)<<4|_i)<<4|_t)<<8          ; digit
	dd ((((_2<<4|_e)<<5|_m)<<4|_i)<<4|_t)<<8          ; 2emit
	dd _dot<<25                                       ; .
	dd (_h<<7|_dot)<<18                               ; h.
	dd ((_h<<7|_dot)<<4|_n)<<14                       ; h.n
	dd (_c<<4|_r)<<23                                 ; cr
	dd ((((_s<<7|_p)<<4|_a)<<5|_c)<<4|_e)<<7          ; space
	dd (((_d<<4|_o)<<5|_w)<<4|_n)<<12                 ; down
	dd (((_e<<7|_d)<<4|_i)<<4|_t)<<13                 ; edit
	dd _e<<28                                         ; e
	dd (_l<<5|_m)<<22                                 ; lm
	dd (_r<<5|_m)<<23                                 ; rm
	dd (((((_s<<5|_w)<<4|_i)<<4|_t)<<5|_c)<<7|_h)<<2  ; switch
	dd (((((_f<<4|_r)<<4|_e)<<4|_e)<<7|_z)<<4|_e)<<4  ; freeze
	dd (((_t<<4|_e)<<7|_x)<<4|_t)<<13                 ; text
	dd ((_t<<4|_o)<<7|_p)<<17                         ; top
	dd ((((_k<<4|_e)<<5|_y)<<7|_b)<<4|_o)<<5          ; keybo(ard)
	dd (((_d<<4|_e)<<7|_b)<<7|_u)<<7                  ; debu(g)
	dd (_a<<4|_t)<<24                                 ; at
	dd ((_plus<<4|_a)<<4|_t)<<17                      ; +at
	dd (_x<<5|_y)<<20                                 ; xy
	dd ((_f<<4|_o)<<7|_v)<<16                         ; fov
	dd (((_f<<4|_i)<<5|_f)<<4|_o)<<14                 ; fifo
	dd ((_b<<4|_o)<<7|_x)<<14                         ; box
	dd (((_l<<4|_i)<<4|_n)<<4|_e)<<15                 ; line
	dd ((((_c<<4|_o)<<5|_l)<<4|_o)<<4|_r)<<10         ; color
	dd (((((_o<<5|_c)<<4|_t)<<4|_a)<<4|_n)<<4|_t)<<7  ; octant
	dd (_s<<7|_p)<<20                                 ; sp
	dd (((_l<<4|_a)<<5|_s)<<4|_t)<<14                 ; last
	dd ((((_u<<4|_n)<<7|_p)<<4|_a)<<5|_c)<<5          ; unpac(k)
	dd ((_b<<5|_l)<<7|_k)<<13                         ; blk
	dd (((_c<<7|_u)<<4|_r)<<5|_s)<<11                 ; curs
	dd (((_w<<4|_o)<<4|_r)<<7|_d)<<12                 ; word
	dd ((_e<<7|_k)<<4|_t)<<17                         ; ekt
kforths	equ ($-fnames_)/4 ; number of forth words in the kernel
faddrs_:
	dd boot
	dd warm
	dd dopause
	dd macro
	dd forth
	dd c_
	dd stopf
	dd readf
	dd writef
	dd nc_
	dd formatf
	dd show
	dd serve
	dd load
	dd here
	dd qlit
	dd comma3
	dd comma2
	dd comma1
	dd comma
	dd less
	dd jump
	dd north
	dd dev
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
	dd down
	dd edit
	dd e
	dd lms
	dd rms
	dd switch_
	dd freeze
	dd text1
	dd top
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
	dd octant
	dd sps
	dd last_
	dd unpack
	dd blk_
	dd curs_
	dd _word
	dd ekeys_

boot:
	mov al, 0xfe
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
	push edi
	mov edi, eax
	shl edi, 2+8
	push esi
	mov esi, [blk]
	shl esi, 2+8
	mov ecx, 512
	rep movsd
	pop esi
	pop edi
	mov [blk], eax
	DROP
	ret

debug:
	mov dword [xy], char_padding*0x10000+(vertical_chars-2)*char_height+char_padding
	DUP_
	mov eax, [main]
	push dword [eax]
	call dot
	DUP_
	pop eax
	call dot
	DUP_
	mov eax, [draw]
	call dot
	DUP_
	mov eax, esi
	jmp dot

	align 4
xy:	dd char_padding*0x10000+char_padding
lm:	dd char_padding
rm:	dd screen_width+char_padding
fov:	dd 512

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

blk_:
	DUP_
	mov eax, (blk-start)/4
	ret

curs_:
	DUP_
	mov eax, (curs-start)/4
	ret

ekeys_:
	DUP_
	mov eax, (ekeys-start)/4
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; include 'gen.asm'
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
	mov ecx, screen_width*screen_height*bpp/4
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
	imul ecx, screen_width*bpp
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
	mov ecx, icon_width
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
	mov ecx, icon_width
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
	imul eax, byte icon_width*icon_height/8
	lea esi, [icons+eax]
	call pen_addr
	mov edx, [fore]
	mov ecx, icon_height
.0:	push ecx
	call bit16
	add edi, (screen_width-icon_width)*bpp
	pop ecx
	NEXT .0
	pop edx
	pop edi
	pop esi
bl_:	DROP
space:	add dword [xy], char_width*0x10000
	ret

; display a double-size character.
; : 2emit ( c -- )
emit2:
	push esi
	push edi
	push edx
	imul eax, byte icon_width*icon_height/8
	lea esi, [icons+eax]
	call pen_addr
	mov edx, [fore]
	mov ecx, icon_height
.0:	push ecx
	call bit32
	add edi, 2*(screen_width-icon_width)*bpp
	pop ecx
	NEXT .0
	pop edx
	pop edi
	pop esi
	add dword [xy], 2*char_width*0x10000
	DROP
	ret

text1:
	call white
	mov dword [lm], char_padding
	mov dword [rm], horizontal_chars*char_width
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
	cmp eax, screen_height+1
	js .0
	mov eax, screen_height
.0:	mov ecx, eax
	sub ecx, [yc]
	jle .9
	cmp dword [esi], screen_width+1
	js .1
	mov dword [esi], screen_width
.1:	mov eax, [xc]
	sub [esi], eax
	jle .9
	mov edx, screen_width
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; include 'gen.asm'

cyan:
	DUP_
	mov eax, _cyan
	jmp color

magenta:
	DUP_
	mov eax, _mag
	jmp color

silver:
	DUP_
	mov eax, _silver
	jmp color

blue:
	DUP_
	mov eax, _dkblue
	jmp color

red:
	DUP_
	mov eax, _red
	jmp color

green:
	DUP_
	mov eax, _green
	jmp color

histsz	equ 11
history:
	times histsz db 0

; : echo ( ch -- )
; add character to history
echo:
	push esi
	mov ecx, histsz-1
	lea edi, [history]
	lea esi, [edi+1]
	rep movsb
	pop esi
	mov [history+histsz-1], al
	DROP
	ret

; : right ( -- )
; zero history
right:
	DUP_
	mov ecx, histsz
	lea edi, [history]
	xor eax, eax
	rep stosb
	DROP
	ret

down:
	DUP_
	xor edx, edx
	mov ecx, char_height
	div ecx
	mov eax, edx
	add edx, char_padding*0x10000 + 0x8000-char_height+char_padding
	mov [xy], edx
zero:	test eax, eax
	mov eax, 0
	jnz .9
	inc eax
.9:	ret

blank:
	DUP_
	xor eax, eax
	mov [xy], eax
	call color
	DUP_
	mov eax, screen_width
	DUP_
	mov eax, screen_height
	jmp box

top:
	mov ecx, [lm]
	shl ecx, 16
	add ecx, byte 3
	mov [xy], ecx
	ret

qcr:
	mov cx, [xy+2]
	cmp cx, [rm]
	js cr.9
cr:
	mov ecx, [lm]
	shl ecx, 16
	mov cx, [xy]
	add ecx, byte char_height
	mov [xy], ecx
.9:	ret

lms:
	mov [lm], eax
	DROP
	ret

rms:
	mov [rm], eax
	DROP
	ret

; ( x y -- )
at_:
	mov [xy], ax
	DROP
	mov [xy+2], ax
	DROP
	ret

pat:
	add [xy], ax
	DROP
	add [xy+2], ax
	DROP
	ret

octant:
	DUP_
	mov eax, 0x43
	mov edx, [esi+4]
	test edx, edx
	jns .0
	neg edx
	mov [esi+4], edx
	xor al, 1
.0:	cmp edx, [esi]
	jns .9
	xor al, 4
.9:	ret

; display one line of the onscreen keyboard (eight icons)
; four chars at addr+12, space, four chars at addr.
; IN: edi = addr-4
; OUT: edi = addr
eight:
	add edi, byte 12
	call four
	call space
	sub edi, byte 16
four:
	mov ecx, 4
; display ECX chars from EDI+4, incrementing EDI with each char.
nchars:
	push ecx
	DUP_
	xor eax, eax
	mov al, [edi+4]
	inc edi
	call emit
	pop ecx
	NEXT nchars
	ret

; display top element of (paused) main data stack, if any.
stack:
	mov edi, maind - 4
.0:	mov edx, [main]
	cmp [edx], edi
	jae .9
	DUP_
	mov eax, [edi]
	sub edi, byte 4
	call qdot
	jmp .0
.9:	ret

; display the current keyboard layout
keyboard:
	; color = keyboard color
	DUP_
	mov eax, [keyc]
	call color
	; left margin = xkb
	mov eax, [xkb]
	add eax, byte 0
	mov [lm], eax
	; right margin is 9 icons over from left margin
	mov edx, eax
	add edx, 9*char_width
	mov [rm], edx
	; xy = (xkb, ykb)
	shl eax, 16
	add eax, [ykb]
	mov [xy], eax
	; display finger keys
	mov edi, [board]
	call eight
	call eight
	call eight
	; display thumb keys (leave a blank line, move 4 chars in).
	call cr
	add dword [xy], 4*char_width*0x10000
	mov edi, [shift]
	add edi, byte 12
	mov ecx, 3
	call nchars
	; display top element of stack (if any), at left.
	mov dword [lm], char_padding
	mov word [xy+2], char_padding
	call stack
	; display input history just to the left of keyboard display
	mov word [xy+2], screen_width+char_padding - (histsz+kbw)*char_width
	lea edi, [history-4]
	mov ecx, histsz
	jmp nchars

; These are the keyboard layouts for your 8 fingers
alpha:
	; right hand
	db Ig, Ic, Ir, Il ; top
	db icon_height, It, In, Is ; center
	db Ib, Im, icon_width, Iv ; bottom
	; left hand
	db Ip, Iy, If, Ii ; top
	db Ia, Io, Ie, Iu ; center
	db Iq, Ik, Ix, Id ; bottom
graphics:
	db I1, I2, I3, 0
	db I4, I5, I6, I0
	db I7, I8, I9, Iquestion

	db Icolon, Isemi, Istore, Ifetch
	db Iz, Ij, Idot, Icomma
	db Itimes, Islash, Iplus, Iminus
numbers:
	db I1, I2, I3, 0
	db I4, I5, I6, I0
	db I7, I8, I9, 0

	db 0, 0, 0, 0
	db 0, 0, 0, 0
	db 0, 0, 0, 0
octals:
	db I1, I2, I3, 0
	db I4, I5, I6, I0
	db I7, I8, I9, 0
	db 0, Ia, Ib, Ic
	db 0, Id, Ie, If
	db 0, 0, 0, 0

; layouts for the thumb (shift) keys
; these sort of go in pairs:
;	foo0 is for the first character of a word
;	foo1 is used for the rest
alpha0:
	dd nul0, nul0, number, star0	; handler routines
	db 0, I9, Itimes, 0		; and icons for display
alpha1:
	dd word0,   x,  lj, graph
	db Ix, Idot, Itimes, 0
graph0:
	dd nul0, nul0, nul0, alph0
	db 0, 0, Ia, 0
graph1:
	dd word0,   x, lj, alph
	db Ix, Idot, Ia, 0
numb0:
	dd nul0, minus, alphn, octal
	db Iminus, Ia, If, 0
numb1:
	dd number0, xn,  endn, number0
	db Ix, Idot, 0, 0

letter:
	cmp al, 4
	js .9
	mov edx, [board]
	mov al, [eax+edx]
.9:	ret

; our values (1-27) for the raw keycodes that we use.
key0	equ 16		; the table starts at keycode 16
keys:
	db 16, 17, 18, 19,  0,  0,  4,  5
	db  6,  7,  0,  0,  0,  0, 20, 21
	db 22, 23,  0,  0,  8,  9, 10, 11
	db  0,  0,  0,  0, 24, 25, 26, 27
	db  0,  1, 12, 13, 14, 15,  0,  0
	db  3,  2
nkeys	equ $-keys	; and contains N values

key:
	DUP_
	xor eax, eax
.0:	call dopause
	in al, 0x64
	test al, 1
	jz .0
	in al, 0x60
	test al, 0xf0		; keycode too low, ignore it.
	jz .0
	cmp al, key0+nkeys	; keycode too high, ignore it.
	jae .0
	mov al, [keys-key0+eax]	; look it up in the table and return it.
	ret



; keyboard display is 9 chars wide, 4 high
kbw	equ 9
kbh	equ 4
; location of keyboard display (bottom right)
xkb:	dd screen_width - kbw*char_width + char_padding 
ykb:	dd screen_height - kbh*char_height + char_padding

board:	dd alpha-4	; current keyboard (finger keys)
shift:	dd alpha1	; current shift (thumb) keys
base:	dd 10
current:
	dd decimal
keyc:	dd _yellow
chars:	dd 7
; "after word" - when you finish entering a word, this is called.
aword:	dd exword
anumber:
	dd nul
; how many cells on top of the stack hold huffman-encoded characters?
words:	dd 0

nul0:
	DROP
	jmp short accept2

accept:
	mov dword [shift], alpha0
	lea edi, [alpha-4]
accept1:
	mov dword [board], edi
accept2:
	call key
	cmp al, 4
	jns near first
	mov edx, [shift]
	jmp [edx+eax*4]

bits_:	db 2

pack0:
	add eax, byte 120q
	mov cl, 7
	jmp short pack1
pack:
	cmp al, 20q
	jnc pack0
	mov cl, 4
	test al, 10q
	jz pack1
	inc ecx
	xor al, 30q
pack1:	mov edx, eax
	mov ch, cl
.0:	cmp [bits_], cl
	jnc .1
	shr al, 1
	jc full
	dec cl
	jmp .0
.1:	shl dword [esi], cl
	xor [esi], eax
	sub [bits_], cl
	ret

lj0:
	mov cl, [bits_]
	add cl, 4
	shl dword [esi], cl
	ret

lj:
	call lj0
	DROP
	ret

full:
	call lj0
	inc dword [words]
	mov byte [bits_], 28
	sub [bits_], ch
	mov eax, edx
	DUP_
	ret

x:
	call right
	mov eax, [words]
	lea esi, [esi+eax*4]
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
	jns .0
	mov edx, [shift]
	jmp dword [edx+eax*4]
.0:	test al, al
	jz word0
	DUP_
	call echo
	call pack
	inc dword [chars]
word0:
	DROP
	call key
	jmp word1

decimal:
	mov dword [base], 10
	mov dword [shift], numb0
	mov dword [board], numbers-4
	ret

hex:
	mov dword [base], 16
	mov dword [shift], numb0
	mov dword [board], octals-4
	ret

octal:
	xor dword [current], (decimal-start) ^ (hex-start)
	xor byte [numb0+18], 41q ^ 16q
	call [current]
	jmp short number0

xn:
	DROP
	DROP
	jmp accept

digit:
	db 14, 10,  0,  0
	db  0,  0, 12,  0,  0,  0, 15,  0
	db 13,  0,  0, 11,  0,  0,  0,  0
	db  0,  1,  2,  3,  4,  5,  6,  7
	db  8,  9

sign:	db 0

minus:
	mov [sign], al
	jmp short number2

number0:
	DROP
	jmp short number3
number:
	call [current]
	mov byte [sign], 0
	xor eax, eax
number3:
	call key
	call letter
	jns .0
	mov edx, [shift]
	jmp dword [edx+eax*4]
.0:	test al, al
	jz number0
	mov al, [digit-4+eax]
	test byte [sign], 37q
	jz .1
	neg eax
.1:	mov edx, [esi]
	imul edx, dword [base]
	add edx, eax
	mov [esi], edx
number2:
	DROP
	mov dword [shift], numb1
	jmp number3

endn:
	DROP
	call [anumber]
	jmp accept

alphn:
	DROP
alph0:
	mov dword [shift], alpha0
	lea edi, [alpha-4]
	jmp short star1

star0:
	mov dword [shift], graph0
	lea edi, [graphics-4]
star1:	DROP
	jmp accept1

alph:
	mov dword [shift], alpha1
	lea edi, [alpha-4]
	jmp short graphb
graph:
	mov dword [shift], graph1
	lea edi, [graphics-4]
graphb:	mov dword [board], edi
	jmp word0

first:
	add dword [shift], byte 4*4 + 4		; move to next shift table
	call word_
	call [aword]
	jmp accept
	
hicon:
	db I0, I1, I2, I3, I4, I5, I6, I7
	db I8, I9, Ia, Ib, Ic, Id, Ie, If

edig1:
	DUP_
edig:
	push ecx
	mov al, [hicon+eax]
	call emit
	pop ecx
	ret

odig:
	rol eax, 4
	DUP_
	and eax, byte 0xf
	ret

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
	dec ecx
	jnz hdot1
	DROP
	ret

dot:
	mov ecx, 7
.0:	call odig
	jnz .3
	DROP
	NEXT .0
	inc ecx
.1:	call odig
.2:	call edig
	NEXT .1
	call space
	DROP
	ret
.3:	inc ecx
	jmp .2

qdot:
	cmp dword [base], byte 10
	jne dot
dot10:
	mov edx, eax
	test edx, edx
	jns .0
	neg edx
	DUP_
	mov eax, Iminus
	call emit
.0:	mov ecx, 8
.1:	mov eax, edx
	xor edx, edx
	div dword [tens+ecx*4]
	test eax, eax
	jnz d_1
	dec ecx
	jns .1
	jmp short d_2

d_0:	mov eax, edx
	xor edx, edx
	div dword [tens+ecx*4]
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
	js .0
	shl dword [esi], 4
	rol eax, 4
	and eax, byte 7
	ret
.0:	shl eax, 1
	js .1
	shl dword [esi], 5
	rol eax, 4
	and eax, byte 7
	xor al, 8
	ret
.1:	shl dword [esi], 7
	rol eax, 6
	and eax, byte 77q
	sub al, 20q
	ret

qring:
	DUP_
	inc dword [esi]
	cmp [curs], edi
	jnz .0
	mov [curs], eax
.0:	cmp eax, [curs]
	jz ring
	jns .1
	mov [pcad], edi
.1:	DROP
	ret

ring:
	mov [cad], edi
	sub dword [xy], char_width*0x10000
	DUP_
	mov eax, _orange
	call color
	mov eax, 48	; index of first capital icon
	mov cx, [xy+2]
	cmp cx, [rm]
	js .9
	call emit
	sub dword [xy], char_width*0x10000
	ret
.9:	jmp emit

rw:
	mov cx, [xy+2]
	cmp cx, [lm]
	jz .0
	call cr
.0:	call red
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
	sub dword [xy], char_width*0x10000
	test dword [-4+edi*4], -16
	jnz type_
	  dec edi
	  mov [lcad], edi
	  call space
	  call qring
	  pop edx
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
	jmp type2
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
	mov eax, _green
	cmp dword [bas], dot10
	jz nw2
	mov eax, _dkgrn
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
	mov eax, _dkylw
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
	mov [cad], eax				; for curs beyond end
	xor eax, eax
	mov edi, [blk]
	shl edi, 8
	mov [pcad], edi				; for curs=0
ref1:	test dword [edi*4], 0xf
	jz .0
	call qring
.0:	mov edx, [edi*4]
	inc edi
	mov dword [bas], dot10
	test dl, 20q
	jz .1
	mov dword [bas], dot
.1:	and edx, byte 0xf
	call [display+edx*4]
	jmp ref1

	align 4
display:
	dd type0, ww, nw, rw
	dd gw, gnw, gsw, mw
	dd sw, text, cap, caps
	dd var, nul, nul, nul

tens:
	dd 10, 100, 1000, 10000, 100000, 1000000
	dd 10000000, 100000000, 1000000000

bas:	dd dot10
blk:	dd 18, 18
curs:	dd 0
cad:	dd 0
pcad:	dd 0
lcad:	dd 0
trash:	dd buffer

ekeys:	dd nul, del, eout, destack
	dd act1, act3, act4, shadow
	dd mcur, mmcur, ppcur, pcur
	dd mblk, actv, act7, pblk
	dd nul, act11, act10, act9
	dd nul, nul, nul, popblk
ekbd0:	dd nul, nul, nul, nul
	db Ix, Idot,  7 ,  0
ekbd:	db Iy, Ir, Ig, Itimes
	db Il, Iu, Id, Ir
	db Iminus, Im, Ic, Iplus
	db 0, cap_S, cap_C, It
	db 0, 0, If, Ij
	db 0, 0, 0, 0
actc:	dd _yellow, _black, _red, _dkgrn
	dd _black, _black, _cyan, _black
	dd _white, _white, _white, _blue
vector:	dd 0
action:	db 9

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
actt:	mov [action], al
	mov dword [aword], insert
	mov eax, [actc-4+eax*4]
actn:	mov dword [keyc], eax
	pop eax
	DROP
	jmp accept

actv:	mov byte [action], 12
	mov eax, _mag
	mov dword [aword], .0
	jmp actn
.0:	; gets called after word is read in.
	DUP_
	xor eax, eax
	inc dword [words]
	jmp insert

mcur:	dec dword [curs]
	jns pcr1
pcur:	inc dword [curs]
pcr1:	ret

mmcur:	sub dword [curs], byte 8
	jns .9
	mov dword [curs], 0
.9:	ret

ppcur:	add dword [curs], byte 8
	ret

pblk:	add dword [blk], byte 2
	add dword [esi], byte 2
	ret

mblk:	cmp dword [blk], byte 20
	js .9
	sub dword [blk], byte 2
	sub dword [esi], byte 2
.9:	ret

popblk: ; ??
	; [blk], N <= [blk+4] <= N
	mov ecx, [esi]
	xchg ecx, [blk+4]
	mov [blk], ecx
	mov [esi], ecx
	ret

shadow:
	xor dword [blk], byte 1
	xor dword [esi], byte 1
	ret

e0:	DROP
	jmp short e_1

edit:	mov ecx, [blk]
	mov [blk+4], ecx
	mov [blk], eax
	DROP
e:	DUP_
	mov eax, [blk]
	mov dword [anumber], format
	mov byte [alpha0+16], Idot
	mov dword [alpha0+4], e0
	call refresh
e_1:	mov dword [shift], ekbd0
	mov dword [board], ekbd-4
	mov dword [keyc], _yellow
.0:	call key
	call near [ekeys+eax*4]
	DROP
	jmp .0

eout:	pop eax
	DROP
	DROP
	mov dword [aword], exword
	mov dword [anumber], nul
	mov byte [alpha0+4*4], 0
	mov dword [alpha0+4], nul0
	mov dword [keyc], _yellow
	jmp accept

destack:
	mov edx, [trash]
	cmp edx, buffer
	jnz .0
	ret
.0:	sub edx, byte 8
	mov ecx, [edx+4]
	mov [words], ecx
.1:	DUP_
	mov eax, [edx]
	sub edx, byte 4
	NEXT .1
	add edx, byte 4
	mov [trash], edx

insert0:
	mov ecx, [lcad]				; room available?
	add ecx, [words]
	xor ecx, [lcad]
	and ecx, -0x100
	jz insert1
	mov ecx, [words]			; no
.0:	DROP
	NEXT .0
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
	js .0
	shl esi, 2
	std
	rep movsd
	cld
.0:	pop esi
	shr edi, 2
	inc edi
	mov [curs], edi
	mov ecx, [words]
.1:	dec edi
	mov [edi*4], eax
	DROP
	NEXT .1
	ret

insert:
	call insert0
	mov cl, [action]
	xor [edi*4], cl
	cmp cl, 3
	jnz .9
	mov byte [action], 4
	mov dword [keyc], _dkgrn
.9:	jmp accept

; read in a word and leave it on the stack
_word:
	mov dword [aword], .0
	jmp accept		; accept will read a word and then call [aword].
.0:	; restore [aword] to default, and return to caller of _word.
	pop dword [aword]
	mov dword [aword], exword
	ret

format:
	test byte [action], 10
	jz .0
	DROP
	ret
.0:	mov edx, eax
	and edx, 0xfc000000
	jz .1
	cmp edx, 0xfc000000
	jne format2
.1:	shl eax, 5
	xor al, 2
	cmp byte [action], 4
	je .2
	xor al, 13q
.2:	cmp dword [base], byte 10
	je .3
	xor al, 20q
.3:	mov dword [words], 1
	jmp insert

format2:
	DUP_
	mov eax, 1
	cmp byte [action], 4
	jz .0
	mov al, 3
.0:	cmp dword [base], byte 10
	jz .1
	xor al, 20q
.1:	xchg eax, [esi]
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
	jz .1
	mov ecx, eax
	xchg eax, edx
	push esi
	mov esi, [cad]
	lea esi, [-4+esi*4]
	mov edi, [trash]
.0:	std
	lodsd
	cld
	stosd
	NEXT .0
	xchg eax, edx
	stosd
	mov [trash], edi
	pop esi
.1:	DROP
	ret

pad:
	pop edx
	mov [vector], edx
	add edx, 28*5
	mov [board], edx
	sub edx, byte 4*4
	mov [shift], edx
.0:	call key
	mov edx, [vector]
	add edx, eax
	lea edx, [5+eax*4+edx]
	add edx, [edx-4]
	DROP
	call edx
	jmp .0

	; the kernel gets 12 blocks - fill out to the end
	times 12*1024-($-$$) db 0

; vim:ts=8:sw=8
