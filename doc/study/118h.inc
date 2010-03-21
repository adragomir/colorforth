; VESA video mode number: 1024x768 32-bit color (RGBA, apparently)
vesa	equ 0x4118

; width, height, BYTES per pixel
hp	equ 1024
vp	equ 768
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

%macro PUTPIX 0
	mov [edi], edx
%endm

%macro PUT4PIX 0
	mov [edi], edx
	mov [edi+bpp], edx
	mov [edi+hp*bpp], edx
	mov [edx+(hp+1)*bpp], edx
%endm

%macro REP_STOPIX 0
	rep stosd
%endm

; vim:ts=8:sw=8
