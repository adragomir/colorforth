; VESA video mode 4117h: 1024x768 16-bit color (5:6:5)
vesa	equ 0x4117

; width, height, BYTES per pixel
hp	equ 1024
vp	equ 768
bpp	equ 2
bpplog2	equ 1

_yellow	equ 0xffe0
_dkylw	equ 0xc600
_orange	equ 0xe200

_green	equ 0x07e0
_dkgrn	equ 0x0600

_red	equ 0xf800
_cyan	equ 0x07ff
_mag	equ 0xf81f

_black	equ 0x0000
_silver	equ 0xC618
_white	equ 0xffff

_blue	equ 0x841f
_dkblue	equ 0x421f

%macro PUTPIX 0
	mov [edi], dx
%endm

%macro PUT4PIX 0
	mov [edi], dx
	mov [edi+bpp], dx
	mov [edi+hp*bpp], dx
	mov [edx+(hp+1)*bpp], dx
%endm

%macro REP_STOPIX 0
	rep stosw
%endm

; vim:ts=8:sw=8
