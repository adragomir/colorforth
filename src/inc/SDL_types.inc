%define SDL_FALSE	0
%define SDL_TRUE	1

%define SDL_PRESSED	0x01
%define SDL_RELEASED	0x00

%macro	Uint8	1
  %1		resb	1
%endmacro

%macro	Sint8	1
  Uint8 %1
%endmacro

%macro	Uint16	1
  %1		resw	1
%endmacro

%macro	Sint16	1
  Uint16 %1
%endmacro

%macro	Uint32	1
  %1		resd	1
%endmacro

%macro	Sint32	1
  Uint32 %1
%endmacro

