%ifndef _SDL_VIDEO_INC_
%define _SDL_VIDEO_INC_ 1

%include "macros.inc"

extern _SDL_SetVideoMode
extern _SDL_Flip

SDL_SWSURFACE	equ 0x00000000	;/* Surface is in system memory */
SDL_HWSURFACE	equ 0x00000001	;/* Surface is in video memory */
SDL_ASYNCBLIT	equ 0x00000004	;/* Use asynchronous blits if possible */
;/* Available for SDL_SetVideoMode() */
SDL_ANYFORMAT	equ 0x10000000	;/* Allow any video depth/pixel-format */
SDL_HWPALETTE	equ 0x20000000	;/* Surface has exclusive palette */
SDL_DOUBLEBUF	equ 0x40000000	;/* Set up double-buffered video mode */
SDL_FULLSCREEN	equ 0x80000000	;/* Surface is a full screen display */
SDL_OPENGL    equ 0x00000002    ;/* Create an OpenGL rendering context */
SDL_OPENGLBLIT	equ 0x0000000A	;/* Create an OpenGL rendering context and use it for blitting */
SDL_RESIZABLE	equ 0x00000010	;/* This video mode may be resized */
SDL_NOFRAME	equ 0x00000020	;/* No window caption or edge frame */
;/* Used internally (read-only) */
SDL_HWACCEL	equ 0x00000100	;/* Blit uses hardware acceleration */
SDL_SRCCOLORKEY	equ 0x00001000	;/* Blit uses a source color key */
SDL_RLEACCELOK	equ 0x00002000	;/* Private flag */
SDL_RLEACCEL	equ 0x00004000	;/* Surface is RLE encoded */
SDL_SRCALPHA	equ 0x00010000	;/* Blit uses source alpha blending */
SDL_PREALLOC	equ 0x01000000	;/* Surface uses preallocated memory */

%macro		__SDL_SetVideoMode 4  ; width, height, bpp, flags
  ccall _SDL_SetVideoMode, %1, %2, %3, %4
%endmacro

%macro		__SDL_FillRect 3 ; title, icon
  ccall _SDL_WM_SetCaption, %1, %2, %3
%endmacro

%macro		__SDL_Flip 1
  ccall _SDL_Flip, %1
%endmacro

;======================================================================================
; STRUCTURES
;======================================================================================

struct SDL_Point
    mdword    x
    mdword    y
ends

struct SDL_Rect
    mdword	x
    mdword	y
    mdword	w
    mdword	h
ends

%endif
