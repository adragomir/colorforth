%include "SDL_types.inc"
%include "SDL_video.inc"
%include "SDL_event.inc"
%include "SDL_pixels.inc"
%include "SDL_surface.inc"

extern _SDL_Init
extern _SDL_InitSubSystem
extern _SDL_QuitSubSystem
extern _SDL_WasInit
extern _SDL_Quit
extern _SDL_WM_SetCaption

SDL_INIT_TIMER		equ 0x00000001
SDL_INIT_AUDIO		equ 0x00000010
SDL_INIT_VIDEO		equ 0x00000020
SDL_INIT_CDROM		equ 0x00000100
SDL_INIT_JOYSTICK	equ 0x00000200
SDL_INIT_NOPARACHUTE	equ 0x00100000	;/* Don't catch fatal signals */
SDL_INIT_EVENTTHREAD	equ 0x01000000	;/* Not supported on all OS's */
SDL_INIT_EVERYTHING	equ 0x0000FFFF

%macro		__SDL_Init 1
  ccall _SDL_Init, %1
%endmacro	;__SDL_Init

%macro		__SDL_InitSubSystem 1
  ccall _SDL_InitSubSystem, %1
%endmacro	;__SDL_InitSubSystem

%macro		__SDL_QuitSubSystem 1
  ccall _SDL_QuitSubSystem, %1
%endmacro	;__SDL_QuitSubSystem

%macro		__SDL_WasInit 1
  ccall _SDL_WasInit, %1
%endmacro	;__SDL_WasInit

%macro		__SDL_Quit 0
  ccall _SDL_Quit
%endmacro	;__SDL_Quit

%macro		__SDL_WM_SetCaption 2
  ccall _SDL_WM_SetCaption, %1, %2
%endmacro	;__SDL_WM_SetCaption

