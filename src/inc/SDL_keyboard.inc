extern _SDL_EnableKeyRepeat

%macro __SDL_EnableKeyRepeat 2
  ccall _SDL_EnableKeyRepeat, %1, %2
%endmacro

SDLK_ESCAPE		equ	27

KMOD_LSHIFT equ 0x0001
KMOD_RSHIFT equ 0x0002

;======================================================================================
; STRUCTURES
;======================================================================================

struct SDL_keysym
    ;alignb 4
    mdword      scancode
    mdword      sym
    mdword      mod
    mdword      unicode
ends

