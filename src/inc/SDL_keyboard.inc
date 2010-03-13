%include "SDL_keysym.inc"

extern _SDL_EnableKeyRepeat

%macro __SDL_EnableKeyRepeat 2
  ccall _SDL_EnableKeyRepeat, %1, %2
%endmacro


;======================================================================================
; STRUCTURES
;======================================================================================

struct SDL_keysym
    mdword      scancode
    mdword      sym
    mdword      mod
    mdword      unicode
ends

