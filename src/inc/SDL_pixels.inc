%ifndef _SDL_PIXELS_INC_
%define _SDL_VIDEO_INC_ 1

%include "macros.inc"


extern _SDL_MapRGB


%macro		__SDL_MapRGB 4  ; surface->format, r, g, b
  ccall _SDL_MapRGB, %1, %2, %3, %4
%endmacro	;__SDL_MapRGB

;======================================================================================
; STRUCTURES
;======================================================================================

struct SDL_Color
    mbyte r
    mbyte g
    mbyte b
    mbyte unused
ends


%endif

