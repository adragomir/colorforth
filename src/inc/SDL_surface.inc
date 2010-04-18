%include 'macros.inc'

extern _SDL_DrawPoint
extern _SDL_DrawLine

%macro __SDL_DrawPoint 4
  ccall _SDL_DrawPoint, %1, %2, %3, %4
%endmacro

%macro __SDL_DrawLine 6
  ccall _SDL_DrawLine, %1, %2, %3, %4, %5, %6
%endmacro


;======================================================================================
; STRUCTURES
;======================================================================================

struct SDL_Surface
    mdword flags
    mdword format
    mdword w
    mdword h
    mdword pitch
    mdword pixels
    mdword userdata
    mdword locked
    mdword lock_data
    struct SDL_Rect, clip_rect
    mdword map
    mdword format_version
    mdword refcount
ends

