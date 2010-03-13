%ifndef _SDL_IMAGE_INC_
%define _SDL_IMAGE_INC_

%include "macros.inc"

%macro __IMG_Load 1
  ccall IMG_Load, %1
%endmacro

%endif
