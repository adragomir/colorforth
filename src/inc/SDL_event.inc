%include "SDL_keyboard.inc"

SDL_KEYDOWN		equ	0x002
SDL_QUIT		equ	0x00F

extern _SDL_PollEvent

%macro __SDL_PollEvent 1 ; event
  ccall _SDL_PollEvent, %1
%endmacro	;__SDL_PollEvent

;======================================================================================
; STRUCTURES
;======================================================================================

struct SDL_ActiveEvent
    alignb 4
    mbyte	type
    mbyte	gain
    mbyte	state
ends

struct SDL_KeyboardEvent 
    alignb 4
    mbyte	type
    mbyte	which
    mbyte	state
    struct SDL_keysym, keysym
ends

struct SDL_MouseMotionEvent
    alignb 4
    mbyte	type
    mbyte	which
    mbyte	state
    mword	x
    mword	y
    mword	xrel
    mword	yrel
ends

struct SDL_MouseButtonEvent
    alignb 4
    mbyte	type
    mbyte	which
    mbyte	button
    mbyte	state
    mword	x
    mword	y
ends

struct SDL_JoyAxisEvent
    alignb 4
    mbyte	type
    mbyte	which
    mbyte	axis
    mword	value
ends

struct SDL_JoyBallEvent
    alignb 4
    mbyte	type
    mbyte	which
    mbyte	ball
    mword	xrel
    mword	yrel
ends

struct SDL_JoyHatEvent
    alignb 4
    mbyte	.type
    mbyte	.which
    mbyte	.hat
    mbyte	.value
ends

struct SDL_JoyButtonEvent
    alignb 4
    mbyte	.type
    mbyte	.which
    mbyte	.button
    mbyte	.state
ends

struct SDL_ResizeEvent
    alignb 4
    mbyte	type
    mdword	w			; *TODO*
    mdword	h			; *TODO*
ends

struct SDL_ExposeEvent
    alignb 4
    mbyte	type
ends

struct SDL_QuitEvent
    alignb 4
    mbyte	type
ends

struct SDL_UserEvent
    alignb 4
    mbyte	type
    mdword	code			; *TODO*
    mdword	data1
    mdword	data2
ends

struct SDL_SysWMEvent
    alignb 4
    mbyte	type
    mdword	msg
ends

union SDL_Event
    alignb 4
    mbyte	type
    struct SDL_ActiveEvent, active
    struct SDL_KeyboardEvent, key
    struct SDL_MouseMotionEvent, motion
    struct SDL_MouseButtonEvent, button
    struct SDL_JoyAxisEvent, jaxis
    struct SDL_JoyBallEvent, jball
    struct SDL_JoyHatEvent, jhat
    struct SDL_JoyButtonEvent, jbutton
    struct SDL_ResizeEvent, resize
    struct SDL_ExposeEvent, expose
    struct SDL_QuitEvent, quit
    struct SDL_UserEvent, user
    struct SDL_SysWMEvent, syswm
endu

