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
    mdword	type
    mdword	gain
    mdword	state
ends

struct SDL_KeyboardEvent 
    mdword	type
    mdword	which
    mdword	state
    struct SDL_keysym, keysym
ends

struct SDL_MouseMotionEvent
    mdword	type
    mdword	which
    mdword	state
    mdword	x
    mdword	y
    mdword	xrel
    mdword	yrel
ends

struct SDL_MouseButtonEvent
    mdword	type
    mdword	which
    mdword	button
    mdword	state
    mword	x
    mword	y
ends

struct SDL_JoyAxisEvent
    mdword	type
    mdword	which
    mdword	axis
    mdword	value
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
    mdword	.type
    mdword	.which
    mdword	.hat
    mdword	.value
ends

struct SDL_JoyButtonEvent
    mdword	.type
    mdword	.which
    mdword	.button
    mdword	.state
ends

struct SDL_ResizeEvent
    mdword	type
    mdword	w			; *TODO*
    mdword	h			; *TODO*
ends

struct SDL_ExposeEvent
    mdword	type
ends

struct SDL_QuitEvent
    mdword	type
ends

struct SDL_UserEvent
    mdword	type
    mdword	code			; *TODO*
    mdword	data1
    mdword	data2
ends

struct SDL_SysWMEvent
    mdword	type
    mdword	msg
ends

union SDL_Event
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

