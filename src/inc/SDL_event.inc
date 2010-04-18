%include "SDL_keyboard.inc"

SDL_KEYDOWN  equ 0x300
SDL_QUIT  equ 0x100

extern _SDL_PollEvent

%macro __SDL_PollEvent 1 ; event
  ccall _SDL_PollEvent, %1
%endmacro ;__SDL_PollEvent

;======================================================================================
; STRUCTURES
;======================================================================================

struct SDL_version
  mbyte major
  mbyte minor
  mbyte patch
ends

struct SDL_WindowEvent
  mdword type
  mdword windowID
  mbyte event
  mbyte padding1
  mbyte padding2
  mbyte padding3
  mdword data1
  mdword data2
ends

struct SDL_KeyboardEvent 
  mdword type
  mdword windowID
  mbyte which
  mbyte state
  mbyte padding1
  mbyte padding2
  struct SDL_keysym, keysym
ends

struct SDL_TextEditingEvent
  mdword type
  mdword windowID
  mbyte which
  mbyte text, 35
  mdword start
  mdword length
ends

struct SDL_TextInputEvent
  mdword type
  mdword windowID
  mbyte which
  mbyte padding1
  mbyte padding2
  mbyte padding3
  mbyte text, 35
ends

struct SDL_MouseMotionEvent
  mdword type
  mdword windowID
  mbyte which
  mbyte state
  mbyte padding1
  mbyte padding2
  mdword x
  mdword y
  mdword z
  mdword pressure
  mdword pressure_max
  mdword pressure_min
  mdword rotation
  mdword tilt_x
  mdword tilt_y
  mdword cursor
  mdword xrel
  mdword yrel
ends

struct SDL_MouseButtonEvent
  mdword type
  mdword windowID
  mbyte which
  mbyte button
  mbyte state
  mbyte padding1
  mdword x
  mdword y
ends

struct SDL_MouseWheelEvent
  mdword type
  mdword windowID
  mbyte which
  mbyte padding1
  mbyte padding2
  mbyte padding3
  mdword x
  mdword y
ends

struct SDL_ProximityEvent
  mdword type
  mdword windowID
  mbyte which
  mbyte padding1
  mbyte padding2
  mbyte padding3
  mdword cursor
  mdword x
  mdword y
ends

struct SDL_JoyAxisEvent
  mdword type
  mbyte which
  mbyte axis
  mbyte padding1
  mbyte padding2
  mdword value
ends

struct SDL_JoyBallEvent
  alignb 4
  mdword type
  mbyte which
  mbyte ball
  mbyte padding1
  mbyte padding2
  mdword xrel
  mdword yrel
ends

struct SDL_JoyHatEvent
  mdword type
  mbyte which
  mbyte hat
  mbyte value
  mbyte padding1
ends

struct SDL_JoyButtonEvent
  mdword type
  mbyte which
  mbyte button
  mbyte state
  mbyte padding1
ends

struct SDL_QuitEvent
  mdword type
ends

struct SDL_UserEvent
  mdword type
  mdword windowID
  mdword code   ; *TODO*
  mdword data1
  mdword data2
ends

struct SDL_SysWMmsg
  struct SDL_version, version
  mdword data
ends

struct SDL_SysWMinfo
  struct SDL_version, version
  mdword data
ends

struct SDL_SysWMEvent
  mdword type
  mdword msg
ends

struct SDL_ActiveEvent
  mdword type
  mbyte gain
  mbyte state
ends

struct SDL_ResizeEvent
  mdword type
  mbyte w   ; *TODO*
  mbyte h   ; *TODO*
ends

union SDL_Event
  mbyte type
  struct SDL_WindowEvent, window
  struct SDL_KeyboardEvent, key
  struct SDL_TextEditingEvent, edit
  struct SDL_TextInputEvent, text
  struct SDL_MouseMotionEvent, motion
  struct SDL_MouseButtonEvent, button
  struct SDL_MouseWheelEvent, wheel
  struct SDL_JoyAxisEvent, jaxis
  struct SDL_JoyBallEvent, jball
  struct SDL_JoyHatEvent, jhat
  struct SDL_JoyButtonEvent, jbutton
  struct SDL_QuitEvent, quit
  struct SDL_UserEvent, user
  struct SDL_SysWMEvent, syswm
  struct SDL_ProximityEvent, proximity
endu

