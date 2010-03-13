; sdl scancodes on my machine

sdl_scancode_to_raw:
  times 4 db 0, 
  ;   a -> z = 4 -> 29
  ;  a     b     c     d     e     f     g     h
  db 0x1f, 0x30, 0x2e, 0x20, 0x12, 0x21, 0x22, 0x23; a
  ;  i     j     k     l     m     n     o     p
  db 0x17, 0x24, 0x25, 0x26, 0x32, 0x31, 0x18, 0x19; a
  ;  q     r     s     t     u     v     w     x
  db 0x10, 0x13, 0x1f, 0x14, 0x16, 0x2f, 0x11, 0x2d; a
  ;  y     z
  db 0x15, 0x2c
  ;   1 -> 0 = 30 -> 39
  ; numbers
  ;  1     2     3     4     5     6     7     8
  db 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09 
  ;  9     0
  ;   enter = 40
  ;   esc = 41
  ;   backspace = 42
  ;   tab = 43
  ;   space = 44
  db 0x0A, 0x0B
  ;  cr    ESC   bksp  tab   space
  db 0x1c, 0x01, 0x0e, 0x0f, 0x39
  ;   - = 45
  ;   = = 46
  ;   [ = 47
  ;   ] = 48
  ;   \ = 49
  ;   ; = 51
  ;   ' = 52
  ;   top left weird key - 53
  ;  -_    =+    [{    ]}    \|    ;:    '"    weird left
  db 0x0c, 0x0d, 0x1a, 0x1b, 0x2b, 0x27, 0x28, 0x00
  ;   , = 54
  ;   . = 55
  ;   / = 56
  ;  ,<    .>    /?
  db 0x33, 0x34, 0x35, 
  ;   f1 -> f12 = 58 -> 69
  ;  na    f1    f2    f3    f4    f5    f6    f7    f8    f9    f10   f11   f12
  db 0x00, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x57, 0x58
  ;  na
  db 0x00, 0x00, 0x00, 0x00
  ;   home 74 
  ;   pg up 75
  ;   end 77 
  ;   delete = 76
  ;   pgdn 78
  ;   right = 79
  ;   left = 80
  ;   down = 81
  ;   up = 82
  ;  home  pgup  end   del   pgdn  rt    lt    dn    up
  db 0x47, 0x49, 0x4f, 0x53, 0x51, 0x4d, 0x4b, 0x50, 0x48
  ; 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
  times 17 db 0
  ;   tilde = 100
  ;  ~
  db 0x29
  times 124 db 0
  ;   left shift 225
  ;   left alt 226
  ;   right shift 229
  ;   right alt 231
  ;  lsh   lalt              rsh         ralt > lalt
  db 0x2a, 0x38, 0x00, 0x00, 0x36, 0x00, 0x38

; QWERTY (entire routine) to QWERTY end	
letter:
    ; filters 0..9 and a..f for numeric, and returns
    ;  flags according to al
    and	al, al
    cmp	shift, offset numb0		; numbers?
    js	letx
    jc	letf				; yes
    cmp	current, decimal		; decimal?
    jz	leth				;  yes
    cmp	al, 04h				;  no check for hex
    jz	letf
    cmp	al, 05h
    jz	letf
    cmp	al, 0ah
    jz	letf
    cmp	al, 0eh
    jz	letf
    cmp	al, 10h
    jz	letf
    cmp	al, 13h
    jz	letf
leth:
    cmp	al, 18h
    jc	let0
    cmp	al, 22h
    jc	letf
let0:
    xor	eax, eax
letf:
    and	al, al				; set flag
letx:
    ret
; QWERTY end

getkey:
    call   scankeys
    jz     getkey
    test   eax, 8000h		; was key still down at time of call?
    jz     getkey		; skip it if not
    push   0			; uMap=MAPVK_VK_TO_VSC (return scan code)
    push   ecx			; uCode
    call   MapVirtualKeyA	; map the virtual-key code to a scan code (ASCII version)
    and    eax, 0ffh
    jz     getkey
    ret

getcfkey:
gck1:
    call getkey
    push eax
    xor eax,eax
    mov shifted, eax
    push 10h				; virtual-key code for 'shift'
    call GetAsyncKeyState	; key up or down? pressed since last call?
    test eax, 8000h			; key down?
    jz @f
    xor eax,eax
    inc eax
    mov shifted, eax
@@:
    pop eax
    ret

; QWERTY till QWERTY end
    align 2
    ; scan code to colorforth char conversion.
    ; the codes are huffman compressed etc...
    ; -1 for backspace/esc, -2 for return/space
    ; and -3 for alt.

keys	db 00,00
    dw 0ffffh, 2a19h, 2c1ah, 001bh	    ;  1  esc !1 @2 #3
    dw 001ch, 001dh, 001eh, 001fh	      ;  5  $4 %5 ^6 &7
    dw 2d20h, 0021h, 0018h, 0023h	      ;  9  *8 (9 )0 _-
    dw 2b00h, 0ffffh, 0000h, 1717h	    ;  d  += bs tab Qq
    dw 0f0fh, 0404h, 0101h, 0202h	      ; 11  Ww Ee Rr Tt
    dw 0b0bh, 1616h, 0707h, 0303h	      ; 15  Yy Uu Ii Oo
    dw 1212h, 0000h, 0000h, 0fefeh	    ; 19  Pp {[ }] ret
    dw 0000h, 0505h, 0808h, 1010h	      ; 1d  Lctrl Aa Ss Dd
    dw 0e0eh, 0d0dh, 1414h, 2222h	      ; 21  Ff Gg Hh Jj
    dw 2424h, 0c0ch, 2928h, 0000h	      ; 25  Kk Ll :; "'
    dw 0000h, 0000h, 0000h, 2626h	      ; 29  ~` Lshift |\ Zz
    dw 1515h, 0a0ah, 1111h, 1313h	      ; 2d  Xx Cc Vv Bb
    dw 0606h, 0909h, 002eh, 0025h	      ; 31  Nn Mm <, >.
    dw 2f27h, 0000h, 2d2dh, 0fdfdh	    ; 35  ?/ Rshift * Lalt
    dw 0fefeh			                      ; 39  space


; returns huffman coded chars or -1 (Esc), -2 (spacebar or Enter) or -3 (Alt)
; zero is filtered
; sets flags according to al
key:
    DUP_
    push esi
    push edi
key0:
    xor	eax, eax
    call dopause
    call getcfkey
    cmp al, 3ah			; limit to 39
    jnc key0
    add eax, eax		; double to account for shifted characters
    add eax, shifted		; +1 if shifted
    mov al, [keys + eax]		; index into keys
    and al, al
    jz key0			; repeat if zero
    pop edi
    pop esi
    ret

; programmable keys. Scan code to colorforth character codes
; 0x00:
;     . esc 1 2 3 4 5 6
; 0x08:
;     7 8 9 0 - = bs tab
; 0x10:
;     Q W E R T Y U I
; 0x18:
;     O P [ ] ret Lctrl A S
; 0x20:
;     D F G H J K L ;
; 0x28:
;     ' ` Lshift \ Z X C V
; 0x30:
;     B N M , . / Rshift *
; 0x38:
;     Lalt space.F1|F2|F3|F4|F5
; 0x40:
;     F6|F7|F8|F9|F10..KP7
; 0x48:
;     KP8|KP9|KP-|KP4|KP5|KP6|KP+|KP1
; 0x50:
;     KP2|KP3|KP0|KP.|...F11
; 0x58:
;     F12
pkeys db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 	;  0..7
    db 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0	  ;  8..f
    db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 	  ; 10..17
    db 0 , 0 , 0 , 0 , 2 , 0 , 0 , 0	  ; 18..1f
    db 0 , 0 , 0 , 0 , 20, 17, 25, 22	  ; 20..27
    db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0	  ; 28..2f
    db 0 , 0 , 18, 0 , 0 , 26, 0 , 0 	  ; 30..37
    db 3 , 2 , 0 , 4 , 5 , 6 , 7 , 8	  ; 38..3f
    db 9 , 10, 11, 12, 13, 0 , 0 , 16	  ; 40..47
    db 17, 18, 19, 20, 21, 22, 23, 24	  ; 48..4f
    db 25, 26, 27, 28, 0 , 0 , 0 , 14	  ; 50..57
    db 15								                ; 58

; key handler for pad. Returns 0..27 for
; the 28 programmable keys.

pkey:
  DUP_
  push esi
  push edi
  .0:
    xor	eax, eax
    call dopause
    call getcfkey
    cmp al, 59h ; al - 59h
    jnc .0 ; jump if not carry, so if al > 59h
    mov al, [pkeys + eax] ; else, al was smaller than 59h
    and al, al ; WHY ?
    jz .0
    dec al
    pop edi
    pop esi
    ret

; QWERTY end
