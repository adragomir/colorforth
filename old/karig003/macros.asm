;###############################################################
;
;   MACROS.ASM
;
;###############################################################

;===============================================================
; CONDITIONAL-INSTRUCTION MACROS
;===============================================================

; Conditional instruction prefixes:
; -- "call" = call
;    (call on condition)
; -- "cab" = call with abort
;    (after call, return on condition)
; -- "ret" = return

; Conditional instruction suffixes are what you'd expect:
; -- a (above), b (below), c (carry/error), e (equal),
;    g (greater), l (less), n (not), o (overflow),
;    p (parity), s (signed), z (zero/false)
; -- Note that I use the carry flag to signal an error,
;    and the zero flag to answer a yes-or-no question
;    (e.g, call is_cache_dirty -- jz cache_is_clean).
;    [CF=0 -- OK; CF=1 -- error; ZF=0 -- true; ZF=1 -- false]

; Thus:
; -- "callz" means "CALL if Zero flag is set" or "CALL if false."
; -- "cabc" means "Call, then ABort if Carry is set."
; -- "reto" means "RETurn if Overflow flag is set."

;-----------------------------------------------------------------------
; CONDITIONAL-RETURN INSTRUCTIONS
;-----------------------------------------------------------------------

%imacro retcc 1 ; return on condition
		j%-1	%%skip
		ret
	%%skip:
	%endmacro

; A = ABOVE
%idefine reta   retcc a
%idefine retna  retcc na
%idefine retae  retcc ae
%idefine retnae retcc nae

; B = BELOW
%idefine retb   retcc b
%idefine retnb  retcc nb
%idefine retbe  retcc be
%idefine retnbe retcc nbe

; C = CARRY/ERROR
%idefine retc   retcc c
	; Return (abort) if carry is set.
%idefine retnc  retcc nc
	; Return if carry clear; rest of routine handles error.

; E = EQUAL
%idefine rete   retcc e
%idefine retne  retcc ne

; G = GREATER
%idefine retg   retcc g
%idefine retng  retcc ng
%idefine retge  retcc ge
%idefine retnge retcc nge

; L = LESS
%idefine retl   retcc l
%idefine retnl  retcc nl
%idefine retle  retcc le
%idefine retnle retcc nle

; O = OVERFLOW
%idefine reto   retcc o
%idefine retno  retcc no

; P = PARITY
%idefine retp   retcc p
%idefine retnp  retcc np
%idefine retpe  retcc pe
%idefine retpo  retcc po

; S = SIGNED
%idefine rets   retcc s
%idefine retns  retcc ns

; Z = ZERO/FALSE
%idefine retz   retcc z
%idefine retnz  retcc nz

;-----------------------------------------------------------------------
; CONDITIONAL-CALL INSTRUCTIONS
;-----------------------------------------------------------------------

%imacro callcc 2 ; generic call-on-condition macro
		j%-1	%%skip
		call	%2
	%%skip:
	%endmacro

%imacro calla 1
		callcc a, %1
		%endmacro
%imacro callna 1
		callcc na, %1
		%endmacro
%imacro callae 1
		callcc ae, %1
		%endmacro
%imacro callnae 1
		callcc nae, %1
		%endmacro
%imacro callb 1
		callcc b, %1
		%endmacro
%imacro callnb 1
		callcc nb, %1
		%endmacro
%imacro callbe 1
		callcc be, %1
		%endmacro
%imacro callnbe 1
		callcc nbe, %1
		%endmacro
%imacro callc 1
		callcc c, %1
		%endmacro
%imacro callnc 1
		callcc nc, %1
		%endmacro
%imacro calle 1
		callcc e, %1
		%endmacro
%imacro callne 1
		callcc ne, %1
		%endmacro
%imacro callg 1
		callcc g, %1
		%endmacro
%imacro callng 1
		callcc ng, %1
		%endmacro
%imacro callge 1
		callcc ge, %1
		%endmacro
%imacro callnge 1
		callcc nge, %1
		%endmacro
%imacro calll 1
		callcc l, %1
		%endmacro
%imacro callnl 1
		callcc nl, %1
		%endmacro
%imacro callle 1
		callcc le, %1
		%endmacro
%imacro callnle 1
		callcc nle, %1
		%endmacro
%imacro callo 1
		callcc o, %1
		%endmacro
%imacro callno 1
		callcc no, %1
		%endmacro
%imacro callp 1
		callcc p, %1
		%endmacro
%imacro callnp 1
		callcc np, %1
		%endmacro
%imacro callpe 1
		callcc pe, %1
		%endmacro
%imacro callpo 1
		callcc po, %1
		%endmacro
%imacro calls 1
		callcc s, %1
		%endmacro
%imacro callns 1
		callcc ns, %1
		%endmacro
%imacro callz 1
		callcc z, %1
		%endmacro
%imacro callnz 1
		callcc nz, %1
		%endmacro

;-----------------------------------------------------------------------
; CONDITIONAL-CALL-AND-ABORT INSTRUCTIONS
; Call subroutine, then abort (return) if condition exists.
;-----------------------------------------------------------------------

%imacro cabcc 2
		call	%2
		retcc	%1
	%endmacro

%imacro caba 1
		cabcc a, %1
		%endmacro
%imacro cabna 1
		cabcc na, %1
		%endmacro
%imacro cabae 1
		cabcc ae, %1
		%endmacro
%imacro cabnae 1
		cabcc nae, %1
		%endmacro
%imacro cabb 1
		cabcc b, %1
		%endmacro
%imacro cabnb 1
		cabcc nb, %1
		%endmacro
%imacro cabbe 1
		cabcc be, %1
		%endmacro
%imacro cabnbe 1
		cabcc nbe, %1
		%endmacro
%imacro cabc 1
		cabcc c, %1
		%endmacro
%imacro cabnc 1
		cabcc nc, %1
		%endmacro
%imacro cabe 1
		cabcc e, %1
		%endmacro
%imacro cabne 1
		cabcc ne, %1
		%endmacro
%imacro cabg 1
		cabcc g, %1
		%endmacro
%imacro cabng 1
		cabcc ng, %1
		%endmacro
%imacro cabge 1
		cabcc ge, %1
		%endmacro
%imacro cabnge 1
		cabcc nge, %1
		%endmacro
%imacro cabl 1
		cabcc l, %1
		%endmacro
%imacro cabnl 1
		cabcc nl, %1
		%endmacro
%imacro cable 1
		cabcc le, %1
		%endmacro
%imacro cabnle 1
		cabcc nle, %1
		%endmacro
%imacro cabo 1
		cabcc o, %1
		%endmacro
%imacro cabno 1
		cabcc no, %1
		%endmacro
%imacro cabp 1
		cabcc p, %1
		%endmacro
%imacro cabnp 1
		cabcc np, %1
		%endmacro
%imacro cabpe 1
		cabcc pe, %1
		%endmacro
%imacro cabpo 1
		cabcc po, %1
		%endmacro
%imacro cabs 1
		cabcc s, %1
		%endmacro
%imacro cabns 1
		cabcc ns, %1
		%endmacro
%imacro cabz 1
		cabcc z, %1
		%endmacro
%imacro cabnz 1
		cabcc nz, %1
		%endmacro

;===============================================================
; KORTH MACROS
; Mainly derived from code written for Chuck Moore's ColorForth
;===============================================================

;---------------------------------------------------------------
; BASIC STACK OPERATIONS
;---------------------------------------------------------------

%macro	_dup 0
		; ( n -- n n )
		; Duplicate first item on data stack.
		lea	esi, [esi-4]
		mov	[esi], eax
		%endmacro

%macro	_drop 0
		; ( n -- )
		; Drop first item from data stack.
		lodsd
		%endmacro

%macro	_lit 1
		; ( -- n )
		; Push literal onto data stack.
		_dup
		mov	eax, %1
		%endmacro

%macro	_over 0
		; ( m n -- m n m )
		; Duplicate second item on data stack.
		_dup
		mov	eax, [esi+4]
		%endmacro

%macro	_nip 0
		; ( m n -- n )
		; Drop second item from data stack.
		lea	esi, [esi+4]
		%endmacro

%macro	_swap 0
		; ( m n -- n m )
		; Swap first and second items on data stack.
		mov	ebx, eax
		mov	eax, [esi]
		mov	[esi], ebx
		%endmacro

%macro	_push 0
		; ( n -- ) [ -- n ]
		; Move item from data stack to return stack.
		push	eax
		_drop
		%endmacro

%macro	_pop 0
		; ( -- n ) [ n -- ]
		; Move item from return stack to data stack.
		_dup
		pop	eax
		%endmacro

;---------------------------------------------------------------
; FLOW CONTROL
;---------------------------------------------------------------

%macro	_if 0
		; ( -- )
		; Branch to "then" if zero flag is set.
		jz	.then
		%endmacro

%macro	_then 0
		; ( -- )
		; Jump here if zero flag not set (after "_if").
	.then:
		%endmacro

%macro	_for 0
		; ( n -- ) [ -- n ]
		; Start loop to do something n times.
	.for:	_push             ; Push count onto return stack.
		%endmacro

%macro	_next 0
		; ( -- n ) [ n -- ] IF WE LOOP BACK
		; ( -- ) [ n -- ] IF LOOP IS FINISHED
		; End loop to do something n times.
		_pop	          ; Get count from return stack.
		dec	eax       ; Keep looping back until
		jnz	.for      ;   count reaches zero.
		_drop	          ; Drop the zero from the stack.
		%endmacro

