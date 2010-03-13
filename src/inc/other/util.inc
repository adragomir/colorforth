%push dummy

; optimization Macros stolen from system.inc by Konstantin Boldyshev
%assign __O_SIZE__  0
%assign __O_SPEED__ 1

%ifndef __OPTIMIZE__
%assign __OPTIMIZE__ __O_SIZE__ ;size/speed
%endif
;
;intellectual register assignment, generates smaller code.. long weird macro.
;warning macro may touch flags!
;

%macro _mov 2-3.nolist
%ifnidn %2,EMPTY
 %if __OPTIMIZE__=__O_SPEED__
    %ifid %2
    mov %1,%2
    %elifnum %2
     %if %2=0
    sub %1,%1
     %else
    mov %1,%2
     %endif
    %else
    mov %1,%2
    %endif
 %else          ;%if __OPTIMIZE__=__O_SIZE__
    %ifid %2
     %ifnidni %1,%2
    mov %1,%2
     %endif
    %elifstr %2
    mov %1,%2
    %elifnum %2
     %if %2=0
    xor %1,%1
     %elif %2<0 && %2>0xffffff7f
    push    byte %2
    pop %1
     %elif %2<0
    mov %1,%2
     %elif %2<0x80
    push    byte %2
    pop %1
     %elif %2<0x100
    %ifidn %1,eax
        __setreg32_08_lo al,%1,%2
    %elifidn %1,ebx
        __setreg32_08_lo bl,%1,%2
    %elifidn %1,ecx
        __setreg32_08_lo cl,%1,%2
    %elifidn %1,edx
        __setreg32_08_lo dl,%1,%2
    %else
        mov %1,%2
    %endif
     %elif %2<0x00010000
    %if (%2 % 0x100) = 0
        %ifidn %1,eax
        __setreg32_08_hi ah,%1,%2
        %elifidn %1,ebx
            __setreg32_08_hi bh,%1,%2
        %elifidn %1,ecx
        __setreg32_08_hi ch,%1,%2
        %elifidn %1,edx
        __setreg32_08_hi dh,%1,%2
        %else
        mov %1,%2
        %endif
    %else
        %ifidn %1,eax
        __setreg32_16 ax,%1,%2
        %elifidn %1,ebx
        __setreg32_16 bx,%1,%2
        %elifidn %1,ecx
        __setreg32_16 cx,%1,%2
        %elifidn %1,edx
        __setreg32_16 dx,%1,%2
        %else
        mov %1,%2
        %endif
    %endif
     %else
    mov %1,%2
     %endif
    %else
    mov %1,%2
    %endif
 %endif
%endif
%endmacro

;
; _mov helpers
;

%macro __setreg32_08_lo 3
%ifnidn %3,EMPTY
    xor %2,%2
    mov %1,%3
%endif
%endmacro

%macro __setreg32_08_hi 3
%ifnidn %3,EMPTY
    xor %2,%2
    mov %1,(%3 / 0x100)
%endif
%endmacro

%macro __setreg32_16 3
%ifnidn %3,EMPTY
    %if %3=0xFFFF
        xor %2,%2
    dec %1
    %else
    mov %2,%3
    %endif
%endif
%endmacro

;
; another weird one )
;

%macro _add 2.nolist
%if %2 != 0
 %if __OPTIMIZE__=__O_SPEED__

    add %1,%2

 %else          ;%if __OPTIMIZE__=__O_SIZE__

    %if %2=1
    inc %1
    %elif %2=2
    inc %1
    inc %1
    %elif %2=0xFFFFFFFF || %2=-1
    dec %1
    %elif %2=0xFFFFFFFE || %2=-2
    dec %1
    dec %1
    %elif %2=0x80
    add %1,byte 0x7F
    inc %1
    %elif %2>0 && %2<0x80
    add %1,byte %2
    %elif %2<0 && %2>-0x80
    sub %1, byte  -(%2)
    %else
    add %1,%2
    %endif

 %endif

%endif
%endmacro

;
;
;

%macro _sub 2.nolist
%if %2 != 0
 %if __OPTIMIZE__=__O_SPEED__

    sub %1,%2

 %else          ;%if __OPTIMIZE__=__O_SIZE__

    %if %2=1
    dec %1
    %elif %2=2
    dec %1
    dec %1
    %elif %2=0x80
    sub %1,byte 0x7F
    dec %1
    %elif %2>0 && %2<0x80
    sub %1,byte %2
    %elif %2=-2 || %2=0xfffffffe
    inc %1
    inc %1
    %elif %2=-1 || %2=0xffffffff
    inc %1
    %elif %2>-0x80 && %2<0
    add %1, byte  -(%2)
    %else
    sub %1,%2
    %endif

 %endif

%endif
%endmacro

%macro _cmp 2.nolist
%ifid   %2
    cmp %1,%2
%elif %2=0
    or  %1,%1
%elif (%2>0 && %2<0x80) || (%2<0 && %2>-0x80)
    cmp %1,byte %2
%else
    cmp %1,%2
%endif
%endmacro

%macro _and 2.nolist
%ifid   %2
    and %1,%2
%elif %2=0
    xor %1,%1
%elif (%2>0 && %2<0x80) || (%2<0 && %2>-0x80)
    and %1,byte %2
%else
    and %1,%2
%endif
%endmacro

%macro _or 2.nolist
%ifid   %2
    or  %1,%2
%elif (%2>0 && %2<0x80) || (%2<0 && %2>-0x80)
    or  %1,byte %2
%else
    or  %1,%2
%endif
%endmacro

%macro _xor 2.nolist
%ifid   %2
    xor %1,%2
%elif (%2>0 && %2<0x80) || (%2<0 && %2>-0x80)
    xor %1,byte %2
%else
    xor %1,%2
%endif
%endmacro

%macro _push 1-2.nolist
%ifidn  %1,EMPTY
    %if %0>1
    push    %2
    %endif
%elifid %1
    push    DWORD %1
%elifstr %1
    push    DWORD %1
%elifnum    %1
    %if %1=0 || (%1>0 && %1<0x80) || (%1<0 && %1>-0x80)
    push    byte %1
    %else
    push    DWORD %1
    %endif
%else
    push    DWORD %1
%endif
%endmacro

;PR <xchg> is very slow, reg/mem takes 33 clocks.
;this macro for nasm helps speeding up regardless of what addressing mode used

%macro _xchg 2.nolist
%if __OPTIMIZE__=__O_SPEED__
%assign ea 4
%else
%assign ea 0
%ifid %{2}
%assign ea ea|2
%endif
%ifid %{1}
%assign ea ea|1
%endif
%endif
;handles reg,mem and mem,reg differently, because
;   9 clocks if last, negated opd is in memory
;   5 clocks if that one is a register
%if ea=2
    add %{1},%{2}
    sub %{2},%{1}
    add %{1},%{2}
    neg DWORD %{2}
%elif ea=1
    add %{2},%{1}
    sub %{1},%{2}
    add %{2},%{1}
    neg DWORD %{1}
%else
;  2 clocks w. reg,reg
;   (lea variant wasn't faster, may be cpu dependent?)
; 33 clocks w. reg,mem or mem,reg - either case
    xchg %{1},%{3}
%endif
%endmacro

;this one is from icedump

%assign UNNAMED_LOCAL_LABEL 0

%define @F ..@UNNAMED_LOCAL_LABEL0

%macro define@F 1
%define @F ..@UNNAMED_LOCAL_LABEL%{1}
%endmacro

%define @@ @@ UNNAMED_LOCAL_LABEL

%macro @@ 1.nolist
..@UNNAMED_LOCAL_LABEL%{1}:
%define @B ..@UNNAMED_LOCAL_LABEL%{1}
%assign UNNAMED_LOCAL_LABEL UNNAMED_LOCAL_LABEL+1
define@F UNNAMED_LOCAL_LABEL
%endmacro

%macro param2str 2
        db      %2%1%2
%endmacro

%imacro globaldef 1.nolist
    %define %1 _%1
    global %1
%endmacro

%imacro externdef 1.nolist
    %define %1 _%1
    extern %1
%endmacro


%imacro proc 0-*.nolist
    %ifctx proc
        %error Cannot nest procedures
    %endif
    %push   proc
%if __OPTIMIZE__=__O_SPEED__
    align 32
%endif
%{00}:
    %assign %$procargs 0
    %assign %$LOCAL 0
    %assign %$ARG 8
    %define %$procname %00
    %rep %0  
        %assign %%1 %$ARG
        %xdefine .%{1} ebp+%%1
        %assign %$ARG %$ARG+4
        %assign %$procargs %$procargs+1
        %rotate 1
    %endrep
%endmacro

%imacro endp 0.nolist
    %ifnctx proc
        %error expected 'proc' before 'endproc'.
    %else
        %if %$procname <> %00
            %error proc name mismatch
        %endif
        %pop
    %endif
%{00}.size  equ $-%{00}
%endmacro

%macro return 0-1.nolist
    %ifctx proc
    %ifdef %$savedregs
        popd    %$savedregs
    %endif
    %if %$LOCAL > 0
            sfree   %$LOCAL
    %endif
    %ifdef %$enter
    leave
    %endif
    %endif ;ctx proc
%ifctx PROC
    %ifnum %1
        retn %1
    %else
    %IF (%$procargs*4)<>0
        retn (%$procargs*4)
    %else
        retn
    %endif
    %endif
%else
    retn %1
%endif
%endmacro

%imacro local 1.nolist
    %assign %$LOCAL %$LOCAL+%1
    %assign %%1 %$LOCAL
    %xdefine .%{00} ebp-%%1
%endmacro

%imacro locald 1.nolist
%00 local 4*%1
%endmacro

%idefine localb local

%imacro localw 1.nolist
%00  local 2*%1
%endmacro

%imacro staticb  1+.nolist
udataseg
.%00  resb    %1
codeseg
%endmacro

%imacro staticw     1+.nolist
udataseg
.%00  resw    %1
codeseg
%endmacro

%imacro staticd  1+.nolist
udataseg
.%00  resd    %1
codeseg
%endmacro

;enter HLL proc and allocate space on stack with proper alingment
%imacro enterproc 0.nolist
    %define %$enter
    %if %$LOCAL>0
        %if %$LOCAL < 4
            %assign %$LOCAL 4
        %endif
        push    ebp
        mov     ebp,esp
        salloc  %$LOCAL
    %else
        push	ebp
        mov		ebp,esp
    %endif
%endmacro

%macro push 1.nolist
    %ifstr %1
    CONSTSEG
    %%str:  TEXTA %1
        db 0
    CODESEG
    push    %%str
    %else
    _push    %1
    %endif
%endmacro

%macro pushW 1.nolist
    %ifstr %1
    CONSTSEG
    %%str:  TEXTW %1
        dw 0
    CODESEG
    push    %%str
    %else
        %error Argument must be UNICODE string!
    %endif
%endmacro

%imacro pushd 1-*.nolist
    %rep %0
        push %1
    %rotate 1
    %endrep
%endmacro

%imacro popd 1-*.nolist
    %rep %0
    %rotate -1
        pop %1
    %endrep
%endmacro

%imacro uses    1+.nolist
    enterproc
    %define %$savedregs %1
    pushd   %1
%endmacro

%IMACRO cc   1-*.nolist
%define %%1 %1
%rep (%0-1)
    %rotate -1
    %ifnidn %1,EMPTY
    push    DWORD %1
    %endif
%endrep
    call    %%1
    _add    esp,(%0-1)*4
%ENDMACRO

%IMACRO sc 1-*.nolist
%define %%1 %1
%rep (%0-1)
    %rotate -1
    %ifnidn %1,EMPTY
    push    DWORD %1
    %endif
%endrep
    call %%1
%ENDMACRO

%define D DWORD
%define B BYTE
%define W WORD
%idefine jmps jmp short

;------------------------------------------------------------------------------
; macro support for NT kernel like debug flags, example:
;
;   segment .data
;   DebugFlags: times 2 dd 0	; room for 64 bit flags, [0..63]
;
;   segment .text
;   debug_start DebugFlags, 43	; test bit 43, DebugFlags[43]
;   <code to emit some debug message>
;   debug_end
;
; flags should be allocated in dword amounts because of the nature of the BT
; instruction.
;
; !!! macros modify/corrupt EFLAGS !!!
;------------------------------------------------------------------------------

%macro debug_start 2
	%push debug
	bt	dword [%{1}+4*(%{2}/32)], %{2}%32
	jnc	%$skip_debug
%endmacro

%macro debug_end 0
%ifctx debug
	%$skip_debug:
	%pop
%else
	%error no matching debug_start for debug_end
%endif
%endmacro


;------------------------------------------------------------------------------
; macro support for initialization level
;
; the idea is similar to c++ constructors/destructors
; the macros build a table of function pairs and call them in the right order
;
; constructors/destructors MUST preserve ECX/EDX at least and return STC on
; error (although it is ignored for destructors)
;
; example:
;
;   segment .data
;   init_data_begin MYAPP
;   init_data_item Constructor1, Destructor1
;   init_data_item Constructor2, 0            ; these two are
;   init_data_item Constructor3               ; the same
;   init_data_item 0,            Destructor4
;   init_data_end
;
;   segment .text
;
;   MainInitFunc:
;     init_construct MYAPP
;     jc .error
;
;     retn
;
;   .error:
;     call MainDeinitFunc
;     retn
;
;   MainDeinitFunc:
;     init_destruct MYAPP
;     retn
;
; note that if ConstructorX fails, then DestructorX will NOT be called, (only
; DestructorX-1 and below), in other words, a failing Constructor MUST do a
; proper cleanup (e.g. call DestructorX itself if it can handle that)
;------------------------------------------------------------------------------

%macro init_data_begin 1
	%push init_data
	align 4
	%{1}_init_data:
	.level:		dd 0
	.funcs:
%endmacro

%macro init_data_item 1-2 0
%ifctx init_data
			dd %{1}, %{2}
%else
	%error no matching init_data_start for init_data_item
%endif
%endmacro

%macro init_data_end 0
%ifctx init_data
	.size:		dd ($-.funcs)/8
	%pop
%else
	%error no matching init_data_start for init_data_end
%endif
%endmacro

%macro init_construct 1
	push	ecx
	push	edx

	mov	ecx,[%{1}_init_data.size]
	mov	edx,%{1}_init_data.funcs

%%next:
	cmp	dword [edx],byte 0
	jz	%%skip

	call	[edx]
	jc	%%exit

%%skip:
	add	edx,byte 8
	inc	dword [%{1}_init_data.level]
	loop	%%next

	clc

%%exit:
	pop	edx
	pop	ecx	
%endmacro

%macro init_destruct 1
	push	ecx
	push	edx

	mov	ecx,[%{1}_init_data.level]
	jecxz	%%exit

	lea	edx,[%{1}_init_data.funcs+4-8+8*ecx]

%%prev:
	cmp	dword [edx],byte 0
	jz	%%skip

	call	[edx]

%%skip:
	sub	edx,byte 8
	dec	dword [%{1}_init_data.level]
	loop	%%prev

%%exit:
	pop	edx
	pop	ecx	
%endmacro

; numit_or
%macro TEXTA 1
 %assign  %%a 0
 %assign  %%b 0
 %strlen  %%b %1
 %assign  %%c 1
 %assign  %%d 0
 %rep     %%b
  %substr  %%a %1 %%c
  %if %%d=0
    %ifidn %%a, '\'
      %assign %%d 1
    %else
      DB %%a
    %endif
  %else
    %assign %%d 0
    %ifidn %%a, '\'
      DB '\'
    %elifidn %%a, 'n'
      DB 0Dh, 0Ah
    %elifidn %%a, 'r'
      DB 0Dh
    %elifidn %%a, 'l'
      DB 0Ah
    %elifidn %%a, 's'
      DB 20h
    %elifidn %%a, 'c'
      DB 3Bh
    %elifidn %%a, 't'
      DB 09h
    %elifidn %%a, '0'
      DB 0
    %elifidn %%a, '1'
      DB 1
    %else
      DB '\', %%a
    %endif
  %endif
  %assign %%c %%c+1
 %endrep
%endmacro

%macro TEXTW 1
 %assign  %%a 0
 %assign  %%b 0
 %strlen  %%b %1
 %assign  %%c 1
 %assign  %%d 0
 %rep     %%b
  %substr  %%a %1 %%c
  %if %%d=0
    %ifidn %%a, '\'
        %assign %%d 1
    %else
      DW %%a
    %endif
  %else
    %assign %%d 0
    %ifidn %%a, '\'
      DW '\'
    %elifidn %%a, 'n'
      DW 0Dh, 0Ah
    %elifidn %%a, 'r'
      DW 0Dh
    %elifidn %%a, 'l'
      DW 0Ah
    %elifidn %%a, 's'
      DW 20h
    %elifidn %%a, 'c'
      DW 3Bh
    %elifidn %%a, 't'
      DW 09h
    %elifidn %%a, '0'
      DW 0
    %elifidn %%a, '1'
      DW 1
    %else
      DW '\', %%a
    %endif
  %endif
  %assign %%c %%c+1
 %endrep
%endmacro

%macro salloc 1
    %if %1 == 4
        push     ecx
    %elif %1 == 8
        push    ecx
        push    edx
    %elif   %1 == 12
        push    ecx
        push    edx
        push    ecx
    %else
	    _sub	esp,((%1) + 3) & 0xFFFFFFFC
    %endif
%endmacro

%macro sfree 1
    %if %1 == 4
        pop     ecx
    %elif %1 == 8
        pop     ecx
        pop     edx
    %elif   %1 == 12
        pop     ecx
        pop     edx
        pop     ecx
    %else
    	_add    esp,(((%1) + 3) & 0xFFFFFFFC)
    %endif
%endmacro

%macro  max 3
    cmp     %1,%2
    jg      %%skip
        mov     %3,%2
        jmp     %%end
    %%skip:
        mov     %3,%1
    %%end:  
%endmacro

%macro  min 3
    cmp     %1,%2
    jl      %%skip
        mov     %3,%2
        jmp     %%end
    %%skip:
        mov     %3,%1
    %%end:  
%endmacro

struc _PUSHAD
.EDI:   resd 1
.ESI:   resd 1
.EBP:   resd 1
.ESP:   resd 1
.EBX:   resd 1
.EDX:   resd 1
.ECX:   resd 1
.EAX:   resd 1
endstruc

FLAGS.CF				EQU	0000000000000001B	; CARRY	FLAG
FLAGS.PF				EQU	0000000000000100B	; PARITY FLAG
FLAGS.AF				EQU	0000000000010000B	; AUXILARY FLAG
FLAGS.ZF				EQU	0000000001000000B	; ZERO FLAG
FLAGS.SF				EQU	0000000010000000B	; SIGN FLAG
FLAGS.TF				EQU	0000000100000000B	; TRAP FLAG
FLAGS.IF				EQU	0000001000000000B	; INTERRUPT FLAG
FLAGS.DF				EQU	0000010000000000B	; DIRECTION FLAG
FLAGS.OF				EQU	0000100000000000B	; OVERFLOW FLAG