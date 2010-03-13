%ifndef __MACROS_H
%define __MACROS_H

;--------- Extracted from Mammon's APJ #3 "Extending Nasm" tutorial ------------------------
; http://www.eccentrix.com/members/mammon/Text/nasm_apj.txt

%macro save_regs 0
  push eax
  push ebx
  push ecx
  push edx
%endmacro

%macro push_all 1-*
  %rep %0
    push %1
    %rotate 1
  %endrep
%endmacro

%macro pop_all 1-*
  %rep %0
    %rotate -1
    pop %1
  %endrep
%endmacro

;-Sys Call
;   syscall funcName, param1, param2, param3
%macro syscall 2-*
  %define _sysfunc %1
  ;push eax
  %rep %0 - 1
    %rotate -1
    push dword %1
  %endrep
  mov eax, _sysfunc
  sub esp, 4
  int 0x80
  add esp, (%0) * 4
  ;pop eax
%endmacro


%macro syscall 1-*
  %define _sysfunc %1
  %assign __params %0 - 1
  %assign __stack_substraction (4 - (__params % 4 ))

  ; comparable to a ENTER instruction
  push ebp
  mov ebp, esp

  ; http://www.asmcommunity.net/board/index.php?topic=29506.0 - duntemann calling conventions, save registers
  push_all ebx, ecx, edx, esi, edi

  ; Mac OS X requires a fucking stack alignment to 16 bytes
  and esp, 0xFFFFFFF0

  ; then, we must substract something more, so that after substracting and adding x parameters, the stack is 16 byte aligned again
  sub esp, __stack_substraction * 4

  %rep %0-1
    %rotate -1
    push %1
  %endrep
  mov eax, _sysfunc
  sub esp, 4
  int 0x80

  ; because we substracted, we need to remake the stack
  add esp, __params * 4 + __stack_substraction * 4

  pop_all ebx, ecx, edx, esi, edi

  ; comparable to a leave instruction
  mov esp, ebp
  pop ebp
%endmacro


;-High-Level Call
;   ccall  FuncName, param1, param2, param 3... ;Pascal: 1st-1st, no clean
;   pcall  FuncName, param1, param2, param 3... ;C: Last-1st, stack cleanup
;   stdcall  FuncName, param1, param2, param 3... ;StdCall: last-1st, no clean
;   fastcall FuncName, param1, param2, param 3... ;FastCall: registers/stack
%macro pcall 2-*         
  %define _func %1
  %rep %0 - 1
    %rotate -1
    push %1
  %endrep
  call _func
%endmacro

; push ebp
; mov ebp, esp
; sub esp, X
; 
; For example, here is a C function code fragment and the resulting assembly instructions:
; 
; void MyFunction()
; {
;   int a, b, c;
;   ...
; 
; push ebp   ; save the value of ebp
; mov ebp, esp ; ebp now points to the top of the stack
; sub esp, 12  ; space allocated on the stack for the local variables

%macro ccall 1-*
  %define _func %1
  %assign __params %0 - 1
  %assign __stack_substraction (4 - (__params % 4 ))

  ; comparable to a ENTER instruction
  push ebp
  mov ebp, esp

  ; http://www.asmcommunity.net/board/index.php?topic=29506.0 - duntemann calling conventions, save registers
  push_all ebx, ecx, edx, esi, edi

  ; Mac OS X requires a fucking stack alignment to 16 bytes
  and esp, 0xFFFFFFF0

  ; then, we must substract something more, so that after substracting and adding x parameters, the stack is 16 byte aligned again
  sub esp, __stack_substraction * 4

  %rep %0-1
    %rotate -1
    push %1
  %endrep
  call _func

  ; because we substracted, we need to remake the stack
  add esp, __params * 4 + __stack_substraction * 4

  pop_all ebx, ecx, edx, esi, edi

  ; comparable to a leave instruction
  mov esp, ebp
  pop ebp
%endmacro

%macro stdcall 2-* ;Windows StdCall: push last value first, no cleanup
  %define _func %1
  %rep %0-1
    %rotate -1
    push %1
  %endrep
    call _func
%endmacro

%macro fastcall 2-*        
  %define _func %1
  %assign __pnum 1
  %rep %0-4
    %rotate -1
    %if __pnum = 1
      mov eax, %1
    %elif __pnum = 2
      mov edx, %1
    %elif __pnum = 3
      mov ebx, %1
    %else
      push %1
    %endif
    %assign __pnum __pnum + 1
  %endrep
    call _func
%endmacro

;-IF-THEN-ELSE Loops
;----------IF-THEN-ELSE Loops
;Usage:
;   IF Value, Cond, Value
;  ;if code here
;   ELSIF Value, Cond, Value
;  ;else-if code here
;   ELSE
;  ;else code here
;   ENDIF
;Where Value is a variable, register, or integer -- but not EAX or EBX!!
;Where Cond is the conditional -- a, ae, b, be, e, g, ge, le, ne, nz, z,
;etc..
%macro IF 3
  %push if
  %assign __curr 1
; %if %1 != eax
  mov eax, %1
; %endif
; %if %3 != ebx
  mov ebx, %3
; %endif
  cmp eax, ebx
  j%+2 %%if_code
  jmp %$loc(__curr)
  %%if_code:
%endmacro

%macro ELSIF 3
  %ifctx if
    jmp %$end_if
    %$loc(__curr):
    %assign __curr __curr+1
    ;  %if %1 != eax
    mov eax, %1
    ;  %endif
    ;  %if %3 != ebx
    mov ebx, %3
    ;  %endif
    cmp eax, ebx
    j%+2 %%elsif_code
    jmp %$loc(__curr)
    %%elsif_code:
  %else
    %error "'ELSIF' can only be used following 'IF'"
  %endif
%endmacro

%macro ELSE 0
  %ifctx if
    jmp %$end_if
    %$loc(__curr):
    %assign __curr __curr+1
  %else
    %error "'ELSE' can only be used following an 'IF'"
  %endif
%endmacro

%macro ENDIF 0
  %$loc(__curr):
  %$end_if:
  %pop
%endmacro


;-SWITCH-CASE Blocks
;Usage:
;   SWITCH Variable
;   CASE Int
;    ;case1 code here
;    BREAK
;   CASE Int
;    ;case2 code here
;    BREAK
;   DEFAULT
;    ;default code here
;    BREAK
;   ENDSWITCH
;Where Variable is a variable of INT to DWORD value [eax is used to store]
;Where Int is an immediate INT to DWORD value [ebx is used to store]
%macro SWITCH 1
  %push switch
  %assign __curr 1
  mov eax, %1
  jmp %$loc(__curr)
%endmacro

%macro CASE 1
  %ifctx switch
    %$loc(__curr):
    %assign __curr __curr+1
    mov ebx, %1
    cmp eax, ebx
    jne %$loc(__curr)
  %else
    %error "'CASE' can only be used in a 'SWITCH' block"
  %endif
%endmacro

%macro DEFAULT 0
  %ifctx switch
    %$loc(__curr):
  %else
    %error "'DEFAULT' can only be used in a 'SWITCH' block"
  %endif
%endmacro

%macro BREAK 0
  jmp %$endswitch
%endmacro

%macro ENDSWITCH 0
  %ifctx switch
  %$endswitch:
  %pop
  %else
     %error "'ENDSWITCH' can only be used in an 'SWITCH' block"
  %endif
%endmacro

;-DO-FOR and DO-WHILE Loops
;Usage:
;   DO
;  ;code to do here
;   FOR min, cond, max, step
;Where Min is a value or variable of INT to DWORD value [eax is used]
;Where Cond is condition -- a, ae, b, be, g, ge, l, le, n, ne, z
;Where Max is an INT to DWORD value [ebx is used to store]
;Where Step is an INT to DWORD value to be added to Min at each loop
;e.g. FOR 0, l, 100, 1 == for(x=0; x<100, x++) do code...;
;   DO
;  ;code to do here
;   WHILE variable, condition, value
;Where variable is a value or variable of INT to DWORD value [eax is used]
;Where condition is a conditional -- a, ae, b, be, g, ge, l, le, n, ne, z
;Where Value is an INT to DWORD value [ebx is used to store]
;e.g. WHILE Counter, l, 100 == while( counter < 100 ) do code...;
%macro DO 0
  %push do
  jmp %$init_loop
  %$start_loop:
  push eax
%endmacro

%macro FOR 4
  %ifctx do
    pop eax
    add eax, %4
    cmp eax, %3
    j%-2 %%end_loop
    jmp %$start_loop
    %$init_loop:
    mov eax, %1
    jmp %$start_loop
    %%end_loop:
    %pop
  %else
    %error "'FOR' must be preceded by a 'DO'"
  %endif
%endmacro

%macro WHILE 3
  %ifctx do
    pop eax
    mov ebx, %3
    cmp eax, ebx
    j%+2 %%end_loop
    jmp %$start_loop
    %$init_loop:
    mov eax, %1
    jmp %$start_loop
    %%end_loop:
    %pop
  %else
    %error "'WHILE' must be preceded by a 'DO'"
  %endif
%endmacro

;-Procedure Declarations
;Usage:
;PROC ProcName Parameter1, Parameter2, Parameter3...
;LOCALDD Dword_Variable [opt]
;LOCALDW Word_Variable [opt]
;LOCALDB Byte_Variable [opt]
;   ;procedure code here
;ENDP
;
;ENTRYPROC
  ;procedure code here
;ENDP
;
;Note that parameters are set up to EQU to ebp-x
;Note that locals are set up to EQU to ebp+x
;Note that neither LOCALS or Parameters are necessary, as in ENTRYPROC

%macro PROC 1-9
  GLOBAL %1
  %1:
    %assign _i 4
    %rep %0-1
    %2 equ [ebp-_i]
    %assign _i _i+4
    %rotate 1
    %endrep
    push ebp
    mov ebp, esp
    %push local
    %assign __ll 0
%endmacro

%macro ENDP 0
  %ifctx local
    %pop
  %endif
  pop ebp
%endmacro

%macro ENTRYPROC 0
  PROC main
%endmacro

%macro LOCALVAR 1
  sub esp, 4
  %1 equ [ebp + __ll]
%endmacro

%macro LOCALDB 1
  %assign __ll __ll+1
  LOCALVAR %1
%endmacro

%macro LOCALDW 1
  %assign __ll __ll+2
  LOCALVAR %1
%endmacro

%macro LOCALDD 1
  %assign __ll __ll+4
  LOCALVAR %1
%endmacro

;Usage:
;CHAR Name, String
;INT Name, Byte
;WORD Name, Word
;DWORD Name, Dword
;e.g.  CHAR  UserName, "Joe User"
;    INT   Timeout,  30
;    WORD  Logins
;    DWORD Password
;Note that not specifying a value causes the variable to default to 0
;Note that strings are automatically zero-terminated
%macro CHAR 1-2 0
  %1: DB   %2,0
%endmacro

%macro INT 1-2 0
  %1: DB   %2
%endmacro

%macro WORD 1-2 0
  %1: DW   %2
%endmacro

%macro DWORD 1-2 0
  %1: DD   %2
%endmacro

%macro	pointer	1
    %1		resd	1
%endmacro

%macro extern_def 1
  %ifndef %1_extern_func
    %define %1_extern_func
    extern %1
  %endif
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;macpac

;;;;;;;;;; start misc.mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MPUSH - Pushes multiple items onto the stack.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters:
;  2-* - list of itesm to push
;
%imacro MPUSH 2-*.nolist
;
 %rep %0
  push %1
  %rotate 1
 %endrep
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MPOP - Pops multiple items off the stack.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters:
;  2-* - list of itesm to pop
;
%imacro MPOP 2-*.nolist
;
 %rep %0
  %rotate -1
  pop %1
 %endrep
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; XISREG - Tests if an identifier is a register. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters:
;  1 - identifier to test 
;
; Sets the single line macro `xisreg_result` to 1 if the paramter is
; a general purpose or floating point register, and 0 if not.
;
%imacro XISREG 1.nolist
 %undef xisreg_result
 %ifidni %1,eax
  %assign xisreg_result 1 
 %elifidni %1,ebx
  %assign xisreg_result 1
 %elifidni %1,ecx
  %assign xisreg_result 1
 %elifidni %1,edx
  %assign xisreg_result 1
 %elifidni %1,esi
  %assign xisreg_result 1
 %elifidni %1,edi
  %assign xisreg_result 1
 %elifidni %1,ebp
  %assign xisreg_result 1
 %elifidni %1,esp
  %assign xisreg_result 1
 %elifidni %1,ax
  %assign xisreg_result 1
 %elifidni %1,bx
  %assign xisreg_result 1
 %elifidni %1,cx
  %assign xisreg_result 1
 %elifidni %1,dx
  %assign xisreg_result 1
 %elifidni %1,si
  %assign xisreg_result 1
 %elifidni %1,di
  %assign xisreg_result 1
 %elifidni %1,bp
  %assign xisreg_result 1
 %elifidni %1,sp
  %assign xisreg_result 1
 %elifidni %1,al
  %assign xisreg_result 1
 %elifidni %1,ah
  %assign xisreg_result 1
 %elifidni %1,bl
  %assign xisreg_result 1
 %elifidni %1,bh
  %assign xisreg_result 1
 %elifidni %1,cl
  %assign xisreg_result 1
 %elifidni %1,ch
  %assign xisreg_result 1
 %elifidni %1,dl
  %assign xisreg_result 1
 %elifidni %1,dh
  %assign xisreg_result 1
 %elifidni %1,st0
  %assign xisreg_result 1
 %elifidni %1,st1
  %assign xisreg_result 1
 %elifidni %1,st2
  %assign xisreg_result 1
 %elifidni %1,st3
  %assign xisreg_result 1
 %elifidni %1,st4
  %assign xisreg_result 1
 %elifidni %1,st5
  %assign xisreg_result 1
 %elifidni %1,st6
  %assign xisreg_result 1
 %elifidni %1,st7
  %assign xisreg_result 1
 %else
  %assign xisreg_result 0 
 %endif
%endmacro
;;;;;;;;; end misc.mac


;;;;;;;;; start variables.mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MBYTE - Declares one or more byte sized variables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - variable names 
;
%imacro MBYTE 1-*.nolist
 %ifctx struct_context
 
  %rep %0 
   STRUCT_UNION_COMMON %1, %$struct_name, 1
   %{$struct_name}.%{1}: resb 1
   %rotate 1
  %endrep

 %elifctx union_context
  
  %if 1 > %$union_largest
   %assign %$union_largest 1
  %endif

  %rep %0
   STRUCT_UNION_COMMON %1, %$union_name, 1
   %{$union_name}.%{1}:
   %rotate 1
  %endrep

 %elifctx rvar_context

  %rep %0
   %1:  
   resb 1
   sizeof_%{1} equ 1 
   %xdefine typeof_%{1} byte 
   %rotate 1
  %endrep

 %else
  %push mbyte_context
 
  %assign %$loop_count %0
  %assign %$value 0
  %if %0 > 1
   ;Get last paramter in the list
   %rep (%0 - 1)
    %rotate 1
   %endrep

   ;If the last paramter is a number, use it as a value.
   %ifnum %1
    %if %1 > 255
     %error "Value for MBYTE is too large."
    %endif
    %assign %$loop_count (%0 - 1)
    %assign %$value %1
   %endif

   ;Restore paramter list to original state
   %rep (%0 + 1)
    %rotate 1
   %endrep
  %endif

  ;Loop through the paramters, creating a variable for each
  %rep %$loop_count
%1:
db %$value
sizeof_%{1} equ 1 
%xdefine typeof_%{1} byte
   %rotate 1
  %endrep

  %pop ;mbyte_context
 %endif
%endmacro

%imacro RBYTE 1+.nolist
 %push rvar_context
  MBYTE %1
 %pop
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MWORD - Declares one or more word sized variables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - variable names 
;
%imacro MWORD 1-*.nolist
 %ifctx struct_context
 
  %rep %0 
   STRUCT_UNION_COMMON %1, %$struct_name, 2
   %{$struct_name}.%{1}: resw 1
   %rotate 1
  %endrep

 %elifctx union_context
  
  %if 2 > %$union_largest
   %assign %$union_largest 2
  %endif

  %rep %0
   STRUCT_UNION_COMMON %1, %$union_name, 2
   %{$union_name}.%{1}:
   %rotate 1
  %endrep

 %elifctx rvar_context

  %rep %0
   %1:  
   resw 1
   sizeof_%{1} equ 2 
   %xdefine typeof_%{1} word 
   %rotate 1
  %endrep

 %else
  %push mword_context

  %assign %$loop_count %0
  %assign %$value 0
  %if %0 > 1
   ;Get last paramter in the list
   %rep (%0 - 1)
    %rotate 1
   %endrep

   ;If the last paramter is a number, use it as a value.
   %ifnum %1
    %if %1 > 65535 
     %error "Value for MWORD is too large."
    %endif
    %assign %$loop_count (%0 - 1)
    %assign %$value %1
   %endif

   ;Restore paramter list to original state
   %rep (%0 + 1)
    %rotate 1
   %endrep
  %endif

  ;Loop through the paramters, creating a variable for each
  %rep %$loop_count
   %1:
   dw %$value
   sizeof_%{1} equ 2 
   %xdefine typeof_%{1} word 
   %rotate 1
  %endrep
 
  %pop ;mword_context
 %endif
%endmacro

%imacro RWORD 1+.nolist
 %push rvar_context
  MWORD %1
 %pop
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MDWORD - Declares one or more dword sized variables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - variable names 
;
%imacro MDWORD 1-*.nolist
 %ifctx struct_context
 
  %rep %0 
   STRUCT_UNION_COMMON %1, %$struct_name, 4
   %{$struct_name}.%{1}: resd 1
   %rotate 1
  %endrep

 %elifctx union_context
  
  %if 4 > %$union_largest
   %assign %$union_largest 4
  %endif

  %rep %0
   STRUCT_UNION_COMMON %1, %$union_name, 4
   %{$union_name}.%{1}:
   %rotate 1
  %endrep

 %elifctx rvar_context

  %rep %0
   %1:  
   resd 1
   sizeof_%{1} equ 4 
   %xdefine typeof_%{1} dword 
   %rotate 1
  %endrep

 %else
  %push mdword_context

  %assign %$loop_count %0
  %assign %$value 0
  %if %0 > 1
   ;Get last paramter in the list
   %rep (%0 - 1)
    %rotate 1
   %endrep

   ;If the last paramter is a number, use it as a value.
   %ifnum %1
    %assign %$loop_count (%0 - 1)
    %assign %$value %1
   %else
    ;%ifnum does not catch floating point numbers...
    XIFNUM %1
    %if xifnum_result = 1
     %assign %$loop_count (%0 - 1)
     %undef %$value
     %xdefine %$value %1
    %endif 
   %endif

   ;Restore paramter list to original state
   %rep (%0 + 1)
    %rotate 1
   %endrep
  %endif

  ;Loop through the paramters, creating a variable for each
  %rep %$loop_count
   %1:
   dd %$value
   sizeof_%{1} equ 4 
   %xdefine typeof_%{1} dword 
   %rotate 1
  %endrep

  %pop ;mdword_context
 %endif
%endmacro

%imacro RDWORD 1+.nolist
 %push rvar_context
  MDWORD %1
 %pop
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MFLOAT - Declares one or more dword sized variables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - variable names 
;
%imacro MFLOAT 1+.nolist
 MDWORD %1
%endmacro

%imacro RFLOAT 1+.nolist
 %push rvar_context
  MDWORD %1
 %pop
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MQWORD - Declares one or more qword sized variables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - variable names 
;
%imacro MQWORD 1-*.nolist
 %ifctx struct_context
 
  %rep %0 
   STRUCT_UNION_COMMON %1, %$struct_name, 8
   %{$struct_name}.%{1}: resq 1
   %rotate 1
  %endrep

 %elifctx union_context
  
  %if 8 > %$union_largest
   %assign %$union_largest 8
  %endif

  %rep %0
   STRUCT_UNION_COMMON %1, %$union_name, 8
   %{$union_name}.%{1}:
   %rotate 1
  %endrep

 %elifctx rvar_context

  %rep %0
   %1:  
   resq 1
   sizeof_%{1} equ 8 
   %xdefine typeof_%{1} qword 
   %rotate 1
  %endrep

 %else
  %push mqword_context

  %assign %$loop_count %0
  %assign %$value 0
  %if %0 > 1
   ;Get last paramter in the list
   %rep (%0 - 1)
    %rotate 1
   %endrep

   ;If the last paramter is a number, use it as a value.
   %ifnum %1
    %assign %$loop_count (%0 - 1)
    %assign %$value %1
   %else
    ;%ifnum does not catch floating point numbers...
    XIFNUM %1
    %if xifnum_result = 1
     %assign %$loop_count (%0 - 1)
     %undef %$value
     %xdefine %$value %1
    %endif 
   %endif

   ;Restore paramter list to original state
   %rep (%0 + 1)
    %rotate 1
   %endrep
  %endif

  ;Loop through the paramters, creating a variable for each
  %rep %$loop_count
   %1:
   dq %$value
   sizeof_%{1} equ 8 
   %xdefine typeof_%{1} qword 
   %rotate 1
  %endrep

  %pop ;mqword_context
 %endif
%endmacro

%imacro RQWORD 1+.nolist
 %push rvar_context
  MQWORD %1
 %pop
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MDOUBLE - Declares one or more qword sized variables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - variable names 
;
%imacro MDOUBLE 1+.nolist
 MQWORD %1
%endmacro

%imacro RDOUBLE 1+.nolist
 %push rvar_context
  MQWORD %1
 %pop
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MTWORD - Declares one or more tword sized variables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - variable names 
;
%imacro MTWORD 0-*.nolist
 %ifctx struct_context
 
  %rep %0 
   STRUCT_UNION_COMMON %1, %$struct_name, 10
   %{$struct_name}.%{1}: rest 1
   %rotate 1
  %endrep

 %elifctx union_context
  
  %if 10 > %$union_largest
   %assign %$union_largest 10
  %endif

  %rep %0
   STRUCT_UNION_COMMON %1, %$union_name, 10
   %{$union_name}.%{1}:
   %rotate 1
  %endrep

 %elifctx rvar_context

  %rep %0
   %1:  
   rest 1
   sizeof_%{1} equ 10
   %xdefine typeof_%{1} tword 
   %rotate 1
  %endrep

 %else
  %push mtword_context

  %assign %$loop_count %0
  %assign %$value 0
  %if %0 > 1
   ;Get last paramter in the list
   %rep (%0 - 1)
    %rotate 1
   %endrep

   ;If the last paramter is a number, use it as a value.
   %ifnum %1
    %assign %$loop_count (%0 - 1)
    %assign %$value %1
   %else
    ;%ifnum does not catch floating point numbers so we
    ; assume that if the first character is a number then the
    ; user specified a floating point number.
    XIFNUM %1
    %if xifnum_result = 1
     %assign %$loop_count (%0 - 1)
     %undef %$value
     %xdefine %$value %1
    %endif 
   %endif

   ;Restore paramter list to original state
   %rep (%0 + 1)
    %rotate 1
   %endrep
  %endif

  ;Loop through the paramters, creating a variable for each
  %rep %$loop_count
   %1:
   dt %$value
   sizeof_%{1} equ 10 
   %xdefine typeof_%{1} tword 
   %rotate 1
  %endrep

  %pop ;mtword_context
 %endif
%endmacro

%imacro RTWORD 1+.nolist
 %push rvar_context
  MTWORD %1
 %pop
%endmacro

;
; Used internally by above macros, not to be used explicitly. 
;
%macro STRUCT_UNION_COMMON 3.nolist
 ;to define value of this member
 %if %3 = 1
  %define %{2}.%{1}_rop resb
  %define %{2}.%{1}_dop db
  %xdefine typeof_%{2}.%{1} byte 
 %elif %3 = 2
  %define %{2}.%{1}_rop resw
  %define %{2}.%{1}_dop dw
  %xdefine typeof_%{2}.%{1} word 
 %elif %3 = 4
  %define %{2}.%{1}_rop resd
  %define %{2}.%{1}_dop dd
  %xdefine typeof_%{2}.%{1} dword 
 %elif %3 = 8
  %define %{2}.%{1}_rop resq
  %define %{2}.%{1}_dop dq
  %xdefine typeof_%{2}.%{1} qword 
 %elif %3 = 10
  %define %{2}.%{1}_rop rest
  %define %{2}.%{1}_dop dt
  %xdefine typeof_%{2}.%{1} tword 
 %endif

 ;numrical size of this member
 sizeof_%{2}.%{1} equ %3

 ;numrical position of this member
 %assign %$member_count (%$member_count + 1)
 %xdefine %{2}_%{1}_position %$member_count

 ;for mapping position to member's label
 %define %{2}_member%{$member_count} %{1}
%endmacro


;
; Used internally by above macros, not to be used explicitly.
; (Created this because %ifnum does not work on floating point constants.
;
%imacro XIFNUM 1.nolist
 %push num_check_context
 %undef xifnum_result
 %defstr %$str %1 
 %substr %$chr %$str 1 
 %ifidn %$chr,'0'
  %xdefine xifnum_result 1
 %elifidn %$chr,'1'
  %xdefine xifnum_result 1
 %elifidn %$chr,'2'
  %xdefine xifnum_result 1
 %elifidn %$chr,'3'
  %xdefine xifnum_result 1
 %elifidn %$chr,'4'
  %xdefine xifnum_result 1
 %elifidn %$chr,'5'
  %xdefine xifnum_result 1
 %elifidn %$chr,'6'
  %xdefine xifnum_result 1
 %elifidn %$chr,'7'
  %xdefine xifnum_result 1
 %elifidn %$chr,'8'
  %xdefine xifnum_result 1
 %elifidn %$chr,'9'
  %xdefine xifnum_result 1
 %elifidn %$chr,'-'
  %xdefine xifnum_result 1
 %else
  %xdefine xifnum_result 0
 %endif
 %pop 
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ENUM - Enumerate identifiers. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters: 
; 1-* - identifiers 
;
%imacro ENUM 1-*.nolist
 %push enum_context
 %xdefine %$first %1
 %assign %$value 0
 %assign %$pos 1
 %xdefine %$tmp
 %rep %0 
  %xdefine %$tmp %1
  %if %$pos < %0
   %rotate 1
   %ifnum %1
    %assign %$value %1
;    %$tmp equ %$value
     %xdefine %$tmp %$value
    %assign %$pos (%$pos + 1)
   %else
    %rotate -1
;    %1 equ %$value
     %xdefine %1 %$value
   %endif 
  %else
   %ifnidn %$first, %1
;    %1 equ %$value
    %xdefine %1 %$value
   %else
    %exitrep
   %endif
  %endif
  %assign %$pos (%$pos + 1)
  %assign %$value (%$value + 1)
  %rotate 1 
 %endrep
 %pop   
%endmacro

;;;;;;;;; end variables.mac

;;;;;;;;;; start proc.mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; PROC - Begins a procedure definition. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;;
;; Parameters:
;;  1 - procedure name
;;  Paramters 2&3 can come in either order
;;  2 - cleanup style (C|Pascal); default: Pascal 
;;  3 - type (near|far|int); default: near 
;;
;%imacro PROC 1-3.nolist 
;
; %ifctx proc_setup_context
;  %error "PROC cannot be used inside another PROC."
; %elifctx proc_context
;  %error "PROC cannot be used inside another PROC."
; %else
;
;  ;Create new context
;  %push proc_setup_context
;
;  ;Save procedure name
;  %xdefine %$macpac_proc_name %1
;
;  ;This nasty chunk of code determines which paramter is
;  ; which, and if they are valid.
;  %assign %$macpac_proc_is_type 0
;  %assign %$macpac_proc_cleanup 1
;  %if %0 = 1
;   %xdefine %$macpac_proc_type near
;  %elif %0 = 2
;   MACPAC_PROC_IS_TYPE %2
;   %if %$macpac_proc_is_type = 1
;    %xdefine %$macpac_proc_type %2
;   %else
;    %xdefine %$macpac_proc_type near
;    %ifidni %2,C
;     %assign %$macpac_proc_cleanup 0
;    %endif
;   %endif 
;  %elif %0 = 3
;   MACPAC_PROC_IS_TYPE %2
;   %if %$macpac_proc_is_type = 1
;    %xdefine %$macpac_proc_type %2
;    %ifidni %3,C
;     %assign %$macpac_proc_cleanup 0
;    %endif
;   %else
;    MACPAC_PROC_IS_TYPE %3
;    %if %$macpac_proc_is_type = 1
;     %xdefine %$macpac_proc_type %3
;     %ifidni %2,C
;      %assign %$macpac_proc_cleanup 0
;     %endif
;    %else
;     %error "Invalid paramters for PROC."
;    %endif
;   %endif
;  %endif
;
;  ;%$macpac_proc_stack_align is the default stack alignment 
;  ;It should be 4 (dword) in 32-bit code, and word (2) for 16-bit code. 
;  %assign %$macpac_proc_stack_align 4 
;
;  ;%$macpac_proc_ret_size is the size of the return information pushed
;  ; on the stack by a call instruction or an interrupt.
;  %ifidni %$macpac_proc_type, near
;   %assign %$macpac_proc_ret_size 4  ; EIP 
;  %elifidni %$macpac_proc_type, far
;   %assign %$macpac_proc_ret_size 8 ; CS:EIP
;  %else
;   %assign %$macpac_proc_ret_size 12 ; EFLAGS and CS:EIP
;  %endif
;
;  ; __BITS__ does not seem to get set when the -E command line option
;  ; is used, and thus this check will fail and assume __BITS__ = 32.
;  %if __BITS__ = 16 
;   %assign %$macpac_proc_stack_align (%$macpac_proc_stack_align / 2) 
;   %assign %$macpac_proc_ret_size  (%$macpac_proc_ret_size / 2) 
;   %xdefine %$macpac_proc_base_pointer bp
;   %xdefine %$macpac_proc_stack_pointer sp
;  %else 
;   %xdefine %$macpac_proc_base_pointer ebp
;   %xdefine %$macpac_proc_stack_pointer esp
;  %endif
;
;  ; Add an extra 2 or 4 bytes to account for the base pointer
;  ; being pushed when creating the stack frame.
;  %assign %$macpac_proc_ret_size (%$macpac_proc_ret_size + %$macpac_proc_stack_align)
;
;  ; Keeps track of stack arugments
;  %assign %$macpac_proc_arg_offset 0
;
;  ; Keeps track of local arguments
;  %assign %$macpac_proc_local_size 0
;
;  ; This variable is proveded so the RETURN macro can be used inside other
;  ; macro contexts. Any other macro that uses the context stack inside
;  ; a PROC, should inrease this value by 1. 
;  %assign %$macpac_proc_depth 1
;
;  %assign %$macpac_proc_preserve_count 0
; %endif
;%endmacro
;
;
;;
;; Used internally by PROC, not to be called explicitly
;;
;%macro MACPAC_PROC_IS_TYPE 1
; %ifidni %1,near
;  %assign %$macpac_proc_is_type 1
; %elifidni %1,far
;  %assign %$macpac_proc_is_type 1
; %elifidni %1,int
;  %assign %$macpac_proc_is_type 1
; %endif
;%endmacro
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; ARG - Create a label for an procedure argument. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; Paramters:
;;  1-* - argument name(s) 
;;  * - size of the argument (byte|word|dword|qword|tword)
;; 
;%imacro ARG 2-*.nolist
;
; %ifnctx proc_setup_context
;  %error "ARG used outside of PROC."
; %else
;  
;  ;Grab last paramater in the list
;  %rep (%0 - 1)
;   %rotate 1
;  %endrep
;  %xdefine %$macpac_size_spec %1
;
;  ;Restore the paramter list to its original state
;  %rep (%0 + 1)
;   %rotate 1
;  %endrep
;
;  %ifidni %$macpac_size_spec,tword
;   %assign %$macpac_tmp_sz 16 
;   %assign %$macpac_tmp (%$macpac_proc_arg_offset % 16)
;   %if %$macpac_tmp != 0
;    %assign %$macpac_tmp (16 - %$macpac_tmp) 
;    %assign %$macpac_proc_arg_offset (%$macpac_proc_arg_offset + %$macpac_tmp) 
;   %endif
;   %assign %$macpac_tmp_szof 10
;  %elifidni %$macpac_size_spec,qword
;   %assign %$macpac_tmp_sz 8 
;   %assign %$macpac_tmp (%$macpac_proc_arg_offset % 8)
;   %if %$macpac_tmp != 0
;    %assign %$macpac_tmp (8 - %$macpac_tmp) 
;    %assign %$macpac_proc_arg_offset (%$macpac_proc_arg_offset + %$macpac_tmp) 
;   %endif
;   %assign %$macpac_tmp_szof 8 
;  %elifidni %$macpac_size_spec,dword
;   %assign %$macpac_tmp_sz 4
;   %assign %$macpac_tmp_szof 4 
;  %elifidni %$macpac_size_spec,word
;   %assign %$macpac_tmp_sz %$macpac_proc_stack_align 
;   %assign %$macpac_tmp_szof 2 
;  %elifidni %$macpac_size_spec,byte
;   %assign %$macpac_tmp_sz %$macpac_proc_stack_align 
;   %assign %$macpac_tmp_szof 1 
;  %else
;   %if %0 > 2
;    %error "ARG given too many identifiers for it's type."
;   %else
;    XISREG %2
;    %if xisreg_result= 0
;     %error Invalid type ( %2 ) for ARG %1 .
;    %else
;     %xdefine %$%{1} %2
;     %assign %$macpac_tmp_sz 0
;    %endif
;   %endif
;  %endif 
;
;  ;%$macpac_tmp_sz == 0 only if the argument type is a register.
;  %if %$macpac_tmp_sz > 0
;   ;Now loop through the paramters, creating an identifier for each. 
;   %rep (%0 - 1)
;    %xdefine %$%{1} %$macpac_proc_base_pointer + (%$macpac_proc_ret_size + %$macpac_proc_arg_offset)
;    %xdefine %$typeof_%{1} %$macpac_size_spec 
;    %assign %$sizeof_%{1} %$macpac_tmp_szof 
;    %assign %$macpac_proc_arg_offset %$macpac_tmp_sz + %$macpac_proc_arg_offset
;    %rotate 1
;   %endrep
;  %endif
; %endif ;proc_setup_context check
;%endmacro
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; LOCAL - Reserves stack space, and makes a label, for a local variable.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; Paramters:
;;  1-* local variable name(s)
;;  * - size of the variable (byte|word|dword|qword|tword)
;;
;%imacro LOCAL 2-*.nolist
;
; %ifctx proc_context
;  %error "Attempting to use LOCAL after BEGIN."
; %elifnctx proc_setup_context
;  %error "Attempt use LOCAL outside a PROC."
; %else
;
;  ;Grab last paramater in the list
;  %rep (%0 - 1)
;   %rotate 1
;  %endrep
;  %xdefine %$macpac_size_spec %1
;
;  ;Restore the paramter list to its original state
;  %rep (%0 + 1)
;   %rotate 1
;  %endrep
;
;  %ifidni %$macpac_size_spec,tword
;   %assign %$macpac_tmp_sz 16
;   %assign %$macpac_tmp (%$macpac_proc_local_size % 16)
;   %if %$macpac_tmp != 0
;    %assign %$macpac_tmp (16 - %$macpac_tmp) 
;    %assign %$macpac_proc_local_size (%$macpac_proc_local_size + %$macpac_tmp) 
;   %endif
;   %assign %$macpac_tmp_szof 10
;  %elifidni %$macpac_size_spec,qword
;   %assign %$macpac_tmp_sz 8
;   %assign %$macpac_tmp (%$macpac_proc_local_size % 8)
;   %if %$macpac_tmp != 0
;    %assign %$macpac_tmp (8 - %$macpac_tmp) 
;    %assign %$macpac_proc_local_size (%$macpac_proc_local_size + %$macpac_tmp) 
;   %endif
;   %assign %$macpac_tmp_szof 8 
;  %elifidni %$macpac_size_spec,dword
;   %assign %$macpac_tmp_sz 4
;   %assign %$macpac_tmp (%$macpac_proc_local_size % 4)
;   %if %$macpac_tmp != 0
;    %assign %$macpac_tmp (4 - %$macpac_tmp) 
;    %assign %$macpac_proc_local_size (%$macpac_proc_local_size + %$macpac_tmp) 
;   %endif
;   %assign %$macpac_tmp_szof 4 
;  %elifidni %$macpac_size_spec,word
;   %assign %$macpac_tmp_sz 2
;   %assign %$macpac_tmp (%$macpac_proc_local_size % %$macpac_tmp_sz)
;   %assign %$macpac_proc_local_size (%$macpac_proc_local_size + %$macpac_tmp) 
;   %assign %$macpac_tmp_szof 2 
;  %elifidni %$macpac_size_spec,byte
;   %assign %$macpac_tmp_sz 1
;   %assign %$macpac_tmp_szof 1 
;  %else
;
;   %ifidn typeof_%{2},struct
;
;    %xdefine %$%{1} %$macpac_proc_base_pointer - (%$macpac_proc_stack_align + %$macpac_proc_local_size)
;    %xdefine %$sizeof_%{1} sizeof_%{2}
;
;    %assign %$i 0
;    %rep %{2}_member_count
;     %assign %$i (%$i + 1)
;     %xdefine %$label %{2}_member%{$i}
;     ;%{2}.%{$label} equ %2 + %{2}.%{$label}
;     %xdefine %$%{1}.%{$label} %$macpac_proc_base_pointer - (%$macpac_proc_stack_align + %$macpac_proc_local_size + (%{2}.%{$label} - %2) )
;     %xdefine %$sizeof_%{1}.%{$label} sizeof_%{2}.%{$label}
;     %xdefine %$typeof_%{1}.%{$label} typeof_%{2}.%{$label}
;    %endrep
;  
;    %assign %$macpac_tmp_sz 0
;    %assign %$macpac_proc_local_size (%$macpac_proc_local_size + (sizeof_%{2} - %2) )
;
;   %else
;    %assign %$macpac_tmp_sz 0
;    %error Invalid type ( %2 ) for LOCAL %1 .
;   %endif
;  %endif 
;
;  %if %$macpac_tmp_sz > 0
;   %rep (%0 - 1)
;    %xdefine %$%{1} %$macpac_proc_base_pointer - (%$macpac_proc_stack_align + %$macpac_proc_local_size)
;    %xdefine %$typeof_%{1} %$macpac_size_spec
;    %assign %$sizeof_%{1} %$macpac_tmp_szof 
;    %assign %$macpac_proc_local_size (%$macpac_proc_local_size + %$macpac_tmp_sz)
;    %rotate 1
;   %endrep
;  %endif
; %endif
;%endmacro
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; PRESERVE - Preserves the values of registers. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;%imacro PRESERVE 1+.nolist
; %assign %$macpac_proc_preserve_count %0
; %xdefine %$macpac_proc_preserve_regs %1
;%endmacro
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; BEGIN - Begins a PROC. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;%imacro BEGIN 0.nolist
;
; %ifnctx proc_setup_context
;  %error "BEGIN used outside of PROC."
; %else
;
;  %repl proc_context
;
;  ;Label to call procedure with
;%{$macpac_proc_name}:
;
;  ; Align the local size to the stack if it is not already.
;  %assign %$macpac_tmp (%$macpac_proc_local_size % %$macpac_proc_stack_align)
;  %if %$macpac_tmp != 0
;   %assign %$macpac_tmp (%$macpac_proc_stack_align - %$macpac_tmp) 
;   %assign %$macpac_proc_local_size (%$macpac_proc_local_size + %$macpac_tmp) 
;  %endif
;
;  %if %$macpac_proc_local_size > 0 || %$macpac_proc_arg_offset > 0
;   push %$macpac_proc_base_pointer
;   mov %$macpac_proc_base_pointer, %$macpac_proc_stack_pointer
;  %endif
;
;  ; Only aquire space for local variables if necessary 
;  %if %$macpac_proc_local_size > 0
;   %if %$macpac_proc_local_size < 256
;    sub %$macpac_proc_stack_pointer, byte %$macpac_proc_local_size
;   %elif %$macpac_proc_local_size < 65536
;    sub %$macpac_proc_stack_pointer, word %$macpac_proc_local_size
;   %else
;    sub %$macpac_proc_stack_pointer, %$macpac_proc_local_size
;   %endif
;  %endif
;
;  %if %$macpac_proc_preserve_count > 0
;   MPUSH %$macpac_proc_preserve_regs
;  %endif
; %endif
;%endmacro
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; RETURN - Leave a PROC. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;%imacro RETURN 0.nolist
; %if %$macpac_proc_depth = 1
;  jmp %$macpac_proc_exit
; %elif %$macpac_proc_depth = 2
;  jmp %$$macpac_proc_exit
; %elif %$macpac_proc_depth = 3
;  jmp %$$$macpac_proc_exit
; %elif %$macpac_proc_depth = 4
;  jmp %$$$$macpac_proc_exit
; %elif %$macpac_proc_depth = 5
;  jmp %$$$$$macpac_proc_exit
; %elif %$macpac_proc_depth = 6
;  jmp %$$$$$$macpac_proc_exit
; %elif %$macpac_proc_depth = 7
;  jmp %$$$$$$$macpac_proc_exit
; %elif %$macpac_proc_depth = 8
;  jmp %$$$$$$$$macpac_proc_exit
; %elif %$macpac_proc_depth = 9
;  jmp %$$$$$$$$$macpac_proc_exit
; %else
;  %error "RETURN not used in PROC context or nested too deep."
; %endif
;%endmacro
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; 
;; END - Ends a PROC.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;%imacro END 0.nolist 
;
; %ifctx proc_setup_context 
;  %error "No BEGIN used before END."
; %elifnctx proc_context
;  %error "END not used in PROC context. "
; %else
;
;  ;Label for RETURN
;%$macpac_proc_exit:
;
;  ;Restore any preserved registers
;  %if %$macpac_proc_preserve_count > 0
;   MPOP %$macpac_proc_preserve_regs
;  %endif
;
;  ;Only generate stack frame code if necessary
;  %if %$macpac_proc_local_size > 0 || %$macpac_proc_arg_offset > 0
;   mov %$macpac_proc_stack_pointer, %$macpac_proc_base_pointer
;   pop %$macpac_proc_base_pointer
;  %endif
;
;; Use a appropriate return instruction (RET|RETF|IRET), also clean up
;; any arguments passed on the stack. 
;  %ifidni %$macpac_proc_type,near
;   %if %$macpac_proc_cleanup = 1 && %$macpac_proc_arg_offset > 0
;    ret %$macpac_proc_arg_offset
;   %else 
;    ret
;   %endif
;  %elifidni %$macpac_proc_type,far
;   %if %$macpac_proc_cleanup = 1 && %$macpac_proc_arg_offset > 0
;    retf %$macpac_proc_arg_offset
;   %else 
;    retf
;   %endif 
;  %else
;   iret
;  %endif
;
;  %pop ; destroy proc_context
;
; %endif
;%endmacro
;
;;;;;;;;;; end proc.mac

;;;;;;;;; start struct.mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; STRUCT - Begins a structure definition.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; Paramters:
; 1 - structure name 
;
%imacro STRUCT 1.nolist
 %ifnctx struct_context
  %ifnctx union_context
 
   ;Create a new context
   %push struct_context

   ;Save structure name 
   %xdefine %$struct_name %1

   ;Keep track of the number of members
   %assign %$member_count 0

   ;Change to the 'nowhere' segment 
[absolute 0]

%$struct_name:
  %else
   %error "STRUCT not given label, or used in a bad context."
  %endif
 %else
  %error "STRUCT not given label, or used in bad context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; STRUCT - Declare a nested structure.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; This version of struct takes two parameters and is to be used when
; declaring a struct as a member of another struct or union.
;
; Paramters:
; 1 - structure name 
; 2 - member label 
;
%imacro STRUCT 2.nolist
 %ifctx struct_context

  ;SizeOf and TypeOf
  %xdefine typeof_%{$struct_name}.%{2} struct
  sizeof_%{$struct_name}.%{2} equ sizeof_%{1}

  ;Numrical position of this struct in the member list
  %assign %$member_count (%$member_count + 1)
  %xdefine %{$struct_name}_%{2}_position %$member_count

  ;For mapping position to member's label
  %define %{$struct_name}_member%{$member_count} %2

  %{$struct_name}.%{2}:
  ;Loop through this struct's members creating a label for each.
  %assign %$i 0
  %rep %{1}_member_count
   %assign %$i (%$i + 1) 
   %xdefine %$label %{1}_member%{$i}
   %ifidn typeof_%{1}.%{$label},struct 
    %xdefine typeof_%{$struct_name}.%{2}.%{$label} struct
    sizeof_%{$struct_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$struct_name}.%{2}.%{$label}:
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$struct_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$struct_name}_member%{$member_count} %{2}.%{$label}
   %elifidn typeof_%{1}.%{$label},union 
    %xdefine typeof_%{$struct_name}.%{2}.%{$label} union 
    sizeof_%{$struct_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$struct_name}.%{2}.%{$label}:
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$struct_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$struct_name}_member%{$member_count} %{2}.%{$label}
   %else
    STRUCT_UNION_COMMON %{2}.%$label, %{$struct_name}, sizeof_%{1}.%{$label}
    %{$struct_name}.%{2}.%{$label} equ (%{$struct_name}.%{2} + %{1}.%{$label} )
   %endif
  %endrep

  resb sizeof_%{1}

 %elifctx union_context

  ;SizeOf and TypeOf
  %xdefine typeof_%{$union_name}.%{2} struct 
  sizeof_%{$union_name}.%{2} equ sizeof_%{1}

  ;Numrical position of this struct in the member list
  %assign %$member_count (%$member_count + 1)
  %xdefine %{$union_name}_%{2}_position %$member_count

  ;For mapping position to member's label
  %define %{$union_name}_member%{$member_count} %2

  %{$union_name}.%{2}:
  %if sizeof_%{1} > %$union_largest
   %assign %$union_largest sizeof_%{1}
  %endif

  ;Loop through this struct's members creating a label for each.
  %assign %$i 0
  %rep %{1}_member_count
   %assign %$i (%$i + 1)
   %xdefine %$label %{1}_member%{$i}
   %ifidn typeof_%{1}.%{$label},struct
    %xdefine typeof_%{$union_name}.%{2}.%{$label} struct
    sizeof_%{$union_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$union_name}.%{2}.%{$label} equ (%$union_base + %{1}.%{$label} ) 
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$union_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$union_name}_member%{$member_count} %{2}.%{$label}
   %elifidn typeof_%{1}.%{$label},union
    %xdefine typeof_%{$union_name}.%{2}.%{$label} union 
    sizeof_%{$union_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$union_name}.%{2}.%{$label} equ (%$union_base + %{1}.%{$label} ) 
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$union_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$union_name}_member%{$member_count} %{2}.%{$label}
   %else
    STRUCT_UNION_COMMON %{2}.%$label, %{$union_name}, sizeof_%{1}.%{$label}
    %{$union_name}.%{2}.%{$label} equ (%$union_base + %{1}.%{$label} ) 
   %endif
  %endrep

 %else
  %error "STRUCT used in bad context, or given a label in wrong context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; ENDS - Ends a structure definition.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; The paramters for this macro are generated automaticaly, do not
; supply any yourself.
;
%imacro ENDS 0.nolist
 %ifctx struct_context
  
  ;Create a macro containing the total number of members for this
  ;struct. This will be used when instantiating structures of this
  ;type, and for nesting structures.
  %xdefine %{$struct_name}_member_count %$member_count

  %xdefine typeof_%{$struct_name} struct

  ;Create a labe for this structure's size.
sizeof_%{$struct_name}:

  ;Destroy this context
  %pop

  ;Return to the previous section
__SECT__
 %else
  %error "ENDS not used in STRUCT context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; ISTRUCT - Begins instantiation of a structure.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; Parameters:
; 1 - name of structure to instantiate 
; 2 - label to give this structure instance
;
%imacro ISTRUCT 2.nolist 
 ;Create new context
 %push istruct_context

 ;Save the name of the structure
 %xdefine %$struct_name %1

 ;Base for labels
 %xdefine %$struct_base %2

 sizeof_%{2} equ sizeof_%{1}
 %xdefine typeof_%{2} struct 

 ;Keeps track of initialized members 
 %assign %$current_member 1

 ;Label for this instance
%2:

 ;Used internaly for offset calculations 
%$istruct_start:

 ;loop through struct members, creating a label for each.
 %assign %$i 0
 %rep %{1}_member_count
  %assign %$i (%$i + 1)
  %xdefine %$label %{1}_member%{$i}
  %{2}.%{$label} equ %2 + %{1}.%{$label}
  sizeof_%{2}.%{$label} equ sizeof_%{1}.%{$label}
  %xdefine typeof_%{2}.%{$label} typeof_%{1}.%{$label}
 %endrep
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; IENDS - Ends instantiation of a structure.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
%imacro IENDS 0.nolist
 %ifnctx istruct_context
  %error "IENDS not used in ISTRUCT context."
 %else

  ;Fill uninitialized space with 0s
  times (sizeof_%{$struct_name} - ($ - %$istruct_start)) db 0

  %pop  ; destroy istruct_context
 %endif

%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SET - Initialize a member during structure or union instantiation.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters:
; 1-* label(s) of members
; * - value assigned to all listed members
%imacro SET 2-*.nolist
 %ifctx istruct_context

  ;Grab the last paramter and save it as the value.
  %rep (%0 - 1)
   %rotate 1
  %endrep
  %assign %$value %1

  ;Restore the paramter list to it's original state
  %rep (%0 + 1)
   %rotate 1
  %endrep

  ;Now loop through the paramters, assigning %$value to each.
  %rep (%0 - 1)

   ;Fill in any gaps with zeros.
   times (%{$struct_name}.%{1} - ($ - %$istruct_start )) db 0

   ;Initialize the member's value.
   %{$struct_name}.%{1}_dop %$value

   ;advance the current member count
   %assign %$current_member (%$current_member + 1)

   %rotate 1
  %endrep

 %elif iunion_context

  ;Grab the last paramter and save it as the value.
  %rep (%0 - 1)
   %rotate 1
  %endrep
  %assign %$value %1

  ;Restore the paramter list to it's original state
  %rep (%0 + 1)
   %rotate 1
  %endrep

  ;Now loop through the paramters, creating a label for each.
  %rep (%0 - 1)

   ;Fill in any gaps with zeros.
   times (%{$union_name}.%{1} - ($ - %$iunion_start )) db 0

   %{$union_name}.%{1}_dop %$value

   ;advance the current member count
   %assign %$current_member (%$current_member + 1)

   %rotate 1
  %endrep
 %else
  %error "SET used in unknown context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; RSTRUCT - Begins instantiation of an uninitialized structure.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;
; Parameters:
; 1 - name of structure to instantiate 
; 2 - label to give this structure instance
;
%imacro RSTRUCT 2.nolist 
 ;Create new context
 %push rstruct_context

 sizeof_%{2} equ sizeof_%{1}

 ;Label for this instance
%2:

 ;loop through struct members, creating a label for each.
 %assign %$i 0
 %rep %{1}_member_count
  %assign %$i (%$i + 1)
  %xdefine %$label %{1}_member%{$i}
  %{2}.%{$label} equ %2 + %{1}.%{$label}
  sizeof_%{2}.%{$label} equ sizeof_%{1}.%{$label}
  %xdefine typeof_%{2}.%{$label} typeof_%{1}.%{$label}
 %endrep

 resb sizeof_%{1}

 %pop
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; UNION - Begins a union definition.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters:
; 1 - union name
;
%imacro UNION 1.nolist
 %ifnctx union_context
  %ifnctx struct_context

   ;Create a new context
   %push union_context

   ;Save union name
   %xdefine %$union_name %1

   ;Base for labels
   %xdefine %$union_base %1

   ;Track largest member size
   %assign %$union_largest 0

   ;Track members
   %assign %$member_count 0

   ;Change to the 'nowhere' segment
[absolute 0]

%$union_base:
  %else
   %error "UNION not given label, or used in bad context."
  %endif
 %else
  %error "UNION not given label, or used in bad context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; UNION - Declare a nested union.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This version of union takes two parameters and is to be used when
; declaring a union as a member of another union or struct.
;
; Paramters:
; 1 - structure name
; 2 - member label
;
%imacro UNION 2.nolist
 %ifctx union_context

  %xdefine typeof_%{$union_name}.%{2} union
  sizeof_%{$union_name}.%{2} equ sizeof_%{1}

  %assign %$member_count (%$member_count + 1)
  %xdefine %{$union_name}_%{2}_position %$member_count

  %define %{$union_name}_member%{$member_count} %2

  %if sizeof_%{1} > %$union_largest
   %assign %$union_largest sizeof_%{1}
  %endif

  %{$union_name}.%{2}:
  %assign %$i 0
  %rep %{1}_member_count
   %assign %$i (%$i + 1)
   %xdefine %$label %{1}_member%{$i}
   %ifidn typeof_%{1}.%{$label},union
    %xdefine typeof_%{$union_name}.%{2}.%{$label} union
    sizeof_%{$union_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$union_name}.%{2}.%{$label}:
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$union_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$union_name}_member%{$member_count} %{2}.%{$label}
   %elifidn typeof_%{1}.%{$label},struct
    %xdefine typeof_%{$union_name}.%{2}.%{$label} struct
    sizeof_%{$union_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$union_name}.%{2}.%{$label}:
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$union_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$union_name}_member%{$member_count} %{2}.%{$label}
   %else
    STRUCT_UNION_COMMON %{2}.%$label, %{$union_name}, sizeof_%{1}.%{$label}
    %{$union_name}.%{2}.%{$label} equ (%$union_base + %{1}.%{$label} )
   %endif
  %endrep

 %elifctx struct_context


  %xdefine typeof_%{$struct_name}.%{2} union
  sizeof_%{$struct_name}.%{2} equ sizeof_%{1}

  %assign %$member_count (%$member_count + 1)
  %xdefine %{$struct_name}_%{2}_position %$member_count

  %define %{$struct_name}_member%{$member_count} %2

  %{$struct_name}.%{2}:
  %assign %$i 0
  %rep %{1}_member_count
   %assign %$i (%$i + 1)
   %xdefine %$label %{1}_member%{$i}
   %ifidn typeof_%{1}.%{$label},union
    %xdefine typeof_%{$struct_name}.%{2}.%{$label} union
    sizeof_%{$struct_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$struct_name}.%{2}.%{$label}:
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$struct_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$struct_name}_member%{$member_count} %{2}.%{$label}
   %elifidn typeof_%{1}.%{$label},struct
    %xdefine typeof_%{$struct_name}.%{2}.%{$label} struct
    sizeof_%{$struct_name}.%{2}.%{$label} equ sizeof_%{1}.%{$label}
    %{$struct_name}.%{2}.%{$label}:
    %assign %$member_count (%$member_count + 1)
    %xdefine %{$struct_name}_%{2}.%{$label}_position %$member_count
    %xdefine %{$struct_name}_member%{$member_count} %{2}.%{$label}
   %else
    STRUCT_UNION_COMMON %{2}.%$label, %{$struct_name}, sizeof_%{1}.%{$label}
    %{$struct_name}.%{2}.%{$label} equ (%{$struct_name}.%{2} + %{1}.%{$label} )
   %endif
  %endrep

  resb sizeof_%{1}

 %else
  %error "UNION given to many paramters or used in bad context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ENDU - Ends a union definition.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%imacro ENDU 0
 %ifctx union_context

  ;Create a macro containing the total number of members for this union.
  %xdefine %{$union_name}_member_count %$member_count

  ;reserve space enough for largest member
  resb %$union_largest

  ;label for union size
sizeof_%{$union_name}:

  ;destroy union_context
  %pop

  ;restore previous section
__SECT__

 %else
  %error "ENDU not used in UNION context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; IUNION - Create an instance of a union.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Paramters:
; 1 - name of union to instantiate
; 2 - label to give this union instance
;
%imacro IUNION 2.nolist
  ;Create a new context
  %push iunion_context

  ;Save the union name
  %xdefine %$union_name %1

  ;Base for labels
  %xdefine %$union_base %2

  sizeof_%{2} equ sizeof_%{1}
  %xdefine typeof_%{2} union

  ;Track number of members
  %assign %$current_member 1

  ;Label for this instance
%2:

  ;Used internaly for offset calculations
%$iunion_start:

  ;Loop through this union's members, creating a label for each.
  %assign %$i 0
  %rep %{$union_name}_member_count
   %assign %$i (%$i + 1)
   %xdefine %$label %{$union_name}_member%{$i}
   %{2}.%{$label} equ %2 + %{1}.%{$label}
   sizeof_%{2}.%{$label} equ sizeof_%{1}.%{$label}
   %xdefine typeof_%{2}.%{$label} typeof_%{1}.%{$label}
  %endrep
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; IENDU - Ends instantiation of a union.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%imacro IENDU 0
 %ifnctx iunion_context
  %error "IENDU not used in IUNION context."
 %else

  times (sizeof_%{$union_name} - ($ - %$iunion_start)) db 0

  %pop  ; destroy iunion_context
 %endif
%endmacro

;;;;;;;;; end struct.mac

;;;;;;;;; start loops.mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; DO - Begins a DO-WHILE/UNTIL loop. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
%imacro DO 0.nolist
 %push do_context
 %ifnum %$macpac_proc_depth 
  %assign %$macpac_proc_depth (%$macpac_proc_depth + 1)
 %endif

 %assign %$macpac_loop_depth 1

%$macpac_loop_begin:
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; DOWHILE - Completes a DO-WHILE loop. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; This verison of takes one parameter. It may be a condition code
; (e,ne,a,ae,b,be,g,ge,l,le,z,nz) but can also be a true value 
; (`true` or a number != 0) to create an endless loop which is exited 
; by some other means.
;
%imacro DOWHILE 1.nolist
 %ifnctx do_context 
  %error "DOWHILE not used in DO context."
 %else
  %ifnum %1
   %if %1 <> 0
    jmp %$macpac_loop_begin
   %endif
  %elifidni %1,true
   jmp %$macpac_loop_begin
  %else
   j%+1 %$macpac_loop_begin
  %endif
  %$macpac_loop_exit:
  %pop
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; DOWHILE - Completes a DO-WHILE loop. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; Parameters:
; 1&3 - operands to be tested
; 2   - condition code (e,ne,a,ae,b,be,g,ge,l,le,z,nz)
; 4   - the testing instruction; it defaults to 'cmp' but
; can be replaced with 'test' or some other instruction which affects
; the flags register.
;
%imacro DOWHILE 3-4.nolist cmp
 %ifnctx do_context 
  %error "DOWHILE not used in DO context."
 %else 
  %4 %1, %3 
  j%+2 %$macpac_loop_begin 
  %$macpac_loop_exit:
  %pop
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; DOWHILE - Completes a DO-WHILE loop, takes multiple conditionals. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
%imacro DOWHILE 7-*.nolist
 %ifnctx do_context
  %error "DOWHILE not used in DO context."
 %else

  %assign %$macpac_do_i 1
  %rep %0

   %ifidni %1,AND
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 5
     %assign %$macpac_a1 (%$macpac_do_i - 4)
     %assign %$macpac_cc (%$macpac_do_i - 3)
     %assign %$macpac_a2 (%$macpac_do_i - 2)
     %assign %$macpac_in (%$macpac_do_i - 1)

     %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_donarg%{$macpac_cc} %$macpac_loop_exit

     %assign %$macpac_do_i (%$macpac_do_i + 6)
    %elif %$macpac_argcheck = 4
     %assign %$macpac_a1 (%$macpac_do_i - 3)
     %assign %$macpac_cc (%$macpac_do_i - 2)
     %assign %$macpac_a2 (%$macpac_do_i - 1)

     cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_donarg%{$macpac_cc} %$macpac_loop_exit

     %assign %$macpac_do_i (%$macpac_do_i + 7)
    %else
     %error "Invalid number or order of paramters given to DOWHILE."
    %endif
   %elifidni %1,OR
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 5
     %assign %$macpac_a1 (%$macpac_do_i - 4)
     %assign %$macpac_cc (%$macpac_do_i - 3)
     %assign %$macpac_a2 (%$macpac_do_i - 2)
     %assign %$macpac_in (%$macpac_do_i - 1)

     %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_doarg%{$macpac_cc} %$macpac_loop_begin

     %assign %$macpac_do_i (%$macpac_do_i + 6)
    %elif %$macpac_argcheck = 4
     %assign %$macpac_a1 (%$macpac_do_i - 3)
     %assign %$macpac_cc (%$macpac_do_i - 2)
     %assign %$macpac_a2 (%$macpac_do_i - 1)

     cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_doarg%{$macpac_cc} %$macpac_loop_begin

     %assign %$macpac_do_i (%$macpac_do_i + 7)
    %else
     %error "Invalid number or order of paramters given to DOWHILE."
    %endif
   %else
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 2
     %xdefine %$macpac_doarg%{$macpac_do_i} j%+1
     %xdefine %$macpac_donarg%{$macpac_do_i} j%-1
    %else
     %xdefine %$macpac_doarg%{$macpac_do_i} %1
    %endif
    %assign %$macpac_do_i (%$macpac_do_i + 1)
   %endif

   %rotate 1
  %endrep

  %assign %$macpac_argcheck (%$macpac_do_i % 10)
  %if %$macpac_argcheck = 5
   %assign %$macpac_a1 (%$macpac_do_i - 4)
   %assign %$macpac_cc (%$macpac_do_i - 3)
   %assign %$macpac_a2 (%$macpac_do_i - 2)
   %assign %$macpac_in (%$macpac_do_i - 1)

   %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
   %$macpac_doarg%{$macpac_cc} %$macpac_loop_begin

   %assign %$macpac_do_i (%$macpac_do_i + 6)
  %elif %$macpac_argcheck = 4
   %assign %$macpac_a1 (%$macpac_do_i - 3)
   %assign %$macpac_cc (%$macpac_do_i - 2)
   %assign %$macpac_a2 (%$macpac_do_i - 1)

   cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
   %$macpac_doarg%{$macpac_cc} %$macpac_loop_begin

   %assign %$macpac_do_i (%$macpac_do_i + 7)
  %else
   %error "Invalid number or order of paramters given to DOWHILE."
  %endif

  %$macpac_loop_exit:

 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; UNTIL - Completes a DO-UNTIL loop. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; Parameters:
; 1&3 - operands to be tested
; 2   - condition code (e,ne,a,ae,b,be,g,ge,l,le,z,nz)
; 4   - the testing instruction; it defaults to 'cmp' but
; can be replaced with 'test' or some other instruction which affects
; the flags register.
;
%imacro UNTIL 3-4.nolist cmp
 %ifnctx do_context 
  %error "UNTIL not used in DO context."
 %else 
  %4 %1, %3 
  j%-2 %$macpac_loop_begin 
  %pop
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; UNTIL - Completes a DO-UNTIL loop, takes multiple conditionals.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
%imacro UNTIL 7-*.nolist
 %ifnctx do_context
  %error "UNTIL not used in DO context."
 %else

  %assign %$macpac_do_i 1
  %rep %0

   %ifidni %1,AND
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 5
     %assign %$macpac_a1 (%$macpac_do_i - 4)
     %assign %$macpac_cc (%$macpac_do_i - 3)
     %assign %$macpac_a2 (%$macpac_do_i - 2)
     %assign %$macpac_in (%$macpac_do_i - 1)

     %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_donarg%{$macpac_cc} %$macpac_loop_begin

     %assign %$macpac_do_i (%$macpac_do_i + 6)
    %elif %$macpac_argcheck = 4
     %assign %$macpac_a1 (%$macpac_do_i - 3)
     %assign %$macpac_cc (%$macpac_do_i - 2)
     %assign %$macpac_a2 (%$macpac_do_i - 1)

     cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_donarg%{$macpac_cc} %$macpac_loop_begin

     %assign %$macpac_do_i (%$macpac_do_i + 7)
    %else
     %error "Invalid number or order of paramters given to UNTIL."
    %endif
   %elifidni %1,OR
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 5
     %assign %$macpac_a1 (%$macpac_do_i - 4)
     %assign %$macpac_cc (%$macpac_do_i - 3)
     %assign %$macpac_a2 (%$macpac_do_i - 2)
     %assign %$macpac_in (%$macpac_do_i - 1)

     %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_doarg%{$macpac_cc} %$macpac_loop_exit

     %assign %$macpac_do_i (%$macpac_do_i + 6)
    %elif %$macpac_argcheck = 4
     %assign %$macpac_a1 (%$macpac_do_i - 3)
     %assign %$macpac_cc (%$macpac_do_i - 2)
     %assign %$macpac_a2 (%$macpac_do_i - 1)

     cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_doarg%{$macpac_cc} %$macpac_loop_exit

     %assign %$macpac_do_i (%$macpac_do_i + 7)
    %else
     %error "Invalid number or order of paramters given to UNTIL."
    %endif
   %else
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 2
     %xdefine %$macpac_doarg%{$macpac_do_i} j%+1
     %xdefine %$macpac_donarg%{$macpac_do_i} j%-1
    %else
     %xdefine %$macpac_doarg%{$macpac_do_i} %1
    %endif
    %assign %$macpac_do_i (%$macpac_do_i + 1)
   %endif

   %rotate 1
  %endrep

  %assign %$macpac_argcheck (%$macpac_do_i % 10)
  %if %$macpac_argcheck = 5
   %assign %$macpac_a1 (%$macpac_do_i - 4)
   %assign %$macpac_cc (%$macpac_do_i - 3)
   %assign %$macpac_a2 (%$macpac_do_i - 2)
   %assign %$macpac_in (%$macpac_do_i - 1)

   %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
   %$macpac_donarg%{$macpac_cc} %$macpac_loop_begin

   %assign %$macpac_do_i (%$macpac_do_i + 6)
  %elif %$macpac_argcheck = 4
   %assign %$macpac_a1 (%$macpac_do_i - 3)
   %assign %$macpac_cc (%$macpac_do_i - 2)
   %assign %$macpac_a2 (%$macpac_do_i - 1)

   cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
   %$macpac_donarg%{$macpac_cc} %$macpac_loop_begin

   %assign %$macpac_do_i (%$macpac_do_i + 7)
  %else
   %error "Invalid number or order of paramters given to UNTIL."
  %endif

  %$macpac_loop_exit:

 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; UNTIL - Completes a DO-UNTIL loop. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; This verison of takes one parameter: a condition code
;
%imacro UNTIL 1.nolist
 %ifnctx do_context 
  %error "UNTIL not used in DO context."
 %else
  j%-1 %$macpac_loop_begin
  %$macpac_loop_exit:
  %pop
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; WHILE - Begins a WHILE loop. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; This verison of takes one parameter. It may be a condition code
; (e,ne,a,ae,b,be,g,ge,l,le,z,nz) but can also be a true value 
; (`true` or a number != 0) to create an endless loop which is exited 
; by some other means.
;
%imacro WHILE 1.nolist
 %push while_context

 %ifnum %$macpac_proc_depth
  %assign %$macpac_proc_depth (%$macpac_proc_depth + 1)
 %endif

 %assign %$macpac_loop_depth 1

%$macpac_loop_begin:
 %ifnum %1
  %if %1 = 0
   jmp %$macpac_loop_exit
  %endif
 %elifidni %1,true
  ;do nothing
 %else
  j%-1 %$macpac_loop_exit
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; WHILE - Begins a WHILE loop. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; Parameters:
; 1&3 - operands to be tested
; 2   - condition code (e,ne,a,ae,b,be,g,ge,l,le,z,nz)
; 4   - the testing instruction; it defaults to 'cmp' but
; can be replaced with 'test' or some other instruction which affects
; the flags register.
;
%imacro WHILE 3-4.nolist cmp
 %push while_context

 %ifnum %$macpac_proc_depth 
  %assign %$macpac_proc_depth (%$macpac_proc_depth + 1)
 %endif

 %assign %$macpac_loop_depth 1

%$macpac_loop_begin:
 %4 %1, %3
 j%-2 %$macpac_loop_exit   
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; WHILE - Begins a WHILE loop, takes multiple conditionals. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
%imacro WHILE 7-*.nolist
 %push while_context

%$macpac_loop_begin:

  %assign %$macpac_do_i 1
  %rep %0

   %ifidni %1,AND
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 5
     %assign %$macpac_a1 (%$macpac_do_i - 4)
     %assign %$macpac_cc (%$macpac_do_i - 3)
     %assign %$macpac_a2 (%$macpac_do_i - 2)
     %assign %$macpac_in (%$macpac_do_i - 1)

     %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_donarg%{$macpac_cc} %$macpac_loop_exit

     %assign %$macpac_do_i (%$macpac_do_i + 6)
    %elif %$macpac_argcheck = 4
     %assign %$macpac_a1 (%$macpac_do_i - 3)
     %assign %$macpac_cc (%$macpac_do_i - 2)
     %assign %$macpac_a2 (%$macpac_do_i - 1)

     cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_donarg%{$macpac_cc} %$macpac_loop_exit

     %assign %$macpac_do_i (%$macpac_do_i + 7)
    %else
     %error "Invalid number or order of paramters given to WHILE."
    %endif
   %elifidni %1,OR
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 5
     %assign %$macpac_a1 (%$macpac_do_i - 4)
     %assign %$macpac_cc (%$macpac_do_i - 3)
     %assign %$macpac_a2 (%$macpac_do_i - 2)
     %assign %$macpac_in (%$macpac_do_i - 1)

     %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_doarg%{$macpac_cc} %$macpac_loop_pass

     %assign %$macpac_do_i (%$macpac_do_i + 6)
    %elif %$macpac_argcheck = 4
     %assign %$macpac_a1 (%$macpac_do_i - 3)
     %assign %$macpac_cc (%$macpac_do_i - 2)
     %assign %$macpac_a2 (%$macpac_do_i - 1)

     cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
     %$macpac_doarg%{$macpac_cc} %$macpac_loop_pass

     %assign %$macpac_do_i (%$macpac_do_i + 7)
    %else
     %error "Invalid number or order of paramters given to WHILE."
    %endif
   %else
    %assign %$macpac_argcheck (%$macpac_do_i % 10)
    %if %$macpac_argcheck = 2
     %xdefine %$macpac_doarg%{$macpac_do_i} j%+1
     %xdefine %$macpac_donarg%{$macpac_do_i} j%-1
    %else
     %xdefine %$macpac_doarg%{$macpac_do_i} %1
    %endif
    %assign %$macpac_do_i (%$macpac_do_i + 1)
   %endif

   %rotate 1
  %endrep

  %assign %$macpac_argcheck (%$macpac_do_i % 10)
  %if %$macpac_argcheck = 5
   %assign %$macpac_a1 (%$macpac_do_i - 4)
   %assign %$macpac_cc (%$macpac_do_i - 3)
   %assign %$macpac_a2 (%$macpac_do_i - 2)
   %assign %$macpac_in (%$macpac_do_i - 1)

   %$macpac_doarg%{$macpac_in} %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
   %$macpac_donarg%{$macpac_cc} %$macpac_loop_exit

   %assign %$macpac_do_i (%$macpac_do_i + 6)
  %elif %$macpac_argcheck = 4
   %assign %$macpac_a1 (%$macpac_do_i - 3)
   %assign %$macpac_cc (%$macpac_do_i - 2)
   %assign %$macpac_a2 (%$macpac_do_i - 1)

   cmp %$macpac_doarg%{$macpac_a1}, %$macpac_doarg%{$macpac_a2}
   %$macpac_donarg%{$macpac_cc} %$macpac_loop_exit

   %assign %$macpac_do_i (%$macpac_do_i + 7)
  %else
   %error "Invalid number or order of paramters given to WHILE."
  %endif

  %$macpac_loop_pass:

%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; ENDW - Completes a WHILE loop.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
%imacro ENDW 0.nolist
 %ifnctx while_context
  %error "ENDW not used in WHILE context."
 %else
  jmp %$macpac_loop_begin
  %$macpac_loop_exit:
  %pop
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; BREAK - Breaks out of current loop (or switch).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
%ifnmacro BREAK
%imacro BREAK 0.nolist
 %ifctx do_context
  jmp %$macpac_loop_exit
 %elifctx while_context
  jmp %$macpac_loop_exit
 %elifctx switch_context
  jmp %$macpac_switch_exit
 %else

  %if %$macpac_loop_depth = 1
   jmp %$macpac_loop_exit
  %elif %$macpac_loop_depth = 2
   jmp %$$macpac_loop_exit
  %elif %$macpac_loop_depth = 3
   jmp %$$$macpac_loop_exit
  %elif %$macpac_loop_depth = 4
   jmp %$$$$macpac_loop_exit
  %elif %$macpac_loop_depth = 5
   jmp %$$$$$macpac_loop_exit
  %elif %$macpac_loop_depth = 6
   jmp %$$$$$$macpac_loop_exit
  %elif %$macpac_loop_depth = 7
   jmp %$$$$$$$macpac_loop_exit
  %elif %$macpac_loop_depth = 8
   jmp %$$$$$$$$macpac_loop_exit
  %elif %$macpac_loop_depth = 9
   jmp %$$$$$$$$$macpac_loop_exit
  %else
   %error "BREAK nested too deep or used in invalid context."
  %endif

 %endif
%endmacro
%endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;
; CONTINUE - Jumps to top of current loop.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
%imacro CONTINUE 0.nolist
 %ifctx do_context
  jmp %$macpac_loop_begin
 %elifctx while_context
  jmp %$macpac_loop_begin
 %else

  %if %$macpac_loop_depth = 1
   jmp %$macpac_loop_begin
  %elif %$macpac_loop_depth = 2
   jmp %$$macpac_loop_begin
  %elif %$macpac_loop_depth = 3
   jmp %$$$macpac_loop_begin
  %elif %$macpac_loop_depth = 4
   jmp %$$$$macpac_loop_begin
  %elif %$macpac_loop_depth = 5
   jmp %$$$$$macpac_loop_begin
  %elif %$macpac_loop_depth = 6
   jmp %$$$$$$macpac_loop_begin
  %elif %$macpac_loop_depth = 7
   jmp %$$$$$$$macpac_loop_begin
  %elif %$macpac_loop_depth = 8
   jmp %$$$$$$$$macpac_loop_begin
  %elif %$macpac_loop_depth = 9
   jmp %$$$$$$$$$macpac_loop_begin
  %else
   %error "CONTINUE nested too deep or used in invalid context."
  %endif

 %endif
%endmacro

;;;;;;;;; end loops.mac

;;;;;;;;; start branch.mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; IF - Starts an IF block. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters:
; 1&3 - operands to test
; 2   - jump condition (e,ne,a,ae,b,be,g,ge,l,le,z,nz) 
; 4   - comparison instruction; defaults to `cmp` but can be replaced
; with `test` or some other instruction which manipulates eflags.
;
%imacro IF 3-4.nolist cmp 
 %push if_context

 %ifnum %$macpac_proc_depth
  %assign %$macpac_proc_depth (%$macpac_proc_depth + 1)
 %endif
 %ifnum %$macpac_loop_depth
  %assign %$macpac_loop_depth (%$macpac_loop_depth + 1)
 %endif

 ;The following two are used by ELSEIF
 %assign %$macpac_if_i 1 
 %assign %$macpac_elseif_count 1

 %4 %1, %3
 j%-2  %$macpac_if_not 
%endmacro 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; IF - Starts an IF block with conjunctions or disjunctions. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%imacro IF 7-*.nolist
 %push if_context

 %ifnum %$macpac_proc_depth
  %assign %$macpac_proc_depth (%$macpac_proc_depth + 1)
 %endif
 %ifnum %$macpac_loop_depth
  %assign %$macpac_loop_depth (%$macpac_loop_depth + 1)
 %endif

 %assign %$macpac_if_i 1
 %rep %0 

  %ifidni %1,AND
   %assign %$macpac_argcheck (%$macpac_if_i % 10)
   %if %$macpac_argcheck = 5
    %assign %$macpac_a1 (%$macpac_if_i - 4)
    %assign %$macpac_cc (%$macpac_if_i - 3)
    %assign %$macpac_a2 (%$macpac_if_i - 2)
    %assign %$macpac_in (%$macpac_if_i - 1)

    %$macpac_ifarg%{$macpac_in} %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifnarg%{$macpac_cc} %$macpac_if_not 

    %assign %$macpac_if_i (%$macpac_if_i + 6)
   %elif %$macpac_argcheck = 4 
    %assign %$macpac_a1 (%$macpac_if_i - 3)
    %assign %$macpac_cc (%$macpac_if_i - 2)
    %assign %$macpac_a2 (%$macpac_if_i - 1)

    cmp %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifnarg%{$macpac_cc} %$macpac_if_not 

    %assign %$macpac_if_i (%$macpac_if_i + 7)
   %else
    %error "Invalid number or order of paramters given to IF."
   %endif
  %elifidni %1,OR
   %assign %$macpac_argcheck (%$macpac_if_i % 10)
   %if %$macpac_argcheck = 5
    %assign %$macpac_a1 (%$macpac_if_i - 4)
    %assign %$macpac_cc (%$macpac_if_i - 3)
    %assign %$macpac_a2 (%$macpac_if_i - 2)
    %assign %$macpac_in (%$macpac_if_i - 1)

    %$macpac_ifarg%{$macpac_in} %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifarg%{$macpac_cc} %$macpac_if_pass 

    %assign %$macpac_if_i (%$macpac_if_i + 6)
   %elif %$macpac_argcheck = 4 
    %assign %$macpac_a1 (%$macpac_if_i - 3)
    %assign %$macpac_cc (%$macpac_if_i - 2)
    %assign %$macpac_a2 (%$macpac_if_i - 1)

    cmp %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifarg%{$macpac_cc} %$macpac_if_pass 

    %assign %$macpac_if_i (%$macpac_if_i + 7)
   %else
    %error "Invalid number or order of paramters given to IF."
   %endif
  %else
   %assign %$macpac_argcheck (%$macpac_if_i % 10)
   %if %$macpac_argcheck = 2
    %xdefine %$macpac_ifarg%{$macpac_if_i} j%+1
    %xdefine %$macpac_ifnarg%{$macpac_if_i} j%-1 
   %else
    %xdefine %$macpac_ifarg%{$macpac_if_i} %1
   %endif
   %assign %$macpac_if_i (%$macpac_if_i + 1)
  %endif

  %rotate 1
 %endrep
 
 %assign %$macpac_argcheck (%$macpac_if_i % 10)
 %if %$macpac_argcheck = 5
  %assign %$macpac_a1 (%$macpac_if_i - 4)
  %assign %$macpac_cc (%$macpac_if_i - 3)
  %assign %$macpac_a2 (%$macpac_if_i - 2)
  %assign %$macpac_in (%$macpac_if_i - 1)

  %$macpac_ifarg%{$macpac_in} %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
  %$macpac_ifnarg%{$macpac_cc} %$macpac_if_not 

  %assign %$macpac_if_i (%$macpac_if_i + 6)
 %elif %$macpac_argcheck = 4
  %assign %$macpac_a1 (%$macpac_if_i - 3)
  %assign %$macpac_cc (%$macpac_if_i - 2)
  %assign %$macpac_a2 (%$macpac_if_i - 1)

  cmp %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
  %$macpac_ifnarg%{$macpac_cc} %$macpac_if_not 

  %assign %$macpac_if_i (%$macpac_if_i + 7)
 %else
  %error "Invalid number or order of paramters given to IF."
 %endif
 
 %$macpac_if_pass:

 %assign %$macpac_elseif_count 1

%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; IF - Starts an IF block. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This version takes one paramter, a condition 
; code: e,ne,a,ae,b,be,g,ge,l,le,z,nz
;
%imacro IF 1.nolist
 %push if_context

 %ifnum %$macpac_proc_depth 
  %assign %$macpac_proc_depth (%$macpac_proc_depth + 1)
 %endif
 %ifnum %$macpac_loop_depth
  %assign %$macpac_loop_depth (%$macpac_loop_depth + 1)
 %endif

 %assign %$macpac_elseif_count 1

 j%-1 %$macpac_if_not
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ELSEIF - Alternate execution path for an IF block. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters:
; 1&3 - operands to test
; 2   - jump condition (e,ne,a,ae,b,be,g,ge,l,le,z,nz) 
; 4   - comparison instruction; defaults to `cmp` but can be replaced
; with `test` or some other instruction which manipulates eflags.
;
%imacro ELSEIF 3-4.nolist cmp
 %ifctx if_context
  %repl elseif_context

  jmp %$macpac_if_end 
  %$macpac_if_not: 
  %4 %1, %3
  j%-2  %$macpac_elseif_not%{$macpac_elseif_count} 

 %elifctx elseif_context

  jmp %$macpac_if_end
  %$macpac_elseif_not%{$macpac_elseif_count}:
  %assign %$macpac_elseif_count (%$macpac_elseif_count + 1)
  %4 %1, %3
  j%-2  %$macpac_elseif_not%{$macpac_elseif_count} 

 %else
  %error "ELSEIF not used in IF context."
 %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ELSEIF - Alternate IF block path with conjunctions or disjunctions. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%imacro ELSEIF 7-*.nolist
 %ifctx if_context
  %repl elseif_context

  jmp %$macpac_if_end
  %$macpac_if_not:

 %elifctx elseif_context

  jmp %$macpac_if_end
  %$macpac_elseif_not%{$macpac_elseif_count}:  
  %assign %$macpac_elseif_count (%$macpac_elseif_count + 1)

 %else
  %error "ELSEIF not used in IF context."
 %endif

 %rep %0 

  %ifidni %1,AND
   %assign %$macpac_argcheck (%$macpac_if_i % 10)
   %if %$macpac_argcheck = 5
    %assign %$macpac_a1 (%$macpac_if_i - 4)
    %assign %$macpac_cc (%$macpac_if_i - 3)
    %assign %$macpac_a2 (%$macpac_if_i - 2)
    %assign %$macpac_in (%$macpac_if_i - 1)

    %$macpac_ifarg%{$macpac_in} %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifnarg%{$macpac_cc} %$macpac_elseif_not%{$macpac_elseif_count}

    %assign %$macpac_if_i (%$macpac_if_i + 6)
   %elif %$macpac_argcheck = 4 
    %assign %$macpac_a1 (%$macpac_if_i - 3)
    %assign %$macpac_cc (%$macpac_if_i - 2)
    %assign %$macpac_a2 (%$macpac_if_i - 1)

    cmp %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifnarg%{$macpac_cc} %$macpac_elseif_not%{$macpac_elseif_count} 

    %assign %$macpac_if_i (%$macpac_if_i + 7)
   %else
    %error "Invalid number or order of paramters given to IF."
   %endif
  %elifidni %1,OR
   %assign %$macpac_argcheck (%$macpac_if_i % 10)
   %if %$macpac_argcheck = 5
    %assign %$macpac_a1 (%$macpac_if_i - 4)
    %assign %$macpac_cc (%$macpac_if_i - 3)
    %assign %$macpac_a2 (%$macpac_if_i - 2)
    %assign %$macpac_in (%$macpac_if_i - 1)

    %$macpac_ifarg%{$macpac_in} %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifarg%{$macpac_cc} %$macpac_elseif_pass%{$macpac_elseif_count} 

    %assign %$macpac_if_i (%$macpac_if_i + 6)
   %elif %$macpac_argcheck = 4 
    %assign %$macpac_a1 (%$macpac_if_i - 3)
    %assign %$macpac_cc (%$macpac_if_i - 2)
    %assign %$macpac_a2 (%$macpac_if_i - 1)

    cmp %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
    %$macpac_ifarg%{$macpac_cc} %$macpac_elseif_pass%{$macpac_elseif_count} 

    %assign %$macpac_if_i (%$macpac_if_i + 7)
   %else
    %error "Invalid number or order of paramters given to IF."
   %endif
  %else
   %assign %$macpac_argcheck (%$macpac_if_i % 10)
   %if %$macpac_argcheck = 2
    %xdefine %$macpac_ifarg%{$macpac_if_i} j%+1
    %xdefine %$macpac_ifnarg%{$macpac_if_i} j%-1 
   %else
    %xdefine %$macpac_ifarg%{$macpac_if_i} %1
   %endif
   %assign %$macpac_if_i (%$macpac_if_i + 1)
  %endif

  %rotate 1
 %endrep
 
 %assign %$macpac_argcheck (%$macpac_if_i % 10)
 %if %$macpac_argcheck = 5
  %assign %$macpac_a1 (%$macpac_if_i - 4)
  %assign %$macpac_cc (%$macpac_if_i - 3)
  %assign %$macpac_a2 (%$macpac_if_i - 2)
  %assign %$macpac_in (%$macpac_if_i - 1)

  %$macpac_ifarg%{$macpac_in} %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
  %$macpac_ifnarg%{$macpac_cc} %$macpac_elseif_not%{$macpac_elseif_count}

  %assign %$macpac_if_i (%$macpac_if_i + 6)
 %elif %$macpac_argcheck = 4
  %assign %$macpac_a1 (%$macpac_if_i - 3)
  %assign %$macpac_cc (%$macpac_if_i - 2)
  %assign %$macpac_a2 (%$macpac_if_i - 1)

  cmp %$macpac_ifarg%{$macpac_a1}, %$macpac_ifarg%{$macpac_a2}
  %$macpac_ifnarg%{$macpac_cc} %$macpac_elseif_not%{$macpac_elseif_count}

  %assign %$macpac_if_i (%$macpac_if_i + 7)
 %else
  %error "Invalid number or order of paramters given to IF."
 %endif
 
 %$macpac_elseif_pass%{$macpac_elseif_count}:

%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ELSE - An alternate execution path for an IF block. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%imacro ELSE 0.nolist 
 %ifctx if_context 
  %repl else_context 

  jmp %$macpac_if_end 
  %$macpac_if_not: 

 %elifctx elseif_context

  jmp %$macpac_if_end
  %$macpac_elseif_not%{$macpac_elseif_count}:

 %else 
  %error "ELSE not used in IF or ELSEIF context." 
 %endif 
%endmacro 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ENDIF - Ends an IF block. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%imacro ENDIF 0.nolist 
 %ifctx if_context 
  %$macpac_if_not: 
  %pop 
 %elifctx elseif_context
  %$macpac_if_end: 
  %pop
 %elifctx else_context 
  %$macpac_if_end: 
  %pop 
 %else 
  %error "ENDIF not used in IF, ELSEIF or ELSE context." 
 %endif 
%endmacro


;;;;;;;;; end branch.mac

%endif
