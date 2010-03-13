;;
;; xstruc -- NASM extended struc macro (2002.07.04)
;;
;; Copyright (c) 2002 by Joergen Ibsen / Jibz
;; All Rights Reserved
;;

%ifndef __XSTRUC_INC_INCLUDED
%define __XSTRUC_INC_INCLUDED

; =============================================================

%imacro xstruc 1-4.nolist 1,0,xstruc
  %ifctx xstruc
    %define %%toplevel 0              ; figure out if this is a
  %elifctx xunion                     ; sub struc/union
    %define %%toplevel 0              ;
  %else                               ;
    %define %%toplevel 1              ;
  %endif
  %ifidni %1, "-"
    %define %%noname 1                ; figure out if this is a
  %else                               ; nameless sub struc/union
    %define %%noname 0                ;
  %endif
  %if (%%toplevel == 1)
    %push %4                          ; push first xstruc context
    %define %$name %1                 ; define name in first context
    %assign %$size %3                 ; define size in first context
    %define %$nameless %%noname       ; define nameless in first context
  %else
    %if (%%noname == 1)
      %xdefine %%tname %{$name}       ; store substruc name in tmp
    %else
      %xdefine %%tname %{$name}.%1    ; store substruc name in tmp
    %endif
    %assign %%tsize %$size            ; store current offset in tmp
    %push %4                          ; new xstruc context
    %define %$name %%tname            ; define name in new context
    %assign %$size %%tsize            ; define size in new context
    %define %$nameless %%noname       ; define nameless in new context
    %if (%%noname == 0)
      %$name equ %$size               ; substruc name equ offset in struc
    %endif
  %endif
  %assign %$osize %$size              ; save offset at start
  %assign %$usize 0                   ; zero max size of union member
  %assign %$times %2                  ; save times argument
%endmacro

%imacro xunion 1-3.nolist 1,0         ; xunion is identical to xstruc,
  xstruc %1,%2,%3,xunion              ; except for context name pushed
%endmacro

%imacro xstruc 0.nolist
  xstruc "-",1,0,xstruc
%endmacro

%imacro xunion 0.nolist
  xstruc "-",1,0,xunion
%endmacro

; =============================================================

%imacro xitemb 1-2.nolist 1
  %{$name}.%1 equ %$size              ; item name equ offset
  %assign %$size %$size + (%2)        ; add size to offset
  %ifctx xunion
    %assign %%tsize %$size-%$osize    ; tmp = size of item
    %if (%%tsize > %$usize)           ;
      %assign %$usize %%tsize         ; update max size of union member
    %endif                            ;
    %if (%$nameless == 0)
      %{$name}.%{1}_size equ %%tsize  ; _size equ size of item
    %endif
    %assign %$size %$osize            ; size = offset of union
  %endif
%endmacro

%imacro xitemw 1-2.nolist 1
  xitemb %1,2*(%2)                    ; word = 2 bytes
%endmacro

%imacro xitemd 1-2.nolist 1
  xitemb %1,4*(%2)                    ; dword = 4 bytes
%endmacro

%imacro xitemq 1-2.nolist 1
  xitemb %1,8*(%2)                    ; qword = 8 bytes
%endmacro

%imacro xitemt 1-2.nolist 1
  xitemb %1,10*(%2)                   ; tword = 10 bytes
%endmacro

%imacro xspace 1.nolist
  %assign %$size %$size + (%1)        ; add size to offset
%endmacro

%imacro xalign 1.nolist
  %assign %$size %$size + (((%$size + %1) / %1) * %1) ; align
%endmacro

; =============================================================

%imacro xends 0.nolist
  %if ((%$size-%$osize) > %$usize)    ; update max size of union member,
    %assign %$usize %$size-%$osize    ; or calculate size of struc
  %endif                              ;
  %assign %$usize %$times*%$usize     ; make room for times copies
  %if (%$nameless == 0)
    %{$name}_size equ %$usize         ; _size equ size of struc/union
  %endif
  %assign %%tsize %$usize             ; store size in tmp
  %pop                                ; pop context
  %ifctx xstruc
    %assign %$size %$size+%%tsize     ; update offset in outer struc
  %elifctx xunion
    %assign %$size %$size+%%tsize     ; update offset in outer union
    %if ((%$size-%$osize) > %$usize)  ;
      %assign %$usize %$size-%$osize  ; update max size of union member
    %endif                            ;
    %assign %$size %$osize            ; size = offset of union
  %endif
%endmacro

; =============================================================

%imacro xistruc 1.nolist
  %ifctx xistruc
    %xdefine %%tname %{$name}.%1      ; store substruc name in tmp
    ; %%tname            = offset we should be at in enclosing struc
    ; $ - %$xistrucstart = actual offset in enclosing struc
    times %%tname-($-%$xistrucstart) db 0 ; pad up to substruc
    %push xistruc                     ; new xistruc context
    %define %$name %%tname            ; define name in new context
    %define %$sname %$name            ; define strucname in new context
  %else
    %push xistruc                     ; new xistruc context
    %define %$name %1                 ; define name in first context
    %define %$sname 0                 ; define strucname in first context
  %endif
  %$xistrucstart:                     ; label at start of struc/union
%endmacro

%idefine xiunion xistruc

; =============================================================

%imacro xatoffs 1-2+.nolist
  ; %1                 = offset we should be at in enclosing struc
  ; $ - %$xistrucstart = actual offset in enclosing struc
  times (%1)-($-%$xistrucstart) db 0 ; pad to this value
  %2
%endmacro

%imacro xat 1-2+.nolist
  ; %{$name}.%1 - %$sname = offset we should be at in enclosing struc
  xatoffs %{$name}.%1-%$sname,%2
%endmacro

; =============================================================

%imacro xiends 0.nolist
  ; %{$name}_size = size of struc = offset struc should end at
  xatoffs %{$name}_size
  %pop
%endmacro

; =============================================================

%endif ; __XSTRUC_INC_INCLUDED
