%include 'macros.inc'

%macro debug_print 1-*
  %push _debug_print_
  push edx
  push ecx
  push ebx
  push eax
    %ifstr %1
      string %%tmp, %1
      syscall SYS_write, 1, str_ %+ %%tmp, str_ %+ %%tmp %+ .Size
    %elifnum %1
      string %%tmp, %1
      syscall SYS_write, 1, str_ %+ %%tmp, str_ %+ %%tmp %+ .Size
    %endif
  pop eax
  pop ebx
  pop ecx
  pop edx
  %pop
%endmacro

