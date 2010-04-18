%include 'macros.inc'

%macro debug_print 1+
  %push _debug_print_
  pushfd
  pushad
    %ifstr %1
      string %%tmp, %1
      ccall _write, 1, str_ %+ %%tmp, str_ %+ %%tmp %+ .Size
    %elifnum %1
      string %%tmp, %1
      ccall _write, 1, str_ %+ %%tmp, str_ %+ %%tmp %+ .Size
    %endif
  popad
  popfd
  %pop
%endmacro

