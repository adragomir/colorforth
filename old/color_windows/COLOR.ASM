;colorForth, 2001 Jul 22, Chuck Moore, Public Domain

; Boots into 32-bit mode with a flat address space. 
; Segment registers are based at zero; essentially unused. Interrupts off. Protections off. 
; Data are addressed as 32-bit words, not bytes. 
; But esp and esi hold byte addresses, for optimization. 
; Instructions are optimized if agruments are literals. 
; Registers are assigned:
; 0 eax: stack (1st number on Data stack) 
; 1 ecx: string counter, scratch 
; 2 edx: address register A, I/O port, scratch 
; 3 ebx: unused 
; 4 esp: byte pointer to top of Return stack 
; 5 ebp: unused 
; 6 esi: byte pointer to 2nd number on Data stack 
; 7 edi: dword pointer to next word to be interpreted

; blocks.bin originally at offset 4800h (1200h * 4)
; icons.bin  originally at offset 3000h

.486p
.MODEL flat, stdcall

_STACK     segment para 'STACK'

Godd_size  equ 750*4
mains_size equ 1500*4
maind_size equ 750*4
gap_size   equ 750*4
area_size  equ Godd_size+mains_size+maind_size+maind_size+gap_size

           dd  area_size/4 dup (offset dummy)

Gods       equ $

trash_adr  db  64 dup ('T')
dummy      dd  0

Godd       equ Gods-Godd_size
mains      equ Godd-mains_size
maind      equ mains-maind_size
_STACK     ends

.code

code_begin:

includelib kernel32.lib
includelib gdi32.lib
includelib user32.lib

; imports from kernel32.dll
    ExitProcess        PROTO :DWORD
    GetLastError       PROTO
    GetModuleHandleA   PROTO :DWORD

    CloseHandle        PROTO :DWORD
    CreateFileA        PROTO :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD
    CreateFileMappingA PROTO :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD
    MapViewOfFile      PROTO :DWORD, :DWORD, :DWORD, :DWORD, :DWORD
    UnmapViewOfFile    PROTO :DWORD
    VirtualAlloc       PROTO :DWORD, :DWORD, :DWORD, :DWORD
    
    LoadLibraryA       PROTO :DWORD
    GetProcAddress     PROTO :DWORD, :DWORD

; imports from user32.dll
    GetAsyncKeyState   PROTO :DWORD
    GetKeyState        PROTO :DWORD
    MapVirtualKeyA     PROTO :DWORD, :DWORD
    RegisterClassExA   PROTO :DWORD
    CreateWindowExA    PROTO :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD
    ShowWindow         PROTO :DWORD, :DWORD
    UpdateWindow       PROTO :DWORD
    DefWindowProcA     PROTO :DWORD, :DWORD, :DWORD,:DWORD
    GetDC              PROTO :DWORD
    LoadIconA          PROTO :DWORD, :DWORD
    LoadCursorA        PROTO :DWORD, :DWORD
    BeginPaint         PROTO :DWORD, :DWORD
    EndPaint           PROTO :DWORD, :DWORD
    InvalidateRect     PROTO :DWORD, :DWORD, :DWORD
      
    PeekMessageA       PROTO :DWORD, :DWORD, :DWORD, :DWORD, :DWORD
    GetMessageA        PROTO :DWORD, :DWORD, :DWORD, :DWORD
    TranslateMessage   PROTO :DWORD
    DispatchMessageA   PROTO :DWORD
    
       
; imports from gdi32.dll
    CreateCompatibleDC PROTO :DWORD
    CreateDIBSection   PROTO :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD
    StretchDIBits      PROTO :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD, :DWORD
          

fail_process:
    push   1      
    call   ExitProcess
    
bye:    
exit_process:
    push   0      
    call   ExitProcess

    
last_error:
    call   GetLastError
    ret        

next   MACRO adr
    dec    ecx
    jnz    adr
ENDM

DUP_   MACRO
    lea    esi, [esi-4]
    mov    [esi], eax
ENDM

DROP   MACRO
    lodsd
ENDM

hp     equ 1024    ; horizontal size
vp     equ 768     ; vertical size
bpc    equ 2       ; bytes per color (16 bit)

;   100000 dictionary
;    a0000 top of return stack
;    9f800 top of data stack
;    9d800 free
;    97000 floppy buffer
;     4800 source
;icons  equ 12*256*4 ; 3000
;     7c00 BIOS boot sector
;        0 Forth

icons_file     db  'icons.bin', 0
blocks_file    db  'blocks.bin', 0

icons_adr      dd  0
blocks_adr     dd  0

icon0: 
    DUP_
    mov    eax, [icons_adr]
    shr    eax, 2
    ret
    
blk0:
    DUP_
    mov    eax, [blocks_adr]
    shr    eax, 2
    sub    eax, 18 shl 10
    ret

; in:  edx - file name
; out: eax - memory address
map_file:
    push   0               ; template file
    push   00000080h       ; attributes
    push   00000003h       ; OPEN_EXISTING
    push   0               ; security attributes
    push   00000003h       ; FILE_SHARE_READ or FILE_SHARE_WRITE
    push   0c0000000h      ; GENERIC_READ or GENERIC_WRITE
    push   edx             ; file name
    call   CreateFileA
    inc    eax
    jz     map_file_ex     ; -1 indicates error

    dec    eax
    xor    ebx, ebx
    push   ebx             ; 0 return address
    push   eax             ; save file handle
    
    push   ebx             ; object name
    push   ebx             ; size
    push   ebx
    push   00000004h       ; PAGE_READWRITE
    push   ebx             ; security attributes
    push   eax             ; file handle
    call   CreateFileMappingA
    or     eax, eax
    jz     map_file_ex1
    
    xor    ebx, ebx
    push   ebx             ; whole file
    push   ebx             ; position
    push   ebx             
    push   00000002h       ; SECTION_MAP_WRITE
    push   eax
    call   MapViewOfFile

    pop    ebx             ; file handle
    pop    edx             ; drop 0 return address
    push   eax             ; save address
    push   ebx
map_file_ex1:
    call   CloseHandle  

    pop    eax   
map_file_ex:    
    ret

; in:  eax - memory address
unmap_file:
    push   eax
    call   UnmapViewOfFile
    ret       

map_work_files:
    mov    edx, offset icons_file
    call   map_file
    or     eax, eax
    jz     fail_process
    mov    [icons_adr], eax
     
    mov    edx, offset blocks_file
    call   map_file
    or     eax, eax
    jz     fail_process
    mov    [blocks_adr], eax

    ret 

; in:  ecx - size
; out: eax - address
alloc_mem:
    push   40h                   ; PAGE_EXECUTE_READWRITE
    push   1000h                 ; MEM_COMMIT
    push   ecx                   ; size 1 MB
    push   00000000h             ; dictionary starting address
    call   VirtualAlloc
    or     eax, eax
    jz     fail_process
    ret
    
alloc_buffers:
; dictionary
    mov    ecx, 1024*1024        ; dictionary size 1 MB
    call   alloc_mem      
    mov    [H], eax

    ret    

WNDCLASSEX struc
  cbSize            DWORD      ?
  style             DWORD      ?
  lpfnWndProc       DWORD      ?
  cbClsExtra        DWORD      ?
  cbWndExtra        DWORD      ?
  hInstance         DWORD      ?
  hIcon             DWORD      ?
  hCursor           DWORD      ?
  hbrBackground     DWORD      ?
  lpszMenuName      DWORD      ?
  lpszClassName     DWORD      ?
  hIconSm           DWORD      ?
WNDCLASSEX ends

wcmain     WNDCLASSEX          <>
hmodule    dd  0
hiconmain  dd  0
hcurmain   dd  0
hwndmain   dd  0                
wndwidth   dd  hp*2/3
wndheight  dd  vp*2/3
szwndclass db  "ColorForth" , 0
szwndname  db  "ColorForth for Windows", 0

wnd_proc   proc hWnd :DWORD, uMsg   :DWORD, wParam :DWORD, lParam :DWORD
    mov    edx, [uMsg]
    
@@: cmp    edx,  2h  ; WM_DESTROY
    jne    @f
    jmp    exit_process

@@: cmp    edx, 0fh  ; WM_PAINT    
    jne    @f
    call   draw_display
    xor    eax, eax
    jmp    wnd_proc_exx

@@: cmp    edx, 5h   ; WM_SIZE
    jne    @f
    mov    ebx, [lParam]
    mov    eax, ebx
    and    eax, 0000ffffh
    mov    [wndwidth], eax
    shr    ebx, 16                                                                    
    mov    [wndheight], ebx
    push   [hwndmain]
    call   UpdateWindow
    xor    eax, eax
    jmp    wnd_proc_ex
    
@@:    
    push   [lParam]
    push   [wParam]
    push   [uMsg]
    push   [hWnd]
    call   DefWindowProcA
    ret

wnd_proc_ex:
    mov    eax, 1
wnd_proc_exx:    
    ret 
wnd_proc   endp

alloc_window:
    push   0
    call   GetModuleHandleA
    mov    [hmodule], eax
    
    push   1
    push   eax
    call   LoadIconA
    mov    [hiconmain], eax
    
    push   32512 ; IDC_ARROW
    push   0
    call   LoadCursorA
    mov    [hcurmain], eax

    mov    ebx, offset wcmain
    mov    [ebx].WNDCLASSEX.cbSize, sizeof WNDCLASSEX
    mov    [ebx].WNDCLASSEX.style, 0001h or 0002h or 2000h ; or 80000000h
    mov    [ebx].WNDCLASSEX.lpfnWndProc, wnd_proc
    mov    [ebx].WNDCLASSEX.cbClsExtra, 0
    mov    [ebx].WNDCLASSEX.cbWndExtra, 0
    mov    eax, [hmodule]
    mov    [ebx].WNDCLASSEX.hInstance, eax
    mov    [ebx].WNDCLASSEX.hbrBackground, 000000h
    mov    [ebx].WNDCLASSEX.lpszMenuName, 0
    mov    [ebx].WNDCLASSEX.lpszClassName, offset szwndclass 
    mov    eax, [hiconmain]
    mov    [ebx].WNDCLASSEX.hIcon, eax
    mov    [ebx].WNDCLASSEX.hIconSm, eax
    mov    eax, [hcurmain]
    mov    [ebx].WNDCLASSEX.hCursor, eax
    push   ebx
    call   RegisterClassExA
    or     eax, eax
    jz     fail_process
    
    xor    ebx, ebx
    push   ebx
    push   [hmodule]
    push   ebx
    push   ebx
    push   [wndheight]
    push   [wndwidth]
    push   0
    push   0
    push   0CF0000h ; WS_SIZEBOX or WM_MINIMIZEBOX or WM_MAXIMIZEBOX
    push   offset szwndname
    push   offset szwndclass
    push   300h ; WS_EX_OVERLAPPEDWINDOW
    call   CreateWindowExA
    or     eax, eax
    jz     fail_process
    mov    [hwndmain], eax
    
    push   3
    push   eax
    call   ShowWindow
    
    push   0
    push   0
    push   [hwndmain]
    call   InvalidateRect
    
;    push   [hwndmain]
;    call   UpdateWindow
               
    ret         
    
POINT struc
  x  DWORD ?
  y  DWORD ?
POINT ends

RECT struc 
  left    dd      ?
  top     dd      ?
  right   dd      ?
  bottom  dd      ?
RECT ends

MSG struc
  hwnd      DWORD      ?
  message   DWORD      ?
  wParam    DWORD      ?
  lParam    DWORD      ?
  time      DWORD      ?
  pt        POINT      <>
MSG ends
    
wm  MSG    <>         
process_messages:
;    push   1      ; PM_REMOVE
    push   0h     ; WM_KEYLAST
    push   0h     ; WM_KEYFIRST
    push   0 ; [hwndmain]      
    push   offset wm
    call   GetMessageA
    or     eax, eax
    jnz    process_messages_disp
    jmp    exit_process
    
process_messages_disp:    
@@: push   offset wm
    call   TranslateMessage
    push   offset wm
    call   DispatchMessageA
    
process_messages_ex:
    ret
    
    
BITMAPINFOHEADER struc
  biSize            DWORD      ?
  biWidth           DWORD      ?
  biHeight          DWORD      ?
  biPlanes          WORD       ?
  biBitCount        WORD       ?
  biCompression     DWORD      ?
  biSizeImage       DWORD      ?
  biXPelsPerMeter   DWORD      ?
  biYPelsPerMeter   DWORD      ?
  biClrUsed         DWORD      ?
  biClrImportant    DWORD      ?
BITMAPINFOHEADER ends
    
bmih       BITMAPINFOHEADER    <>
hdcdesktop dd 0
hbmpframe  dd 0
hdcframe   dd 0

    
alloc_display:
; get DC
    push   [hwndmain]
    call   GetDC
    or     eax, eax
    jz     fail_process
    mov    [hdcdesktop], eax
    
; create bitmap DC
    push   0
    call   CreateCompatibleDC                                                                                    
    or     eax, eax
    jz     fail_process
    mov    [hdcframe], eax

; create bitmap for frame data
    xor    eax, eax
    mov    ebx, offset bmih
    mov    [ebx].BITMAPINFOHEADER.biSize, sizeof BITMAPINFOHEADER
    mov    [ebx].BITMAPINFOHEADER.biWidth, hp
    mov    [ebx].BITMAPINFOHEADER.biHeight, -vp
    mov    [ebx].BITMAPINFOHEADER.biPlanes, 1
    mov    [ebx].BITMAPINFOHEADER.biBitCount, 16
    mov    [ebx].BITMAPINFOHEADER.biCompression, eax               ; BI_RGB = 0
    mov    [ebx].BITMAPINFOHEADER.biSizeImage, eax
    mov    [ebx].BITMAPINFOHEADER.biXPelsPerMeter, eax
    mov    [ebx].BITMAPINFOHEADER.biYPelsPerMeter, eax
    mov    [ebx].BITMAPINFOHEADER.biClrUsed, eax
    mov    [ebx].BITMAPINFOHEADER.biClrImportant, eax
    push   eax
    push   eax
    push   offset frame
    push   eax
    push   ebx
    push   [hdcframe]
    call   CreateDIBSection
    or     eax, eax
    jz     fail_process
    mov    [hbmpframe], eax

    ret

PAINTSTRUCT struc
  hdc           DWORD      ?
  fErase        DWORD      ?
  rcPaint       RECT       <>
  fRestore      DWORD      ?
  fIncUpdate    DWORD      ?
  rgbReserved   BYTE 32 dup(?)
PAINTSTRUCT ends

paintstruc PAINTSTRUCT  <>
hdcpaint   dd  0

draw_display:
    push   offset paintstruc
    push   [hwndmain]
    call   BeginPaint
    or     eax, eax
    jz     fail_process
    mov    [hdcpaint], eax

    xor    ebx, ebx
    push   0CC0020h        ; SRCCOPY
    push   ebx
    push   offset bmih 
    push   [frame]   
    push   vp
    push   hp              
    push   ebx
    push   ebx
    push   [wndheight]
    push   [wndwidth]
    push   ebx
    push   ebx
    push   [hdcpaint]    
    call   StretchDIBits
    
    push   offset paintstruc
    push   [hwndmain]
    call   EndPaint
    
    ret

notimpl:
    jmp    fail_process
    
    
fname  dd  0
       db  0    

; ( $fn -- a )
fmap:
    mov    [fname], eax
    push   esi
    mov    edx, offset fname
    call   map_file
    pop    esi
    shr    eax, 2
    ret
    
; ( a -- )
funmap:
    push   esi
    shl    eax, 2
    call   unmap_file
    pop    esi
    DROP
    ret

; ( block count $fn -- )
bload:
    call   fmap
    mov    ebx, eax
    DROP   
    mov    ecx, eax
    DROP
    push   esi
    push   edi 
    mov    edi, eax
    or     ebx, ebx
    jz     bload_ex
    mov    esi, ebx
    push   esi
    shl    ecx, 8  ; * 256
    sub    edi, 18
    shl    edi, 10
    add    edi, [blocks_adr]
    shl    esi, 2
    rep    movsd
    pop    esi
    call   unmap_file
    
bload_ex:    
    pop    edi
    pop    esi
    DROP
    ret
    
dlopen:    ; ( DLLName -- hDLL )
    push   eax
    call   LoadLibraryA
    ret
    
dlimp:     ; ( hDLL ProcName -- a )
    push   eax
    DROP
    push   eax
    call   GetProcAddress
    ret
     

stack0     dd  0
stack1     dd  0
stack2     dd  0

warm: 
    DUP_
_start:
start: 
; initialize stacks
    mov    eax, esp
    mov    [stack0], eax
    mov    ecx, 64*1024/4
@@: push   ecx
    dec    ecx
    jnz    @b
    mov    esp, eax
    sub    eax, 24*1024
    mov    [stack1], eax
    sub    eax, 24*1024
    mov    [stack2], eax

;       mov    [me], esp
;    mov    esp, Gods
    mov    esi, Godd
    
    call   map_work_files
    call   alloc_buffers
;    call   alloc_signal_char
    call   alloc_window
    call   alloc_display
    
    call   show0
    
    mov    forths, (forth1-forth0)/4
    mov    macros, (macro1-macro0)/4
    
; load block 0
    mov    eax, 18
    call   LOAD
    
    jmp    ACCEPT

ALIGN 4
nc         dd  9 ; Forth+Icons+blocks 24-161
me         dd  offset God
screen     dd  0 ; logo

ROUND: 
    call   unPAAUSE_
God        dd  0 ; Gods-2*4
    call   unPAAUSE_
main       dd  0 ; mains-2*4
    jmp    ROUND

PAAUSE_: 
    DUP_
    push   esi         
    mov    eax, me     
    mov    [eax], esp  
    add    eax, 4      
    jmp    eax         

unPAAUSE_: 
    pop    eax        
    mov    esp, [eax] 
    mov    me, eax    
    pop    esi        
    DROP
    ret

ACT: 
    mov    edx, [stack2] ; maind-4 
    sub    edx, 4
    mov    [edx], eax   
    mov    eax, [stack1] ; mains-4 
    sub    eax, 4       
    pop    [eax]        
    sub    eax, 4       
    mov    [eax], edx   
    mov    main, eax    
    DROP
    ret

COMMENT #
ACT: 
    mov    edx, maind-4 
    mov    [edx], eax   
    mov    eax, mains-4 
    pop    [eax]        
    sub    eax, 4       
    mov    [eax], edx   
    mov    main, eax    
    DROP
    ret
#


show0: 
    call   show
    ret
    
show: 
    pop    screen
    DUP_
    xor    eax, eax
    call   ACT
@@:     
    call   graphic
    call   [screen]
    call   SWITCH
    inc    eax
    jmp    @b

c_:  
    mov    esi, Godd+4
    ret

mark: 
    mov    ecx, macros
    mov    mk, ecx
    mov    ecx, forths
    mov    mk+4, ecx
    mov    ecx, H
    mov    mk+2*4, ecx
    ret

empty: 
    mov    ecx, mk+2*4
    mov    H, ecx
    mov    ecx, mk+4
    mov    forths, ecx
    mov    ecx, mk
    mov    macros, ecx
    mov    class, 0
    ret

mFIND: 
    mov    ecx, macros
    push   edi
    lea    edi, [macro0-4+ecx*4]
    jmp    @f

FIND: 
    mov    ecx, forths
    push   edi
    lea    edi, [forth0-4+ecx*4]
@@: std
    repne  scasd
    cld
    pop    edi
    ret

EX1: 
    dec    words ; from keyboard
    jz     @f
    DROP
    jmp    EX1

@@: call   FIND
    jnz    ABORT1
    DROP
    jmp    [forth2+ecx*4]

execute: 
    mov    lit, offset alit
    DUP_
    mov    eax, [-4+edi*4]
ex2: 
    and    eax, -20o
    call   FIND
    jnz    ABORT
    DROP
    jmp    [forth2+ecx*4]

ABORT: 
    mov    curs, edi
        shl    edi, 2
        sub    edi, [blocks_adr] 
    shr    edi, 10
        add    edi, 18
    mov    blk, edi
ABORT1: 
    mov    esp, [stack0] ; Gods
    mov    spaces+3*4, offset forthd
    mov    spaces+4*4, offset qcompile
    mov    spaces+5*4, offset cnum
    mov    spaces+6*4, offset cshort
    mov    eax, 57o ; ?
    call   ECHO_
    jmp    ACCEPT

sDEFINE: 
    pop    aDEFINE
    ret
    
MACRO_: 
    call   sDEFINE
macrod: 
    mov    ecx, macros
    inc    macros
    lea    ecx, [macro0+ecx*4]
    jmp    @f

FORTH: 
    call   sDEFINE
forthd: 
    mov    ecx, [forths]
    inc    [forths]
    lea    ecx, [forth0+ecx*4]
@@: mov    edx, [-4+edi*4]
    and    edx, -20o
    mov    [ecx], edx
    mov    edx, h
    mov    [forth2-forth0+ecx], edx
    lea    edx, [forth2-forth0+ecx]
    shr    edx, 2
    mov    [last], edx
    mov    [list], esp
    mov    [lit], offset adup
    test   class, -1
    jz     @f
    jmp    [class]
@@: ret

cdrop: 
    mov    edx, [h]
    mov    [list], edx
    mov    byte ptr [edx], 0adh ; lodsd
    inc    [h]
    ret

qdup: 
    mov    edx, [H]
    dec    edx
    cmp    [list], edx
    jnz    cdup
    cmp    byte ptr [edx], 0adh
    jnz    cdup
    mov    [H], edx
    ret
    
cdup: 
    mov    edx, [H]
    mov    dword ptr [edx], 89fc768dh
    mov    byte ptr [4+edx], 06
    add    [H], 5
    ret

adup: 
    DUP_
    ret

var1: 
    DUP_
    mov    eax, [4+forth0+ecx*4]
    ret
    
variable: 
    call   forthd
    mov    [forth2-forth0+ecx], offset var1
    inc    forths ; dummy entry for source address
    mov    [4+ecx], edi
    call   macrod
    mov    [forth2-forth0+ecx], offset @f
    inc    macros
    mov    [4+ecx], edi
    inc    edi
    ret
@@: call   [lit]
    mov    eax, [4+macro0+ecx*4]
    jmp    @f

cNUM: 
    call   [lit]
    mov    eax, [edi*4]
    inc    edi
    jmp    @f

cSHORT:    
    call   [lit]
    mov    eax, [-4+edi*4]
    sar    eax, 5
@@: call   literal
    DROP
    ret

alit: 
    mov    [lit], offset adup
literal: 
    call   qDUP
    mov    edx, [list]
    mov    [list+4], edx
    mov    edx, [h]
    mov    [list], edx
    mov    byte ptr [edx], 0b8h
    mov    [1+edx], eax
    add    [h], 5
    ret

qCOMPILE: 
    call   [lit]
    mov    eax, [-4+edi*4]
    and    eax, -20o
    call   mFIND
    jnz    @f
    DROP
    jmp    [macro2+ecx*4]
@@: call   FIND
    mov    eax, [forth2+ecx*4]
@@: jnz    ABORT
call_: 
    mov    edx, [h]
    mov    [list], edx
    mov    byte ptr [edx], 0e8h
    add    edx, 5
    sub    eax, edx
    mov    [-4+edx], eax
    mov    [h], edx
    DROP
    ret

COMPILE: 
    call   [lit]
    mov    eax, [-4+edi*4]
    and    eax, -20o
    call   mFIND
    mov    eax, [macro2+ecx*4]
    jmp    @b

SHORT_: 
    mov    lit, offset alit
    DUP_
    mov    eax, [-4+edi*4]
    sar    eax, 5
    ret

NUM: 
    mov    [lit], offset alit
    DUP_
    mov    eax, [edi*4]
    inc    edi
    ret

comma: 
    mov    ecx, 4
@@: mov    edx, [h]
    mov    [edx], eax
    mov    eax, [esi] ; drop
    lea    edx, [edx+ecx]
    lea    esi, [esi+4]
    mov    [h], edx
    ret

comma1: 
    mov    ecx, 1
    jmp    @b

comma2: 
    mov    ecx, 2
    jmp    @b

comma3: 
    mov    ecx, 3
    jmp    @b

semi: 
    mov    edx, [H]
    sub    edx, 5
    cmp    [list], edx
    jnz    @f
    cmp    byte ptr [edx], 0e8h
    jnz    @f
    inc    byte ptr [edx] ; jmp
    ret
@@: mov    byte ptr [5+edx], 0c3h ; ret
    inc    [H]
    ret

then: 
    mov    [list], esp
    mov    edx, [h]
    sub    edx, eax
    mov    [-1+eax], DL
    DROP
    ret

begin: 
    mov    [list], esp
here: 
    DUP_
    mov    eax, [h]
    ret

qlit: 
    mov    edx, [h]
    lea    edx, [edx-5]
    cmp    [list], edx
    jnz    @f
    cmp    byte ptr [edx], 0b8h
    jnz    @f
    DUP_
    mov    eax, [list+4]
    mov    [list], eax
    mov    eax, [1+edx]
    cmp    dword ptr [edx-5], 89fc768dh ; dup
    jz     q1
    mov    [h], edx
    jmp    cdrop            
q1: 
    add    [h], -10 ; flag nz
    ret
@@: xor    edx, edx ; flag z
    ret

less: 
    cmp    [esi], eax
    js     @f ; flag nz
    xor    ecx, ecx ; flag z
@@: ret

qIGNORE: 
    test   dword ptr [-4+edi*4], -20o
    jnz    nul
    pop    edi
    pop    edi
nul: 
    ret

jump: 
    pop    edx
    add    edx, eax
    lea    edx, [5+eax*4+edx]
    add    edx, [-4+edx]
    DROP
    jmp    edx

LOAD: 
    sub    eax, 18
    shl    eax, 10-2
    mov    ebx, [blocks_adr]
    shr    ebx, 2
    add    eax, ebx
    push   edi
    mov    edi, eax
    DROP
INTER:  
    mov    edx, [edi*4]
    inc    edi
    and    edx, 17o
    call   spaces[edx*4]
    jmp    INTER

ALIGN 4
spaces     dd   offset qIGNORE,  offset execute, offset NUM                                    
aDEFINE    dd 5+offset MACRO_                                               
           dd   offset qCOMPILE, offset cNUM,    offset cSHORT, offset COMPILE                 
           dd   offset SHORT_,   offset nul,     offset nul,    offset nul                     
           dd   offset variable, offset nul,     offset nul,    offset nul                     
                                                                                               
lit        dd offset adup                                                                      
mk         dd 0, 0, 0                                                                          
H          dd 40000h*4                                                                         
last       dd 0                                                                                
class      dd 0                                                                                
list       dd 0, 0                                                                             
macros     dd 0                ; number of macros (?)                                                                
forths     dd 0                ; number of forth words (?)


macro0 dd 170o shl 25 ; ;
       dd ((140o shl 7+146o)shl 7+142o)shl 11 ; dup
       dd (((177o shl 7+140o)shl 7+146o)shl 7+142o)shl 4 ; ?dup
       dd (((140o shl 4+1)shl 4+3)shl 7+142o)shl 10 ; drop
       dd (((2 shl 7+144o)shl 4+4)shl 4+6)shl 13 ; then
       dd ((((143o shl 4+4)shl 5+25o)shl 4+7)shl 4+6)shl 8 ; begin
macro1 dd 128 dup (0)
forth0 dd (((143o shl 4+3)shl 4+3)shl 4+2)shl 13 ; boot
       dd ((((143o shl 5) + 23o) shl 4) + 4o) shl 16 ; bye
;       dd (((27o shl 4+5)shl 4+1)shl 5+21o)shl 14 ; warm
       dd ((((142o shl 4+5)shl 7+146o)shl 5+20o)shl 4+4)shl 5 ; PAAUSE_
       dd ((((21o shl 4+5)shl 5+22o)shl 4+1)shl 4+3)shl 10 ; MACRO
       dd ((((26o shl 4+3)shl 4+1)shl 4+2)shl 7+144o)shl 8 ; FORTH
       dd 22o shl 27 ; c
       dd (((20o shl 4+2)shl 4+3)shl 7+142o)shl 12 ; stop
       dd (((1 shl 4+4)shl 4+5)shl 7+140o)shl 13 ; read
       dd ((((27o shl 4+1)shl 4+7)shl 4+2)shl 4+4)shl 11 ; write
       dd (6 shl 5+22o)shl 23 ; nc
       dd (((((22o shl 4+3)shl 5+21o)shl 5+21o)shl 4+5)shl 4+6)shl 5; comman d
       dd (((20o shl 4+4)shl 4+4)shl 7+164o)shl 12 ; seek
       dd ((((1 shl 4+4)shl 4+5)shl 7+140o)shl 5+23o)shl 8 ; ready
       dd ((5 shl 5+22o)shl 4+2)shl 19 ; ACT
       dd (((20o shl 7+144o)shl 4+3) shl 5+27o)shl 11 ; SHOW
       dd (((24o shl 4+3)shl 4+5)shl 7+140o)shl 12 ; LOAD
       dd (((144o shl 4+4)shl 4+1)shl 4+4)shl 13 ; here
       dd (((177o shl 5+24o)shl 4+7)shl 4+2)shl 12 ; ?lit
       dd (153o shl 7+176o) shl 18 ; 3,
       dd (152o shl 7+176o) shl 18 ; 2,
       dd (151o shl 7+176o) shl 18 ; 1,
       dd 176o shl 25 ; ,
       dd (((24o shl 4+4)shl 5+20o)shl 5+20o)shl 13 ; less
       dd (((162o shl 7+146o)shl 5+21o)shl 7+142o)shl 6 ; jump
       dd (((((5 shl 5+22o)shl 5+22o)shl 4+4)shl 7+142o)shl 4+2)shl 3 ; accept
       dd ((142o shl 4+5)shl 7+140o)shl 14 ; pad
       dd ((((4 shl 4+1)shl 4+5)shl 5+20o)shl 4+4)shl 11 ; erase
       dd (((22o shl 4+3)shl 7+142o)shl 5+23o)shl 11 ; copy
       dd (((21o shl 4+5)shl 4+1)shl 7+164o)shl 12 ; mark
       dd (((4 shl 5+21o)shl 7+142o)shl 4+2)shl 12 ; empt
       dd (((4 shl 5+21o)shl 4+7)shl 4+2)shl 15 ; emit
       dd ((((140o shl 4+7)shl 5+25o)shl 4+7)shl 4+2)shl 8 ; digit
       dd ((((152o shl 4+4)shl 5+21o)shl 4+7)shl 4+2)shl 8 ; 2emit
       dd 165o shl 25 ; .
       dd (144o shl 7+165o)shl 18 ; h.
       dd ((144o shl 7+165o)shl 4+6)shl 14 ; h.n
       dd (22o shl 4+1)shl 23 ; CR
       dd ((((20o shl 7+142o)shl 4+5)shl 5+22o)shl 4+4)shl 7 ; space
       dd (((140o shl 4+3)shl 5+27o)shl 4+6)shl 12 ; DOWN
       dd (((4 shl 7+140o)shl 4+7)shl 4+2)shl 13 ; edit
       dd 4 shl 28 ; E
       dd (24o shl 5+21o)shl 22 ; lm
       dd (1 shl 5+21o)shl 23 ; rm
       dd ((((25o shl 4+1)shl 4+5)shl 7+142o)shl 7+144o)shl 5 ; graph ic
       dd (((2 shl 4+4)shl 7+145o)shl 4+2)shl 13 ; text
       dd ((((164o shl 4+4)shl 5+23o)shl 7+143o)shl 4+3)shl 5 ; keybo ard
       dd (((140o shl 4+4)shl 7+143o)shl 7+146o)shl 7 ; debu g
       dd (5 shl 4+2)shl 24 ; at
       dd ((173o shl 4+5)shl 4+2)shl 17 ; +at
       dd (145o shl 5+23o)shl 20 ; xy
       dd ((26o shl 4+3)shl 7+141o)shl 16 ; fov
       dd (((26o shl 4+7)shl 5+26o)shl 4+3)shl 14 ; fifo
       dd ((143o shl 4+3)shl 7+145o)shl 14 ; box
       dd (((24o shl 4+7)shl 4+6)shl 4+4)shl 15 ; line
       dd ((((22o shl 4+3)shl 5+24o)shl 4+3)shl 4+1)shl 10 ; color
       dd (((((3 shl 5+22o)shl 4+2)shl 4+5)shl 4+6)shl 4+2)shl 7 ; octant
       dd (20o shl 7+142o)shl 20 ; sp
       dd (((24o shl 4+5)shl 5+20o)shl 4+2)shl 14 ; last
       dd (((((146o shl 4+6)shl 7+142o)shl 4+5)shl 5+22o))shl 5 ; unpac k
       dd (((26o shl 5 + 21o) shl 4 + 5)shl 7+142o)shl 11 ; fmap
       dd (((((26o shl 7 + 146o) shl 4 + 6o)shl 5 + 21o)shl 4 + 5o) shl 7) ; funma p
       dd (((((7o shl 5 + 22o) shl 4 + 3o) shl 4 + 6o) shl 7) + 150o) shl 8 ; icons0 791b6800h
       dd ((((143o shl 5 + 24o) shl 4+3)shl 4+5)shl 7+140o)shl 5 ; bload
       dd (((143o shl 5 + 24o) shl 7 + 164o) shl 7 + 150o) shl 6 ; blk0
       dd 0cf741200h ; qwert y
       dd 0c143c4c0h ; dlopn
       dd 0c1478e20h ; dlimp
       
       
forth1 dd 512 dup (0)
macro2 dd offset semi
       dd offset cdup
       dd offset qdup
       dd offset cdrop
       dd offset then
       dd offset begin
       dd 128 dup (0)
forth2 dd offset notimpl ; boot
       dd offset bye     ; warm
       dd offset PAAUSE_
       dd offset MACRO_
       dd offset FORTH
       dd offset c_
       dd offset notimpl ; stop
       dd offset notimpl ; readf
       dd offset notimpl ; writef
       dd offset nc_
       dd offset notimpl ; cmdf
       dd offset notimpl ; seekf
       dd offset notimpl ; readyf
       dd offset ACT
       dd offset SHOW
       dd offset LOAD
       dd offset here
       dd offset qlit
       dd offset COMMA3
       dd offset COMMA2
       dd offset COMMA1
       dd offset COMMA
       dd offset less
       dd offset jump
       dd offset ACCEPT
       dd offset pad
       dd offset erase
       dd offset copy
       dd offset mark
       dd offset empty
       dd offset emit
       dd offset eDIG
       dd offset emit2
       dd offset dot10
       dd offset hdot
       dd offset hdotn
       dd offset CR
       dd offset space
       dd offset DOWN
       dd offset edit
       dd offset E
       dd offset LMs
       dd offset RMs
       dd offset graphic
       dd offset text1
       dd offset keyboard
       dd offset debug
       dd offset at
       dd offset pat
       dd offset xy_
       dd offset fov_
       dd offset fifof
       dd offset box
       dd offset line
       dd offset color
       dd offset octant
       dd offset sps
       dd offset last_
       dd offset unpack
       dd offset fmap
       dd offset funmap
       dd offset icon0
       dd offset bload 
       dd offset blk0
       dd offset qwerty
       dd offset dlopen
       dd offset dlimp
       dd 512 dup (0)

COMMENT #
boot: mov  AL, 0FEh ; Reset
    out  64h, AL
    jmp  $
#

erase: 
    mov    ecx, eax
    shl    ecx, 8
    DROP
    push   edi
    mov    edi, eax
    sub    edi, 18
    shl    edi, 2+8
    add    edi, [blocks_adr]
    xor    eax, eax
    rep    stosd
    pop    edi
    DROP
    ret

copy: 
    cmp    eax, 12   
    jc     ABORT1    
    mov    edi, eax  
    sub    edi, 18
    shl    edi, 2+8  
    push   esi       
    mov    esi, blk  
    sub    esi, 18
    shl    esi, 2+8  
    mov    ecx, 256  
    add    esi, [blocks_adr]
    add    edi, [blocks_adr]
    rep    movsd     
    pop    esi       
    mov    blk, eax  
    DROP
    ret
    
; move dwords
; ( sa da n -- )    
move:
    mov    ecx, eax
    DROP
    mov    edi, eax
    shl    edi, 2
    DROP
    mov    esi, eax
    shl    esi, 2
    DROP
    rep    movsd
    ret

debug: 
    mov    xy, 3*10000h+(vc-2)*ih+3
    DUP_
    mov    eax, God
    push   [eax]
    call   dot
    DUP_
    pop    eax
    call   dot
    DUP_
    mov    eax, main
    call   dot
    DUP_
    mov    eax, esi
    jmp    dot

iw         equ 16+6
ih         equ 24+6
hc         equ hp/iw ; 46
vc         equ vp/ih ; 25
ALIGN 4
xy         dd 3*10000h+3
lm         dd 3
rm         dd hc*iw ; 1012
xycr       dd 0
fov        dd 10*(2*vp+vp/2)

nc_: 
    DUP_
    mov    eax, (offset nc)
    shr    eax, 2
    ret

xy_: 
    DUP_
    mov    eax, (offset xy)
    shr    eax, 2
    ret

fov_: 
    DUP_
    mov    eax, (offset fov)
    shr    eax, 2
    ret

sps: DUP_
    mov    eax, (offset spaces)
    shr    eax, 2
    ret

last_: DUP_
    mov    eax, (offset last)
    shr    eax, 2
    ret

;; BEGIN gen.asm
;Generic graphics

ALIGN 4
frame      dd 2000000h-hp*vp*2 ; 32 M
;;!displ      dd 0f0000000h ; fujitsu
fore       dd 0f7deh
xc         dd 0
yc         dd 0

rgb: 
    ror    eax, 8
    shr    AX, 2
    ror    eax, 6
    shr    AL, 3
    rol    eax, 6+5
    and    eax, 0f7deh
    ret

white: 
    DUP_
    mov    eax, 0ffffffh
color: 
    call   rgb
    mov    fore, eax
    DROP
    ret

COMMENT #
north: 
    mov  edx, 0cf8h
    out  DX, eax
    add  edx, 4
    in   eax, DX
    ret
dev: mov  eax, 80001008h ; Find display, start at device 2
    mov  ecx, 31-1 ; end with AGP: 10008, bus 1, dev 0
@@:     DUP_
        call north
        and  eax, 0ff000000h
        cmp  eax, 3000000h
        DROP
        jz   @f
        add  eax, 800h
        next @b
@@: ret

ati0: call dev
    or   dword ptr [eax-4], 2 ; Enable memory
    add  AL, 24h-8 ; look for prefetch
    mov  CL, 5
@@:     DUP_
        call north
        xor  AL, 8
        jz   @f
        DROP
        sub  eax, 4
        next @b
    DUP_
    call north
    and  eax, 0fffffff0h
@@: mov  displ, eax
    DROP
    ret

#    

fifof: 
    DROP
graphic: 
    ret

switch:
    call   draw_display
    jmp    PAAUSE_

clip: 
    mov    edi, xy
    mov    ecx, edi
    test   CX, CX
    jns    @f
    xor    ecx, ecx
@@: and    ecx, 0ffffh
    mov    yc, ecx
    imul   ecx, hp*2
    sar    edi, 16
    jns    @f
    xor    edi, edi
@@: mov    xc, edi
    lea    edi, [edi*2+ecx]
    add    edi, frame
    ret

bit16: 
    lodsw
    xchg   AL, AH
    mov    ecx, 16
b16: 
    shl    AX, 1
    jnc    @f
    mov    [edi], DX
@@: add    edi, 2
    next   b16
    ret

bit32: 
    lodsw
    xchg   AL, AH
    mov    ecx, 16
b32: 
    shl    eax, 1
    jnc    @f
    mov    [edi], DX
    mov    [edi+2], DX
    mov    [edi+hp*2], DX
    mov    [edi+hp*2+2], DX
@@: add    edi, 4
    next   b32
    ret

emit: 
    call   qcr
    push   esi
    push   edi
    push   edx
    imul   eax, 16*24/8
    mov    esi, [icons_adr]
    add    esi, eax
;;!    lea    esi, icons[eax]
    call   clip
    mov    edx, fore
    mov    ecx, 24
@@:     
    push   ecx
    call   bit16
    add    edi, (hp-16)*2
    pop    ecx
    next   @b
    pop    edx
    pop    edi
    pop    esi
BL_: 
    DROP
SPACE: 
    add    xy, iw*10000h
    ret

emit2: 
    push   esi
    push   edi
    push   edx
    imul   eax, 16*24/8
    mov    esi, [icons_adr]
    add    esi, eax
;;!    lea    esi, 4icons[eax]
    call   clip
    mov    edx, fore
    mov    ecx, 24
@@: push   ecx
    call   bit32
    add    edi, (2*hp-16*2)*2
    pop    ecx
    next   @b
    pop    edx
    pop    edi
    pop    esi
    add    xy, iw*10000h*2
    DROP
    ret

text1: 
    call   WHITE
    mov    lm, 3
    mov    rm, hc*iw
    jmp    TOP

line: 
    call   clip
    mov    ecx, [esi]
    shl    ecx, 1
    sub    edi, ecx
    mov    ecx, eax
    mov    eax, fore
    rep    stosw
    inc    xy
    DROP
    DROP
    ret

box: 
    call   clip
    cmp    eax, vp+1
    js     @f
    mov    eax, vp
@@: mov    ecx, eax
    sub    ecx, yc
    jng    no
    cmp    dword ptr [esi], hp+1
    js     @f
    mov    dword ptr [esi], hp
@@: mov    eax, xc
    sub    [esi], eax
    jng    no
    mov    edx, hp
    sub    edx, [esi]
    shl    edx, 1
    mov    eax, fore
@@: push   ecx
    mov    ecx, [esi]
    rep    stosw
    add    edi, edx
    pop    ecx
    next   @b
no: DROP
    DROP
    ret

;; END gen.asm

yellow     equ 0ffff00h
CYAN: 
    DUP_
    mov    eax, 0ffffh
    jmp    color
MAGENTA: 
    DUP_
    mov    eax, 0ff00ffh
    jmp    color
SILVER:    
    DUP_
    mov    eax, 0c0c0c0h
    jmp    color
BLUE: 
    DUP_
    mov    eax, 4040ffh
    jmp    color
RED: 
    DUP_
    mov    eax, 0ff0000h
    jmp    color
GREEN: 
    DUP_
    mov    eax, 8000ff00h
    jmp    color

history    db 11 dup (0)

ECHO_: 
    push   esi
    mov    ecx, 11-1
    lea    edi, history
    lea    esi, [1+edi]
    rep    movsb
    pop    esi
    mov    history+11-1, AL
    DROP
    ret

RIGHT: 
    DUP_
    mov    ecx, 11
    lea    edi, history
    xor    eax, eax
    rep    stosb
    DROP
    ret

DOWN: 
    DUP_
    xor    edx, edx
    mov    ecx, ih
    div    ecx
    mov    eax, edx
    add    edx, 3*10000h+8000h-ih+3
    mov    xy, edx
ZERO: 
    test   eax, eax
    mov    eax, 0
    jnz    @f
    inc    eax
@@: ret

blank: 
    DUP_
    xor    eax, eax
    mov    xy, eax
    call   color
    DUP_
    mov    eax, hp
    DUP_
    mov    eax, vp
    jmp    box

TOP: 
    mov    ecx, lm
    shl    ecx, 16
    add    ecx, 3
    mov    xy, ecx
    mov    xycr, ecx
    ret

qcr: 
    mov    CX, word ptr xy+2
    cmp    CX, word ptr rm
    js     @f
cr: 
    mov    ecx, lm
    shl    ecx, 16
    mov    CX, word ptr xy
    add    ecx, ih
    mov    xy, ecx
@@: ret

LMs: 
    mov    lm, eax
    DROP
    ret

RMs: 
    mov    rm, eax
    DROP
    ret

at: 
    mov    word ptr xy, AX
    DROP
    mov    word ptr xy+2, AX
    DROP
    ret

pAT: 
    add    word ptr xy, AX
    DROP
    add    word ptr xy+2, AX
    DROP
    ret

octant: 
    DUP_
    mov    eax, 43h ; poly -last y+ x+ ;23h ; last y+ x+
    mov    edx, [4+esi]
    test   edx, edx
    jns    @f
    neg    edx
    mov    [4+esi], edx
    xor    AL, 1
@@: cmp    edx, [esi]
    jns    @f
    xor    AL, 4
@@: ret

; Keyboard

EIGHT: 
    add    edi, 12
    call   FOUR
    call   SPACE
    sub    edi, 16
FOUR: 
    mov    ecx, 4
FOUR1:  
    push   ecx
    DUP_
    xor    eax, eax
    mov    AL, [4+edi]
    inc    edi
    call   EMIT
    pop    ecx
    next   FOUR1
    ret

stack: 
    mov    edi, Godd-4
@@: mov    edx, God
    cmp    [edx], edi
    jnc    @f
    DUP_
    mov    eax, [edi]
    sub    edi, 4
    call   qDOT
    jmp    @b
@@: ret

KEYBOARD: 
    call   text1
    mov    edi, [board]
    DUP_
    mov    eax, [keyc]
    call   color
    mov    [rm], hc*iw
    mov    [lm], hp-9*iw+3
    mov    [xy], (hp-9*iw+3)*10000h+vp-4*ih+3
    call   EIGHT
    call   EIGHT
    call   EIGHT
    call   CR
    add    [xy], 4*iw*10000h
    mov    edi, [shift]
    add    edi, 4*4-4
    mov    ecx, 3
    call   FOUR1
    mov    [lm], 3
    mov    word ptr [xy+2], 3
    call   stack
    mov    word ptr [xy+2], hp-(11+9)*iw+3
    lea    edi, [history-4]
    mov    ecx, 11
    jmp    FOUR1

alpha      db 15o, 12o,  1 , 14o
           db 24o,  2 ,  6 , 10o
           db 23o, 11o, 17o, 21o
           db 22o, 13o, 16o,  7
           db  5 ,  3 ,  4 , 26o
           db 27o, 44o, 25o, 20o
           
graphics   db 31o, 32o, 33o,  0 
           db 34o, 35o, 36o, 30o
           db 37o, 40o, 41o, 57o
           db 51o, 50o, 52o, 54o ; : ; ! @
           db 46o, 42o, 45o, 56o ; Z J . ,
           db 55o, 47o, 53o, 43o ; * / + -
           
numbers    db 31o, 32o, 33o,  0
           db 34o, 35o, 36o, 30o
           db 37o, 40o, 41o,  0
           db  0,   0 ,  0 ,  0
           db  0,   0 ,  0 ,  0
           db  0,   0 ,  0 ,  0
           
octals     db 31o, 32o, 33o,  0
           db 34o, 35o, 36o, 30o
           db 37o, 40o, 41o,  0
           db  0 ,  5 , 23o, 12o
           db  0 , 20o,  4 , 16o
           db  0 ,  0 ,  0 ,  0

LETTER: 
    mov    edx, [board]
    test   byte ptr [isqwerty], 0ffh
    jnz    QWLETTER
CMLETTER:    
    cmp    AL, 4
    jns     @f      ; if al >= 4
    ret
@@: 
    mov    AL, [edx][eax]
    ret

QWLETTER:
    cmp    edx, offset alpha - 4
    je     @f
    cmp    edx, offset graphics - 4
    je     @f
    cmp    edx, offset numbers - 4
    je     @f
    cmp    edx, offset octals - 4
    je     @f
    call   QWADJUST
    jmp    CMLETTER

@@: 
    cmp    al, 72o
    js     @f
    sub    al, 72o
    ; QWDECODE uses what's in eax as an index
    jmp    QWDECODE
@@: 
    cmp    al, 4
    mov        cl, al
    js     @f
    mov    cl, 1
    cmp    al, 1  ; esc
    je     @f
    cmp    al, 14 ; bksp
    je     @f
    inc    cl
    cmp    al, 57 ; space
    je     @f
    inc    cl
    cmp    al, 28 ; enter
    je     @f
    cmp    al, 56 ; alt
    je     @f

QWDECODE:
    mov    edx, offset qwkeys
    mov    AL, [edx][eax]
    test   AL, AL
    ret

@@: mov    al, cl
    cmp    al, 4
    ret 

    
@@: mov    eax, 0
    ret
QWADJUST:
    test   byte ptr [isqwerty], 0ffh
    jz     @f
    cmp    AL, 4
    js     @f
    cmp    AL, 20o
    js     @b
    cmp    AL, 72o
    jns    @b
    mov    AL, [keys-20o+eax]    
@@: ret

isqwerty   db   0

qwkeys     db   0,   1
           db 31o, 32o, 33o, 34o, 35o, 36o, 37o, 40o, 41o, 30o, 43o, 53o,   1 ; 14
           db   0, 27o, 17o,  4o,  1o,  2o, 13o, 26o,  7o,  3o, 22o, 52o, 54o,   3      ; 28
           db   0,  5o, 10o, 20o, 16o, 15o, 24o, 42o, 44o, 14o, 51o, 50o, 57o,   0      ; 42
           db 55o, 46o, 25o, 12o, 21o, 23o,  6o, 11o, 56o, 45o, 47o,   0,   0,   0,   3 ; 56
           db   2,   0,   1,   2,   3

qwerty: ; ( isqwerty -- )
    mov    byte ptr [isqwerty], al
    DROP
    ret   

keys       db 16, 17, 18, 19,  0,  0,  4,  5 ; 20
           db  6,  7,  0,  0,  3,  3, 20, 21
           db 22, 23,  0,  0,  8,  9, 10, 11 ; 40
           db  0,  0,  0,  0, 24, 25, 26, 27
           db  0,  1, 12, 13, 14, 15,  0,  0 ; 60 N
           db  3,  2 ; alt space
    
wkey:
    DUP_
    xor    eax, eax
    call   PAAUSE_
    push   esi
    push   edi
    
    push   0
    push   0
    push   [hwndmain]
    call   InvalidateRect
    push   [hwndmain]
    call   UpdateWindow
    
wkey1:
    call   process_messages
    
    push   145      ; VK_SCROLL
    call   GetKeyState
    test   eax, 1
    mov    byte ptr [isqwerty], 1
    jz     @f
    mov    byte ptr [isqwerty], 0
    
@@: mov    ecx, 255
@@: push   ecx
    push   ecx
    call   GetAsyncKeyState
    pop    ecx
    test   eax, 1
    jnz    @f
    dec    ecx
    jnz    @b
    jmp    wkey1
@@: 
;    cmp    ecx, 164 ; alt
;    test   eax, 00008000h
;    jz     wkey1
    
    
@@: push   0
    push   ecx
    call   MapVirtualKeyA
    or     eax, eax
    jz     wkey1
    
    
@@: cmp    al, 72o ; 
    jnc    wkey1 ; jump if not carry, that is al >= 58
;
    test   byte ptr [isqwerty], 0ffh ; test, logical and
    mov    cl, al
    jnz    @f
    cmp    al, 20o                                                   
    jl     wkey1 ; if al < 16, where the chars begin
    mov    cl, [keys-20o+eax]
wkeyex:    
    mov    al, cl
    pop    edi
    pop    esi
    ret
    
@@: mov    cl, 1
    cmp    al, 1  ; esc
    je     wkeyex
    cmp    al, 14 ; bksp
    je     wkeyex
    inc    cl
    cmp    al, 57 ; space
    je     wkeyex

@@: mov    cl, al
    cmp    al, 4
    jnl    wkeyex
    add    cl, 72o
    jmp    wkeyex
    
KEY: 
    jmp    wkey

    DUP_
    xor    eax, eax
@@: call   PAAUSE_
    in     AL, 144o
    test   AL, 1
    jz     @b                                    
    in     AL, 140o
    test   AL, 360o
    jz     @b
    cmp    AL, 72o
    jnc    @b
    mov    AL, [keys-20o+eax]
    ret

ALIGN 4
graph0     dd offset nul0, offset nul0, offset nul0, offset ALPH0
           db  0 ,  0 ,  5 , 0 ;     a
graph1     dd offset WORD0, offset X, offset LJ, offset ALPH
           db 25o, 45o,  5 , 0 ; x . a
alpha0     dd offset nul0, offset nul0, offset NUMBER, offset STAR0
           db  0 , 41o, 55o, 0 ;   9 *
alpha1     dd offset WORD0, offset X, offset LJ, offset GRAPH
           db 25o, 45o, 55o, 0 ; x . *
numb0      dd offset nul0, offset MINUS, offset ALPHn, offset OCTAL
           db 43o,  5 , 16o, 0 ; - a f
numb1      dd offset NUMBER0, offset Xn, offset ENDN, offset NUMBER0
           db 25o, 45o,  0 , 0 ; x .

board      dd offset alpha-4
shift      dd offset alpha0
base       dd 10
current    dd offset decimal
keyc       dd yellow
chars      dd 1
aword      dd offset EX1
anumber    dd offset nul
words      dd 1

nul0: 
    DROP
    jmp    @f
    
ACCEPT:
acceptn: 
    mov    [shift], offset alpha0
    lea    edi, alpha-4
ACCEPT1: 
    mov    [board], edi
@@: call   KEY
    cmp    AL, 4
    jns    first
    mov    edx, [shift]
    jmp    dword ptr [edx+eax*4]

bits       db 28

@@: add    eax, 120o
    mov    CL, 7
    jmp    @f
    
PACK: 
    cmp    AL, 20o
    jnc    @b
    mov    CL, 4
    test   AL, 10o
    jz     @f
    inc    ecx
    xor    AL, 30o
@@: mov    edx, eax
    mov    CH, CL
@@: cmp    [bits], CL
    jnc    @f
    shr    AL, 1
    jc     FULL
    dec    CL
    jmp    @b
@@: shl    dword ptr [esi], CL
    xor    [esi], eax
    sub    [bits], CL
    ret

LJ0: 
    mov    CL, [bits]
    add    CL, 4
    shl    dword ptr [esi], CL
    ret

LJ: 
    call   LJ0
    DROP
    ret

FULL: 
    call   LJ0
    inc    words
    mov    bits, 28
    sub    bits, CH
    mov    eax, edx
    DUP_
    ret

X:  
    call   RIGHT
    mov    eax, words
    lea    esi, [eax*4+esi]
    DROP
    jmp    ACCEPT

WORD_: 
    call   RIGHT
    mov    words, 1
    mov    chars, 1
    DUP_
    mov    dword ptr [esi], 0
    mov    bits, 28
WORD1:  
    call   LETTER
    jns    @f
    mov    edx, [shift]
    jmp    dword ptr [edx+eax*4]
@@: test   AL, AL
    jz     WORD0
    DUP_
    call   ECHO_
    call   PACK
    inc    chars

WORD0:   
    DROP
    call   KEY
    jmp    WORD1

decimal: 
    mov    [base], 10
    mov    [shift], offset numb0
    mov    [board], offset numbers-4
    ret

hex: 
    mov    [base], 16
    mov    [shift], offset numb0 ; oct0
    mov    [board], offset octals-4
    ret

octal: 
    xor    [current], (offset decimal) 
    xor    [current], (offset hex)
    xor    byte ptr numb0+18, 41o xor 16o ; f vs 9
    call   [current]
    jmp    NUMBER0

Xn: DROP
    DROP
    jmp    ACCEPTn

digit      db 14, 10,  0,  0
           db  0,  0, 12,  0,  0,  0, 15,  0
           db 13,  0,  0, 11,  0,  0,  0,  0
           db  0,  1,  2,  3,  4,  5,  6,  7
           db  8,  9
sign       db 0

MINUS: 
    mov    [sign], AL
    jmp    NUMBER2

NUMBER0: 
    DROP
    jmp    NUMBER3

NUMBER: 
    call   [current]
    mov    [sign], 0
    xor    eax, eax
NUMBER3: 
    call   KEY
    call   LETTER
    jns    @f
    mov    edx, [shift]
    jmp    dword ptr [edx+eax*4]
@@: test   AL, AL
    jz     NUMBER0
    mov    AL, [digit-4+eax]
    test   [sign], 37o
    jz     @f
    neg    eax
@@: mov    edx, [esi]
    imul   edx, [base]
    add    edx, eax
@@: mov    [esi], edx

NUMBER2:   
    DROP
    mov    [shift], offset numb1
    jmp    NUMBER3

ENDN: 
    DROP
    call   [anumber]
    jmp    ACCEPTn

ALPHn: 
    DROP
ALPH0: 
    mov    [shift], offset alpha0
    lea    edi, [alpha-4]
    jmp    @f
STAR0: 
    mov    [shift], offset graph0
    lea    edi, [graphics-4]
@@: DROP
    jmp    ACCEPT1

ALPH: 
    mov    [shift], offset alpha1
    lea    edi, [alpha-4]
    jmp    @f
GRAPH: 
    mov    [shift], offset graph1
    lea    edi, [graphics-4]
@@: mov    [board], edi
    jmp    WORD0

first: 
    add    [shift], 4*4+4
    call   WORD_
    call   [aword]
    jmp    ACCEPT

hicon      db 30o, 31o, 32o, 33o, 34o, 35o, 36o, 37o
           db 40o, 41o,  5 , 23o, 12o, 20o,  4 , 16o 
eDIG1: 
    DUP_
eDIG: 
    push   ecx
    mov    AL, hicon[eax]
    call   EMIT
    pop    ecx
    ret

oDIG: 
    rol    eax, 4
    DUP_
    and    eax, 0Fh
    ret

hdotn: 
    mov    edx, eax
    neg    eax
    lea    ecx, [32+eax*4]
    DROP
    rol    eax, CL
    mov    ecx, edx
    jmp    @f
hdot: 
    mov    ecx, 8
@@: call   oDIG
    call   eDIG
    next   @b
    DROP
    ret

dot: 
    mov    ecx, 7
@@: call   oDIG
    jnz    @h
    DROP
    next   @b
    inc    ecx
@@: call   oDIG
@h1:call   eDIG
    next   @b
    call   space
    DROP
    ret
@h: inc    ecx
    jmp    @h1

qdot: 
    cmp    base, 10
    jnz    dot
dot10: 
    mov    edx, eax
    test   edx, edx
    jns    @f
    neg    edx
    DUP_
    mov    eax, 43o
    call   EMIT
@@: mov    ecx, 8
@@: mov    eax, edx
    xor    edx, edx
    div    tens[ecx*4]
    test   eax, eax
    jnz    D_1
    dec    ecx
    jns    @b
    jmp    D_2
@@: mov    eax, edx
    xor    edx, edx
    div    tens[ecx*4]
D_1: 
    call   eDIG1
    dec    ecx
    jns    @b
D_2: 
    mov    eax, edx
    call   eDIG1
    call   space ; spcr
    DROP
    ret

unPACK: 
    DUP_
    test   eax, eax
    js     @f
    shl    dword ptr [esi], 4
    rol    eax, 4
    and    eax, 7
    ret
@@: shl    eax, 1
    js     @f
    shl    dword ptr [esi], 5
    rol    eax, 4
    and    eax, 7
    xor    AL, 10o
    ret
@@: shl    dword ptr [esi], 7
    rol    eax, 6
    and    eax, 77o
    sub    AL, 20o
    ret

qRING: 
    DUP_
    inc    dword ptr [esi]
    cmp    curs, edi ; from abort, insert
    jnz    @f
    mov    curs, eax        
@@: cmp    eax, curs
    jz     RING
    jns    @f
    mov    pcad, edi
@@: DROP
    ret

RING: 
    mov    cad, edi
    sub    xy, iw*10000h ; bksp
    DUP_
    mov    eax, 0e04000h
    call   color
    mov    eax, 60o
    mov    CX, word ptr xy+2
    cmp    CX, word ptr rm
    js     @f
    call   EMIT
    sub    xy, iw*10000h ; bksp
    ret
@@: jmp    EMIT

rW: mov    CX, word ptr xy+2
    cmp    CX, word ptr lm
    jz     @f
    call   cr
@@: call   RED
    jmp    TYPE_

gW: call   GREEN
    jmp    TYPE_
mW: call   CYAN
    jmp    TYPE_
wW: DUP_
    mov    eax, yellow
    call   color
    jmp    TYPE_

type0: 
    sub    xy, iw*10000h ; call bspcr
    test   dword ptr [-4+edi*4], -20o
    jnz    type1
    dec    edi
    mov    lcad, edi
    call   space
    call   qRING
    pop    edx ; End of block
    DROP
    jmp    KEYBOARD

Cap: 
    call   white
    DUP_
    mov    eax, [-4+edi*4]
    and    eax, -20o
    call   unPACK
    add    AL, 48
    call   EMIT
    jmp    type2

CAPS: 
    call   white
    DUP_
    mov    eax, [-4+edi*4]
    and    eax, -20o
@@: call   unPACK
    jz     @f
    add    AL, 48
    call   EMIT
    jmp    @b

text: 
    call   WHITE
TYPE_:
type1: 
    DUP_
    mov    eax, [-4+edi*4]
    and    eax, -20o
type2:     
    call   unPACK
    jz     @f
    call   EMIT
    jmp    type2
@@: call   space
    DROP
    DROP
    ret

gsW: 
    mov    edx, [-4+edi*4]
    sar    edx, 5
    jmp    gnW1

var: 
    call   MAGENTA
    call   TYPE_
gnW: 
    mov    edx, [edi*4]
    inc    edi
gnW1: 
    DUP_
    mov    eax, 0f800h ; Green
    cmp    bas, offset dot10
    jz     @f        
    mov    eax, 0c000h ; dark green
    jmp    @f

sW: mov    edx, [-4+edi*4]
    sar    edx, 5
    jmp    nW1

nW: mov    edx, [edi*4]
    inc    edi
nW1: 
    DUP_
    mov    eax, yellow
    cmp    bas, offset dot10
    jz     @f        
    mov    eax, 0c0c000h ; dark yellow
@@: call   color
    DUP_
    mov    eax, edx
    jmp    [bas]

REFRESH: 
    call   SHOW
    call   blank
    call   text1
    DUP_            ; Counter
    mov    eax, lcad
    mov    cad, eax ; for curs beyond end

    mov    eax, blk
    sub    eax, 18
    shl    eax, 10-2
    mov    ebx, [blocks_adr]
    shr    ebx, 2
    add    eax, ebx
    mov    edi, eax 

    xor    eax, eax
    mov    pcad, edi ; for curs=0
ref1:   
    test   dword ptr [edi*4], 0fh
    jz     @f
    call   qRING
@@: mov    edx, [edi*4]
    inc    edi
    mov    bas, offset dot10
    test   DL, 20o
    jz     @f
    mov    bas, offset dot
@@: and    edx, 17o
    call   display[edx*4]
    jmp    ref1

ALIGN 4
display    dd offset TYPE0, offset wW,   offset nW,  offset rW
           dd offset gW,    offset gnW,  offset gsW, offset mW
           dd offset sW,    offset text, offset Cap, offset CAPS
           dd offset var,   offset nul,  offset nul, offset nul
tens       dd 10, 100, 1000, 10000, 100000, 1000000
           dd 10000000, 100000000, 1000000000
bas        dd offset dot10
blk        dd 18
curs       dd 0
cad        dd 0
pcad       dd 0
lcad       dd 0
trash      dd offset trash_adr ;;! buffer*4
ekeys      dd offset nul, offset DEL, offset Eout, offset destack
           dd offset act1, offset act3, offset act4, offset shadow
           dd offset mcur, offset mmcur, offset ppcur, offset pcur
           dd offset mblk, offset actv, offset act7, offset pblk
           dd offset nul, offset act11, offset act10, offset act9
           dd offset nul, offset nul, offset nul, offset nul
ekbd0      dd offset nul, offset nul, offset nul, offset nul
           db 25o, 45o,  7 ,  0  ; x  .  i
ekbd       db 17o,  1 , 15o, 55o ; w  r  g  *
           db 14o, 26o, 20o,  1  ; l  u  d  r
           db 43o, 11o, 12o, 53o ; -  m  c  +
           db  0 , 70o, 72o,  2  ;    S  C  t
           db  0 ,  0 ,  0 ,  0
           db  0 ,  0 ,  0 ,  0
actc       dd yellow, 0, 0ff0000h, 0c000h, 0, 0, 0ffffh
           dd 0, 0ffffffh, 0ffffffh, 0ffffffh, 8080ffh
vector     dd 0
action     db 1

act1: 
    mov    AL, 1
    jmp    @f
act3: 
    mov    AL, 3
    jmp    @f
act4: 
    mov    AL, 4
    jmp    @f
act9: 
    mov    AL, 9
    jmp    @f
act10: 
    mov    AL, 10
    jmp    @f
act11: 
    mov    AL, 11
    jmp    @f
act7: 
    mov    AL, 7
@@: mov    action, AL
    mov    eax, [actc-4+eax*4]
    mov    aword, offset insert
actn: 
    mov    keyc, eax
    pop    eax
    DROP
    jmp    ACCEPT

actv: 
    mov    action, 12
    mov    eax, 0ff00ffh ; Magenta
    mov    aword, offset @f
    jmp    actn

@@: DUP_
    xor    eax, eax
    inc    words
    jmp    insert    

mcur: 
    dec    curs
    jns    @f
pcur: 
    inc    curs
@@: ret

mmcur: 
    sub    curs, 8
    jns    @f
    mov    curs, 0
@@: ret
ppcur: 
    add    curs, 8
    ret

pblk: 
    add    blk, 2
    add    dword ptr [esi], 2
    ret
mblk: 
    cmp    blk, 20
    js     @f
    sub    blk, 2 
    sub    dword ptr [esi], 2
@@: ret

shadow: 
    xor    blk, 1
    xor    dword ptr [esi], 1
    ret

E0: DROP
    jmp    @f

edit: 
    mov    blk, eax
    DROP
E:  DUP_
    mov    eax, blk
    mov    anumber, offset FORMAT
    mov    byte ptr alpha0+4*4, 45o ; .
    mov    alpha0+4, offset E0
    call   REFRESH
@@: mov    [shift], offset ekbd0
    mov    [board], offset ekbd-4
    mov    [keyc], yellow
@@: call   KEY
    call   QWADJUST
    call   ekeys[eax*4]
    DROP
    jmp    @b

eout: 
    pop    eax
    DROP
    DROP
    mov    aword, offset EX1
    mov    anumber, offset nul
    mov    byte ptr alpha0+4*4, 0
    mov    alpha0+4, offset nul0
    mov    keyc, yellow
    jmp    ACCEPT

destack: 
    mov    edx, trash
    cmp    edx, offset trash_adr ;;! buffer*4
    jnz    @f
    ret
@@: sub    edx, 2*4
    mov    ecx, [edx+1*4]
    mov    words, ecx
@@: DUP_
    mov    eax, [edx]
    sub    edx, 1*4
    next   @b
    add    edx, 1*4
    mov    trash, edx

insert0: 
    mov    ecx, lcad  ; room available?
    add    ecx, words
    xor    ecx, lcad
    and    ecx, -100h
    jz     insert1
    mov    ecx, words ; no
@@:          
    DROP
    next @b
    ret

insert1: 
    push   esi
    mov    esi, lcad
    mov    ecx, esi
    dec    esi
    mov    edi, esi
    add    edi, words
    shl    edi, 2
    sub    ecx, cad
    js     @f
    shl    esi, 2
    std
    rep movsd
    cld
@@: pop    esi
    shr    edi, 2
    inc    edi
    mov    curs, edi ; like abort
    mov    ecx, words
@@: dec    edi
    mov    [edi*4], eax
    DROP   ; requires cld
    next   @b
    ret

insert: 
    call   insert0
    mov    CL, action
    xor    [edi*4], CL
    jmp    ACCEPT

FORMAT: 
    test   action, 12o ; ignore 3 and 9
    jz     @f
    DROP
    ret
@@: mov    edx, eax
    and    edx, 0FC000000h
    jz     @f
    cmp    edx, 0FC000000h
    jnz    FORMAT2
@@: shl    eax, 5
    xor    AL, 2 ; 6
    cmp    action, 4
    jz     @f
    xor    AL, 13o ; 8
@@: cmp    base, 10
    jz     @f
    xor    AL, 20o
@@: mov    words, 1
    jmp    insert

FORMAT2: 
    DUP_
    mov    eax, 1 ; 5
    cmp    [action], 4
    jz     @f
    mov    AL, 3 ; 2
@@: cmp    [base], 10
    jz     @f
    xor    AL, 20o
@@: xchg   eax, [esi]
    mov    [words], 2
    jmp    insert

DEL: 
    call   enstack
    mov    edi, [pcad]
    mov    ecx, [lcad]
    sub    ecx, edi
    shl    edi, 2
    push   esi
    mov    esi, [cad]
    shl    esi, 2
    rep    movsd
    pop    esi
    jmp    mcur

enstack: 
    DUP_
    mov    eax, [cad]
    sub    eax, [pcad]
    jz     ens
    mov    ecx, eax
    xchg   eax, edx
    push   esi
    mov    esi, [cad]
    lea    esi, [esi*4-4]
    mov    edi, [trash]
@@: std
    lodsd
    cld
    stosd
    next   @b
    xchg   eax, edx
    stosd
    mov    [trash], edi
    pop    esi
ens: 
    DROP
    ret

pad: 
    pop    edx
    mov    [vector], edx
    add    edx, 28*5
    mov    [board], edx
    sub    edx, 4*4
    mov    [shift], edx
@@:     
    call   KEY
    call   QWADJUST
    mov    edx, [vector]
    add    edx, eax
    lea    edx, [5+eax*4+edx]
    add    edx, [-4+edx]
    DROP
    call   edx
    jmp    @b

;org (1200h-1)*4
;    dd 0

code_end:

end _start

