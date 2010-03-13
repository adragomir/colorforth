.equ blockstart, .; .equ blocknumber, -1

;# pad to block boundary
.macro BLOCK number
 .nolist
 .ifb \number
  .print "compiling next block"
  .equ blocknumber, blocknumber + 1
 .else
  .print "compiling block \number"
  .equ blocknumber, \number
 .endif
 .ifdef ABSOLUTE_BLOCKNUMBER
  .org blocknumber * 1024
 .else
  .org blockstart
  .equ blockstart, blockstart + 1024
 .endif
 SET_DEFAULT_TYPETAG
 .list
.endm

.macro SET_DEFAULT_TYPETAG
 .if blocknumber % 2  ;# even screens are code, odd are documentation
  .equ default_typetag, 9  ;# type 9 is plain text
 .else
  .equ default_typetag, 4  ;# type 4 is compile
 .endif
.endm

.macro SETTYPE word
 .nolist
 .equ type, 0
 .irp function [EXTENSION], [EXECUTE], [EXECUTELONG], [DEFINE], [COMPILEWORD]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [COMPILELONG], [COMPILESHORT], [COMPILEMACRO], [EXECUTESHORT]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [TEXT], [TEXTCAPITALIZED], [TEXTALLCAPS], [VARIABLE]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [], [], [], [], [], [EXECUTELONGHEX], [], [], [COMPILELONGHEX]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [COMPILESHORTHEX], [], [EXECUTESHORTHEX], [SKIP], [BINARY]
  NEXTTYPE "\word", "\function"
 .endr
 .list
.endm

.macro NEXTTYPE word, function
 .nolist
 .ifdef DEBUG_FORTH
  ;#.print "comparing \"\word\" with \"\function\""
 .endif
 .ifeqs "\word", "\function"
  .equ default_typetag, type
 .else
  .equ type, type + 1
 .endif
 .list
.endm

;# compile Forth words with Huffman coding
.macro FORTH words:vararg
 .nolist
 .equ wordcount, 0
 .irp word, \words
 .ifeq wordcount
  .equ typetag, 3  ;# first word is almost always definition
 .else
  .equ typetag, default_typetag
 .endif
 SETTYPE "\word"
 COMPILETYPE "\word"
 .equ wordcount, wordcount + 1
 .endr
 .list
.endm

.macro COMPILETYPE word
 .nolist
 .ifeq type - 27  ;# means the SETTYPE macro didn't find a match
  SET_DEFAULT_TYPETAG
  .if typetag == 2 || typetag == 5
   .long typetag, \word
  .elseif typetag == (2 + 16) || typetag == (5 + 16)
   .long typetag, 0x\word
  .elseif typetag == 6 || typetag == 8
   .long typetag | (\word << 5)
  .elseif typetag == (6 + 16) || typetag == (8 + 16)
   .long typetag | (0x\word << 5)
  .elseif typetag == 25  ;# SKIP
   .fill \word, 4, 0
  .elseif typetag == 26  ;# BINARY
   .long 0x\word
  .else
   FORTHWORD "\word"
  .endif
 .endif
 .list
.endm

.macro FORTHWORD word
 .nolist
 .equ packed, 0; .equ bits, 28
 ;# only 28 bits are used for a word, the low 4 bits are for the type tag
 .irpc letter, "\word"
  GETCODE "\letter"
  .equ maskbits, (1 << bits) - 1
  .equ testcode, (huffcode >> (shift - bits) & maskbits) << (shift - bits)
  .if bits - shift >= 0
   .equ packed, (packed << shift) | huffcode
   .equ bits, bits - shift
  ;# otherwise will the prefix plus all set bits fit in remaining space?
  .elseif testcode == huffcode
   .equ packed, (packed << bits) | (huffcode >> (shift - bits))
   .equ bits, 0
  .else
   .long (packed << (bits + 4)) | typetag
   .equ packed, huffcode; .equ bits, 28 - shift; .equ typetag, 0
  .endif
 .endr
 .ifne packed
  .long (packed << (bits + 4)) | typetag
 .endif
 .list
.endm

.macro GETCODE letter
;# see more documentation on this under 'pack' in color.asm source
 .nolist  ;# don't pollute listing with all these byte comparisons
 .equ huffindex, 0; .equ huffcode, 0
 .irpc huffman, " rtoeanismcylgfwdvpbhxuq0123456789j-k.z/;:!+@*,?"
  .ifeqs "\letter", "\huffman"
   .equ shift, 4 + (huffindex / 8)
   .ifge shift - 6
    .equ shift, 7
   .endif
   .exitm
  .else
   NEXTCODE
  .endif
 .endr
 .list  ;# go ahead and generate listing if enabled
.endm

.macro NEXTCODE
 .equ huffindex, huffindex + 1
 .equ huffcode, huffcode + 1
 .ifeq huffcode - 0b00001000 ;# going from 4-bit to 5-bit code
  .equ huffcode, 0b00010000
 .endif
 .ifeq huffcode - 0b00011000 ;# going from 5-bit to 7-bit code
  .equ huffcode, 0b01100000
 .endif
.endm

/* test cases for common problems
BLOCK 19
FORTH "jul31", "colored", "keypad", "number"
BLOCK
*/
BLOCK # should be 64
FORTH [TEXTALLCAPS], ans, [TEXT], core
FORTH abs, [TEXT], n-n, 0, or, -if, negate, then, ";"
FORTH align, [COMPILEMACRO], align, ";"
FORTH core, ";"  # redefine to no-op
BLOCK
FORTH [TEXT], some, [TEXTCAPITALIZED], ans, core, words,
FORTH [TEXT], not, previously, defined
FORTH abs, return, absolute, value
FORTH align, allow, compiling, of, macro, that, aligns,
 FORTH [TEXTCAPITALIZED], here, pointer
FORTH core, not, a, core, word, but, a, redefinition, of, the, word, that,
FORTH [TEXT], loaded, this, "screen;", now, a, no-op
BLOCK
FORTH [TEXTCAPITALIZED], mandelbrot, [EXECUTE], empty, [EXECUTE], core
 FORTH [VARIABLE], xl, [BINARY], 0, [VARIABLE], xr, [BINARY], 0
 FORTH [VARIABLE], yt, [BINARY], 0, [VARIABLE], yb, [BINARY], 0
 FORTH [VARIABLE], xspan, [BINARY], 0, [VARIABLE], yspan, [BINARY], 0
 FORTH [VARIABLE], dark, [BINARY], 0, [VARIABLE], pixel, [BINARY], 0,
 FORTH [VARIABLE], count, [BINARY], 0, [VARIABLE], level, [BINARY], 0
FORTH zlen, [EXECUTE], hp, [EXECUTE], vp, [EXECUTE], *, [EXECUTE], 1+,
 FORTH [EXECUTE], dup, [EXECUTE], +, ";"
 FORTH [VARIABLE], z, [BINARY], 0, [EXECUTE], align, [EXECUTE], here,
 FORTH [EXECUTE], zlen, [EXECUTE], cells,
 FORTH [EXECUTE], allot, [EXECUTE], 1, [EXECUTE], cells, [EXECUTE], /,
 FORTH [EXECUTE], z, [EXECUTE], !
FORTH fixed, [COMPILELONGHEX], 10000000, [COMPILESHORT], 10000, */, ";"
FORTH clear, [COMPILESHORTHEX], ffff, color, screen, zlen,
 FORTH [EXECUTE], z, @, zero, 0,
 FORTH [EXECUTE], pixel, !, ";"
FORTH reinit,
 FORTH [EXECUTE], xr, @, [EXECUTE], xl, @, negate, +,
 FORTH [EXECUTE], xspan, !,
 FORTH [EXECUTE], yt, @, [EXECUTE], yb, @, negate, +,
 FORTH [EXECUTE], yspan, !,
 FORTH [COMPILEWORD], ";"
FORTH init, 
 FORTH [EXECUTESHORT], -21000, [EXECUTE], fixed, nop, [EXECUTE], xl, "!",
 FORTH [EXECUTESHORT], 11000, [EXECUTE], fixed, nop, [EXECUTE], xr, "!",
 FORTH [EXECUTESHORT], 12000, [EXECUTE], fixed, nop, [EXECUTE], yt, "!",
 FORTH [EXECUTESHORT], -12000, [EXECUTE], fixed, nop, [EXECUTE], yb, "!",
 FORTH [COMPILESHORT], -2, [EXECUTE], dark, !
 FORTH [COMPILESHORT], 5000, [EXECUTE], count, !
 FORTH [COMPILESHORT], 2, [EXECUTE], level, !
 FORTH [COMPILEWORD], ";"
FORTH darker, [TEXT], n-, 2*, vframe, +, dup, w@,
 FORTH [COMPILEWORD], 0, +, drop, if,
 FORTH [EXECUTE], dark, @, swap, +w!, ";", then, drop, ";"
FORTH z@, 2*, [EXECUTE], z, @, +, dup, @, swap, 1+, @, ";"
FORTH ge2, ;# 'if' tests true (nz) if abs(n) >= 2
 FORTH [COMPILEWORD], abs, [EXECUTESHORT], -20000, [EXECUTE], fixed,
 FORTH [COMPILEWORD], +, drop, -if, 0, drop, then, ";"
FORTH fx*, [COMPILELONGHEX], 10000000, */, ";"
FORTH escaped, dup, z@, ge2, if, drop, drop, ";",
 FORTH [COMPILEWORD], then, ge2, if, drop, ";",
 FORTH [COMPILEWORD], then, z@, dup, fx*, swap, dup, fx*, +, 2/, ge2, ";"
FORTH z!, 2*, [EXECUTE], z, @, +, dup, push, 1+, !, pop, !, ";"
FORTH [EXECUTESHORT], 2, [EXECUTE], +load,
 FORTH [EXECUTESHORT], 4, [EXECUTE], +load
 FORTH [EXECUTE], ok, [EXECUTE], h
BLOCK
FORTH zlen, helper, word, returns, length, of, z, array
FORTH allot, grabs, space, at, [COMPILEWORD], here, and, returns, that, "address;", z, points, to, the, array, of, values, as, generated, by, "z**2+z0"
FORTH abs, absolute, value
FORTH fixed, convert, to, fixed, point
FORTH clear, wipes, out, the, [COMPILEWORD], z, array, and, clears, screen
FORTH reinit, sets, [COMPILEWORD], xspan, and, [COMPILEWORD], yspan
FORTH init, sets, screen, boundaries, based, on, zoom, and, pan, settings
FORTH darker, changes, pixel, color
FORTH z@, returns, complex, number, at, specified, index
FORTH ge2, checks, if, fixed-point, number, above, 2
FORTH fx*, multiplies, two, fixed-point, numbers
FORTH escaped, checks, if, complex, number, above, 2
FORTH z!, stores, complex, number, at, specified, index
BLOCK
FORTH x0, [COMPILELONGHEX], 10000000, hp, */, ;# scale to A(3,28) fixed
 FORTH [EXECUTE], xspan, @, fx*, [EXECUTE], xl, @, +, ";"
FORTH y0, [COMPILELONGHEX], 10000000, vp, */, ;# make fixed-point number
 FORTH [EXECUTE], yspan, @, fx*, negate, [EXECUTE], yt, @, +, ";"
FORTH z0, [TEXT], -a, [EXECUTE], z, [EXECUTE], @, [EXECUTE], zlen,
 FORTH [EXECUTESHORT], -2, [EXECUTE], +, [EXECUTE], +, ";"
FORTH z0!, [TEXT], n-, hp, /mod, y0, z0, 1+, !, x0, z0, !, ";"
FORTH z0@, [TEXT], -nn, [EXECUTE], z0, @, z0, 1+, @, ";"
FORTH z+c, [TEXT], n-, dup, z0!, dup, push, z@, z0@, swap, push, +,
 FORTH [COMPILEWORD], swap, pop, +, swap, pop, z!, ";"
FORTH z**2, [TEXT], n-, dup, push, z@, dup, fx*, dup, 2/, ge2, swap, ;# b**2 a
 FORTH [COMPILEWORD], if, pop, z!, ";", then, dup, fx*, dup, 2/, ge2, swap,
 FORTH [COMPILEWORD], if, pop, z!, ";", then, negate, +,
 FORTH [COMPILEWORD], pop, dup, push, z@, fx*, dup, 2/, ge2,
 FORTH [COMPILEWORD], if, pop, z!, ";", then, 2*, pop, z!, ";"
FORTH z2+c, [TEXT], n-, dup, z**2, z+c, ";"
FORTH update, [TEXT], n-, dup, escaped, if, drop, ";", then,
 FORTH [COMPILEWORD], dup, z2+c, dup, escaped, if, drop, ";", then, darker, ";"
FORTH iter,
 FORTH [EXECUTE], pixel, @, [EXECUTE], count, @, for, dup, update,
 FORTH [COMPILEWORD], 1+, [EXECUTE], hp, [EXECUTE], vp, [EXECUTE], *, mod,
 FORTH [COMPILEWORD], next, [EXECUTE], pixel, !, ";"
FORTH zoom, [TEXT], nn-,
 FORTH [EXECUTE], xr, @, [EXECUTE], xl, @, +, 2/,
 FORTH [COMPILEWORD], over, over, +, [EXECUTE], xr, !,
 FORTH [COMPILEWORD], swap, negate, +, [EXECUTE], xl, !,
 FORTH [EXECUTE], yt, @, [EXECUTE], yb, @, +, 2/,
 FORTH [COMPILEWORD], over, over, +, [EXECUTE], yt, !,
 FORTH [COMPILEWORD], swap, negate, +, [EXECUTE], yb, !,
 FORTH [COMPILEWORD], 0, [EXECUTE], xspan, !, ";" ;# force reinit
FORTH +zoom,
 FORTH [EXECUTE], level, @, 2*, [EXECUTE], level, !,
 FORTH [EXECUTE], yspan, @, [COMPILESHORT], 4, /,
 FORTH [EXECUTE], xspan, @, [COMPILESHORT], 4, /,
 FORTH [COMPILEWORD], zoom, ";"
FORTH -zoom,
 FORTH [EXECUTE], level, @, -1, +, drop, if,
 FORTH [EXECUTE], level, @, 2/, [EXECUTE], level, !,
 FORTH [EXECUTE], yspan, @,
 FORTH [EXECUTE], xspan, @,  ;# expanding by two, so add it to both sides
 FORTH [COMPILEWORD], zoom, then, ";"
BLOCK
FORTH x0, creates, real, part, of, complex, number, at, specified, index
FORTH y0, creates, imaginary, part, of, complex, number, at, specified, index
FORTH z0, returns, address, of, temporary, storage, for, z0, the, constant, value, for, this, index
FORTH z0!, generate, complex, number, z0, [TEXTALLCAPS], aka, c, of, z2+c, for, this, index
FORTH z0@, returns, current, z0, on, stack
FORTH z+c, adds, complex, number, at, specified, index, to, z0
FORTH z**2, the, square, of, complex, number, "a,", "b", is,  a**2, -, b**2, ",", 2a*b
FORTH z2+c, calculate, z**2, +, c
FORTH update, z, and, pixel, if, not, already, past, the, limit
FORTH iter, iterates, over, the, array, updating, continuously
FORTH zoom, zooms, in, or, out
FORTH +zoom, zooms, in, 2, times, closer
FORTH -zoom, zooms, out
BLOCK
FORTH left, [EXECUTE], xspan, @, [COMPILESHORT], 10, /, negate, dup,
 FORTH [EXECUTE], xl, @, +, 2/, ge2, if, drop, ";", then, dup,
 FORTH [EXECUTE], xl, +!, [EXECUTE], xr, +!, 0, [EXECUTE], xspan, !, ";"
FORTH right, [EXECUTE], xspan, @, [COMPILESHORT], 10, /, dup,
 FORTH [EXECUTE], xr, @, +, 2/, ge2, if, drop, ";", then, dup,
 FORTH [EXECUTE], xl, +!, [EXECUTE], xr, +!, 0, [EXECUTE], xspan, !, ";"
FORTH up, [EXECUTE], yspan, @, [COMPILESHORT], 10, /, dup,
 FORTH [EXECUTE], yt, @, +, 2/, ge2, if, drop, ";", then, dup,
 FORTH [EXECUTE], yt, +!, [EXECUTE], yb, +!, 0, [EXECUTE], xspan, !, ";"
FORTH down, [EXECUTE], yspan, @, [COMPILESHORT], 10, /, negate, dup,
 FORTH [EXECUTE], yb, @, +, 2/, ge2, if, drop, ";", then, dup,
 FORTH [EXECUTE], yt, +!, [EXECUTE], yb, +!, 0, [EXECUTE], xspan, !, ";"
FORTH nul, ";"
FORTH h, pad, nul, nul, accept, nul,
 FORTH [COMPILEWORD], nul, nul, nul, nul,  left, up, down, right,
 FORTH [COMPILEWORD], -zoom, nul, nul, +zoom, nul, nul, nul, nul,
 FORTH [COMPILEWORD], nul, nul, nul, nul,  nul, nul, nul, nul,
 FORTH [EXECUTESHORTHEX], 250000 [EXECUTE], ",",
 FORTH [EXECUTESHORT], 0, [EXECUTE], ","
 FORTH [EXECUTELONGHEX], 110160c, [EXECUTE], ","
 FORTH [EXECUTELONGHEX], 2b000023, [EXECUTE], ","
 FORTH [EXECUTESHORT], 0, [EXECUTE], ",",
 FORTH [EXECUTESHORT], 0, [EXECUTE], ","
 FORTH [EXECUTESHORT], 0, [EXECUTE], ","
FORTH 0., [TEXT], n-, [COMPILESHORTHEX], 18, +, emit, ";"
FORTH fx., [TEXT], n-, ;# show fixed-point as float
 FORTH [COMPILELONGHEX], -1, and, -if,
 FORTH [COMPILESHORTHEX], 23, [TEXT], -, emit, negate, then,
 FORTH [COMPILELONGHEX], 10000000, /mod, 0.,
 FORTH [COMPILESHORTHEX], 25, [TEXT], ., emit,
 FORTH [COMPILELONGHEX], 10000000, [COMPILESHORT], 5, for,
 FORTH [COMPILESHORT], 5, +, [TEXT], round, [TEXT], up,
 FORTH [COMPILESHORT], 10, /, swap,
 FORTH [COMPILEWORD], over, /mod, 0., swap, next, drop, drop, space,
 FORTH [COMPILEWORD], ";"
FORTH  ok, init, show, [EXECUTE], xspan, @, -1, +, drop, -if,
 FORTH [COMPILEWORD], reinit, clear, then, iter, keyboard,
 FORTH [COMPILEWORD], 0, [EXECUTE], vc, [EXECUTESHORT], -2, [EXECUTE], +,
 FORTH [EXECUTE], ih, [EXECUTE], *, at,
 FORTH [COMPILESHORT], 45, [TEXT], *, emit, [EXECUTE], level, @, .,
 FORTH [EXECUTE], xr, @, [EXECUTE], xl, @, +, 2/, fx.,
 FORTH [EXECUTE], yt, @, [EXECUTE], yb, @, +, 2/, fx.,
 FORTH [COMPILEWORD], ";"
;# test words
;#FORTH g, ge2, if, 1, ";", then, 0, ";"
;#FORTH f, escaped, if, 1, ";", then, 0, ";"
BLOCK
FORTH left, pans, left, 1/10, of, screen
FORTH right, pans, right
FORTH up, pans, upwards
FORTH down, pans, downwards
FORTH h, sets, up, keypad
FORTH ok, sets, the, display, and, starts, the, generator
BLOCK
