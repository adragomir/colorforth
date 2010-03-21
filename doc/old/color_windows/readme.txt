ColorForth for Windows

2003-12-15
==========

What's new
----------

* QWERTY keyboard support
Keyboard is qwerty when Scroll lock is turned off, when it's on, 
original keyboard layout is active

* Import from DLL supported
* ASCII strings
Check blocks 234-236 


2003-02-27
==========

Status
------

I did not update the archive for long time, 'cause I wanted to do two things:
- check what applications are available for ColorForth and make 'em work under this version
- write some applications myself

I'm halfway on both of these goals, so I did not want to publish any version,
but anyway I had a few inquiries, so I think it's time to make an update. 

Bugfixes
--------

Fixes related to block manipulation words, mostly caused by inability to run ColorForth at address 0,
it's necessary to ad offset. 

Some other small bugfixes.

What's new
----------

* "icons" application works now  - block 34

New Words
---------

* bload ( target-block block-count file-name -- )
  Loads <block-count> 1K blocks from specified file into memory starting from <target-block>
  Implemented in assembler as : bload fmap if dup >r copy r> unmap ;
  
  An example of using bload is in 18th block
  : extd 70 37 $64747865 bload ;
  64747865 stands for 'extd', little-endian representation. Please remember, file name limitation
  if the same as for fmap (see below)
  
* icon0 ( -- a )
  Returns address (ofset from 0 in 32-bit words) of icons block. 
  Should be used instead of "12 block"
  
* blk0  ( -- a )
  Returns address of 0th block (not 18th, but 0th). Memory between 0th and 18th block is not 
  guaranteed to be accessible
  
Changes in blocks
-----------------

* 18 block - added "extd" word, can be used as a demo for "bload" and "fmap"

* 28 block - added "swap", "over", "drop" compiled macros - these can be used in interpretation mode now

* 70 - 107 blocks - loaded "extd" file

* 110 - 112 blocks - my playground, did not have much time to make it work

New and changed files
---------------------

* blocks.bin
  Now it's size is exactly 96K . For those who find it too big, there's 64 K version
  
* 16K0.bin
  16K of zeroes - if some needs more free blocks
  
* extd, mbrt  
  Two files from www.thelma-louise.net 
  See comments for "bload" word above


2003-02-08
==========

What's new
----------
* Now it works under Windows 95 ! And still works under NT
  The problem was it too small stack and some tricks with task swapping
  
New and changed files
---------------------

* blocks.bin
  Now it's size is exactly 64K 
  
* c.cmd, r.cmd
  Renamed to c.bat, r.bat to run under Win95

* 4K0.bin
  4K of zeroes - if some needs more free blocks

* color.ico

* resource.rc

Todo
----

* Make icons demo run - it's still tied to 3000h offset      
  

2003-02-07
==========

What's new
----------

* It's now a WINDOWS subsystem application, not a console one painting on desktop 
  It was not so trivial as it seemed at first glance

* Icon

Bug fixes
---------

* Fixed crash which occured when a key with scan code below 10h was pressed

Known Issues and Todo
----------------------

* Crash under Win95 (and, probably, 98). Crashed on LoadIconA, before I added icon, crashed
  on RegisterClassExA. Have to explore more. Win95 says it's stack fault, so I added stack
  20000 bytes more, but it does not seem to have helped

* Clean up code -- some wrong ideas are buried there

* Implement block loading from files, check extended and mandelbrot blocks

* Celebrate my birthday this evening :)


2003-01-26
==========


Package contents
----------------
* COLOR.ASM 

One source file only. gen.asm merged with color.asm to facilitate debugging in Visual Studio.
boot.asm not used


* blocks.bin
* icons.bin

Blocks and icons are not linked into the executable, they are mapped into memory on startup.


* c.cmd

Command file used to compile color.exe. If you want to compile yourself, please look at 
linker options (protection for .text section is EWR)

* r.cmd

start color.exe

* BD.COM

The hacker deebugger we (me and Pavlo Mosorin, aka BOSS) wrote in 1992-94. A bit outdated,
but was useful for extracting pieces from color.com. UI compatible with Turbo Debugger,
even some documentation available. Maybe, someone will find it interesting.

One of uses can be appending zeroes to blocks.bin

* COLOR.COM

Original binary from www.colorforth.com

* color.exe

Windows console subsystem executable

* GDI32.LIB, KERNEL32.LIB, USER32.LIB

Linker expects these in current directory, included to make compiling smoother.
MASM32 assembler was used (download from www.google.com)


* readme.txt 

This file

Compatibility
-------------

Nothing too windowish was added (see "New words" below though). The idea was to make
ColorForth run under Windows. Not to make ColorForth ANSish, but to make it available for
broader audiance - many of those who do not like it just did not try it. Including me till today.
Also, in process of making it work under Windows I could see how it works inside, it was not 
interesting to me just to run it, but also participate in development a little bit.

First 10 minutes (after it was stable enough) I wanted to add support for qwerty keyboard,
but after half an hour I found Chuck's onr much more convenient. Try it and you'll like it.

Block screen editor is amazing - I was expecting smth much worse. Usability one of the best
for source editors, as to me. 

Two main problems I was fighting (not sure all places found, but I'm anxoius to put in somewhere
in the net, because weekend is almost over), are:
- Original version runs at offset 0, what is impossible when protection is on, specially under windows
- 32bit word addressing vs byte addressing

I hope it looks and behaves much like the original version.

Display
-------

Desktop draw context is used in this version, so ColorForth's screen will be painted over other windows. 
1024*768 original picture is stretched to 914*688 -- for those who keep taskbar in the bottom and
for those who keep taskbar in the right; in this case try icons do not interfere with the picture and 
vice versa. You are welcome to recompile it if you do not like layout.

Probably, in-window I/O will be added

Keyboard
--------

KEY calls GetAsyncKeyState and loops with Sleep(1) if no button was pressed.
It does not consume 100% CPU, what is good, but it's quite dirty, so probably this part will be changed. 

New words
---------

* fmap  ( fn -- a | 0 )

Maps file into memory and returns base address.
First parameter - file name - is a 32bit integer, what limits file name to 4 characters, but simplifies
implementation and does not violate minalist's approach.

So, 48 is equivalent to file name "0", 31323334h -- "4321". Those for whom this is not enough, can create
NTFS links.

Please remember that files are open for writing, so you can change contents.  

* funmap ( a -- )

Unmaps previously mapped file

These 2 functions are the only interface with Windows, so it's posible to develop and debug ColorForth
application under Windows w/o making it very Windowsish. 

* bye ( -- )

Terminate application 


Removed words
-------------
boot
warm
stop
seekf
cmdf
readf
writef

All these poing to notimpl, which actually does nothing


Changes in blocks
-----------------

* Block 32 - dump

x and y initialized with 100000h (4MB, executable base address)
Dump task does not run immediately, run either "r" or "cmp"


Caveats
-------

* Block and icon, and all memory mapped files are open for reading and writing, so everything you change
  in memory will be changed on disk, including source in blocks. So, keep backups. "Save" command is
  obsolete.
  
* Memory mapped files are not expandable, i.e. you cannot access memory beyond end of file  
  
* If you try to go beyond the last available block in editor, most probably you'll get GPF exception

* If the application cannot initialize, it exits with code 1, without displaying anything.


What would be nice to have
--------------------------

* Did anyone try to write any games in ColorForth? It seems for me that ?KEY is missing. 
  Are there other mechanisms available?
  
* The first line in color.asm says
  ;colorForth, 2001 Jul 22, Chuck Moore, Public Domain
  2001 -- 1.5 years ago. Was there any progress? Any applications?
  
  