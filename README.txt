Tested only on Mac OS X Snow Leopard (i386), on a MacBook Pro latest generation 
(I am using raw SDL keycode entries, which may be different on your machine !)
With minor modifications, I think could run on any x86 *bsd or linux system. 

Disclaimer
==========

Use this totally at your own risk ! I am not responsible for any consequences that may arise. 

Installation
============
Build and install libSDL (1.3.0):
	http://www.libsdl.org/tmp/SDL-1.3.tar.gz

Build and install SDL_image:
	http://www.libsdl.org/projects/SDL_image/release/SDL_image-1.2.10.tar.gz

Build and install SDL_ttf:
	http://www.libsdl.org/projects/SDL_ttf/release/SDL_ttf-2.0.9.tar.gz

Build and install yasm from SVN:
	svn co http://www.tortall.net/svn/yasm/trunk/yasm@2307 (tested with revision 2307)

Running
=======

Look in the makefile and change the paths to suit your needs. 

$ make clean
$ make
$ ./build/cf

Hacking
=======

Very much a work in progress. Could crash, etc

I have changed an existing Windows cf, and added my own qwerty implementation !!!

The source is in src/main.asm. I am using some macros for posix and other things, defined in ./src/inc/

I have modified the keyboard entries. Look in the pkeys array in the src/main.asm file to find out the editor keys.

colorForth ascii block tool
===========================

Requires Python >2.5 and PyParsing (http://pyparsing.wikispaces.com/).

is in tools/colorforth_block_tool

How to

./tools/colorforth_block_tool totext OkadWork.cf OkadWork.txt
Make whatever modifications in the file, and then, to get back a cf blocks file:

./tools/colorforth_block_tool tocf OkadWork.txt OkadWork.cf

To test if the function works correctly, you can do a full round trip (look in the Makefile, at the testtool task).

The tool works with all releases I tested it with, including the arrayForth releases. 

No error checking whatsoever, so make sure you don't make mistakes

TODO
====

Implement a bunch of kernel words.

Fix keyboard handling: 
	- separate drawing and actions
	- make full keyboard map

ekbd, ekbd0: used only in edit
board: used in keyboard, pad, and e
shift used in keyboard, letter, accept, accept2, acmdk, word1, decimal, hex, number3, number1, alph, first


CREDITS
=======

colorForth is made and copyrighted by Charles H. Moore - http://colorforth.com
arrayForth is copyright Green Arrays, Inc. - http://www.greenarraychips.com
