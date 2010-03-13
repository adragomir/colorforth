This is a demo of colorForth running under the Bochs-2.3 emulator. Install that
first, downloading from bochs.sourceforge.net. This cfBochs installer assumes
you've installed it under \Program Files\Bochs-2.3\.

Because Bochs-2.3 doesn't have the same fullscreen support as the patched
Bochs (that still comes in this package but is not used by the .bat file),
this colorForth was built in 800x600 mode.

Keystroke recognition is faster than under the patched Bochs in the cfBochs
download, but still somewhat slow, so be patient. You might
try changing the IPS (emulated instructions per second) in the bochsrc.bxrc
file.

For documentation, see http://colorforth.com/ or search the web for 
"colorforth tutorial".  No coordinated effort has really yet emerged, 
but it's getting better.

Then there is always the source code! Follow the Code menu at
http://sourceforge.net/projects/colorforth/ to the CVS page, which gives
instructions for anonymous CVS access. The compilation (assembly, to be
precise) has only been tested under GNU/Cygwin under Windows XP.

The sources currently used for cfBochs are found in the jc2007/ subdirectory.
These have been modified so that the x86 "unreal" mode can be used for
BIOS services, meaning that the interrupt table and BIOS area are left alone,
and all buffers needed for I/O have been relocated to the first 64K of RAM.
Check the README under the source directory for more details.

Enjoy! -- jcATunternet.net
