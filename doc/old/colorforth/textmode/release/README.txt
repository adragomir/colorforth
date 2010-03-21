This is a demo of the latest colorForth using a linear VESA framebuffer and
BIOS calls for floppy I/O. It includes a Mandelbrot set display application
invoked as 'mandelbrot' from the startup screen. The included ISO image
is an ISOLinux/FreeDOS boot disk which runs a modified COLOR.COM with the
Mandelbrot application at block 38, overwriting the Print, PNG, and LZ77
application code from Chuck Moore's 2001 image. The other included images have
the Mandelbrot set in blocks 64 and above.

You can download the floppy image, zipped, or a Windows installer which
creates desktop shortcuts for Bochs-2.3, QEMU 0.8.2, and VMware Player 1.0.3.
The directories it assumes are, respectively, "\Program Files\Bochs-2.3",
"\Program Files\QEMU\", and "\Program Files\VMware\VMware Player". If you
chose other locations for your emulator installations, or don't have one or
more of those, edit or delete the shortcuts as appropriate.

For the floppy image, you can "burn" it to a floppy using:
gunzip 1024x768.ima.gz
dd if=1024x768.ima of=/dev/fd0  # from any GNU/Linux system including DSL

The installer includes both screen sizes, 1024x768 and 800x600. The QEMU
and WMware shortcuts use the 1024x768, and Bochs uses 800x600. You can, of
course, edit the shortcuts or the cfbochs.bat files to change these defaults;
be aware that Bochs may not be able to provide fullscreen mode for 1024x768
and you will be missing part of the virtual screen.

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
