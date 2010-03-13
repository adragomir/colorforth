.PHONY: all clean

all: color16 color32

clean:
	rm -f icons
	rm -f kernel16 color16
	rm -f kernel32 color32

sources:=boot.asm gen.asm
includes:=hardware.inc huffman.inc icons.inc

icons: icons.asm
	nasm -o $@ $<

kernel16: kernel.asm $(sources) $(includes) 117h.inc
	nasm -dVIDEO=117 -o $@ $<

kernel32: kernel.asm $(sources) $(includes) 118h.inc
	nasm -dVIDEO=118 -o $@ $<

color16: kernel16 icons blocks.cf
	cat $^ >$@

color32: kernel32 icons blocks.cf
	cat $^ >$@
