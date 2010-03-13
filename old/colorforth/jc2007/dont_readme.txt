What I like about colorForth

it's small and elegant; an operating system/IDE/UI in a 64-k box.

it's free, in the public domain thanks to Chuck Moore.

What I don't like about colorForth

these complaints pertain to the 2001 sources. i haven't yet found complete
sources for the 2005 release, and since i can't get the binaries themselves
to work, i can't use it. every binary i've found for download has the "cyl 1"
bug that causes the bulk of high-level code to fail to load. wtf? yes, i could
patch the binaries but they have other incompatibilities with my available 
systems too.

nothing similar to a bss segment, which is zeroed at boot and then used
for variables and string space. having and using something like this would
reduce the incidence of the "cylinder" bug, which skips loading the second
cylinder on every colorforth binary i've found on the web. what happens is that
the image contains a 1 in the 'cylinder' location; this 1 overwrites the 0
that the bootloader had, when cylinder 0 loads over the running code. then
the cylinder variable is incremented from 1 to 2, skipping cylinder 1, the
second cylinder. this wouldn't be so bad except that's the cylinder that
starts at block 18, which has the bulk of Forth source on it.

coding in machine language. ugh. BIG step backwards.if no cF assembler yet,
code the core words in the kernel using a good (open source or free) assembler.
it's not cheating, really. if you want to later, write an assembler in cF to
recompile the sources.

no built-in support for strings, another step backwards.

no built-in support for fixed-point arithmetic, another step backwards.

too many "magic numbers" in CM sources with no explanation.

labels and comments are cryptic, obviously meant for CM himself.

duplicating definitions from kernel in highlevel forth forces one to
change them in two places or more: a BAD THING.

"only one BIOS call", INT 10 to set the video mode, but is this something to
be proud of? using BIOS for disk access would make colorForth available to a
lot more people without having to roll their own low-level disk access code.

These are the problems i'm trying to address in this edition of colorForth.
