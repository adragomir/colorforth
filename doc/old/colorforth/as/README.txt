This is the current focus of my (jc's) development of colorForth. Originally
a bunch of sed recipes translated the masm sources into something acceptable
to GNU's "as" assembler, but now it's gone far beyond that, with even a macro
that creates the packed words.

Some items to note.

When you 'make debug' using the fullscreen image, be aware that you'll need to
click the screen in order for colorForth to see the keystrokes. Even though
the whole screen is occupied by the colorForth logo, in actuality the console
is in the foreground as far as Windows events are concerned, until you click
somewhere in the logo.

When colorForth starts up with the default Dvorak keyboard, you need to hit
the spacebar to get it into numeric entry mode. The 'alt' key also shows
numeric digits, but those are actually just text characters of decimal digits;
you can't enter "20 load" using this extended keyboard because it treats the
"20" as a text word, which it can't find in the dictionary.

The screen refresh takes up so much time in the Bochs emulator, you will get
very slow recognition of keyhits. Take your time and try not to curse.

Dumping the character map tells a different story than that on CM's chars.html
page. He says there are 48 + 26 + 12 characters; in fact there are 48 * 2
characters, with the upper set mirroring the lower with 2 exceptions. Those
are: the upper set has the cursor in place of the space, and a block character
(all pixels filled) instead of the question mark. Past the block character
are still more characters, but not 16x24. They might be 32x24 or 32x48.
It should also be noted that the upper 48 are all different than the lower;
even the numbers and punctuation are slightly different from the lower set.
