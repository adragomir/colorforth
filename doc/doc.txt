# Okad 2.0 documentation #

## Windows Changes ##

The first thing you will notice on booting 2.0a is that instead of reading 90 blocks into memory at boot time, the system reads 1440. Colorforth 1.0 read from between one to eighty cylinders from a floppy with nc, cylinders from a (native) floppy, set to 5 for loading 90 blocks to memory at boot.

`block`
:    works as it always has in colorForth – converting a block number to a word address in memory.  Absolute block zero is still at absolute memory address zero.  However,  block  internally adds the value of a variable called  offset  to incoming block numbers before multiplying them by 256.  The value set in  offset  reflects the absolute memory address at which the disk image was read at boot.  Various BIOSes have forced this address to be two binary million (200000 hex as a byte address; 80000 as a word address) so that the value in  offset  is  hex 800, decimal 2048.  This affects all uses of  block  including  load  and  edit .
`offset`
:    has been added to the dictionary so it is accessible from colorForth source.

The working Forth dictionary lies immediately after the disk image, at `offset` -relative block 1440.  The dictionary is followed immediately by the initial position of **`here`** as of the start of loading block 18.

Other fixed allocations are made relative to very large block numbers and float above the point we call "block0" (200000 hex as a byte address in Native systems.)

In addition to  offset  and  homef  as described above, there are several other additions to the dcitionary:

`abort`
:     This function existed in the 1.0 kernel but did not have a "head" and was accessed via kludging.  The head now exists.  Terminates execution and goes back to keyboard processing.  Sets editor pointers if  W  addresses block 18 or higher.  Empties return stack and resets a number of cells in the  spaces  table.  Echoes a  ?.
`h ( - a)`
:     This is the name of the variable whose current value  here  reports.  Simply added the head to avoid kludging.
`aper ( - a)`
:     Again, simply adds a head to an already existing variable, the one which points to the frame buffer that is to be used for refreshing the display (frame, which is used by the drawing primitives, is not given a head at this time.)
`tic ( _ - a)`
:     is a brand new function intended for interpretive use only.  Type  tic  and hit space.  Then type another word and hit space.  tic  will search for this word and leave its address on the stack.  Intended for use with garden variety, non macro red words to help interpret register values on hardware traps.
`winver ( - tf)`
:     is a new flag exported by both Native and Windows kernels; returns both a value and CPU flags reflecting that value.  For Native systems it returns zero.  For Windows systems it returns 1.

New coding practices are required.  If you add something that depends on the native environment – such as priviliged code for I/O or CPU control register manipulation, or such as allocation of memory below block 0 or above 512 megs, you must do so in a manner that will not cause the windows version to fail.  See the definition of  env  in block 18, and the environmental dependency blocks 34 and 36 for the two systems.  At minimum make it possible to load both systems without failure; if the function won't work on both systems but has to be executed for some reason, put a no op in the native system.

Hardware traps such as accessing unmapped memory, or divide by zero, display machine state and stack dumps.  For example, writing  0 @  produces a trap since location zero is not accessible by the program:

~~~~~~~~~
    Trap/Fault:
    10006424  C0000005 00000000 00000000 1018C47B
              Windows code
    10006434  00000002 00000000

    1000643C  0001003F 00000000 00000000 00000000
    1000644C  00000000 00000000 00000000 FFFF027F
    1000645C  FFFF0000 FFFFFFFF 00000000 04910000
    1000646C  00000000 FFFF0000 01B8CF60 0176CB94
    1000647C  EE180000 07387C90 FFFF7C91 7C9106AB
    1000648C  7C9106EB 0034D054 0000C000 00000000
    1000649C  00000000 020A0014 0732CC1C 00057C91
    100064AC  07780000 01B557A8 0176CBF4 EE18BED8
    100064BC  07387C90 FFFF7C91 00000000 00000000
    100064CC  0000003B 00000023 00000023 10003F22
                                         W (7/di)
    100064DC  10015800 10004C8E 10004248 00000050
               S (si)   3 (bx)   2 (dx)   1 (cx) 
    100064EC  00000000 0012FFF0 1018C47B 0000001B
               0 (ax)   5 (bp)      IP   CS (seg)
    100064FC  00010246 0012FF90 00000023
                 flg    R (sp)

    10015800  00000000 00000000 00000000 00000000
              Data stack top

    0012FF90  100044AB 10006799 00000000 0012FFE0
              Return stack top
~~~~~~~~~

The keyboard is a faithful rendition of the Native keyboard with two major exceptions:

1.    The native system has a certain number of characters of typeahead buffer kept inside of the keyboard processor chip.  The keyboard processor is polled, not serviced by interrupt code, so when you hold down a key such as a cursor movement, you can have a fair amount of movement "queued" up in the chip when you lift your finger.  The Windows version receives keyboard events as they occur and as a result there is exactly one character of type ahead buffer.  When combined with the fast drawing speed of the Windows system, this leads to very satisfying results when navigating among blocks in the editor.
2.    Because we are ignoring the extended keyboard flags, the Windows keyboard recognizes both left and right "alt" keys if the keyboard in use has them.

New words unique to the Windows system:

**`utime ( - u)`**
:     returns the unix time in seconds since midnight UTC on Friday 1 Jan 1970.
**`endram ( - a)`**
:     returns the address of the first byte past the end of memory allocated to the application.
**`keych ( - a)`**
:     is a double cell variable.  The first cell if nonzero is a keyboard character (Windows key code) waiting to be processed by the keyboard task.  The second cell contains extended key flags which are not now relevant to us and probably never will be.
**`retain`**
:     writes the 1440 block image from memory into a file called OkadWork.cf.  This is the same file name that is read when starting the program.  If you wish to save the original, do so at some time before  retain  - the file is not open while the program is running, so it can be renamed after starting the program.  Note however that the file must exist by that name, even if zero length.

## Editor, keyboard ##

Bottom of screen:

~~~~~~~~~~~~
                                   qwer  [hex]
                                         [text/num]
stack                     current text
~~~~~~~~~~~~~

While in text entry mode of the editor, the Esc key is disabled while a word is in the current text area. The following table lists the keys, corresponding actions, a sample key sequence, and changes in the hint area. Enter or Spacebar will place something on the stack or execute if in the interpreter. They will place the word after the cursor if in text entry mode of the editor.

`a-z !@*-+;,.?/`
:     Enters text.
:     add
`0-9`
:     Enters a number. The number is written in the stack area; not in the current text area.
`-`
:     Negates current number. Negates the current number. A digit must be entered before - or it will be entered as text.
`` (grave accent)`
:     Forces text for number
`F1`
:     Toggles Hex / Decimal
`Backspace`
:     Aborts current word
`Enter / Spacebar`
:     Enters a word on the stack in the interpreter. Places the current word after the cursor in a block in editor text entry mode. Clears current text after entry.
`Esc`
:     Exits text entry mode of editor. NOTE: Current word must be entered or aborted or the key will have no effect.

## Interpreter commands ##

`bye`
:     closes coloforth
`e or edit`
:     enter editor
`find`
:     Waits for a second word to be typed then searches for the first occurrence of that word starting from block 18. Find will enter the editor after execution if the word is found. If the word is not found it will stay in the interpreter. Does not find literals.
`literal (n)`
:     Same as find but looks for n  as a yellow or green number in either hex or decimal, matching the value and not the representation in either radix
`def`
:     Same as find but looks for a (red word) / definition.
`from (n)`
:     Same as find but start at block n ie n from
`c`
:     Clears stack

## Editor commands ##

S (Shift Lock) comment
:    Enter comment (white) in capitols 
  C (Capitalize) comment
:    Enter comment (white) first letter capitalized
  t (text) comment
:    Enter comment (white) all lower case
  y (yellow) Interpreter
:    Executes when encountered.  Gets executed during compilation if used inside a definition.
  r (red) Word
:    Word definition.  After the word is completed the color switches to green to enter instructions that the word defines.
  g (green) Default compilation.
:    Call Forth words or execute macros.
  x is really an *
:    Toggles between an even (source) block and odd (shadow) block.  This is normally used to document the associated text.
  c (change)
:    Changes color of word that follows cursor.
  d (definition)
:    Finds the (red word) / definition of the (green word) / call that is following the cursor.
  f ( find)
:    Finds the next word color combination occurrence of the search that was entered from the interpreter.
  J
:    Toggles between current block and the last block.  To write a block to the last block, enter <nn> edit from the interpretive mode.  <nn> is the number of the block to be edited. The <nn> block will be the last block and will be toggled with the current block.
  l (left)
:    Moves cursor left one word
  u (up)
:    Moves cursor up. ( left eight words )
  d (down)
:    Moves cursor down. (right eight words)
  r (right)	
:    Moves cursor right one word.
  k (copy)
:    Copies word to the left of the cursor and places on the stack.
  - (down block)
:    Moves down one block of memory
  m (magenta) variable
:    Variable name must fit in one cell.  Four characters will always fit in one cell.  The maximum length is seven characters but this depends on Shannon code.
  c (cyan)
:    Always compile.  Forces a call to a word.
  + (up block)
:    Moves up one block of memory.
  x (cut)
:    Deletes word to left of cursor and places on the stack.
  .	
:    Returns to interpreter.
  i (insert)
:    Pops words off of stack and places to at cursor. 
  Esc	
:    Returns to the editor control panel.

##	Search utility ##

This utility, integrated with the editor, facilitates searching the source in several practical and useful ways. When in the interpreter, there are four words for starting a search, and one for continuing it. The words for starting the search are find def from and literal . When a search is started, we begin at block 18 (except in the case of from ) and scan forward looking for the particular target. If it is not found, we remain silently in the interpreter. If it is found, we enter the editor with the cursor set at the target. Once a search has been started, it may be continued from the point of the last target found by using the "f" control panel key, if in the editor, or by typing f if in the interpreter. In either case a find will display a block, while a failure to find will remain silently in whatever environment (editor or interpreter) the continuation was requested.
The searches continue until the end of nc cylinders. For the interpretive f below, searches continue from the point of last find in that same search. The continuation point is the current cursor position when the editor’s "f" key is used.
The searches automatically consider only blocks of the same sort (source or shadow) as the block in which the search was started or continued. Thus, all searches start with source only; if they are continued using the interpretive f or if the editor cursor is never changed before using the editor’s "f" key, shadows will never be considered in that search. To search shadows, begin a search, ignore the first find (or no find), edit block 19, and then continue the search with the editor’s "f" key.

`find`
:     awaits a word from the keyboard and starts a new search for that word.  It will find all instances of that word as a definition (red or magenta), reference (green, yellow or cyan), or any sort of comment.
`def`
:     awaits a word from the keyboard and starts a new search for that word, as a definition (red or magenta) only.
`from (n)`
:     awaits a word from the keyboard and starts a new search for that word, in any form as in  find  but starting in block  n  instead of block 18.
`literal (n)`
:     starts a new search for the value n as a yellow or green number in either hex or decimal, matching the value and not the representation in either radix.
`f`
:     continues the most recently started search immediately after the point at which the last target was found in that search.  Due to the new dichotomy between source and shadows, this word is of limited usefulness.

When in the editor, two keys activate word search functions without leaving the editor:

**`"f"`**
:    control panel key continues the most recently begun search, beginning after the current editor cursor position.  If the current block is source, only source will be searched; if it is a shadow, only shadows.
 **`"d"`**
:    control panel key starts a new search based on the word immediately preceding the cursor.
    *	If the word is a definition (red or magenta) then the new search will be for references (green, yellow, or cyan) to that word.
    *	If the word is a reference or a comment, then the new search will be for definitions of that word.
    *	If the word is a literal number, then we do not search as such but simply edit the block by that number.

## Reading and Writing ##
On activating ColorForth2, the disk image file named OkadWork.cf is read into memory for 1440 blocks, and the file is closed.  From then on you are working with a RAM image of virtual disk.  The actual file is only updated when you explicitly say to do so.
In the native system, this is done by typing floppy archive .  On the Windows system it is done by typing retain .
Disk operations have been adjusted to give equivalent functionality on both native and Windows systems.  The following resident operations now exist on both platforms:

`save`
:     writes the active disk image ( nc  cylinders at block 0) to (native) floppy or (Windows) OkadWork.cf file.  This includes boot, native kernel, icon table and source code.
`wrtboot`
:     writes the boot and kernel image from block 0 to a native floppy, as an economical way to update the native kernel on floppy after executing  ati  or  nvidia  in the  floppy  utility.  It does nothing on Windows systems.
`!back ( n)`
:     writes nc cylinders to a (native) floppy or (Windows) OkadBack.cf file from RAM block  n .
`@back ( n)`
:     reads nc cylinders from a (native) floppy or (Windows) OkadBack.cf file into RAM at block  n .
`@cyls ( a c n)`
:     reads  n  cylinders to cell address  a  from cylinder  c  on backup.  For native, the read is direct from floppy.  For Windows, the whole OkadBack.cf  file is read into RAM at block 3000 and then the cylinder(s) in question are copied from there.
`readme`
:     reads the two cylinders at  cfuse  from the same cylinders on backup medium, using  @cyls .
`tapeout`
:     copies two cylinders of tapeout configuration from  cftape  to  cfuse  within the working RAM image.


## Colorforth vocabulary ##
The following is a description of colorForth words.  The stack effect for each word is placed to the right of the word name. 

The stack effect notation is as follows: 
* **"("** begins the stack effect and **")"** ends the stack effect.
* Variables to the left of the "-" are consumed from the stack and variables to the right of the "-" are placed on the stack by the word being described.
* If there is no "-" preceding the variables they are consumed by the word.
* If there is a "-" preceding the variables they are produced by the word.
* As with the variables, flags that are set by words precede "-" and flags that are used by the words are placed after the "-". 
* Flags, however do not reside on the stack.  Therefore they may not be in a deterministic state unless changed by the previous command. For this reason flags are represented in italics. 

N
:     Number 
A
:     Cell address
Ba
:     Byte address
M
:     Multiplier
Q
:     Quotient
D
:     Divisor
B
:     Block number
X
:     Non specified 1 cell of memory
R
:     Remainder
C
:     Cylinder or character
F
:     Flag
ZF
:     Zero flag
OF
:     Overflow flag
CF
:     Carry flag
SF
:     Sign flag

**`! (a)							Macro`**
:    Store word at address.

**`! (a)`**
:    Store word at address.

**`!back (b)`**
:    Writes nc cylinders to a (native) floppy or (Windows) OkadBack.cf file from RAM block b.

**`* (n1 n2 – n3 CF OF)				Macro`**
:    32 bit product.

**`* (n1 n2 – n3 CF OF)`**
:    32 bit product

**`*/ (m n d – q CF OF ZF SF	)		Macro`**
:    64 bit product then quotient.

**`*/ (m n d – q CF OF ZF SF)`**
:    64 bit product then quotient.

**`+ (n1 n2 – n3 CF OF ZF SF) 		Macro`**
:    32 bit sum

**`+ (n1 n2 – n3 CF OF ZF SF)`**
:    32 bit sum

**`+! (n a - CF OF ZF SF)			Macro`**
:    Adds "n" to the addressed variable.

**`+at (n1 n2 - CF OF ZF SF)		Kernel`**
:    Adds two sixteen bit numbers from the stack to the variable xy.

**`– (n1 – n2)`**
:    Ones complement.  Not.

**`–if (SF - SF)					Macro`**
:    Jumps if previous statement sets sign flag to 0.  Sign flag is not reset or consumed.

**`–offset (n1 – n2 CF OF ZF SF)`**
:    Fetches offset and subtracts from n1.

**`, (n)						Kernel`**
:    Lays down 4 bytes of the value given in the dictionary.  

**`. (n)							Kernel`**
:    Formats a signed number in decimal with leading 0’s suppressed. 

**`;							Macro`**
:    Exit from a definition.  If immediately preceded by a call turns it into a direct jump.  Otherwise compiles a RET.

**`/ (n d – q CF OF ZF SF)			Macro`**
:    Divide and do not return remainder.

**`/ (n d – q CF OF ZF SF)`**
:    Divide and do not return remainder.

**`/mod (n d – r q CF OF ZF SF)		Macro`**
:    Divide places the quotient at the top of the stack and remainder in the next entry.

**`? (- SF ZF)					Macro`**
:    Test bits and set flags but only for literals. Does a bitwise and without changing data.

**`?dup (n1 – n1 | n1 n1)			Macro`**
:    If h (see h) indicates only one byte exists at List, and if that byte is a drop, then h is decremented (List is not changed) and nothing is compiled.  Otherwise we compile a dup.

**`?lit (-ZF(0) | n ZF(1))			Kernel`**
:    Looks at preceding instructions to see if there is a literal to unravel.  If one is found, it is returned with ZF flag set to 1.  Otherwise returns zero flag set to 0 and no value.  If preceded by DUP literal then kills both dup and the literal; if there is just a literal, it kills the literal and inserts a DROP. 
NOTE: only macros.

**`@ (a)						Macro`**
:    Fetch value at address.

**`@ (a)`**
:    Fetch value at address.

**`@back (b)`**
:    Reads nc cylinders from a (native) floppy or (Windows) OkadBack.cf file into RAM at block  b

**`@cyls (a c n)`**
:    Same as reads.  Reads n cylinders to cell address  a  from cylinder  c  on backup.  For native, the read is direct from floppy.  For Windows, the whole OkadBack.cf  file is read into RAM at block 3000 and then the cylinder(s) in question are copied from there.

**`0							Macro`**
:    Identical to number 0.  Uses XOR to produce value at top of stack.

**`1, (n)						Kernel`**
:    Lays down 1 byte of the value given in the dictionary in little endian format. 

**`1word (-n)`**
:    Returns first cell of word.

**`2*	(n1 – n2 SF ZF)				Macro`**
:    Shift left 1 bit.  Multiply by two.

**`2, (n)						Kernel`**
:    Lays down 2 bytes of the value given in the dictionary in little endian format.  

**`2/ (n1 – n2 SF ZF CF)			Macro`**
:    Shift right 1 bit.  Divide by 2.

**`2/ (n1 – n2 SF ZF CF)`**
:    Shift right 1 bit.  Divide by 2.

**`2emit (n)						Kernel`**
:    Draws a 2x size character.  Unlike emit there is no check for carriage return.

**`3, (n)						Kernel`**
:    Lays down 3 byte of the value given in the dictionary in little endian format.  

**`5*`**
:    Emit letters.

**`7pop							Macro`**
:    Pop W (EDI).

**`7push						Macro`**
:    Push W (EDI)

**`a							Macro`**
:    Moves value at register 2 to register 0.  EDX to EAX.

**`a!							Macro`**
:    Moves value at register 0 to register 2.  EAX to EDX

**`a, `**
:    Compile word address.

**`abs (n1 – n2)`**
:    Returns absolute value.

**`abort						Kernel`**
:    Terminates execution and goes back to keyboard processing.  Sets editor pointers if  W addresses block 18 or higher.  Empties return stack and resets a number of cells in the spaces list.  Echoes a?.

**`accept						Kernel`**
:    Sets the keyboard for beginning of an alpha string and begins accepting it.  If first key is not a shift, "first" changes active shift table to the next one defined in memory; accepts a new Shannon coded string; does what is appropriate with it in aword ; and accepts the next string.

**`align						Macro`**
:    Aligns next call to end on word boundry.

**`altfrm`**
:    Byte address of alternate frame.

**`and (n1 n2 – n3 ZF SF)			Macro`**
:    Bitwise and. Zero flag and sign flag are set accordingly.

**`aper (-a)						Kernel`**
:    Byte address of aperture used for screen refresh

**`at (n1 n2)					Kernel`**
:    Moves two sixteen bit numbers from the stack to the xy location in memory.   Sets cursor location to given pixel coordinates.

**`b! (n ba)						Macro`**
:    Store n in literal byte address ba.

**`beep`**
:    Return;

**`black`**
:    Black foreground

**`blk 							Kernel`**
:    Variable used for current block number being edited. 

**`blks (n1 – n2)`**
:    Multiplies number by 256.   256 is the number of words/block.

**`block (n – a)`**
:    Returns word address of a given block number.

**`blue`**
:    Blue. Foreground.

**`boot							Kernel`**
:    Pulls the CPU reset line using the keyboard processor output port pin attached to same.

**`box	(n1 n2)					Kernel`**
:    Draws a solid box from upper left corner (cursor position xy) to lower right corner just inside absolute position limx limy limiting limx limy to screen size before start of arithmetic.

**`bswap (n1 – n2)				Macro`**
:    Swap byts.

**`buffer						Kernel`**
:    Variable used as a four byte memory buffer.

**`bye							Kernel`**
:    Terminates colorForth in an orderly fashion so that it may be called from and return to a BAT file.

**`c							Kernel`**
:    Empties data stack for keyboard task ONLY - first cell to be stored into the stack goes at keyd.

**`cbg`**
:    Change to big green.  Toggle decimal to hex.

**`cbw`**
:    Command block wrapper.

**`cby`**
:    Change to big yellow.  Toggle decimal to hex.

**`cad (- a)`**
:    Adds 3 to blk (current block).  Kernel hook to cursor address variable.

**`cf`**
:    Displays double size colorForth.

**`change`**
:    Change color of word behind cursor.  See section 3 colorforth human interface.

**`clog								Kernel`**
:    Closes the file.

**`color (n)							Kernel`**
:    Sets foreground for the given pixel value.

**`comment`**
:    Find white word in OKAD.  See section 3 colorforth human interface.

**`copy (n) 							Kernel`**
:    Copies the current block and shadow to block to the given block number, n, leaving the editor set to the destination block.  It will not copy anything to a block number less than 12.    

**` cpoint							Kernel`**
:    Variable is a vector for post 18 LOAD cursor correction on abort.

**`curs 							Kernel`**
:    Cursor position (absolute cell address).

**`cr 								Kernel`**
:    Invokes a "carriage return" by moving xy down ih pixels and left to l.

**`csg`**
:    Change word behind cursor to small green.

**`csw (a n f)`**
:    Command status wrapper.  Increment cbw tag.

**`csy`**
:    Change word behind cursor to small yellow.

**`ctg`**
:    Change word behind cursor to green.

**`ctt`**
:    Change word behind cursor to text (white).

**`cty`**
:    Change word behind cursor to yellow.

**`debug 							Kernel`**
:    Sets cursor position to …Then displays four numbers:  Keyboard task saved return stack pointer, top value on return stack, value of scr, and the running task's data stack pointer.

**`def`**
:    Find definition.  See section 3 Colorforth human interface.

**`device (n - a)						Kernel`**
:    Device searches for a PCI device class whose number is aligned in the high octet of the argument, returning the base of its configuration registers.  It searches downward starting at the literal address for literal number of steps, indexing device number field.  If no match, returns the last device that was examined.  This is why not finding the display often crashes the machine.

**`digit (n)							Kernel`**
:    Displays a digit value.

**`drop (n)							Kernel`**
:    (LODS instruction) Removes the top entry of the stack

**`dup (n1 – n1 n1)					Kernel`**
:    Duplicates the number on the top of the stack.

**`dup (n1 – n1 n1)`**
:    Duplicates the number on the top of the stack

**`e (n)							Kernel`**
:    Starts editor in block n.  If the block number is not provided the editor starts in block 18. 

**`edit (n)							Kernel`**
:    Starts editor in block n.  If the block number is not provided the editor starts in block 18. 

**`ekt 								Kernel`**
:    Variable containing vectors for editor keys beginning with null and the shift keys.  Then follows right hand top, middle, bottom rows, and left hand top, middle, bottom rows.

**`emit (n)							Kernel`**
:    Emit draws the character code given at the position xy using 16x24 patterns in icons with fore pixel value, updating  xy .  It checks for carriage return before start.

**`empt								Kernel`**
:    Restores the forget point in mk and sets class zero.

**`empty`**
:    Empties dictionary and displays logo.

**`end (a)							Macro`**
:    Jump to begin.

**`erase (b n)						Kernel`**
:    Stores zeroes into a string of blocks starting at b for n consecutive blocks.  Shadows are included

**`f`**
:    Find next occurrence after a find is executed.  See section 3 Colorforth human interface. 

**`fall`**
:    Find all words regardless of color.  See section 3 Colorforth human interface.

**`fifo (n)							Kernel`**
:    Synonym for drop.  Removes top stack entry.

**`fill (n1 a n2)`**
:    Writes n1 into a cell string address. Writes the third stack entry into the address that is in the second stack entry.

**`find`**
:    Finds a forth (green) word explained in cF user interface.  Following short compiled word blocks 18 through number of cylinders searched for a 32 bit match that means 1st 4 bytes of name.

**`fk`**
:    Key in edit keyboard.  Drops key and block number.

**`fkc`**
:    Find def of word to left of cursor.

**`format (a c – a c)					Kernel`**
:    formatf  is given cell address a of a format command under a cylinder number, with a cylinder's worth of format data in  buffer . Sets DMA for write from the buffer, sets cylinder in command and ensures motor running, executes the format command, then resets DMA for a cylinder write.
This is a noop in Windows.  The reason is explained in section TBD.

**`forth 							Kernel`**
:    Sets state of system so that subsequent definitions are regular forth definitions and can be used by the interpreter unlike macros.
Forth sets aDEFINE to forthd. The definition behavior is to add an entry to the forth dictionary as appropriate.  The encoded name is taken from the cell preceding the cell address in W.  The value for the definition is the contents of H.  Last is left holding the cell address of the dictionary pointer table just filled.  List is set to the address past the call to this function. lit is reset to adup.  Finally if class is nonzero, we jump off to it rather than returning.  On normal exit, 1 is bytadr of new name in dictionary, 2 is celladr of value field.

**`for 							Macro`**
:    Push count onto the return stack begin. 

**`fov								Kernel`**
:    fov is an abstract scale factor used in some applications.

**`freeze							Kernel`**
:    freeze  is a simple graphic process.  Usage is: : defn ...  freeze <thingstodo> ; The graphic task will endlessly do <thingstodo>. screen is the address of the thingstodo.

**`from (n)`**
:    From is the same as find but searches from a specific block number n.  See section 3 colorforth human interface.

**`graphic 							Kernel `**
:    Graphic is a no-op that is called from show.  It looks as though either it is never used or show would have to be patched.

**`green`**
:    Green foreground

**`h 								Kernel`**
:     Variable containing byte address of next available location in the dictionary.   here returns this address.

**`h. (n)							Kernel`**
:    Displays an 8 digit hex number.     

**`h.n (n1 n2)						Kernel`**
:    Displays the low order w hex digits of a number.

**`here (a) 						Kernel`**
:    Returns location counter H without destroying list.

**`hsvv 							Kernel`**
:    hsvv is stuffed from colorForth code with byte addresses and values by hardsim.

**`i								Macro`**
:    Copy loop index to data stack

**`if (ZF - ZF)						Macro`**
:    Jumps if previous statement is 0 or equal.  If does not reset or consume the zero flag.

**`jump (n)							Kernel`**
:    Jump implements a transfer vector.  Usage: i jump zero one two three.... Indexes the ith call and makes a jump to the routine in question.  Jump exits the calling definition as well.   

**`keyboard 						Kernel`**
:    Displays keyboard mapping in lower right of display.

**`key?`**
:    Exits calling definition if key is struck.

**`last							Kernel`**
:    ?  last  0 ,   ( must follow H till patch in 150 fixed)

**`less (n1 n2 – n1 n2 ZF) 			Kernel`**
:    Execution time behavior for  < .

**`line	(n1 n2)					Kernel`**
:    Line draws a line of fore pixels, n2 pixels long, starting at the cursor position xy adjusted to the left by 2 n1 pixels. Line advances the cursor downward one pixel afterward.

**`lit (n)`**
:    Finds compiled literal.  See section 3 Colorforth human interface.

**`lm (n)							Kernel`**
:    Lm sets the left margin.   

**`load (b)							Kernel`**
:    Sets the interpreter pointer to the start of the given block and begins interpreting it.

**`loads (b n)`**
:    Load block b and n successive blocks. NOTE: This will load successive executable blocks shadows are not counted (ie 2 96 loads will load blocks 96 and 98).

**`logo`**
:    Displays colorForth logo.

**`loop							Macro`**
:    repeat

**`macro 							Kernel`**
:    Sets state of system so that subsequent definitions are regular macro definitions and are not executable from the interpreter as forth commands are.
Forth sets aDEFINE to macrod. The definition behavior is to add an entry to the macro dictionary as appropriate.  The encoded name is taken from the cell preceding the celladdr in W.  The value for the definition is the contents of H.  last is left holding the cell address of the dictionary pointer table just filled.  List is set to the address past the call to this function. lit is reset to adup.  Finally if class is nonzero, we jump off to it rather than returning.  On normal exit, 1 is bytadr of new name in dictionary, 2 is the cell address of value field.

**`mark 							Kernel`**
:    Saves a forget point in mk - #macros, #forths, and H.

**`max	(n1 n2 – n)`**
:    Returns maximum value.

**`min	(n1 n2 – n)`**
:    Returns minimum value

**`minute`**
:    Minutes past midnight.

**`mod (n d – r)						Macro`**
:    Divide but only return remainder

**`move (a1 a2 n)`**
:    Copies contents of address a1, which is the third entry in the stack, into address a2, which is in the second entry in stack. 

**`negate (n1 – n2)`**
:    Negates value 2’s complement.

**`next (a – SF OF ZF)			Macro`**
:    Decrement count.  Jump if not zero to for. Pop return stack. 

**` nc 						Kernel`**
:    Variable containing number of cylinders.

**`nip (n1 n2 n3 – n1 n3)			Macro`**
:    Removes the second entry in the stack.

**`nop							Macro`**
:    No operation

**`null`**
:    Return.  Useful as null jump word.

**`octant (p q - p q n)			Kernel`**
:    ?

**` offset						Kernel`**
:    Offset is a block number added to all regular uses of BLOCK.

**`ok`**
:    Start register display.

**`olog						Kernel`**
:    Olog opens a file by the name OkadSim.log which will be created if not there, and which will be truncated if it exists.  Due to new clib extra argument, if it is created it will probably be read only.  

**`or (n1 n2 – n3 OF CF ZF SF)		Macro`**
:    or is an exclusive or,  xor. OF and CF are always set to 0.  ZF and SF are set according to the result.

**`or! (n1 a - OF CF ZF SF)		Macro`**
:    Bitwise or with location in memory.  OF and CF are always set to 0.  ZF and SF are set according to the result.

**`over (n1 n2 n3 – n1 n2 n3 n2)	Macro`**
:    Copies 2nd entry in stack to top of stack.

**`p@							Macro`**
:    Read register.

**`pad 						Kernel `**
:    pad is Chuck's best redesign of the control pad definition mechanism as of the time he stopped maintaining the kernel.

**`pause						Kernel`**
:    Gives up the machine.  Saved state for current task is who called pause, data stack pointer on return stack, and R saved in the ROUND table.

**`pci (a – n)					Kernel `**
:    Pci fetches a cell from PCI configuration space given address.  Uses Config Mechanism 1 as defined by PCI Local Bus Spec 2.0     

**`pop (-n) 					Macro`**
:    Pops return stack to top of data stack.

**`push (n)						Macro`**
:    Pushes top of data stack on to top of return stack

**`rback (b – n)					Kernel`**
:    Reads OkadBack.cf to RAM block b for n blocks.

**`reclr`**
:    Recolor cycles yellow green white

**`read (c a - `c `a)			Kernel`**
:    Reads the given cylinder into the given CELL address, returning both incremented for the next cylinder.

**`reads (a c n)`**
:    Reads  n  cylinders to cell address  a  from cylinder  c  on backup.  For native, the read is direct from floppy.  For Windows, the whole  OkadBack.cf  file is read into RAM at block 3000 and then the cylinder(s) in question are copied from there.

**`red`**
:    Red foreground

**`rm (n)						Kernel`**
:    Sets right margin.

**`ruu`**
:    Boot.

**`save`**
:    Write entire image. Save all changes made during current OKAD session.  When OKAD is restarted the changes will be present.
	save writes the active disk image ( nc  cylinders at block 0) to (native) floppy or (Windows) OkadWork.cf file.  This includes boot, native kernel, icon table and source code.

**`screen`**
:    Produces a 1024x768 screen of current color.

**`serve 						Kernel`**
:    Serve is a simple receive process, endlessly performing PAUSE <thingstodo>  where the latter is in  receive. 

**`sec`**
:    Seconds past midnight.

**`show						Kernel`**
:    Show  is a simple graphic process.  Usage is: : defn ...  show <thingstodo> ;  The graphic task will endlessly do <thingstodo> switchm graphic is a fixed nop; switchm refreshes screen and PAUSEs.

**`silver`**
:    Silver foreground.

**`sp							Kernel`**
:    sp is the vector table for token types while interpreting.  It is initialized later; some cells stuffed as state changes during compile/interpret; and these are reset on abort.	 

**`space						Kernel`**
:    Advances cursor,  xy, by one character.   

**`swap						Macro`**
:    Exchanges top of stack with next entry.

**`stop 						Kernel`**
:    Stops the floppy motor and points trash floppy buff.

**`switch						Kernel`**
:    switch in the native system copies RAM bitmap into the graphic frame buffer.  For functional equivalence, in Windows we invalidate the entire current window and call for a complete update from our DIB.

**`t,`**
:    Compile word into workspace.

**`td (a n)`**
:    Transfer descriptor.

**`text 							Kernel `**
:    Makes word white

**`tic (–ba)						Kernel`**
:    Tic used at keyboard or interpretively only; returns byte address of a Forth word.  To use type tic and hit space.  Tic will search for this word and leave it’s address on the stack.  See section 1.

**`time (n1)						Macro`**
:    Current value of processor time stamp. Pentium cycle counter, calibrate to actual clock rate.

**`top							Kernel `**
:    Sets xy and xycr to (lm, vs).

**`trash							Kernel`**
:    ???????   LABEL trash  buffer 4* ,

**`tsim 							Kernel `**
:    tsim replaces type in the transistor simulation loop.  Sets W and obtains contact voltages.  On completion processes delta charge. 

**`u+ (n3 n2 n1 – n4 n2)				Macro`**
:    Adds top of stack to third entry.  n4 = n3 + n1.

**`unpack (w - w' c)					Kernel`**
:    unpack removes the leftmost / high order Shannon coded cF character from the packed cell given.	 

**`v+ (v1 v2 – v3)`**
:    Adds a 2 dimensional vector.  v = (x,y). 

**`vesa							Kernel`**
:    Vesa  is a mode number for INT 10 4F02.  Bit 14 means use linear flat frame buffer.  Mode number is VESA-defined (Ralf Brown):4118 is 1024x768x16m  4123 is not standard - 1600x1200x16m?? Note 411B is 1280x1024x16m  Note that DAC may be limited to 6 bits after mode set, may need to do 4F08 afterward  4118 seems to be 24 bit not 32 bit 888wadr (-a) Address of word behind cursor.

**`warm							Kernel `**
:     warm is in the dictionary; analogous to  RELOAD. 

**`wback (b n)						Kernel`**
:    wback writes RAM block b for n blocks to start of OkadBack.cf

**`white`**
:    White foreground.

**`winver (- t | f)					Kernel`**
:    winver returns 1 if windows version, 0 if native, with indicators set for  if .

**`wlog (a n1)						Kernel`**
:    wlog writes a string into the file given *byte* address and *byte* count.

**`word ( *)						Kernel`**
:    Accepts a cF word from keyboard and returns  words  cells of Shannon coded characters on the stack.         

**` write ( a c - a' c')				Kernel`**
:    Writes the given cell address to the given cyl and incrs

**`writes`**
:    Address, cylinder, cylinder count
WARNING: Do not hit any keys while floppy is being written.

**`wrtboot`**
:    Writes boot and kernel. wrtboot writes the boot and kernel image from block 0 to a native floppy, as an economical way to update the nativekernel on floppy after executing  ati  or  nvidia  in the  floppy  utility.  It does nothing on Windows systems.

**`xy  							Kernel`**
:    xy position vector for text display cursor, packed halfcell values y first x second so that fetched as a cell it looks like xy.

**`yel`**
:    Find yellow word.  See section 3 colorforth human interface.


# Blocks changes to make work the fucked qwerty #

    Block 74: 
          def ekeys + 21
          fkc ekeys + 22
    block 76 
          csy ekeys + 20
          csy ekeys + 33
    pkey called from e_1 and pad

# arrayforth.com documentation #
## colorForth tag reference ##

    Tag  Syntax Element      Color

     15  Commented Number    White
     14  Display Macro       Blue
     13  Compiler Feedback   Grey
     12  Variable            Magenta

     11  COMMENT (caps)      (White) Obsolete
     10  Comment             (White) Obsolete
      9  comment (lower)     White
      8  Interpreted Number  Yellow

      7  Compile macro call  Cyan
      6  Compile number      Green
      5  Compile big number  Green
      4  Compile forth word  Green

      3  Define forth word   Red
      2  Interp big number   Yellow
      1  Interp forth word   Yellow
      0  Word extension      (color of preceding word)

## colorForth Characters and Binary Representation ##

The following table shows four values for each of the 48 colorForth character values. Character value zero is not an actual character; instead it denotes the end of a word.

The first two columns are the Shannon coded binary value for the character. The third column shows the graphic for the character, and the fourth column is its internal numerical value when not Shannon coded. ASCII character codes are not used within colorForth; they are only used for communication with external systems.


       0 000    0      10 000 s  8    1100 000 d 16
       0 001 r  1      10 001 m  9    1100 001 v 17
       0 010 t  2      10 010 c 10    1100 010 p 18
       0 011 o  3      10 011 y 11    1100 011 b 19
       0 100 e  4      10 100 l 12    1100 100 h 20
       0 101 a  5      10 101 g 13    1100 101 x 21
       0 110 n  6      10 110 f 14    1100 110 u 22
       0 111 i  7      10 111 w 15    1100 111 q 23

    1101 000 0 24    1110 000 8 32    1111 000 ; 40
    1101 001 1 25    1110 001 9 33    1111 001 ' 41
    1101 010 2 26    1110 010 j 34    1111 010 ! 42
    1101 011 3 27    1110 011 - 35    1111 011 + 43
    1101 100 4 28    1110 100 k 36    1111 100 @ 44
    1101 101 5 29    1110 101 . 37    1111 101 * 45
    1101 110 6 30    1110 110 z 38    1111 110 , 46
    1101 111 7 31    1110 111 / 39    1111 111 ? 47

## Recent Changes ##

This document lists significant enhancements and other changes made to colorForth in each posted version. The most recent versions are shown first.

**Up to September 2009**

This is the first month in which GreenArrays, Inc. makes colorForth available publicly. Since we have done a great deal since the last public release, we will summarize major changes to the generic colorForth since then.

**Generic Changes**

* Basic Syntactic Elements: We have added blue words, which are executed when and where they are encountered during the display of a block, and grey words, which are used by the g18 compiler to record the memory address at which the grey word appears in the source when it was last compiled. The CAPITALIZED and "Camel" versions of comments have been deprecated and are obsolete. Kindly convert all such words to lowercase comments; these tags will be reused for something else in the future. To facilitate this conversion, the deprecated tags are converted to lowercase comments each time source code is compressed for storage. We have also added white numbers, so that it is now simple to comment a phrase that contains numbers.
* Source Code Compression: When a source floppy image has the variable ns at the beginning of block 18 (followed by nblk and nc ), the image is capable of being compressed. save will write it out in compressed format, identified by a negative value for the variable ns as stored. Either uncompressed or compressed images may be used as boot media or as the working file for the Windows version, and either may be read by the audit utility for code reconciliation. To save an uncompressed working image on the Windows system, use retain .
* Display of editor block number: This is now done with a watermark behind the listing. The block number is no longer carried on the stack. 

## Terms elements and concepts ##

This document contains concise definitions of key elements, terms, and concepts of colorForth. For a prose presentation refer to the tutorial documents.

Character
:    A member of the colorForth character set. 

Compilation
:    The process of interpreting a stream of color tagged words and numbers from a block in RAM. White (comment) and Blue (display functions) are ignored as are word extensions. Yellow words and numbers are interpreted immediately when encountered; for words, only the forth word list is searched. When a magenta word is encountered, a variable is added to the word list (forth must be selected) and its value is stored in the source code where the variable is defined; the current value of any active variable is visible when displaying its source. When a red word is encountered, that word is added to the forth or macro word list depending on which was selected most recently, and associated with the next available byte in the dictionary. When a green word is encountered, the macro word list is searched and if found that word is executed immediately. If not found a green word is next searched in the forth word list and if found a call to that word is compiled into the dictionary. Cyan is used to compile a call to a macro rather than to execute it. Grey words are ignored by the PC compiler but are updated by the g18 compiler to hold g18 memory addresses. Every colorForth program, except the kernel, is compiled from source when needed. No "object code" or other binary image format is saved or manipulated. 

Editor
:    The program which displays a block of colorForth source code along with a control panel for a custom keyboard interaction, and which is the main tool for maintaining the content of source code. 

Interpretation
:    The process of acting immediately by direction of words and numbers. When a number is encountered it is pushed onto the stack. When a word is encountered it is executed. This is what the Interpreter does with all input received from the keyboard. When loading a block, the interpreter processes yellow words and numbers in the same way. When other colors are encountered, they are handled as described above in Compilation. 

Number
:    A fixed point binary number in 32-bit, twos complement form. When used in the context of code or communications with g18 processors, only the low order 18 bits of a number are sent to the g18, and only 18 bits are received back. 

Tag
:    A number which is combined with the internal binary representation of a word to identify it syntactically. 

Text Entry Mode
:    A state of the system during which the keyboard is being used for entry of text, meaning words or numbers. When in this mode, one of two possible keyboard behaviors is active: QWERTY, or dvorak. Anything colorForth understands may be typed using either keyboard, but each has its own meanings for the keyboard buttons, each has a different protocol for indicating whether a word or a number is being entered, and each makes different use of the keyboard hint area of the display. 

Word
:    When referring to memory, a word is either 32 bits (on the PC platforms) or 18 bits (on the g18 computers.) When referring to colorForth words, a word is a string of one or more characters that are Shannon coded and tagged. 

## Using the colorForth QWERTY Keyboard ##

When you begin text entry mode with the QWERTY keyboard behavior selected, the hinting area displays the annunciator "qwer" and you may enter words or numbers, one at a time. The software is designed to behave as simply and naturally as feasible in the majority of cases.

After typing a word or number you may enter it using the **space bar**, or alternatively the **enter** (some may know it as carriage return) key. The keyboard normally continues in text entry mode, expecting you to enter another word or number. However, when in the Interpreter, the word you just entered may run another application which can redefine keyboard operation until you return to the Interpreter, as we'll see later.

To erase a word or number before you have *entered* it, use the **backspace** key. To exit *text entry* mode, use the **escape** key. This doesn't do anything in the Interpreter, but some applications, such as the Editor, can "call" the text entry mode and in those cases you need to indicate when you are through by using "escape".

When you begin typing a word or number, the keyboard software needs to determine which you are entering. Like classical Forth, colorForth allows you to define words that start with numeric digits, or even words that consist entirely of numeric digits. The keyboard software makes its decision with the first key you strike:

* if the key is a decimal digit 0..9 then you are entering a number; the key is interpreted as a digit, that digit is displayed on top of the stack, and a "num" annunciator is displayed in the hint area. Subsequent keystrokes must be digits and will be "shifted" into the displayed number using the current radix. The - key negates the number being entered and may be used repeatedly to toggle its sign.
* If the key is any of the other 37 colorForth characters a..z !'*+ ,-./ ;?@ then you are entering a word; the character is displayed to the left of the hint area, a Shannon coded value consisting of that character is displayed on the top of the stack, and a "text" annunciator is displayed in the hint area. Subsequent keystrokes may be any colorForth character and will be appended to the displayed word to the left of the hint area as well as being added to the Shannon coded representation on the stack. If another number appears on the stack while you are typing, this informs you that the last character you typed caused the Shannon coded value to exceed one 32-bit number. That's all right, but is important to know about when creating Forth definitions. 

The behavior above allows you to just type words and numbers naturally in the great majority of cases. There are only two situations that require you to do anything more:

* Negative Numbers: The keyboard software considers the minus sign - to be an alphabetic character when it's the first keystroke seen. Thus, simply typing -123 is results in a word, not a number. To enter a negative number, the minus sign has to be entered some time after the first digit. So you can enter the value that was desired above as 123- .
  We could have made it go the other way, but there are too many Forth words that start with a minus... beginning with the one's complement operator - ... to justify this.
* Words that start with a digit: You can force the keyboard software into text mode by using the grave accent ` key. This is typically on a button immediately to the left of 1 . After pressing it, the "text" annunciator turns on in the hint area, and the next character entered will be taken to be the first character of a word. 

### Decimal and Hexadecimal ###

There is a persistent flag indicating whether the human interface is in decimal or hexadecimal mode.

#### Decimal Mode ####

By default the human interface is in decimal mode. The stack is displayed in decimal, each digit you key into a number is shifted into it using base 10, and the only keys accepted during number entry are zero through 9 and the minus sign.

#### Switching Modes ####
You may place the human interface in hexadecimal mode by pressing the F1 key. Actually this key toggles between decimal and hexadecimal mode. The F1 key is recognized any time the hint area shows just the "qwer" annunciator, or both "qwer" and "num". It instantly sets the other mode, affecting the stack display and subsequent digit entry.

#### Hexadecimal Mode ####
When in hexadecimal mode, the stack is displayed in unsigned hex. Each digit you key into a number is shifted into it using base 16. The first digit of a hex number must be one of the decimal digits 0..9 , but once number entry has begun any of the hex digits 0..9 a..f or the minus sign may be entered. Thus, to enter the hex equivalent of 255, type 0ff . 

## The colorForth Editor ##

To invoke the editor from the interpreter, first enter a block number; let's say, 18. When you have done this, the number 18 will appear at the top of the Interpreter's stack display. Do not attempt to edit blocks 0 through 17; these contain binaries and listing binaries can cause the windows version to trap.

Now type `edit`.

You will see the source code to block 18 and the number 0018 will be displayed in a faint background watermark on the screen, indicating the block being viewed and edited. The editor control panel hint map is displayed in the lower right part of the screen.

### Operating the Editor ###

The editor is operated by pressing buttons on a control panel. All control panels in colorForth are built from the same set of 27 keys; only the assignments differ. These keys are as follow:

![colorForth keyboard](photo/keyboard.jpg)

* The four fingers of the left hand are placed on the qwerty keys "ASDF" and are moved up one row or down one row. This is the standard keyboard "home" position for the left hand, and the left hand is responsible for the keys marked red in the above illustration.
* The four fingers of the right hand are place on the qwerty keys "JKL;" and are moved up one row or down one row. This is also standard home position; keys are marked green.
* The thumb(S) press the qwerty keys "N", "Spacebar", and "Alt", marked blue. 

The editor control panel hint map, displayed in the lower right part of the screen, is as follows:

![](photo/editkeys2.jpg)

### colorForth editor commands ###

    "s"  enter white comments all in the small font (Do not use, will go away!)
    "c"  enter white comments with the first character in the small font and
        the rest in the big font (Do not use, will go away!)
    "t" enter white comment text in lower case only.
    "y" enter yellow text.
    "r" enter red word, then switch to green text.
    "g" enter green text.
    "x" toggle odd/even source/shadow code/comment blocks
     
    "c" cycle color white-yellow-green word before cursor
    "d" Start find-function based on word before cursor.  If red or magenta, find reference.

        If a number, edit that block.  Otherwise, find definition.
    "f" repeat last 'find'
    "j" jump to last edited block
    "l" move cursor left
    "u" move cursor up (eight left)
    "d" move cursor down (eight right)
    "r" move cursor right
     
    "a" enter grey address word
    "b" enter blue word
    "k" copy (similar to "x" cut but does not delete the word)
    "-" decrement edited block number by two
    "m" enter magenta text (variables)
    "c" enter cyan text
    "+" increment edited block number by two
     
    "X" delete the current word (cut for paste)
    "." exit editor to Interpreter
    "i" insert word (paste from buffer)

### Exiting and re-entering the Editor ###

To exit the Editor, returning control to the Interpreter, strike the space bar (. key). To return to the Editor, you may type `e` or enter a new block number and type `edit`. The editor may also be entered using the vocabulary for finding source, described later below.

### Cursor Control ###

When the editor is started for the first time the cursor will appear in the upper left corner of the block. If there is text there it will appear on top of the first character. If the cursor is moved to the right it will be placed in the first space to the right of the first word.

![colorForth cursor](photo/cursor.jpg)

The cursor resembles a pac-man character poised to munch the word to the left when the "X" (delete word) key is pressed. Strings of text can be cut by repeatedly pressing or holding the "X" key. The cursor can be moved to a different location on that block, or to a different block, and inserted with the "i" key.

Left, Up, Down, and Right keys under the fingers of the right hand move the cursor. The cursor cannot be moved above the top of the screen but it can be moved below the bottom of the screen. If the cursor is not visible hold down the Up key. It should always appear eventually.

    "l" move cursor left
    "u" move cursor up (eight left)
    "d" move cursor down (eight right)
    "r" move cursor right

Note that the Up and Down keys do not make precise up and down movements and only move eight words to the left or right respectively if possible.

Words that have been deleted with the "X" key can be re-inserted with the "i" key after the cursor has been moved.

### Text Entry Modes ###

New text in any color can be inserted by pressing one of the Editor buttons that select color and enter text entry mode. To determine the current state of the keyboard, look in the hint area; if you see the editor control panel, buttons will be editor functions, whereas if you see text entry mode hints, you are entering text.

To return to the Editor from any text entry mode, use the method defined by the keyboard in use.

#### white comment entry ####

Comments will appear in white when editing and will be ignored by the compiler. Comments may contain any of the colorForth characters. Comments can be entered in three ways:

    "s" In old systems, enter CAPITALIZED white comments.
       This feature has been deprecated, please do not use it.
    "c" In old systems, enter Camelcase white comments.
       This feature has been deprecated, please do not use it.
    "t" enter white comments in lower case.

In current colorForth systems, decimal and hex numbers may be entered as comments; this facilitates the commenting of most compiled or interpretive code if desired.

When source is compressed using `save` the deprecated tags for capitalized and Camelcase comments are converted to lower case comments.

#### yellow text and number entry ####

Yellow text are words that are interpreted in colorForth when blocks are loaded. Red words are the names of new defined words, they are like the names that follow ":" in traditional Forth. Green words are words being compiled. Magenta words are variables, and cyan words are like "postponed" words in ANS Forth (calls to macros).

Numbers can be entered in green or yellow mode. Numbers can be entered in decimal or hex mode. Hex numbers will appear in a darker green or darker yellow than decimal numbers, and may be italicized when the display medium permits.

If one intends to enter a number but enters a character string that resembles a number instead, it cannot be visually distinguished from a decimal number as it will be the same shade of yellow or green. But when you load the block unless you have actually defined a red word or magenta variable with a name that can be confused for a number, which is generally not a good idea in the first place, you will get a compiler error message that it did not recognize the string (that looked like a number) as a defined name.

There is a Forth wordlist and a macro wordlist in the colorForth compiler and the macro wordlist is searched first during compilation. Words in the macro wordlist act like "immediate" word in traditional Forth. Macros are executed at compile time when referenced in green. To compile a call to a macro, rather than to execute it at compile time, reference that macro with a cyan word.

#### green text and number entry ####

Green words like if are compiled, but because they are in the macro wordlist and act like immediate words, they are executed at compile time like words written explicitly in yellow.

A transition from green words to yellow words and back to green words is interpreted by the compiler as follows: The transition from green to yellow marks the transition from compilation mode to interpretation mode as would be indicated by the ANS Forth word "[" and the transition from yellow to green is interpreted by the compiler to mean a transition from interpretation mode to compile mode, and the compilation of a literal as would be indicated by the ANS Forth phrase "] LITERAL"

#### red name entry ####

When the "r" key is pressed to enter a red word the text for the red name will be aligned on the left side of the screen. A red name is the name of the compiled code that follows it in green. The red name can then be used in green, yellow, cyan or white colors. A call or jump to the word can be compiled with green usage, the word can be interpreted with yellow usage, a defined macro can be postponed (compiled as a call, rather than executed at compile time) with cyan usage, and if written in white will always be a comment.

After entering a red word the editor will change the color of the text or numbers being entered into green because the red word is the name of the compiled, green code that follows.

#### magenta variable name entry ####

*Magenta words are not used in G18 code. This section applies only to programming the host computer.*

When selecting magenta, and a magenta variable name is added to a block, a green number with the value 0 will be added after the magenta name. The magenta word when executed places the address of the location of the green number on that block on the parameter stack. One interesting feature of colorForth is that this address is in fact the address of a cell in the source, specifically the cell from which the green zero is being displayed. The variable, and its value, are part of the source and when you save the source you also save the values of variables! Display a block containing a variable and change the variable using the interpreter. You will see the green number in the source display change immediately! Note: colorForth has historically required that references to variables must always be in yellow; the code compiled for a green variable reference is incorrect. We have not gotten around to fixing this yet.

#### cyan entry ####

Cyan words are not used in G18 code. This section applies only to programming the host computer.

Cyan words are like postponed words in ANS Forth. A macro is executed immediately by the compiler when encountered as a green word. When a macro is referenced as a cyan word, the compiler does not execute the macro but compiles a call to it.

#### blue entry ####

Blue words are words that are executed at edit time. They can format the display with cr br indent etc. or execute custom commands while viewing a screen. The word seeb on block 648 is "see blue" and when yellow will execute at boot time and make blue words visible in the editor. If seeb on block 648 is white then blue words will not be visible but will execute when a block is displayed. After the system has booted, you may execute seeb at any time to toggle the display of blue words on or off.

#### grey entry ####

Grey words display an address wherever desired in G18 code. Any short word (four characters or less) may be used; the word itself does not matter nor is it ever displayed in its own right.

### Other Editor Functions and Related Tools ###

#### Cut, Copy and Paste ####

The Editor has a buffer of finite size (about a block's worth) for holding code that has been "cut" or "copied" for later "pasting". The buffer works like a stack. The buttons that control this are:

    "k" copy (similar to "x" cut but does not delete the word)
    "X" delete the current word (cut for paste)
    "i" insert word (paste from buffer)

Each depression of the cut or copy button pushes a copy of the word or number to the left of the cursor into this stack buffer, leaving the cursor to the left of where the word or number is (or was before it was deleted.) Each depression of the paste button pops the top word or number from this stack buffer and inserts it at the cursor position, moving the cursor to the right.

#### Block Navigation ####

To begin editing some other arbitrary block, exit the editor and re-enter using edit as was described earlier. For local navigation the Editor supplies the following:

    "x" toggle odd/even source/shadow code/comment blocks
    "j" jump to last edited block
    "-" decrement edited block number by two
    "+" increment edited block number by two

The "j" key alternates between the last two blocks that you edited with the word edit . The traditional variable blk contains the number of the current block being edited, and is unchanged when you exit the editor. blk 1 + is the number of the block that was current the last time edit was used. In addition to alternating the displayed block, the "j" key alternates these two values in blk .

The "x" key toggles between odd and even blocks while editing. Even numbered blocks are intended for source code and odd numbered blocks, called shadows, are intended for documentation. Since odd numbered blocks are not normally compiled, red, yellow, green, cyan, or magenta words on those blocks are intended to be comments only and may be used as desired to enhance readability, with the understanding that shadow blocks are not normally loaded.

The "+" and "-" keys on the editor keyboard map move two blocks up or two blocks down. The editor will not decrement the block number to be edited below 18. More advanced navigation is supplied by the Search Utility; see below.

#### Copying Blocks ####

To copy a source block and its shadow, first display the source block that you want to copy using the editor, so that its number is in the variable blk . Then, exit the editor; put a destination source block number on the stack, and type copy . The first block, and its accompanying shadow, will be copied to the destination. If you want to edit the copied block just type e as blk will now be set to the number of the destination block.

#### Search Utility ####

A resident set of utility functions, integrated with the Editor, facilitates searching the source in several practical and useful ways. When in the Interpreter, there are four words for starting a search, and one for continuing it. The words for starting the search are find def from literal .

When a search is started, we begin at block 18 (except in the case of from ) and scan forward looking for the particular target. If it is not found, we remain silently in the Interpreter. If it is found, we enter the Editor with the cursor set at (immediately to the right of) the target.

Once a search has been started, it may be continued from the point of the last target found by using the "f" control panel key, if in the Editor, or by typing f in the Interpreter. In either case, a successful find will display a block, while a failure to find will be as though you had done nothing at all.

When continuation is begun using the word f in the Interpreter, searching resumes at the point of last find in that same search. When continuing using the "f" control panel button in the Editor, the search resumes at the current cursor position in the current block, so you have more control in this case.

The searches continue up to the end of your disk image as defined by the three variables at the start of block 18. Do not attempt to change these variables at this point in your study!

The searches automatically consider only blocks of the same sort (source or shadow) as the block in which the search was started or continued. Thus, searches normally start with source only; they will never consider shadows unless you force them to using the word from with an odd block number, or by using the Editor's "f" key while looking at a shadow. To search all shadows, start a search normally, then edit block 19 and use the Editor's "f" key to continue.

##### Interpreter Search Words #####

* `find` awaits a word from the keyboard and starts a new search for that word. It will find all instances of that word as a definition (red or magenta), reference (green, yellow, blue, or cyan), or any sort of comment.
* `def` awaits a word from the keyboard and starts a new search for that word, as a definition (red or magenta) only.
* `from` takes a number from the stack for use as starting block. It then awaits a word from the keyboard and starts a new search for that word, in any form as in find using this starting block.
* `literal` takes a number from the stack and starts a new search for that value as a green or yellow number, matching its numerical value (regardless of whether displayed as decimal or as hex.)
* `f` continues the most recently started search immediately after the point at which the last target was found in that search. 

##### Editor Search Keys #####

* "f" control panel key continues the most recently begun search, at the current editor cursor position. If the current block is source, only source will be searched; if it is a shadow, only shadows.
* "d" control panel key starts a new search based on the word immediately preceding the cursor, as follows:
    * If the word is a definition (red or magenta) then the new search will be for references (green, yellow, blue, or cyan) to that word.
    * If the word is a reference or a white comment, then the new search will be for definitions of that word.
    * If the word is a literal number, then we do not search as such but simply edit the block by that number. Handy for use in load blocks! 

All of the above work in both Native and Windows systems.

Written by Jeff Fox and friends. 

## The colorForth Human Interface ##

Your interaction with colorForth is accomplished by using a PC keyboard and a graphic display. As you will see, colorForth has unique human interfaces and we are continually experimenting with improvements to them. In fact, each colorForth application may define its own customized human interface if appropriate.

Situational awareness is very important in colorForth! The meaning of pressing a given button on the keyboard varies as your actions change the system from one state to another. With this economy of motion comes the requirement that you maintain a heightened awareness of system state from keystroke to keystroke. With only a very few exceptions, the system state and the meanings of the keyboard buttons are apparent from a glance at the screen once you are familiar with colorForth. Until you are, however, use caution as you would when learning any other new psychomotor skill! For example, quickly drumming the keys in frustration would yield only laksjdf on a plain old text editor which could easily be un-done with backspaces. However, in the colorForth editor such non-deliberate use of keys can have truly amazing, unintended consequences, and by the time the actions have been taken you may have to audit your whole disk to find out what you have wrought! We recommend that you keep cats and small children away from your keyboard when colorForth is running.

Most people find that once they have mastered this new skill it works well for them, and that with the simple but powerful set of tools in colorForth it's a pleasingly productive environment. Take your time getting accustomed to it, and it should serve you very well.

We'll start with the Interpreter interface since it's the first thing you encounter and contains good examples of the common display elements. Here is what you see on booting arrayForth:

![](photo/interp-0.png)

The above display is composed of two major elements: The logo, with the "arrayforth" label, and the interpreter's feedback which is shown in the black areas at the bottom of the screen. The annunciator reading "qwer" is part of this feedback. The feedback is comprised of several distinct components that may be used in other applications' interfaces; these components are discussed below.

The first of the two major elements, the logo, may be replaced during use of the system by other things such as the listing of a block. The interpreter simply displays this element, whatever it is, with the interpreter's feedback superimposed over it. The entire screen is refreshed, from whatever raw data each part is based on, each time you strike a key; thus, the effects of interpreter interaction may be seen right away when they occur.

### Interpreter Feedback Area ###

There are three main sections in this area; here is an example showing all three:

![](photo/interp-1.png)

The annunciators "qwer" and "text" are part of a keyboard / control panel hint rectangle at the lower right corner of the screen. The word "png" is feedback showing the word most recently typed; it changes as you key additional characters. The number 50389 shows the stack content; it is presently the only item on the stack. While you are typing a word or number, the binary value of that number or values(s) of the word are shown at the top of the stack (top is on the right.)

Here is what you see if you have pushed five numbers, one through five, onto the stack and then typed png to capture the screen:

![](photo/interp-2.png)

*Caution: The stack display presently shows the entire content of the stack, no matter how large. If the stack doesn't fit on one line, the display wraps and another line is shown. If the stack is deep enough, the system will trap while displaying the stack.*

If you type a word that the interpreter cannot find in the dictionary, a question mark is appended to the display of the most recently typed work. Here is an example resulting from trying to interpret an unknown word "poobah":

![](photo/interp-3.png)

### Keyboard / control panel hint area ###

What you see in this area depends on the application with which you are interacting. Some applications are operated by pushing buttons on a keyboard "control panel" rather than by typing text; some examples of this are the flat layout display, the hardsim viewer, softsim, and in fact even the Editor. In other contexts, the keyboard is being used for input of text (words, numbers); examples are the Interpreter, and the editor when you have selected a color for text entry. Such cases are called text entry mode.

In text entry mode, one of two possible keyboard behaviors is active: QWERTY, or dvorak. Anything colorForth understands may be typed using either keyboard, but each his its own meanings for the keyboard buttons, each has a different protocol for indicating whether a word or a number is being entered, and each makes different use of the keyboard hint area of the display.

* **The QWERTY keyboard** is as close as practical to the behavior of a conventional typewriter keyboard and will be most familiar to those who have been trained as touch typists in that system.
* **The dvorak keyboard** is a special keyboard developed by Chuck Moore to minimize the number of buttons and amount of hand movement required. Character assignments were made to facilitate the efficient typing of common words. With 24 character and three mode shift / control keys, typing of certain characters or digits requires multiple keystrokes. We call this keyboard dvorak but it differs from the commonly used Dvorak keyboard.

### Selecting QWERTY or dvorak keyboard ###

colorforth lets the user choose the historic dvorak keyboard setup for colorforth or a traditional qwerty setup. The system defaults to qwerty. To change the system to a dvorak keyboard type on the qwerty keyboard, 648 edit and begin editing block 648. Scroll down using the keys for right and down to place the cursor just after the word qwerty and press the key labeled "c" for color change. Make the phrase white instead of yellow to disable the qwerty keyboard setup at startup. Exit the editor with the spacebar and type save to save the system setup to default to dvorak keyboard use and/or type warm to restart using the dvorak keyboard setup. 

# colorforth.com webpages # 

## colorforth ##
Forth has been a recognized programming language since the 1970's. ColorForth is a redesign of this classic language for the 21st century. It also draws upon a 20-year evolution of minimal instruction-set microprocessors. Now implemented to run under Windows, it can also stand-alone without an operating system. Currently being ported to GreenArrays' c18 computer core via the Haypress Creek board. Applications are recompiled from source with a simple optimizing compiler.

### Features ###

* Stand-alone! Includes operating system.
* Compact! 2K bytes for core software.
* Fast! Optimized object code.
* Simple! Applications stored as source. No object library.
* Innovative! Text compressed and pre-parsed.
* Unique! 27-key Dvorak keyboard.

### Status ###

The latest status of colorForth and current projects. Others have been more active than I, so search the web.

### Forth ###

Distinctive for its use of 2 push-down stacks. The Return stack is used for subroutine return addresses, as usual. The Data stack holds parameters for and results of subroutine calls. This distinction between control and data minimizes the cost of subroutine calls. As a result, Forth code is typically highly factored into many tiny subroutines, called words. Referencing a word causes its code to be executed.

These simple words are easily and thoroughly tested by typing them on the command line. The Edit/Compile/Test sequence is extremely fast, boosting programmer productivity. A philosophy of early binding helps to produce efficient, reliable code.

A new word is defined by a string of previously-defined words ending with a semicolon. The only other syntax is that IF must be matched with THEN, and FOR with NEXT.

In Forth, a new word is defined by a preceeding colon, words inside a definition are compiled, outside are executed. In colorForth a new word is red, green words are compiled, yellow executed. This use of color further reduces the syntax, or punctuation, needed. It also makes explicit how the computer will interpret each word.

colorForth does not conform to the ANS Forth Standard. It is a dialect built upon the instruction set of my Forth microprocessor chips. The Pentium version implements those instructions as macros. And adds others as needed to optimize the resulting code.

### Rationale ###

Current software is shameful. Giant operating systems linger from the 1970's. Applications are team-produced with built-in obsolescence. User interfaces feature puzzle-solving.

With the huge RAM of modern computers, an operating system is no longer necessary, if it ever was. colorForth includes multi-tasking, and drivers for essential devices. But that is hardly an operating system in the style of Windows or Linux.

Megabytes of software may be required for historical compatibility, but sometimes we need a fresh start. ColorForth provides the necessary capability with kilobytes of code. At boot, it copies disk into RAM. Then compiles the macros that emulate the stack machine that Forth expects. As applications are requested, they are compiled.

A Forth application can take 1% the code employed by the same application written in C.

### Source ###

Except for a small kernal, source is all there is. Object code is recompiled as needed, which takes no discernable time. This saves untold trouble in maintaining and linking object libraries.

Rather than a string of 8-bit characters, colorForth interprets pre-parsed words. A word starts with 4 bits that indicate its color and function - text, number, etc. Then 28 bits of left-justified, Shannon-coded characters, averaging 5.2 bits each. Numbers are stored in binary. Each word occupies 1 or more 32-bit memory locations.

This pre-parsed source makes instantaneous compile possible. A special text Editor is included that understands this format. The source can be un-parsed into a bit string for compression and security.

Here is a sample of source code, the guts of the IDE hard-disk driver. Yes, that's all it takes. Below the line is a 'shadow block' of comments explaining the code.

And here is a paper describing arithmetic for the GreenArrays' c18 computer. Lots of functions implemented with colorForth instructions.

### Display ###

Source code is organized in 256-word blocks. This helps factor code into a managable heirarchy. The code in a block is analogous to that in a C file. But considerably more compact.

Blocks are numbered, some are named. They are displayed with 16x24pixel characters, arranged in a 40x24 format on a 1024x768 display. At the bottom, the contents of the Data stack, and the current word are displayed.

This display format is also used by applications. The large characters are readable and help minimize clutter. Double-size characters are available, as are graphic shapes (triangle, box, hexagon, circle), images (JPEG, GIF), 3D shapes and anything else that's been coded.

### Keyboard ###

Continuing my experiments with keyboards, I currently prefer using 27 keys to provide the 48 characters needed. These are the home keys of a standard 101-key keyboard, allowing the other 74 to be ignored.

The assignment of the keys changes, with the current one displayed on-screen at lower right. It's pleasantly easy to type while referring to the display. These keys minimize finger travel, as close to Dvorak's arrangement as 27 keys permit.

They are used as function keys (menu selects) for applications. The only text that needs to be typed is when editing source code.

Other arrangements are possible. Including, gulp, standard qwerty.

### References ###

Jeff Fox has written about Forth and colorForth. He also has videos of talks given at Forth meetings.

Glen Haydon publishes literature about Forth. He has copies of Leo Brodie's best-selling book Starting Forth.

The Forth Interest Group organizes meetings and has literature and libraries of Forth code. They are on a Webring that links to other Forth sites.

Elizabeth Rather at Forth, Inc provides commercial-grade Forth systems and applications.

Greg Bailey has information about the ANS Forth Standard and its Technical Committee J14. 

## Status ##

### Philosophy ###

My attitude about software is that it expresses ideas that cannot be owned. Attempting to assert ownership is undesirable and impossible.

So, although colorForth is infinitely valuable, I place it in the Public Domain to make it freely available to anyone for any purpose. There is plenty of money to be made by porting code, programming applications and teaching.

I am having a fine time using colorForth. I won't spend much time promoting it. This site is my attempt to gauge the market. I will rigidly control the version I use.

### Chuck ###

This is what I'm working on, priority on top.

* PPP/TCP
* Go
* Code colorForth in colorForth
* PCI Audio: send data to the D/A, get samples from the A/D

### Projects ###
Here's a list of colorForth code I'd like to have. I'll post the best code with credit to the author.

* Graphics
      * Triangle
      * Circle
      * Cube
      * Cylinder
      * Sphere
      * Modecules
      * Star map
      * Fractal tree
      * Mandlebrot set
      * Wood grain
      * Grass
      * Ray trace
* Audio
      * Play .wav
      * Record voice
      * Analyse voice - extract phonemes
      * Recognize voice
      * Identify voice
      * Synthesize voice
* Devices - Hardware interfaces require information not readily available. Intel publishes excellent specs. Others consider them proprietary. Agreeing to non-disclosure might prevent distributing source. Reverse-engineering Linux code seems the only way.
      * Various graphic accelerators
            + Frame swapping
            + Transparent characters
            + Solid rectangle
            + Solid triangle
            + Z buffer
      * Various audio accelerators
            + Play .wav
            + Record
      * Various modems
* Protocols
      * PPP connection
            + Single ISP
            + 2 blocks?
      * TCP/IP
            + 3 blocks?
      * USB
            + Keyboard
            + Camera
            + Disk
            + 2 blocks + 1/device?
      * JPEG decoder/encoder
      * PNG
            + Encoder - 3 blocks, Chuck Moore
            + Decoder
      * GIF decoder/encoder

### Platforms ###

Although I have no particular interest in other platforms, colorForth could easily be ported. The kernel that needs to be recoded is quite small. The major difficulty is gaining experience with the computer and its interfaces. These have been suggested:

* Mac
* Sun
* Playstation 2
* PDA 

## Philosophy ##

I used to love to solve puzzles: crossword, jigsaw, chess. Programming now gives me the endorphin lift of achievement. Call me a programming junkie.

I believe in objective right and wrong. It applies to computer code. There is a single right way to balance all the conflicting demands. There is a solution to the puzzle. The challenge is to find it.

colorForth is my latest attempt to define a computer environment. Satisfactory for me, the programmer, and me, the user. I've published it because I'm pleased with it. Here are some of the trade-offs:

* It elminates object code, except for a small kernel. No need to manage object libraries, link modules, relocate code or allocate memory. Of course, I haven't done that for a long time.
* It encourages small source modules, compiled on demand. Compiling these takes no perceptible time. Perhaps it's faster than needed. Perhaps it could be faster. In any case, it's satisfactory.
* It encourages 2nd-order factoring into groups of related words, via 256-word blocks of code. Hopefully, a block can be reused in a different context. But principally, it organizes and clarifies the code.
* The source code is pre-parsed into word-size chunks. This simplifies the interpreter and speeds search.
* Each word has a tag that specifies its function and appearance. These properties need not be inferred from context. Thus simplifying the interpreter, reducing syntax and clarifying intention.
* Huffman-coded characters reduce the cost of pre-parsed words. Code size is slightly smaller than a character string, even with added tags. I learned the value and limitations of this elegant loss-less compression.
* A complex Editor is the price paid for a simple interpreter. In fact, total system complexity has probably increased. But time spent by the Editor is miniscule, compared with time spent by the interpreter.
* Compiler macros provide a simple, efficient way of exploiting the hardware of a platform. They factor and document machine code, yet allow its use in a natural way. They make porting efficient software possible.

The result, to my eye, is a nicely balanced system. It's not yet perfect and will continue to evolve. I deliberately sought to make colorForth different. I'm interested in unanticipated consequences. What happens when you change the balance of code size and compile time? What can a simple system do that a complex one cannot? What performance is sufficient to challenge the established paradigm?

The Pentium platform is a bridge to my Forth chips. The code will be easy to port, or to cross-compile. I hate the Pentium, the PCI bus, the bizarre device interfaces. Yet this platform has driven hardware prices down and capability up. Focusing on software for USB and TCP/IP will render legacy hardware irrelevant.

The strange keyboard employed by colorForth is an approximation of the one I want:

* A 3-position switch for each finger
* 15 buttons with 1 hand. One hand free
* 27 buttons with 2 hands
* Buttons labeled on the screen
* Buttons reassignable
* Remote keypad(s) for my recliner
* Suitable for a wearable computer

One day I'll make one from bits of brass. Till then, qwerty will have to do.

### Legal ###

After considering your comments and doing some diligence, I've decided to place colorForth in the Public Domain. Without explicitly doing that, it would have a default copyright.

Public Domain allows anyone to use it for any purpose. Exactly my intention. The various licenses attach restrictions that are complicated and unenforcable.

My motive is simple. colorForth offers a solution to the growing software crisis. I hope to encourage an alternative to bloatware. Besides, I'm not inclined to develop, market and distribute a product. If anyone is, more power to you.

I've been here before: Forth entered the Public Domain around 1972. The issue wasn't copyright, but patent protection. No software patent had yet been issued and the effort required seemed great, so NRAO (the National Radio Astronomy Observatory) declined to pursue one. I don't recall a public statement placing Forth in the Public Domain, merely a presumption.

### Authentication ###

colorForth could be compressed for distribution. But maybe it shouldn't.

It should be signed so you know you got the real thing. In time. 

## Binding ##

### Early Binding ###
The interactive nature of Forth leads to recognition of 3 'times':

* Edit time. The programmer is editing source code.
* Compile time. Source code is being interpreted and compiled.
* Run time. Compiled code is being executed.

The earlier in this Edit/Compile/Run sequence a concept can be resolved, the more efficient the code. It is better to do arithmetic at Compile time, than repeat the calculation every Run time. It is better to factor code at Edit time than to expect a smart compiler to do so every Compile time.

### Floating-point ###

Floating-point arithmetic is a classic example of this. It is easy to implement floating-point in Forth, either hardware or software. I, for one, rarely do so.

Fixed-point add is blindingly fast, with no shifting overhead. Fixed-point multiply avoids normalize and rounding cost. Multiplying by a ratio (*/ operator) accurately rescales integers. None of these needs FIX or FLOAT operators, or has to move data to a different stack. The only drawback is the need to choose units and use them consistently. For example, my VLSI simulator uses mV, uA, fC and uK. Multiplying and dividing these takes a little thought as to scale factors. This thought is done at Edit time!

Floating-point would probably use V, A, C and K. These can be combined with formulas from some book. The computer will do the scaling, but does it at Run time! A gigaflop calculation will scale the results giga times. The is convenient and reasonable for prototyping. But insanity for production code.

To repeat: resolve units early in the Edit/Compile/Run sequence.

### Conditionals ###

Another example concerns conditional statements - IF . . . THEN. Minimize them! A conditional is obviously executed at Run time. Let the programmer make the decision at Edit time. This might reduce the range or versatility of the code, but it also reduces testing and improves speed and reliability.

An alternative is to use words like MAX, MIN, ABS that have conditionals built in. They're simple and have been thoroughly tested.

Avoid like the plague, conditional compilation - conditionals at Compile time. These lead to code extremely difficult to read. And very easy to compile incorrectly. For different situations, simply have different versions of the program. In Forth, this usually means blocks with different LOAD sequences.

### Portability ###

Don't try for platform portability. Most platform differences concern hardware interfaces. These are intrinsically different. Any attempt to make them appear the same achieves the lowest common denominator. That is, ignores the features that made the hardware attractive in the first place.

Achieve portability by factoring out code that is identical. Accept that different systems will be different.

## 1 percent ##

### 1% the code ###

This is a provocative statement. It warrants some discussion.

### C programs ###

I've studied many C programs in the course of writing device drivers for colorForth. Some manufacturers won't make documentation available, instead referring to Linux open source.

I must say that I'm appalled at the code I see. Because all this code suffers the same failings, I conclude it's not a sporadic problem. Apparently all these programmers have copied each others style and are content with the result: that complex applications require millions of lines of code. And that's not even counting the operating system required.

Sadly, that is not an undesirable result. Bloated code does not just keep programmers employed, but managers and whole companies, internationally. Compact code would be an economic disaster. Because of its savings in team size, development time, storage requirements and maintainance cost.

What's wrong with C programs?

* Some problems are intrinsic to the C language:
      * It has elaborate sytnax. Rules that are supposed to promote correctness, but merely create opportunity for error.
      * It has considerable redundancy. This increases trivial errors that can be detected. And program size.
      * It's strongly typed, with a bewildering variety of types to keep straight. More errors.
      * As an infix language, it encourages nested parentheses. Sometimes to a ludicrous extent. They must be counted and balanced.
      * It's never clear how efficiently source will be translated into machine language. Constructs are often chosen because the programmer knows they're efficient. Subroutine calls are expensive.
      * Because of the elaborate compiler, object libraries must be maintained, distributed and linked. The only documentation usually addresses this (apparantly difficult) procedure.
* Others are a matter of style:
      * Code is scattered in a vast heirarchy of files. You can't find a definition unless you already know where it is.
      * Code is indented to indicate nesting. As code is edited and processed, this cue is often lost or incorrect.
      * Sometimes a line of code contains only a parenthesis, or semicolon. This reduces the density of the code, and the difficulty of reading it.
      * There's no documentation. Except for the ubiquitous comments. These interrupt the code, further reducing density, but rarely conveying useful insight.
      * Names tend to be hyphenated. This makes them unique and displays their position in the heirarchy. The significant portion of a name is hard to detect, slow to read.
      * Constants, particularly fields within a word, are named. Even if used, the name rarely provides enough information about the function. And requires continual cross-reference to the definition.
      * Preoccupation with contingencies. In a sense it's admirable to consider all possibilities. But the ones that never occur are never even tested. For example, the only need for software reset is to recover from software problems.
      * Conditional compilation. More constants include or exclude code for particular platforms. More indentation. More difficulty fathoming which code is relevant.
      * Hooks for future enhancements, or abandoned features, are abundant. This is useful only in understanding the programmer's ambitions.
      * It is in a programmer's best interest to exaggerate the complexity of his program.
* Another difficulty is the mindset that code must be portable across platforms and compatible with earlier versions of hardware/software. This is nice, but the cost is incredible. Microsoft has based a whole industry on such compatibility.

### Forth ###

colorForth does it differently. There is no syntax, no redundancy, no typing. There are no errors that can be detected. Forth uses postfix, there are no parentheses. No indentation. Comments are deferred to the documentation. No hooks, no compatibility. Words are never hyphenated. There's no heirarchy. No files. No operating system.

Code is organized so that a block of related words fit on the screen. Names are short with a full semantic load. The definition of a word is typically 1 line. Machine code has a one-to-one correspondance with source.

An application is organized into multiple user interactions, with unique display and keypad. Each is compiled when accessed. Its code is independent, names need not be unique. A background task is always running.

### Comparison ###

Yes, I could write a better C program that those I've seen. It wouldn't be nearly as good as Forth. I can't write an assembler program as good as Forth. No, I don't think Forth is the best possible language. Yet.

But does this add up to 1% the code? Where is the C program I've recoded? No one has paid me to do that. One difficulty is comparing my Forth with the original C. I cheat. The 1% code merely starts an argument that they're not the same.

For example, my VLSI tools take a chip from conception through testing. Perhaps 500 lines of source code. Cadence, Mentor Graphics do the same, more or less. With how much source/object code? They use schematic capture, I don't. I compute transistor temperature, they don't.

But I'm game. Give me a problem with 1,000,000 lines of C. But don't expect me to read the C, I couldn't. And don't think I'll have to write 10,000 lines of Forth. Just give me the specs of the problem, and documentation of the interface.

### My Conclusion ###

colorForth's incredibly small applications provide new estimates of their overstated complexity. 

## Pre-parsed word format ##

    bits 31..........................4 3..0   function             color
          characters                   0      extension            
                                       1      execute              yellow
                                       3      define               red
                                       4      compile              green
                                       7      compile              cyan
                                       9      comment              white
                                       a      Capitalized          white
                                       b      all caps             white
                                       c      variable             magenta
                                       d      feedback             grey
                                       e      display              blue
                                       f      number               white

Numbers have a bit for display format (decimal/hex). Those that fit in 27 bits (with sign extend) are stored

		bits 31.........................5 4    3..0   function   color
		     number                       hex  8      execute    yellow
			                                     6      compile    green

Larger numbers require 2 32-bit words
		bits 31.........................5 4    3..0   function   color
			                                hex  2      execute    yellow
			                                     5      compile    green
		bits 31..............................0
		number

### Functions ###

A red word is defined: its name (1st word only) is placed in the next entry of a table of names. The current location in the dictionary is placed in a parallel table of locations. The name table will be searched backwards (from most recent to earliest). If a word is not found, a ? is displayed and the Editor points at the word in the source code.

A yellow word is executed. Which means that the code it points to is called. Upon return, the Data stack may be different. A yellow number is pushed onto the Data stack.

The word 'forth' selects the main dictionary. The word 'macro' selects another dictionary. Red words will now be defined here. A green word causes this to be searched. If found here, it is executed and presumably compiles some code. Otherwise the forth dictionary is searched and a call to the word's location is compiled. A green number compiles code that pushes it onto the Data stack.

If a macro is cyan it is compiled, not executed. This allows nesting macros. An ordinary word could be cyan, but this is unnecessary and confusing. Should a macro be yellow, it will not be found.

Yellow words within a definition will be executed, as expected. However, they should leave a (single) number on the stack. The transition from yellow to green words causes this number to be compiled. Thus numbers can be calculated inside definitions, which helps understand where they came from.

A magenta word defines a variable. Its code is shared with other variables. The next word in the dictionary is used to point back to the source code, where the value of the variable is stored (initially 0). A yellow variable pushes its address onto the Data stack.

The value of a variable is not disturbed by recompilation. It is saved with the source code and restored with the next boot. Its value is displayed by the Editor. These variables have the flavor of 'state variables'. Temporary variables are usually held on the stack.

On the Pentium, yellow variables are preferred. At compile time, their address is returned and will be compiled as a literal at the yellow-green transition. (Remember, only 1 number is compiled per yellow-green transition.) A green variable compiles a call to code executed at run time to put the address on the stack.

## Pentium colorForth ##

Pentium colorForth
Boots into 32-bit mode with a flat address space. Segment registers are based at zero; essentially unused. Interrupts off. Protections off.

Data are addressed as 32-bit words, not bytes. But ESP and ESI hold byte addresses, for optimization. Instructions are optimized if agruments are literals.

Registers are assigned:

* 0 EAX: stack (1st number on Data stack)
* 1 ECX: string counter, scratch
* 2 EDX: address register A, I/O port, scratch
* 3 EBX: unused
* 4 ESP: byte pointer to top of Return stack
* 5 EBP: unused
* 6 ESI: byte pointer to 2nd number on Data stack
* 7 EDI: dword pointer to next word to be interpreted

27 chip primitives

		Op      	 Word                                 	 Pentium                        	 Action
		0       	 word ;                               	 jmp                            	 Jump to word; tail recursion
		1       	 if                                   	 jz                             	 Jump to 'then' if zero
		2       	 word                                 	 call                           	 Call word
		3       	 -if                                  	 jns                            	 Jump to 'then' if not negative
		6       	 ;                                    	 ret                            	 Return to calling word
		8       	 @                                    	 mov EAX, [EAX*4]               	 Fetch from address on stack
		                                                	 dup; mov EAX, a                	 Fetch from address in A; increment A 
		9       	 @+                                   	 dup; mov EAX, [EDX*4]
							                                      	 inc edx                        	 
		a       	 n                                    	 dup; mov EAX, n                	 Fetch number
		b       	 @r                                   	                                	 Fetch from address in R
		c       	 !                                    	 a!; mov [EDX*4], EAX; drop     	 Store to address on stack
		        	                                      	 mov a*4, EAX; drop
		        	                                      	 mov a*4, n
		d       	                                      	
		e       	 !+                                   	 mov [EDX*4], EAX; inc EDX      	 Store to address in A; increment A
		f       	 !r                                   	                                	 Store to address in R; increment R
		10      	 2*                                   	 shl EAX, 1                     	 Shift stack left
		11      	 2/                                   	 sar EAX, 1                     	 Shift stack right, propagate sign
		12      	 -                                    	 not EAX                        	 Ones complement stack
		14      	 and                                  	 and EAX, [ESI]; nip            	 And to stack
		        	                                      	 and EAX, n
		15      	 or                                   	 xor EAX, [ESI]; nip            	 Exclusive-or to stack
		        	                                      	 xor EAX, n
		16      	 +                                    	 add EAX, [ESI]; nip            	 Add to stack
		        	                                      	 add EAX, n
		17      	 *+                                   	                                	 Multiply step
		18      	 push                                 	 push EAX; drop                 	 Push stack onto Return
		19      	 a                                    	                                	 Load A register onto stack
		1a      	 dup                                  	 lea ESI, ESI-4; mov [ESI], EAX 	 Duplicate stack
		1b      	 over                                 	 dup; mov EAX, [ESI+4]          	 Load 2nd datum onto stack
		1c      	 pop                                  	 dup; pop EAX                   	 Pop Return onto stack
		1d      	 a!                                   	 mov EDX, EAX; drop             	 Store stack into A register
		1e      	 drop                                 	 lodsd                          	 Discard stack
		1f      	 nop                                  	 nop                            	 Do nothing

Other Pentium macros:

		Word                           	 Pentium                             	 Action
		nip                            	 lea ESI, ESI+4                      	 Discard 2nd stack item; preserve flags
		swap                           	 mov EDX, EAX;
		                               	 mov EAX, [ESI]; mov [ESI], EDX      	 Exchange stack with 2nd number
		+!                             	 a!; add [EDX], EAX; drop            	 Add to address on stack
		                               	 add a*4, EAX; drop
		                               	 add a*4, n
		*                              	 imul EAX, [ESI]; nip                	 Multiply with stack
		                               	 imul EAX, n
		*/                             	 mov ECX, EAX; drop                  	 Multiply 2 numbers, divide by stack
		                               	 imul [ESI]; idiv [ECX]; nip

* The word - is the unary ones-complement, not the binary subtract. Its result is 1 less than negate, and much faster in hardware.
* The word or is exclusive-or. Inclusive-or is rarely needed.
* The word */ multiplies by a ratio, with a double-length intermediate product. It eliminates the need for floating-point.

## Forth: the early years ##

### Abstract ###

Forth is a simple, natural computer language. It has achieved remarkable acceptance where efficiency is valued. It evolved in the 1960s on a journey from university through business to laboratory. This is the story of how a simple interpreter expanded its abilities to become a complete programming language/operating system.
### Forward 1999 ###

This paper was written for the HOPL II (History of programming languages) conference. It was summarily rejected, apparently because of its style. Much of the content was included in the accepted paper [Rather 1993].

This HTML version was reformatted from the original typescript. Minimal changes were made to the text. Examples of source code were suggested by reviewer Phil Koopman. They've not yet been added.

### Forth ###
Forth evolved during the decade of the 60s, across America, within university, business and laboratory, amongst established languages. During this period, I was its only programmer and it had no name until the end. This account is retrieved from memory, prompted by sparse documentation and surviving listings.

Forth is hardly original, but it is a unique combination of ingredients. I'm grateful to the people and organizations who permitted me to develop it - often unbeknownst to them. And to you, for being interested enough to read about it.

Forth is a simple, natural computer language. Today it is accepted as a world-class programming language. That it has achieved this without industry, university or government support is a tribute to its efficiency, reliability and versatility. Forth is the language of choice when its efficiency outweighs the popularity of other languages. This is more often the case in real-world applications such as control and communication.

A number of Forth organizations and a plethora of small companies provide systems, applications and documentation. Annual conferences are held in North America, Europe and Asia. A draft ANSI standard will soon be submitted [ANS 1991].

None of the books about Forth quite capture its flavor. I think the best is still the first, Starting Forth by Leo Brodie [Brodie 1981]. Another window is provided by JFAR's invaluable subject and author index [Martin 1987].

The classic Forth we are discussing provides the minimum support a programmer needs to develop a language optimized for his application. It is intended for a work-station environment: keyboard, display, computer and disk.

Forth is a text-based language that is essentially context-free. It combines 'words' separated by spaces, to construct new words. About 150 such words constitute a system that provides (with date of introduction)


      SAO  1958  Interpreter
     SLAC  1961  Data stack
      RSI  1966  Keyboard input
                 Display output, OK
                 Editor
  Mohasco  1968  Compiler
                 Return stack
                 Dictionary
                 Virtual memory (disk)
                 Multiprogrammer
     NRAO  1971  Threaded code
                 Fixed-point arithmetic

Such a system has 3-8K bytes of code compiled from 10-20 pages of source. It can easily be implemented by a single programmer on a small computer.

This account necessarily follows my career. But it is intended to be the autobiography of Forth. I will discuss the features listed above; and the names of the words associated with them. The meaning of many words is obvious. Some warrant description and some are beyond the scope of this paper.

That portion of the Forth dictionary to be mentioned is summarized here:

		Interpreter
				 WORD  NUMBER  INTERPRET  ABORT
				 HASH  FIND  '  FORGET
				 BASE  OCTAL  DECIMAL  HEX
				 LOAD  EXIT  EXECUTE  (
		Terminal
				 KEY  EXPECT
				 EMIT  CR  SPACE  SPACES  DIGIT  TYPE  DUMP
		Data stack
				 DUP  DROP  SWAP  OVER
				 +  -  *  /  MOD  NEGATE
				 ABS  MAX  MIN
				 AND  OR  XOR  NOT
				 0<  0=  =
				 @  !  +!  C@  C!
				 SQRT  SIN.COS  ATAN  EXP  LOG
		Return stack
				 :  ;  PUSH  POP  I
		Disk
				 BLOCK  UPDATE  FLUSH  BUFFER  PREV  OLDEST
		Compiler
				 CREATE  ALLOT  ,  SMUDGE
				 VARIABLE  CONSTANT
				 [  ]  LITERAL  ."  COMPILE
				 BEGIN  UNTIL  AGAIN  WHILE  REPEAT
				 DO  LOOP  +LOOP  IF  ELSE  THEN

### MIT, SAO, 1958 ###

October, 1957 was Sputnik - a most exciting time. I was a sophmore at MIT and got a part-time job with SAO (Smithsonian Astrophysical Observatory, 14 syllables) at Harvard.

SAO was responsible for optical tracking of satellites - Moonwatch visual observations and Baker-Nunn tracking cameras. Caught off-guard by Sputnik, they hired undergraduates to compute predictions with Friden desk calculators. John Gaustad told me about MIT's IBM EDPM 704 and loaned me his Fortran II manual. My first program, Ephemeris 4, eliminated my job [Moore 1958].

Now a Programmer, I worked with George Veis to apply his method of least-squares fitting to determine orbital elements, station positions and ultimately the shape of Earth [Veis 1960]. Of course, this part-time job was at least 40 hours, and yes, my grades went to hell.

At MIT, John McCarthy taught an incredible course on LISP. That was my introduction to recursion, and to the marvelous variety of computer language. Wil Baden has noted that LISP is to Lambda Calculus as Forth is to Lukasewcleicz Postfix.

APL was also a topical language, with its weird right-left parsing. Although I admire and emulate its operators, I'm not persuaded they constitute an optimal set.

The programming environment in the 50s was more severe than today. My source code filled 2 trays with punch cards. They had to be carried about to be put through machines, mostly by me. Compile took 30 minutes (just like C) but limited computer time meant one run per day, except maybe 3rd shift.

So I wrote this simple interpreter to read input cards and control the program. It also directed calculations. The five orbital elements each had an empirical equation to account for atmospheric drag and the non-spherical Earth. Thus I could compose different equations for the several satellites without re-compiling.

These equations summed terms such as P2 (polynomial of degree 2) and S (sine). 36-bit floating-point dominated calculation time so overhead was small. A data stack was unnecessary, and probably unknown to me.

The Forth interpreter began here with the words

     WORD  NUMBER  INTERPRET  ABORT

They weren't spelled that way because they were statement numbers.

INTERPRET uses WORD to read words separated by spaces and NUMBER to convert a word to binary (in this case, floating-point). Such free-format input was unusual, but was more efficient (smaller and faster) and reliable. Fortran input was formatted into specific columns and typographic errors had caused numerous delays.

This interpreter used an IF ... ELSE IF construct, coded in Fortran, finding a match on a single character. Error handling consisted of terminating the run. Then, as now, ABORT asked the user what to do. Since input cards were listed as they were read, you knew where the error was.

### Stanford, SLAC, 1961 ###

In 1961 I went to Stanford to study mathematics. Although Stanford was building its computer science department, I was interested in real computing. I was impressed that they could (dared?) write their own Algol compiler. And I fatefully encountered the Burroughs B5500 computer.

I got another 'part-time' job at SLAC (Stanford Linear Accelerator Center, 12 syllables) writing code to optimize beam steering for the pending 2-mile electron accelerator. This was a natural application of my least-squares experience to phase-space. Hal Butler was in charge of our group and the program, TRANSPORT, was quite successful.

Another application of least-squares was the program CURVE, coded in Algol (1964). It is a general-purpose non-linear differential-corrections data-fitting program. Its statistical rigor provides insight into agreement between model and data.

The data format and model equations were interpreted and a push-down stack used to facilitate evaluation. CURVE was an impressive precursor to Forth. It introduced these words to provide the capability to fit models much more elaborate than simple equations:


     +  -  *  NEGATE
     IF  ELSE  THEN  <
     DUP  DROP  SWAP
     :  ;  VARIABLE  !  (
     SIN  ATAN  EXP  LOG

Spelling was quite different:


     NEGATE  was  MINUS
       DROP       ;
       SWAP       .
          !       <
   VARIABLE       DECLARE
          ;       END
     ( ...)       COMMENT ...;

The interpreter used IF ... ELSE IF to identify a 6-character input word called ATOM (from LISP). DUP DROP and SWAP are 5500 instructions; I'm surprised at the spelling change. The word : was taken from the Algol label format, flipped for left-right parsing (to prevent the interpreter encountering an undefined word):


     Algol - LABEL:
     CURVE - : LABEL

In fact, : marked a position in the input string to be interpreted later. Interpretation was stopped by ; . A version of : was named DEFINE .

The store operator ( ! ) appeared in connection with VARIABLE . But fetching ( @ ) was automatic. Note the input had become complex enough to warrant comments. The sometime-criticised postfix conditional dates from here:


     Algol - IF expression THEN true ELSE false
     CURVE - stack IF true ELSE false THEN

True is interpreted if stack is non-zero. THEN provides unique termination, the lack of which always confused me in Algol. Such expressions were interpreted: IF would scan ahead for ELSE or THEN.

The word < introduces the convention that relations leave a truth value on the stack, 1 for true and 0 for false. The transcendental functions are, of course, library calls.

### Free-lance ###

I left Stanford in 1965 to become a free-lance programmer in the New York City area. This was not unusual, and I found work programming in Fortran, Algol, Jovial, PL/I and various assemblers. I literally carried my card deck about and recoded it as necessary.

Minicomputers were appearing, and with them terminals. The interpreter was ideal for teletype input, and soon included code to handle output. So we aquire the words


     KEY  EXPECT
     EMIT  CR  SPACE  SPACES  DIGIT  TYPE

EXPECT is a loop calling KEY to read a keystroke. TYPE is a loop calling EMIT to display a character.

With the TTY came paper-tape and some of the most un-friendly software imaginable - hours of editing and punching and loading and assembling and printing and loading and testing and repeating. I remember a terrible Sunday in a Manhattan skyscraper when I couldn't find splicing tape (nothing else works) and swore that 'There must be a better way'.

I did considerable work for Bob Davis at Realtime Systems, Inc (RSI). I became a 5500 MCP guru to support his time-sharing service (remote input to a mainframe) and wrote a Fortran-Algol translator and file editing utilities. The translator taught me the value of spaces between words, not required by Fortran.

The interpreter still accepted words with the first 6 characters significant (the 5500 had 48-bit words). The words


     LIST  EDIT  BEGIN  AGAIN  EXIT

appear, with BEGIN ... AGAIN spelled START ... REPEAT and used to bracket the editor commands


     T  TYPE  I  INSERT  D  DELETE  F  FIND

later used in NRAO's editor. The word FIELD was used in the manner of Mohasco and Forth, Inc's data-base management.

One of Forth's distinctive features comes from here. The rule is that Forth acknowledge each line of input by appending OK when interpretation is complete. This may be difficult, for when input is terminated by CR a blank must be echoed, and the CR included with OK. At RSI, OK was on the next line, but it still conveyed friendly reassurance over an intimidating communications line:


     56 INSERT ALGOL IS VERY ADAPTABLE
     OK

This postfix notation suggests a data stack, but it only had to be one deep.

### Mohasco, 1968 ###

In 1968 I transformed into a business programmer at Mohasco Industries, Inc in Amsterdam NY. They are a major home-furnishing company - carpets and furniture. I had worked with Geoff Leach at RSI and he persuaded me to follow him up-state. I had just married, and Amsterdam has a lovely small-town atmosphere to contrast with NYC.

I rewrote my code in COBOL and learned the truth about business software. Bob Rayco was in charge of Corporate data processing and assigned me two relevant projects:

He leased an IBM 1130 minicomputer with a 2250 graphic display. The object was to see if computer graphics helped design patterned carpets. The answer was 'not without color' and the 1130 went away.

Meanwhile I had the latest minicomputer environment: 16-bit CPU, 8K RAM, disk (my first), keyboard, printer, card reader/punch, Fortran compiler. The reader/punch provided disk backup. I ported my interpreter again (back to Fortran) and added a cross-assembler to generate code for the 2250.

The system was a great success. It could draw animated 3-D images when IBM could barely draw static 2-D. Since this was my first real-time graphics, I coded Spacewar, that first video game. I also converted my Algol chess program into Forth and was duely impressed how much simpler it became.

The file holding the interpreter was labeled FORTH, for 4th (next) generation software - but the operating system restricted file names to 5 characters.

This environment for programming the 2250 was far superior to the Fortran environment, so I extended the 2250 cross-assembler into an 1130 compiler. This introduced a flock of words


     DO  LOOP  UNTIL
     BLOCK  LOAD  UPDATE  FLUSH
     BASE  CONTEXT  STATE  INTERPRET  DUMP
     CREATE  CODE  ;CODE  CONSTANT  SMUDGE
     @  OVER  AND  OR  NOT  0=  0< 

They were still differently spelled


					LOOP  was  CONTINUE
				 UNTIL       END
				 BLOCK       GET
					LOAD       READ
					TYPE       SEND
		 INTERPRET       QUERY
				CREATE       ENTER
					CODE       the cent symbol

The only use I've ever found for the cent symbol. The loop index and limit were on the data stack. DO and CONTINUE were meant to acknowledge Fortran.

BLOCK manages a number of buffers to minimize disk access. LOAD reads source from a 1024-byte block. 1024 was chosen as a nice modular amount of disk, and has proven a good choice. UPDATE allows a block to be marked and later rewritten to disk, when its buffer is needed (or by FLUSH ). It implements virtual memory and is concealed in store ( ! ) words.

BASE allows octal and hex numbers as well as decimal. CONTEXT was the first hint of vocabularies and served to isolate editor words. STATE distinguished compiling from interpreting. During compilation, the count and first 3 characters of a word were compiled for later interpretation. Strangely, words could be terminated by a special character, an aberration quickly abandoned. The fetch operator ( @ ) appeared in many guises, since fetching from variables, arrays and disk had to be distinguished. DUMP became important for examining memory.

But most important, there was now a dictionary. Interpret code now had a name and searched a linked-list for a match. CREATE constructs the classic dictionary entry:


     link to previous entry
     count and 3 characters
     code to be executed
     parameters

The code field was an important innovation, since an indirect jump was the only overhead, once a word had been found. The value of the count in distinguishing words, I learned from the compiler writers of Stanford.

An important class of words appeared with CODE . Machine instructions followed in the parameter field. So any word within the capability of the computer could now be defined. ;CODE specifies the code to be executed for a new class of words, and introduced what are now called objects.

SMUDGE avoided recursion during the interpretation of a definition. Since the dictionary would be searched from newest to oldest definitions, recursion would normally occur.

Finally, the return stack appeared. Heretofor, definitions had not been nested, or used the data stack for their return address. Altogether a time of great innovation in the punctuated evolution of Forth.

The first paper on Forth, an internal Mohasco report, was written by Geoff and me [Moore 1970]. It would not be out of place today.

In 1970 Bob ordered a Univac 1108. An ambitious project to support a network of leased lines for an order-entry system. I had coded a report generator in Forth and was confident I could code order-entry. I ported Forth to the 5500 (standalone!) to add credibility. But corporate software was COBOL. The marvelous compromise was to install a Forth system on the 1108 that interfaced with COBOL modules to do transaction processing.

I vividly recall commuting to Schenectady that winter to borrow 1107 time 3rd shift. My TR4-A lacked floor and window so it became a nightly survival exercise. But the system was an incredible success. Even Univac was impressed with its efficiency (Les Sharp was project liason). The ultimate measure was response time, but I was determined to keep it maintainable (small and simple). Alas, an economic downturn led Management to cancel the 1108. I still think it was a bad call. I was the first to resign.

1108 Forth must have been coded in assembler. It buffered input and output messages and shared the CPU among tasks handling each line. Your classic operating system. But it also interpreted the input and PERFORMed the appropriate COBOL module. It maintained drum buffers and packed/unpacked records. The words


     BUFFER  PREV  OLDEST
     TASK  ACTIVATE  GET  RELEASE

date from here. BUFFER avoided a disk read when the desired block was known empty. PREV (previous) and OLDEST are system variables that implement least-recently-used buffer management. TASK defines a task at boot time and ACTIVATE starts it when needed. GET and RELEASE manage shared resources (drum, printer). PAUSE is how a task relinquishes control of the CPU. It is included in all I/O operations and is invisible to transaction code. It allows a simple round-robin scheduling algorithm that avoids lock-out.

After giving notice, I wrote an angry poem and a book that has never been published. It described how to develop Forth software and encouraged simplicity and innovation. It also described indirect-threaded code, but the first implementation was at NRAO.

I struggled with the concept of meta-language, language that talks about language. Forth could now interpret an assembler, that was assembling a compiler, that would compile the interpreter. Eventually I decided the terminology wasn't helpful, but the term Meta-compile for recompiling Forth is still used.
NRAO, 1971
George Conant offered me a position at NRAO (National Radio Astronomy Observatory, 15 syllables). I had known him at SAO and he liked Ephemeris 4. So we moved to Charlottesville VA and spent summers in Tucson AZ when the radio-telescope on Kitt Peak was available for maintainance.

The project was to program a Honeywell 316 minicomputer to control a new filter-bank for the 36' millimeter telescope. It had a 9-track tape and Tektronix storage-tube terminal. George gave me a free hand to develop the system, though he wasn't pleased with the result. NRAO was a Fortran shop and by now I was calling Forth a language. He was right in that organizations have to standardize on a single language. Other programmers now wanted their own languages.

Anyhow, I had coded Forth in assembler on the IBM 360/50 mainframe. Then I cross-compiled it onto the 316. Then I re-compiled it on the 316 (Although I had a terminal on the 360, response time was terrible). The application was easy once the system was available. There were two modes of observing, continuum and spectral-line. Spectral-line was the most fun, for I could display spectra as they were collected and fit line-shapes with least-squares [Moore 1973].

The system was well-received in Tucson, where Ned Conklin was in charge. It did advance the state-of-the-art in on-line data reduction. Astronomers used it to discover and map inter-stellar molecules just as that became hot research.

Bess Rather was hired to provide on-site support. She had first to learn the Forth system and then explain and document it, with minimal help from me. The next year I reprogrammed the DDP-116 to optimize telescope pointing. The next, Bess and I replaced the 116 and 316 with a DEC PDP-11.

The development that made all this possible was indirect-threaded code. It was a natural development from my work at Mohasco, though I later heard that DEC had used direct-threaded code in one of their compilers. Rather than re-interpret the text of a definition, compile the address of each dictionary entry. This improved efficiency for each reference required only 2 bytes and an address interpreter could sequence through a definition enormously faster. In fact, this interpreter was a 2-word macro on the 11:


     : NEXT   IP )+ W MOV  W )+ ) JMP ;

Now Forth was complete. And I knew it. I could write code more quickly that was more efficient and reliable. Moreover, it was portable. I proceeded to recode the 116 pointing the 300' Green Bank telescope, and the HP mini that was inaugurating VLBI astronomy. George gave me a ModComp and I did Fourier transforms for interferometry and pulsar search (64K data). I even demonstrated that complex multiply on the 360 was 20% faster in Forth than assembler.

NRAO appreciated what I had wrought. They had an arrangement with a consulting firm to identify spin-off technology. The issue of patenting Forth was discussed at length. But since software patents were controversial and might involve the Supreme Court, NRAO declined to pursue the matter. Whereupon, rights reverted to me. I don't think ideas should be patentable. Hindsight agrees that Forth's only chance lay in the public domain. Where it has flourished.

Threaded-code changed the structure words (such as DO LOOP IF THEN ). They acquired an elegant implementation with addresses on the data stack during compilation.

Now each Forth had an assembler for its particular computer. It uses post-fix op-codes and composes addresses on the data stack, with Forth-like structure words for branching. The manufacturer's mnemonics are defined as word classes by ;CODE . Might take an afternoon to code. An example is the macro for NEXT above.

Unconventional arithmetic operators proved their value


     M*  */  /MOD  SQRT  SIN.COS  ATAN  EXP  LOG 

M* is the usual hardware multiply of 2 16-bit numbers to a 32-bit product (arguments, of course, on the data stack). */ follows that with a divide to implement rational arithmetic. /MOD returns both quotient and remainder and is ideal for locating records within a file. SQRT produces a 16-bit result from a 32-bit argument. SIN.COS returns both sine and cosine as is useful for vector and complex arithmetic (FFT). ATAN is its inverse and has no quadrant ambiguity. EXP and LOG were base 2.

These functions used fixed-point arithmetic - 14 or 30 bits right of a binary point for trig, 10 for logs. This became a characteristic of Forth since it's simpler, faster and more accurate than floating-point. But hardware and software floating-point are easy to implement.

I'd like to applaud the invaluable work of Hart [Hart 1978] in tabulating function approximations with various accuracies. They have provided freedom from the limitations of existing libraries to those of us in the trenches.

The word DOES> appeared (spelled ;: ). It defines a class of words (like ;CODE ) by specifying the definition to be interpreted when the word is referenced. It was tricky to invent, but particularly useful for defining op-codes.

Nonetheless, I failed to persuade Charlottesville that Forth was suitable. I wasn't going to be allowed to program the VLA. Of any group, 25% like Forth and 25% hate it. Arguments can get violent and compromise is rare. So the friendlies joined forces and formed Forth, Inc. And that's another story.

### Moral ###

The Forth story has the making of a morality play: Persistant young programmer struggles against indifference to discover Truth and save his suffering comrades. It gets better: Watch Forth. Inc go head to head with IBM over a French banking system.

I know Forth is the best language so far. I'm pleased at its success, especially in the ultra-conservative arena of Artificial Intelligence. I'm disturbed that people who should, don't appreciate how it embodies their own description of the ideal programming language.

But I'm still exploring without license. Forth has led to an architecture that promises a wonderful integration of software and silicon. And another new programming environment.

### References ###

[ANS 1991] Draft Proposed ANS Forth, document number X3.215-199x, available from Global Engineering Documents, 2805 McGaw Ave., Irvine CA 92714.

[Brodie, 1981] Brodie, Leo, Starting FORTH, Englewood Cliffs NJ: Prentice-Hall, 1981, ISBN 0 13 842930 8.

[Hart, 1968] Hart, John F. et al, Computer Approximations. Malabar FL: Krieger, 1968; (Second Edition), 1978, ISBN 0 88275 642 7.

[Martin, 1987] Martin, Thea, A Bibliography of Forth References, 3rd Ed, Rochester NY: Institute for Applied Forth Research, 1987, ISBN 0 914593 07 2.

[Moore, 1958] Moore, Charles H. and Lautman, Don A., Predictions for photographic tracking stations - APO Ephemeris 4, in SAO Special Report No. 11, Schilling G. F., Ed., Cambridge MA: Smithsonian Astrophysical Observatory, 1958 March.

[Moore, 1970] --- and Leach, Geoffrey C., FORTH - A Language for Interactive Computing, Amsterdam NY: Mohasco Industries, Inc. (internal pub.) 1970.

[Moore, 1972] --- and Rather, Elizabeth D., The FORTH program for spectral line observing on NRAO's 36 ft telescope, Astronomy & Astrophysics Supplement Series, Vol. 15, No. 3, 1974 June, Proceedings of the Symposium on the Collection and Analysis of Astrophysical Data, Charlottesville VA, 1972 Nov. 13-15.

[Moore, 1980] ---, The evolution of FORTH, an unusual language, Byte, 5:8, 1980 August.

[Rather, 1993] Rather, Elizabeth D., Colburn, Donald R. and Moore, Charles H., The Evolution of Forth, in History of Programming Languages-II, Bergin T. J. and Gibson, R. G., Ed., New York NY: Addison-Wesley, 1996, ISBN 0-201-89502-1.

[Veis, 1960] Veis, George and Moore, C. H., SAO differential orbit improvement program, in Tracking Programs and Orbit Determination Seminar Proceedings, Pasadena CA: JPL, 1960 February 23-26.

# colorforth editor #

### colorforth keyboard and command line $$$

colorforth uses 27 keys on the PC keyboard.

The four fingers of the left hand are placed on the qwerty keys "ASDF" and are moved up one row or down one row.

The four fingers of the right hand are place on the qwerty keys "JKL;" and are moved up one row or down one row.

The thumb(S) press the qwerty keys "N", "Spacebar", and "Alt"

![](photo/editorchapter/keyboard.jpg)

The default keyboard map for the keys is:

![](photo/editorchapter/dvorak.jpg)

The fingers of the left hand are on the "aoeu" keys
The fingers of the right hand are on the "htns" keys.

Qwerty "N" is not programmed,
Spacebar is labeled "9" and invokes the number key function.
Qwerty "Alt" is labeled "x" and invokes the extended key function.

Once you have begun entering characters the keyboard map changes to:

![](photo/editorchapter/enter.jpg)

Qwerty "N" is labeled "X" and will abort word entry and return to the default keyboard map.
Spacebar is labeled "." and enters the word and returns to the default keyboard map.
Qwerty "Alt" is labeled "x" in either of those modes and invokes the extended keyboard map for 23 alternate characters.

![](photo/editorchapter/alt-keys.jpg)

Qwerty "N" is not programmed in this mode.
Qwerty "Alt" returns to the normal keyboard menu.
Spacebar is not shown as programmed, but it will complete entry of a word and return to the normal keyboard menu.

On the normal keyboard map the Spacebar is labeled "9" and invokes number keyboard map in decimal or hex mode Below is the decimal keyboard map.

![](photo/editorchapter/dec.jpg)

Qwerty "N" is labeled "-" and enters a negative number.
Spacebar is labeled "a" and returns to the default text keyboard map.
Qwerty "Alt" is labeled "f" and selects the hex keyboard map.

![](photo/editorchapter/hex.jpg)

Qwerty "N" is labeled "-" and enters a negative number.
Spacebar is labeled "a" and returns to the default text keyboard map.
Qwerty "Alt" is labeled "9" and selects the decimal keyboard map.

When one enters a yellow number on the command line one will see the number that is being entered and will be visible in the stack picture window on the left side of the command line. Text is entered from the right side of the line and each word is executed when a space is entered. When text is entered the number that represents the packed characters in the colorforth word as it is being entered. When the spacebar is pressed colorforth will in the case of number entry leave a number on the stack, or in the case of text entry will attempt to find and execute the word that was typed on the command line.

### colorforth source editor ###

To invoke the editor first enter a block number. Press the spacebar to invoke the number entry keyboard map. If you see a hex number keyboard map press the qwerty "Alt" key labeled "9" to return to decimal entry mode. Move the index finger of the right hand up to the qwerty "U" key, and press the key labeled "1". Move the middle finger of the right hand down one row to the qwerty "," key and press the key labeled "8". Then press the spacebar to put the number "18" on the command line.

The left side of the command line is a stack picture in colorforth. The "18" should the rightmost number showing that it is on top of the parameter stack in colorforth. Because blocks 0-17 are the kernal and font the editor will not edit blocks 0-17.

The normal keyboard map will return and you can type "edit" by pressing down with the middle finger of you left hand for the "e", moving the index finger of the left hand down one row and pressing the "d" key, moving the index finger of the left hand up two rows and pressing the "i" key, the center finger of the right hand pressing the "t" key, and a thumb pressing the spacebar to execute the "edit " command.

You will see the source code to block "18" and the number "18" will be displayed on the left side of the command line as the block being edited. The editor keyboard map is displayed in the lower right part of the screen.

![](photo/editorchapter/editkeys.jpg)

### editor commands ###

		"S" enter white comments in all caps
		"C" enter white comments with the first character capitalized
		"t" enter white comments in lower case
		"y" enter yellow text or numbers
		"r" enter red name
		"g" enter green text or numbers
		"x" toggle odd/even source/shadow code/comment blocks
		"f" repeat 'find'
		"j" jump to last edited block
		"l" move cursor left
		"u" move cursor up, (eight left)
		"d" move cursor down, (eight right)
		"r" move cursor right
		"-" decrement edited block by two
		"m" enter magenta variable name
		"c" enter cyan text
		"+" increment edited block number by two
		"X" delete the current word, (cut for paste)
		"." exit editor to command line
		"i" insert word, (paste from buffer)

### cursor control ###

When the editor is started for the first time the cursor will appear in the upper right corner of the block. If there is text there it will appear on top of the first character. If the cursor is moved to the right of the first word, in the first space after the first word.

![](photo/editorchapter/cursor.jpg)

The cursor resembles a pac-man character poised to munch the word to the left when the "X" (delete word) key is pressed. Strings of text can be cut by repeatedly pressing or holding the "X" key. The cursor can be moved to a different location on that block, or to a different block, and inserted with the "i" key.

Left, Up, Down, and Right keys under the fingers of the right hand move the cursor. The cursor cannot be moved above the top of the screen but it can be moved below the bottom of the screen. If the cursor is not visible hold down the Up key. If it does not appear there may be an error in the editor. Exit the editor with the "." key, and enter a nonsense word like "nth" and the system will show an error on the command line with a "nth?" message. Then type "e " to enter the editor and use the Up key to get the cursor back on the screen.

"l" move cursor left
"u" move cursor up, (eight left)
"d" move cursor down, (eight right)
"r" move cursor right

Note that the Up and Down keys do not make precise up and down movements and only move eight words to the left or right respectively if possible.

Words that have been deleted with the "X" key can be re-inserted with the "i" key after the cursor has been moved. New text in any color can be inserted.

### white comment entry ###

Comments will appear in white when editing and will be ignored by the compiler. Comments may contain any of the normal 24 characters or the 23 extended alternate characters which include the symbols 0-9. Comments can be entered in three ways:

"t" enter white comments in lower case characters.
"S" enter white comments in all capital characters.
"C" enter white comments with the first character capitalized.
When you are in white, comment entry mode, a "9" will appear in the comment entry key menu, and if you press it it will appear to let you enter a number. But numbers are not allowed in comment mode. You must select the "x" extended alternate keyboard menu and use the "0-9" characters, the symbols, in comments.

To exit comment entry mode press the "." key.

### yellow text and number entry ###

Yellow text are words that are interpreted in colorforth when blocks are loaded. Red words are the names of new defined words, they are like the names that follow ":" in traditional Forth. Green words are words being compiled. Magenta words are variables, and cyan words are like "postponed" words in ANS Forth.

Numbers can be entered in green or yellow mode. Numbers can be entered in decimal or hex mode. Hex numbers will appear in a darker green or darker yellow than decimal numbers.

If one intends to enter a number but enters a character string that resembles a number instead it cannot be visually distinguished from a decimal number as it will be the same shade of yellow or green. But when you load the block unless you have actually defined a red word or magenta variable with a name that can be confused for a number, which is generally not a good idea in the first place, you will get a compiler error message that it did not recognize the string (that looked like a number) as a defined name.

There is a Forth wordlist and a macro wordlist in the colorforth compiler and the macro wordlist is searched first durring compilation. Words in the marcro wordlist act like "immediate" word in traditional Forth.

### green text and number entry ###

Green words like "IF" are compiled, but because they are in the macro wordlist and act like immediate words, they are executed at compile time like words written explicity in yellow.

A transistion from green words to yellow words and back to green words is interpreted by the compiler as follows; the transition from green to yellow marks the transition from compilation mode to interpretation mode as would be indicated by the ANS Forth word "[" and the transition from yellow to green is interpreted by the compiler to mean a transition from interpretation mode to compile mode, and the compilation of a literal as would be indicated by the ANS Forth phrase "] LITERAL"

### red name entry ###

When the "r" key is pressed to enter a red word the text for the red name will be aligned on the left side of the screen. A red name is the name of the compiled code that follows it in green. The red name can then be used in green, yellow, cyan or white colors. A call or jump to the word can be compiled with green usage, the word can be interpreted with yellow usage, a defined word can be postponed with cyan usage, and if written in white will always be a comment.

After entering a red word the editor will change the color of the text or numbers being entered into green because the red word is the name of the compiled, green code that follows.

### magenta variable name entry ###

When selecting magenta, and a magenta variable name is added to a block, a green number with the value 0 will be added after the magenta name. The magenta word when executed places the address of the location of the green number on that block on the parameter stack.

### cyan entry ###

Cyan words are like postponed words in ANS Forth.

### cut and paste ###

Cut is performed by the "X" key and paste is performed by the "i" key.

### find again ###

You can exit the editor and find a word in the system by typing the command "find " and then typing the name of the of the word that you want to find and the spacebar.

If the word is found it will take you into the editor with the cursor close to the found word. If you press the "f" key you the editor will take you to the next occurance of the word in the system.

### jump ###

The "j" key jumps between the last two blocks that you edited with the word "edit ". The traditional variable "blk" contains the number of the last block edited with "edit ".

### shadow blocks ###

The "x" key toggles between odd/even blocks while editing. Even numbered blocks are intended for source code and odd numbered blocks are intended for comments. Since odd numbered blocks are not normally compiled and red, yellow, green, cyan, or magenta words on those blocks are intended to be comments only. Those colors may be used for comments on shadow blocks with the understanding that odd numbered blocks are not normally loaded.

### two blocks up or two blocks down ###

The "+" and "-" keys on the editor keyboard map move two blocks up or two blocks down. The editor will not decrement the block number to be edited below 18.

### modifying the editor key table ###

On block 64 a yellow phrase appears just before the red word "fk"

"fk" is the function assigned to the "f" key in the editor keyboard table.

Since "here" in the yellow phrase before the red "fk" is the address of the compiled code for "fk" the yellow phrase "here ekt 22 + !" stores the vector for "fk" into the editor key table (ekt) being used by the editor at an offset of 22 words.

### copying blocks ###

To copy a block first get the block number that you want to copy in the variable "blk". When you edit a block the block number will be left in "blk". The word "copy" takes the destination block number from the stack. Put the destination block number on the stack and type "copy "

If you want to edit the copied block just type "e " as "blk" will now be set to the number of the destination block.


# Keystroke commands in colorForth # 

It's been years since I last wrote a keystroke driven Colorforth program, so I was trawling through the documentation and old code hoping it would jog my memory. It turns out that one word, pad (guessing it's short for 'keypad'), is all that it takes.

Under normal circumstances, that is, when the CF system is in interpretive mode, keystrokes get picked up by a kernel word called accept which is responsible for assembling characters into strings for interpretation. However, pad has the effect of circumventing accept and the interpreter. It enters an infinite loop inside which each incoming key code is treated as an index into a dispatch table. It is up to the application programmer to fill this table with routines that users might need to call.

In all likelihood, the user will eventually need to quit the application's pad loop and escape back into the CF interpreter. This can be provided for by including accept as an entry in the dispatch table.

Before getting into the specifics of programming with pad, it is necessary to know a little about key codes. As CF users are aware — and all the more so, those of us using the Dvorak keyboard configuration — the system pretends that it is hooked up to a pad of 27 keys arranged in two banks of 12 each, plus a smaller group of 3 keys. Keys are mapped to the Qwerty keyboard like this:

![](photo/keystroke_command/cf-keyboard.png)

The numbers 1 to 27 in this diagram are the key codes. There is an additional code, zero, that seems to be assigned to all the remaining keys, a catch-all code of sorts.

So how is `pad` used? The call to `pad` must be followed by a dispatch table, in the form of a sequence of 28 words corresponding to key codes from zero up to 27. To leave any key unmapped, put a word that does nothing into that key's slot. Conveniently, there is a predefined do-nothing word called `nul`. Finally, `pad` needs some additional data specifying the appearance of the hinting area. I'll get back to this final point after showing an example:

![](photo/keystroke_command/cf-keydemo.png)

Here `keydemo`, obviously a contrived application, places 100 on the stack and enters a pad loop where pressing the F key decrements the stacked value while J increments it. (Changes to the stack will be displayed in real time provided that CF's graphic task calls `keyboard`, as it normally would do.) Hitting space quits the application.

Those yellow numbers in the source represent characters in the *hinting area*. That's the bottom right section of the screen where the prevailing keyboard map is displayed. (The map is supposed to show single-character mnemonic labels for the keys, arranged in the 12+12+3 layout of the earlier diagram.) Here's what the bottom of the screen looks like while `keydemo` is running. Plus/minus signs are meant to denote the increment/decrement keys, while the dot labels the quit key:

![](photo/keystroke_command/cf-keydemo-hinting-area.png)

Characters comprising the hinting area are required to appear in the *compiled code* straight after the dispatch table. So it is necessary to colour the dictionary insertion operators (commas) yellow to dictate compile time execution.

CF doesn't use ASCII coding, and there are some inconsistencies in the coding tables that I found online. The 4 character codes relevant to keydemo are 0 for space, hex 23 for minus, 2b for plus and hex 25 for full stop. For the definitive codes look no further than the built in icons utility (whose own source code is worth reading as it uses pad).

In general, pad expects the hinting area characters to be listed in key code order. The first 3 characters label keys 1 to 3. However, the fourth character is an anomaly: it seems to be completely ignored. The fifth and subsequent characters are labels for keys 4 and up.

Now these 28 8-bit character codes could be typed into the source block individually and inserted into the dictionary using the 1, (one-comma) word, but it would be more in keeping with CF's general brevity to group the codes in fours, in little-endian order, and insert them using the plain comma word. keydemo uses the more wordy form for thumb key labels and the brief form for the rest.

