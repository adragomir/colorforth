#!/usr/bin/python
"""dump a colorForth image file -- jc.unternet.net

   public domain code based on Tim Neitz's cf2html"""

import sys, os, struct

# the old huffman code is from http://www.colorforth.com/chars.html
oldcode  = ' rtoeani' + 'smcylgfw' + 'dvpbhxuq' + 'kzj34567' + \
           '891-0.2/' + ';:!+@*,?'
newcode  = ' rtoeani' + 'smcylgfw' + 'dvpbhxuq' + '01234567' + \
           '89j-k.z/' + ';:!+@*,?'
code = newcode  # assume Tim knows what he's doing
#code = oldcode  # assume Chuck's webpage is up-to-date (bad idea as of 2006)

emptyblock = '\0' * 1024

icon_block = 12  # first block of character maps

high_level_block = 18  # first high-level code block in CM2001

output = sys.stdout

hexadecimal = '0123456789abcdef'

escape = chr(0x1b)

colors = ['', 'red', 'green', 'yellow', 'blue',
 'magenta', 'cyan', 'white', '', 'normal'] # escape codes 30 to 39

# added 'fake' functions here:
# 'dictentry' is same as 'extension' but in a "dirty" block
# 'icon' is character graphics, stored between 0x3000 and 0x4800 in color.com
# 'binary' is machine code, from 0x0 to 0x3000 (with some other stuff too)
# 'end_of_block' is just so the markup routines can be called at end-of-block;
# to close open tags and whatnot
uniquefunction = [
 'extension', 'execute', 'executelong', 'define',
 'compileword', 'compilelong', 'compileshort', 'compilemacro',
 'executeshort', 'text', 'textcapitalized', 'textallcaps',
 'variable', 'undefined', 'undefined', 'undefined',
 'undefined', 'undefined', 'executehexlong', 'undefined',
 'undefined', 'compilehexlong', 'compilehexshort', '',
 'executehexshort', 'undefined', 'undefined', 'undefined',
 'undefined', 'undefined', 'undefined', 'undefined',
 'end_of_block', 'binary', 'icon', 'dictentry',
]

# the following arrays are one-based, remember to subtract 1 before indexing

function = [
 'execute', 'execute', 'define', 'compile',
 'compile', 'compile', 'compilemacro', 'execute',
 'text', 'textcapitalized', 'textallcaps', 'variable',
 '', '', '', '',
 '', 'executehex', '', '',
 'compilehex', 'compilehex', '', 'executehex',
 '', '', '', '',
 '', '', '', '',
]

colortags = [
 'brightyellow', 'brightyellow', 'brightred', 'brightgreen',  # 1-4
 'brightgreen', 'brightgreen', 'brightcyan', 'brightyellow', # 5-8
 'brightwhite', 'brightwhite', 'brightwhite', 'brightmagenta', # 9-0xc
 'normal', 'normal', 'normal', 'normal', # 0xd-0x10
 'normal', 'yellow', 'normal', 'normal', # 0x11-0x14
 'green', 'green', 'normal', 'yellow', # 0x15-0x18
 'normal', 'normal', 'normal', 'normal', # 0x19-0x1c
 'normal', 'normal', 'normal', 'normal', # 0x1d-0x20
 'normal', 'normal', 'normal', 'normal', # 0x21-0x24
]

highbit =  0x80000000L
mask =     0xffffffffL

formats = ['', 'html', 'color', 'plaintext']

dump = {  # set up globals as dictionary to avoid declaring globals everywhere
 'dirty': False,  # low-level code detected in a block
 'blocktext': '',  # decompiled high-level Forth
 'print_formats': [],  # filled in during init; routines not yet defined
 'debugging': False,  # set True for copious debugging messages
 'original': False,  # set True for output similar to Tim Neitz's cf2html.c
 'format': '',  # use 'html' or 'color', otherwise plain text
 'index': 0,  # index into block, to match cf2html.c bug
}

def debug(*args):
 if dump['debugging']:
  sys.stderr.write('%s\n' % repr(args))

def print_normal(fulltag):
 if dump['blocktext'] and fulltag == uniquefunction.index('define'):
  dump['blocktext'] += '\n'
 if fulltag != uniquefunction.index('end_of_block'):
  if dump['blocktext'] and fulltag != uniquefunction.index('define'):
   dump['blocktext'] += ' '

def print_color(fulltag):
 #debug('print_color(0x%x)' % fulltag)
 if dump['blocktext']:  # close previous color tag
  dump['blocktext'] += '%s[%d;%dm' % (escape, 0, 30 + colors.index('normal'))
 if dump['blocktext'] and fulltag == uniquefunction.index('define'):
  dump['blocktext'] += '\n'
 if fulltag != uniquefunction.index('end_of_block'):
  color = colortags[fulltag - 1]
  bright = 0
  if color[0:6] == 'bright':
   bright = 1
   color = color[6:]
  if dump['blocktext'] and fulltag != uniquefunction.index('define'):
   dump['blocktext'] += ' '
  dump['blocktext'] += '%s[%d;%dm' % (escape, bright, 30 + colors.index(color))

def print_text(coded):
 text = unpack(coded)
 #debug('text: "%s"' % text)
 dump['blocktext'] += text

def unpack(coded):
 #debug('coded: %08x' % coded)
 bits = 32 - 4  # 28 bits used for compressed text
 text = ''
 while coded:
  nybble = coded >> 28
  coded = (coded << 4) & mask
  bits -= 4
  #debug('nybble: %01x, coded: %08x' % (nybble, coded))
  if nybble < 0x8:  # 4-bit coded character
   text += code[nybble]
  elif nybble < 0xc: # 5-bit code
   text += code[(((nybble ^ 0xc) << 1) | (coded & highbit > 0))]
   coded = (coded << 1) & mask
   bits -= 1
  else:  # 7-bit code
   text += code[(coded >> 29) + (8 * (nybble - 10))]
   coded = (coded << 3) & mask
   bits -= 3
 return text

def print_tags(fulltag):
 if dump['blocktext'] or (dump['original'] and dump['index']):
  dump['blocktext'] += '</code>'
 if dump['blocktext']:
  if fulltag == uniquefunction.index('define'):
   dump['blocktext'] += '<br>'
 if fulltag != uniquefunction.index('end_of_block'):
  dump['blocktext'] += '<code class=%s>' % function[fulltag - 1]
  if fulltag != uniquefunction.index('define'):
   dump['blocktext'] += ' '

def print_format(fulltag):
 index = formats.index(dump['format'])
 dump['print_formats'][index](fulltag)

def print_hex(integer):
 dump['blocktext'] += '%x' % integer

def print_decimal(integer):
 if (highbit & integer):
  integer -= 0x100000000
 dump['blocktext'] += '%d' % integer

def print_plain(fulltag):
 if dump['blocktext'] and fulltag == uniquefunction.index('define'):
  dump['blocktext'] += '\n'
 if fulltag != uniquefunction.index('end_of_block'):
  if dump['blocktext'] and fulltag != uniquefunction.index('define'):
   dump['blocktext'] += ' '
  dump['blocktext'] += '%s ' % uniquefunction[fulltag].upper()

def print_code(chunk):
 """dump as raw hex so it can be undumped"""
 dump['blocktext'] += '%02x' * len(chunk) % tuple(map(ord, chunk))

def dump_block(chunk):
 """see http://www.colorforth.com/parsed.html for meaning of bit patterns"""
 state = 'default'
 if (dump['block'] / 1024) < high_level_block and not dump['original']:
  # assume machine code
  dump['dirty'] = True
 else:  # assume high-level Forth
  dump['dirty'] = False
 for dump['index'] in range(0, len(chunk), 4):
  integer = struct.unpack('<L', chunk[dump['index']:dump['index'] + 4])[0]
  fulltag = integer & 0x1f  # bit 4 set indicates hex numeric output
  tag = integer & 0xf  # only 0 to 0xc used as of CM2001 colorForth
  #debug('fulltag: 0x%x' % fulltag)
  if state == 'print number as hex':
   print_hex(integer)
   state = 'default'
  elif state == 'print number as decimal':
   print_decimal(integer)
   state = 'default'
  elif tag == uniquefunction.index('extension') and ' ' in unpack(integer):
   if (chunk[dump['index']:] == emptyblock[dump['index']:]) and \
    not dump['original']:
    break
   else:
    print_text(integer)
  elif tag == uniquefunction.index('extension') and ' ' not in unpack(integer):
   if dump['dirty']:
    print_format(uniquefunction.index('dictentry'))
   print_text(integer)
  elif dump['dirty']:
   print_format(uniquefunction.index('binary'))
   print_hex(integer)
  elif tag == uniquefunction.index('executelong') or \
   tag == uniquefunction.index('compilelong'):
   print_format(fulltag)
   if integer & 0x10:
    state = 'print number as hex'
   else:
    state = 'print number as decimal'
  elif tag == uniquefunction.index('compileshort') or \
   tag == uniquefunction.index('executeshort'):
   print_format(fulltag)
   if integer & 0x10:
    if integer & highbit:
     print_hex((integer >> 5) | 0xf8000000)
    else:
     print_hex(integer >> 5)
   else:
    if integer & highbit:
     print_decimal((integer >> 5) | 0xf8000000)
    else:
     print_decimal(integer >> 5)
  elif tag == uniquefunction.index('variable'):
   print_format(tag)
   print_text(integer & 0xfffffff0)
   state = 'print number as decimal'
   print_format(uniquefunction.index('compileword'))
  elif not dump['original'] and tag > 0xc:
   #debug('block is dirty: tag = 0x%x' % tag)
   dump['dirty'] = True
   print_format(uniquefunction.index('binary'))
   print_code(struct.pack('<L', integer))
  else:
   print_format(tag)
   print_text(integer & 0xfffffff0)
 if not ((dump['format'] == 'html') and dump['original']):
  print_format(uniquefunction.index('end_of_block'))
 if dump['blocktext'] and not dump['original']:
  dump['blocktext'] += '\n'

def init():
 dump['debugging'] = os.getenv('DEBUGGING')
 if dump['format'] == 'html':
  dump['original'] = os.getenv('TIM_NEITZ')
 dump['print_formats'] = [print_normal, print_tags, print_color, print_plain]

def cfdump(filename):
 init()
 if not filename:
  file = sys.stdin
 else:
  file = open(filename)
 data = file.read()
 file.close()
 debug('dumping %d bytes' % len(data))
 if dump['format'] == 'html':
  output.write('<html>\n')
  output.write('<link rel=stylesheet type="text/css" href="colorforth.css">\n')
 for dump['block'] in range(0, len(data), 1024):
  chunk = data[dump['block']:dump['block'] + 1024]
  debug('block %d: %s' % (dump['block'] / 1024, repr(chunk)))
  output.write('{block %d}\n' % (dump['block'] / 1024))
  if dump['format'] == 'html':
   output.write('<div class=code>\n')
  dump_block(chunk)
  output.write(dump['blocktext'])
  if dump['blocktext']:
   dump['blocktext'] = ''
  if dump['original']:
   output.write('</code>\n')
  if dump['format'] == 'html':
   output.write('</div>\n<hr>\n')
 if dump['format'] == 'html':
  output.write('</html>\n')

def cf2text(filename):
 dump['format'] = 'plaintext'
 cfdump(filename)

def cf2ansi(filename):
 dump['format'] = 'color'
 cfdump(filename)

def cf2html(filename):
 dump['format'] = 'html'
 cfdump(filename)

if __name__ == '__main__':
 os.path.split
 command = os.path.splitext(os.path.split(sys.argv[0])[1])[0]
 sys.argv += ['']  # make sure there's at least 1 arg
 (eval(command))(sys.argv[1])
else:
 pass
