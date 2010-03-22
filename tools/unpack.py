#!/usr/bin/env python
import sys
newcode  = ' rtoeani' + 'smcylgfw' + 'dvpbhxuq' + '01234567' + \
           '89j-k.z/' + ';:!+@*,?'
code = newcode  # assume Tim knows what he's doing
highbit =  0x80000000L
mask =     0xffffffffL

def unpack(coded):
  #debug('coded: %08x' % coded)
  bits = 32 - 4  # 28 bits used for compressed text
  coded &= ~0xf  # so zero low 4 bits
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

if __name__ == "__main__":
  coded = int('0x' + sys.argv[1], 16)
  print unpack(coded)
