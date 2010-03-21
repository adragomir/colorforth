#!/usr/bin/python
import sys, os, re
SYMDEF = re.compile('\s+\.text:([0-9a-f]+)\s+([\w]+)')
DEBUGTEXT = re.compile('\[(\d+)\]\s+\[(0x[0-9a-f]+)\]\s+\S+\s+[^:]+:(.*)')
symbol_table, time_data = {}, []
last_ticks, last_address, nearest_address = 0, 0, 0
output = sys.stdout
symboldatafile = open('color.lst')
symboldata = symboldatafile.readlines()
symboldatafile.close()
for line in symboldata:
 match = SYMDEF.search(line)
 if match:
  address = eval('0x' + match.groups()[0])
  symbol = match.groups()[1]
  symbol_table[address] = symbol
symbol_table[0x100000] = 'COMPILED'
#print repr(symbol_table)
addresses = symbol_table.keys()
profile = dict(map(None, addresses, [0] * len(addresses)))
addresses.sort()
#print repr(addresses)
debugdatafile = open(os.path.join(os.getenv('TMP'), 'newbxnewcf.debug.txt'))
debugdata = debugdatafile.readlines()
debugdatafile.close()
for line in debugdata:
 match = DEBUGTEXT.search(line)
 if match:
  last_address = nearest_address
  ticks = eval(match.groups()[0])
  if last_ticks == 0:
   last_ticks = ticks
   ticks = 0
  else:
   ticks = ticks - last_ticks
   last_ticks = ticks
  address = eval(match.groups()[1])
  instruction = match.groups()[2]
  if symbol_table.has_key(address):
   nearest_address = address
   print '%s: %s' % (symbol_table[address], instruction)
  else:
   nearest_address = filter(lambda a: a < address, addresses)[-1]
   print '%s+%d: %s' % (symbol_table[nearest_address],
    address - nearest_address, instruction)
  profile[last_address] += ticks
output.write('\n')
addresses.remove(0) # this won't have any tick counts anyway
addresses.sort(lambda i, j: cmp(profile[i], profile[j]))
for address in addresses:
 print symbol_table[address], ': ', profile[address]
