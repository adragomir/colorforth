#!/usr/bin/python
"""renumber masm locals to nasm style"""

import sys, os, re
LABEL = re.compile('\s*@@:')  # pattern to identify local label
REFERENCE = re.compile('@[BbFf]\\b')  # backwards or forwards reference
if len(sys.argv) < 2: id = 'local'
else: id = sys.argv[1].split('.')[0]  # create a unique identifier from arg
input = map(str.rstrip, sys.stdin.readlines())
lastlabel, number = '', 0
for line in input:
 label = '..@%s%d' % (id, number)
 nextlabel = '..@%s%d' % (id, number + 1)
 findlabel = LABEL.match(line)
 if findlabel:
  line = '%s:' % label + LABEL.split(line)[1]
  number += 1
  lastlabel = label
  label = nextlabel
 findref = REFERENCE.search(line)
 if findref:
  if findref.group().lower().endswith('f'):
   line = line[0:findref.start()] + label + line[findref.end():]
  else:
   line = line[0:findref.start()] + lastlabel + line[findref.end():]
 print line + '' # '# label=%s, lastlabel=%s' % (label, lastlabel)
