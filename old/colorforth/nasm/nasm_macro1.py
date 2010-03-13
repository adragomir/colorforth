#!/usr/bin/python
"""rewrite one-arg masm macro to nasm

   already lowercased by Makefile using sed"""

import sys, os, re
MACRO = re.compile('^(\w+)\s+macro\s+(\w+)')
ENDMACRO = re.compile('^\s*%endmacro')
input = map(str.rstrip, sys.stdin.readlines())
arg = ''
for line in input:
 findmacro = MACRO.match(line)
 if findmacro:
  #print ';# FOUND MACRO'
  name, arg = findmacro.groups()
  line = '%%macro %s 1' % name
 elif ENDMACRO.match(line):
  #print ';# END OF MACRO'
  arg = ''
  #print ';# ARG IS NOW NULL STRING'
 elif arg:
  match = re.compile('\\b%s\\b' % arg).search(line)
  if match:
   line = line[0:match.start()] + ' %1 ' + line[match.end():]
 print line #+ ' ;# arg: %s' % arg
