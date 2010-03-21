#!/usr/bin/python
"""rewrite one-arg masm macro to as"""

import sys, os, re
MACRO = re.compile('(?i)^\s*\.macro\s+(\w+)\s+(\w+)')
ENDMACRO = re.compile('(?i)^\s*\.endm\\b')
input = map(str.rstrip, sys.stdin.readlines())
arg = ''
for line in input:
 findmacro = MACRO.match(line)
 if findmacro:
  #print ';# FOUND MACRO'
  name, arg = findmacro.groups()
 elif ENDMACRO.match(line):
  #print ';# END OF MACRO'
  arg = ''
  #print ';# ARG IS NOW NULL STRING'
 elif arg:
  match = re.compile('\\b%s\\b' % arg).search(line)
  if match:
   line = line[0:match.start()] + '\\' + arg + line[match.end():]
 print line #+ ' ;# arg: %s' % arg
