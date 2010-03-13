#!/bin/bash
# to correct "color.asm:226: Error: ambiguous operand size for `inc'"
# assume everything is a 32-bit size, which is 'd' not 'l' in Intel syntax
PASS=$1; [ "$PASS" ] || PASS=1
label_if_any='\(\s*\w\+:\)\?'
opcode='\(\s*\w\+\)'
operands='\(\s\+\S\+\)\(.*\)'
lastoperand='\(\S\+\)\s*$'
echo running correction pass $PASS
if [ "$PASS" = "1" ]; then
 command="s/^$label_if_any$opcode$operands/\1\2 dword ptr\3\4/"
elif [ "$PASS" = "2" ]; then
 command="s/$lastoperand/ offset (\1)/"
fi
echo command is "$command"
while read where what d0 d1 d2 d3 d4 d5; do
 if [ "$what" = "Error:" ]; then
  if \
   [ "$d0 $d1 $d2 $d3" = "ambiguous operand size for" ] || \
   [ "$d0 $d1 $d2 $d3 $d4" = "too many memory references for" ]; then
   [ "$d5" ] && declare d4=$d5
   #echo fixing up $where and $d4
   file=$(echo $where | cut -d: -f1)
   line=$(echo $where | cut -d: -f2)
   opcode=$(echo $d4 | tr -c -d 'a-z')
   echo fixing $file:$line $opcode with sed command $command
   sed -n "$line $command p" $file
   sed -i "$line $command" $file
  fi
 fi
done
