#!/bin/bash
while read where what description; do
 if [ "$what" = "error:" ]; then
  file=$(echo $where | cut -d: -f1)
  line=$(echo $where | cut -d: -f2)
  echo $where $what $description
  sed -n "$line p" $file
 fi
done
