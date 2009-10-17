#!/bin/sh

list=`awk '{print $1}' list.txt | tr "[:upper:]" "[:lower:]"`

for f in $list; do
	echo $f
	sed "s/@@@/$f/" serp-list2113.b6 > serp-list.b6
	dispak6 serp-list.b6 > $f.tmp~
	sed '/^          [0-9][0-9][0-9][0-9]) /!d
s/^          [0-9][0-9][0-9][0-9]) //' $f.tmp~ > $f.bemsh
done
