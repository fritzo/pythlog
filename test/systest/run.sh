#!/bin/bash

for PYL in `ls test/systest/*.pyl`
do
	src/compiler/pythlog.py $PYL -o $PYL.tmp.pl && cat src/runtime/runtime.pl $PYL.tmp.pl test/systest/framework.pl > $PYL.result.tmp.pl
	swipl -q -s $PYL.result.tmp.pl -g test_it
done
echo