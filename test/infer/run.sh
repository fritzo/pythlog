#!/bin/bash

for PY in `ls test/infer/*.py`
do
    src/compiler/pythlog-infer.py $PY -o $PY.tmp
    grep "test" $PY.tmp > $PY.tmp.filtered
    grep "# test" $PY | sed "s/# //" > $PY.gold.tmp
    diff $PY.tmp.filtered $PY.gold.tmp
done
