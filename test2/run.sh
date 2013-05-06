#!/usr/bin/env bash

for TEST in `ls test2/*.py`
do
    echo -n $TEST": "
	src/pythlog $TEST
done
echo