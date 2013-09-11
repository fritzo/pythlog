#!/usr/bin/env bash

for TEST in `ls test2/*.py`
do
    echo -n $TEST": "
	/usr/bin/time --format='  %es' src/pythlog $TEST
done
echo
