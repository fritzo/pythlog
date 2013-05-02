#!/usr/bin/env bash

for TEST in `ls test2/*.py`
do
	src/pythlog $TEST
done
echo