#!/bin/bash
swipl -q -s src/runtime/runtime.pl -f test/unittest/assert_test.pl -g main
PYTHONPATH=src/compiler/ nosetests test/unittest/
