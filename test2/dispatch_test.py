class X:
    pass

def nondeterministic_function(x):
    x.value = 0
    return 0

def nondeterministic_function(x):
    x.value = 1
    return 1

def nondeterministic_function(x):
    x.value = 2
    return 2

def dispatch_test0_nondeterministic_function():
    x = X()
    assert nondeterministic_function(x) == 0
    assert x.value == 0
    assert nondeterministic_function(x) == 1
    assert x.value == 1
    assert nondeterministic_function(x) == 2
    assert x.value == 2

def sum_is_correct(a, b, c):
    assert a + b == c
    return 1

def sum_is_correct(a, b, c):
    assert a + b != c
    return 0

def dispatch_test1_predicate_dispatch():
    assert sum_is_correct(1, 2, 3) == 1
    assert sum_is_correct(1, 2, 4) == 0

def main():
    dispatch_test0_nondeterministic_function()
    dispatch_test1_predicate_dispatch()
    print(1)