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

def sum_is_correct(a, b, c, x):
    assert a + b == c
    x.value = 1
    return 1

def sum_is_correct(a, b, c, x):
    assert a + b != c
    x.value = 0
    return 0

def dispatch_test1_predicate_dispatch():
    x = X()
    assert sum_is_correct(1, 2, 3, x) == 1
    assert x.value == 1
    assert sum_is_correct(1, 2, 4, x) == 0
    assert x.value == 0


class Y:
    def __init__(self, a, b):
        self._a = a
        self._b = b

def func2(y: Y(Y(1, 2), b), x):
    return 0

def func2(y: Y(a, b), x):
    return 1

def func2(y: x + 1, x):
    return 2

def func2(y, x):
    assert y + 1 != x
    return 3

def dispatch_test2_match_in_arg_list():
    assert func2(Y(Y(1, 2), 3), 0) == 0
    assert func2(Y(Y(2, 3), 4), 0) == 1
    assert func2(Y(10, 20), 0) == 1
    assert func2(2, 1) == 2
    assert func2(1, 1) == 3

def main():
    dispatch_test0_nondeterministic_function()
    dispatch_test1_predicate_dispatch()
    dispatch_test2_match_in_arg_list()
    print(1)