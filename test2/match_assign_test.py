# Assignments on the form:
#   - x + 1 == 0
#   - f(a, b, c) >= g()
# etc, are called "match statements". They are syntactic sugar for:
#   - x = free; assert x + 1 == 0

def match_assign_test0_simple_arithmetic_match():
    a + 1 == 2
    assert str(a) == "1"

    (b + a) * 2 == 6
    assert str(b) == "2"
    

def func(x):
    return 1 + x

def func(x):
    return x

def match_assign_test1_assign_function_calls():
    func(x) == 8
    assert str(x) == "7"

    func(z) == "9"
    assert z == "9"

class X:
    def __init__(self, a):
        self._a = a

def match_assign_test2_decopose_object():
    x = X(7)
    X(z) == x
    assert str(z) == "7"

def match_assign_test3_new_vars_on_both_sides():
    x = 2
    a == b * x
    b < 8
    a > 13
    assert str(a) == "14"

def main():
    match_assign_test0_simple_arithmetic_match()
    match_assign_test1_assign_function_calls()
    match_assign_test2_decopose_object()
    match_assign_test3_new_vars_on_both_sides()
    print("match_assign_test")
