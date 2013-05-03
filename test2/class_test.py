
class Object0:
    def method(self):
        return 0

class Object1:
    def __init__(self, value):
        assert value != 0
        self._value = value

    def method(self):
        return self._value

class Object2(Object0):
    def method2(self):
        return 20

    def method(self):
        return 1


def class_test0_call_method():
    o0 = Object0()
    o1 = Object1(1)
    o2 = Object1(10)
    assert o0.method() == 0
    assert o1.method() == 1
    assert o2.method() == 10

def class_test1_type_of_object():
    o0 = Object0()
    o1 = Object1(10)
    assert type(o0) == Object0
    assert type(o1) == Object1

def class_test2_infer_type_from_method_call():
    o0 = free
    o1 = free
    assert o0.method() == 0
    assert o1.method() == 1

    assert type(o0) == Object0
    assert type(o1) == Object1

def class_test3_assign_attribute():
    o = Object1(1)
    o._value = 2
    assert o.method() == 2

def class_test4_backtrack_attribute_assignment():
    o = Object1(1)
    if free:
        o._value = 2
        assert False
    else:
        o._value = 3
    assert o.method() == 3

def class_test5_add_new_attribute():
    o = Object0()
    o.new_attribute = 1
    assert o.new_attribute == 1

def class_test6_deconstruct_object():
    o1 = Object1(Object1(1))
    x = free
    assert o1 == Object1(Object1(x))
    assert x == 1

def class_test7_multiply_referenced():
    o0 = Object1(1)
    o1 = o0
    o0.new_attribute = 0
    assert o1.new_attribute == 0

def class_test8_inheritance():
    o0 = Object2()
    assert o0.method() == 0
    assert o0.method() == 1
    assert o0.method2() == 20

class ToStr:
    def __str__(self):
        return "str"
    def __repr__(self):
        return "repr"

def class_test9_to_string():
    s = ToStr()
    assert str(s) == "str"
    assert repr(s) == "repr"


class Arith:
    def __add__(self, _):
        return "add"
    def __radd__(self, _):
        return "radd"
    def __sub__(self, _):
        return "sub"
    def __rsub__(self, _):
        return "rsub"
    def __mul__(self, _):
        return "mul"
    def __rmul__(self, _):
        return "rmul"
    def __truediv__(self, _):
        return "truediv"
    def __rtruediv__(self, _):
        return "rtruediv"
    def __floordiv__(self, _):
        return "floordiv"
    def __rfloordiv__(self, _):
        return "rfloordiv"
    def __mod__(self, _):
        return "mod"
    def __rmod__(self, _):
        return "rmod"
    def __pow__(self, _):
        return "pow"
    def __rpow__(self, _):
        return "rpow"
    def __rshift__(self, _):
        return "rshift"
    def __rrshift__(self, _):
        return "rrshift"
    def __lshift__(self, _):
        return "lshift"
    def __rlshift__(self, _):
        return "rlshift"
    def __and__(self, _):
        return "and"
    def __rand__(self, _):
        return "rand"
    def __or__(self, _):
        return "or"
    def __ror__(self, _):
        return "ror"
    def __xor__(self, _):
        return "xor"
    def __rxor__(self, _):
        return "rxor"
    def __neg__(self):
        return "neg"
    def __invert__(self):
        return "invert"
    def __lt__(self, _):
        return "lt"
    def __le__(self, _):
        return "le"
    def __eq__(self, _):
        return "eq"


def class_test10_arith():
    a = Arith()
    assert a + 1 == "add"
    assert 1 + a == "radd"
    assert a - 1 == "sub"
    assert 1 - a == "rsub"
    assert a * 1 == "mul"
    assert 1 * a == "rmul"
    assert a / 1 == "truediv"
    assert 1 / a == "rtruediv"
    assert a // 1 == "floordiv"
    assert 1 // a == "rfloordiv"
    assert a % 1 == "mod"
    assert 1 % a == "rmod"
    assert a ** 1 == "pow"
    assert 1 ** a == "rpow"
    assert a >> 1 == "rshift"
    assert 1 >> a == "rrshift"
    assert a << 1 == "lshift"
    assert 1 << a == "rlshift"
    assert a & 1 == "and"
    assert 1 & a == "rand"
    assert a | 1 == "or"
    assert 1 | a == "ror"
    assert a ^ 1 == "xor"
    assert 1 ^ a == "rxor"
    assert ~a == "invert"
    assert (a < 1) == "lt"
    assert (a <= 1) == "le"

def main():
    class_test0_call_method()
    class_test1_type_of_object()
    class_test2_infer_type_from_method_call()
    class_test3_assign_attribute()
    class_test4_backtrack_attribute_assignment()
    class_test5_add_new_attribute()
    class_test6_deconstruct_object()
    class_test7_multiply_referenced()
    class_test8_inheritance()
    class_test9_to_string()
    class_test10_arith()
    print("class_test")
