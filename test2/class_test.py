
class Object0:
    def method(self):
        return 0

class Object1:
    def __init__(self, value):
        assert value != 0
        self._value = value

    def method(self):
        return self._value

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

def main():
    class_test0_call_method()
    class_test1_type_of_object()
    class_test2_infer_type_from_method_call()
    class_test3_assign_attribute()
    class_test4_backtrack_attribute_assignment()
    class_test5_add_new_attribute()
    print(1)