
class Object0:
    def method(self):
        return 0

class Object1:
    def __init__(self):
#        self._value = 1
        pass

    def method(self):
        return 1

def class_test0_call_method():
    o0 = Object0()
    o1 = Object1()
    assert o0.method() == 0
    assert o1.method() == 1

def class_test1_type_of_object():
    o0 = Object0()
    o1 = Object1()
    assert type(o0) == Object0
    assert type(o1) == Object1

def class_test2_infer_type_from_method_call():
    o0 = free
    o1 = free
    assert o0.method() == 0
    assert o1.method() == 1

    assert type(o0) == Object0
    assert type(o1) == Object1

def main():
    class_test0_call_method()
    class_test1_type_of_object()
    print(1)