
class Object0:
    def method(self):
        return 20

class Object1:
    def method(self):
        return 10

def class_test0_call_method():
    o0 = Object0()
    o1 = Object1()
    assert o0.method() == 20
    assert o1.method() == 10


def main():
    class_test0_call_method()
    print(1)