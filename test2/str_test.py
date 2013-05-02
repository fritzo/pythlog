
def str_test0_type():
    assert type("") == str

def str_test1_length():
    assert len("") == 0
    assert len("1") == 1

def str_test2_index():
    h = "hello"
    assert h[0] == "h"
    assert h[-1] == "o"
    assert h[-2] == h[2]

def str_test3_add():
    assert "hello " + "world" == "hello world"

def str_test4_subtract():
    assert "hello" == "hello world" - " world"

def str_test5_endswith():
    assert "hello world".endswith("world")
    assert not "hello world".endswith("hello")

def str_test6_startswith():
    assert "hello world".startswith("hello")
    assert not "hello world".startswith("world")

def str_test7_pattern():
    p0 = ~"he??o"
    p1 = ~"h?ll?"
    assert p0 == p1

    p0 = ~"he??o"
    p1 = ~"?world"
    assert "hello world" == p0 + p1

def str_test8_solve_equation():
    p = free
    assert type(p) == str
    assert len(p) == 2
    assert p[0] == "a"
    assert p[1] == "b"
    assert p == "ab"


def main():
    str_test0_type()
    str_test1_length()
    str_test2_index()
    str_test3_add()
    str_test4_subtract()
    str_test5_endswith()
    str_test6_startswith()
    str_test7_pattern()
    str_test8_solve_equation()
    print("str_test")