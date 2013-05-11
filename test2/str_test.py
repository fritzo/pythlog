
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

def str_test3_index_equation():
    p = free
    assert type(p) == str
    assert len(p) == 2
    assert p[0] == "a"
    assert p[1] == "b"
    assert p == "ab"

def str_test4_add():
    assert "hello " + "world" == "hello world"

def str_test5_subtract():
    assert "hello" == "hello world" - " world"

def str_test6_endswith():
    assert "hello world".endswith("world")
    assert not "hello world".endswith("hello")

def str_test7_startswith():
    assert "hello world".startswith("hello")
    assert not "hello world".startswith("world")

def str_test8_pattern():
    p0 = ~"he??o"
    p1 = ~"h?ll?"
    assert p0 == p1

    p0 = ~"he??o"
    p1 = ~"?world"
    assert "hello world" == p0 + p1

def str_test9_join():
    assert "1, 2" == ", ".join(["1", "2"])
    assert "1" == ", ".join(["1"])
    assert "" == ", ".join([])

def str_test10_join_equation():
    assert "1,2" == ~"?".join([~"?", ~"?"])
    x = free
    y = free
    assert "1, 2, 3, 4" == ", ".join([x, y])
    assert x == "1, 2"
    assert y == "3, 4"

def str_test11_in():
    assert "a" in "a"
    assert "a" in "abc"
    assert "ab" in "abc"

def str_test12_in_equation():
    s = ~"?????"
    t = ~"???"
    assert s[0] == "a"
    assert "abc" in s
    assert "cd" in s
    assert "cde" in s
    assert t in s
    assert "ea" in s + t
    assert "bc" in t

    assert "abcde" == s
    assert "abc" == t

def str_test13_pattern():
    p = str("hello" in it)
    assert p == "hello world"

def str_test14_findall():
    solutions = [all x in
                 type(x) == str and
                 "abc" in x and
                 "cd" in x and
                 "cde" in s and
                 x[0] == "a" and
                 len(x) == 5]
    # TODO: this make the test run forever because there are variables left in
    # 'solutions'. Why?
    #print(solutions)

def main():
    str_test0_type()
    str_test1_length()
    str_test2_index()
    str_test3_index_equation()
    str_test4_add()
    str_test5_subtract()
    str_test6_endswith()
    str_test7_startswith()
    str_test8_pattern()
    str_test9_join()
    str_test10_join_equation()
    str_test11_in()
    str_test12_in_equation()
    str_test13_pattern()
    str_test14_findall()
    print("str_test")