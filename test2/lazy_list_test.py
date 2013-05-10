# The syntax for generators in pythosn is used for so called lazy lists.

def lazy_list_test0_type():
    f = 2
    ll = (x * f for x in range(10))
    assert type(ll) == list

def lazy_list_test1_index():
    f = 2
    l0 = (x * f for x in range(3))
    assert l0[0] == 0
    assert l0[1] == 2
    assert l0[2] == 4

    l1 = (x for x in range(5) if x % 2 == 0)
    assert l1[0] == 0
    assert l1[1] == 2
    assert l1[2] == 4

def calc(x, f):
    if x == 2:
        f[0] = 1
    return x

def lazy_list_test2_is_lazy():
    f = [0]
    ll = (calc(x, f) for x in range(3))
    assert ll[0] == 0
    assert f[0] == 0
    assert ll[1] == 1
    assert f[0] == 0
    assert ll[2] == 2
    assert f[0] == 1

def lazy_list_test3_append():
    ll = (x * 2 for x in range(3))
    ll.append(10)
    assert ll[0] == 0
    assert ll[1] == 2
    assert ll[2] == 4
    assert ll[3] == 10

def lazy_list_test4_extend():
    l0 = (0 for _ in range(2))
    l1 = (1 for _ in range(2))
    l0.extend(l1)
    assert l0[0] == 0
    assert l0[1] == 0
    assert l0[2] == 1
    assert l0[3] == 1

def lazy_list_test5_add_list():
    l0 = (x for x in range(1, 2))
    l1 = (x for x in range(2, 4))
    l2 = (x for x in range(4, 5))
    assert l0 + l1 + l2 == [1, 2, 3, 4]

    l3 = (x for x in range(1, 2))
    l = free
    assert l3 + l == [1, 2]
    assert l == [2]

def lazy_list_test6_subtract_list():
    l0 = (x for x in range(1, 4))
    l1 = (x for x in range(3, 4))
    assert l0 - l1 == [1, 2]

    l3 = (x for x in range(1, 4))
    l = free
    assert l0 - l == [1]
    assert l == [2, 3]

def lazy_list_test7_assign_element():
    l = (1 for _ in range(0, 2))
    l[0] = 0
    assert l == [0, 1]

def lazy_list_test8_is_lazy_when_used_as_iter():
    f = [0]
    ll = (calc(x, f) for x in range(6) if x < 3)
    i = iter(ll)
    assert i.__next__() == 0
    assert f[0] == 0
    assert i.__next__() == 1
    assert f[0] == 0
    assert i.__next__() == 2
    assert f[0] == 1
    assert i.__next__() == StopIteration

def lazy_list_test9_len():
    assert len(x for x in range(10)) == 10
    assert len(x for x in range(10) if x % 2 == 0) == 5

def main():
    lazy_list_test0_type()
    lazy_list_test1_index()
    lazy_list_test2_is_lazy()
    lazy_list_test3_append()
    lazy_list_test4_extend()
    lazy_list_test5_add_list()
    lazy_list_test6_subtract_list()
    lazy_list_test7_assign_element()
    lazy_list_test8_is_lazy_when_used_as_iter()
    lazy_list_test9_len()
    print("lazy_list_test")