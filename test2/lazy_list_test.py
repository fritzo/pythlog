# The syntax for generators in pythosn is used for so called lazy lists.

def lazy_list_test0_type():
    f = 2
    ll = (x * f for x in range(10))
    assert type(ll) == list

def lazy_list_test1_index():
    f = 2
    ll = (x * f for x in range(3))
    assert ll[0] == 0
    assert ll[1] == 2
    assert ll[2] == 4

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

def main():
    lazy_list_test0_type()
    lazy_list_test1_index()
    lazy_list_test2_is_lazy()
    lazy_list_test3_append()
    print("lazy_list_test")