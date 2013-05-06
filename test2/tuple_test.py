
def tuple_test0_length():
    assert len((1, "2", 3)) == 3
    assert len((1,)) == 1
    assert len(()) == 0

def tuple_test1_indexing():
    t = (1, 2, 3)
    assert t[0] == 1
    assert t[1] == 2
    assert t[2] == 3
    assert t[-1] == 3
    assert t[-2] == 2
    assert t[-3] == 1

def tuple_test2_to_string():
    t = (2, 3)
    assert str(t) == "(2, 3)"
    assert repr(t) == "(2, 3)"

def tuple_test3_add():
    t0 = (1, 2)
    t1 = (3, 4)
    assert t0 + t1 == (1, 2, 3, 4)

    t0 = free
    assert t0 + t1 == (5, 4, 3, 4)
    assert t0 == (5, 4)

def tuple_test4_sub():
    t0 = (1, 2, 3, 4)
    t1 = (3, 4)
    assert t0 - t1 == (1, 2)

    t0 = free
    assert t0 - t1 == (5, 4)
    assert t0 == (5, 4, 3, 4)

def tuple_test5_count():
    assert (1, 2, 3, 1).count(1) == 2
    t = free
    assert (t, 2, 3, 1).count(1) == 2
    assert t == 1

def tuple_test6_index():
    t = (11, 22, 22, 11)
    assert t.index(11) == 0
    assert t.index(11) == 3
    assert t.index(22) == 1
    assert t.index(22) == 2

    f = free
    assert t.index(f) == 0
    assert f == 11

def tuple_test7_mul():
    pass
    # Disabled because there is an issue with overloading the m___mul__
    # predicated causing the int_test to fail.
    #assert (1, 2) * 2 == (1, 2, 1, 2)

def tuple_test8_in():
    assert 1 in (1, 2)
    assert not (3 in (1, 2))

    f = free
    assert f in (1, 2)
    assert f == 1

    f = free
    assert f in (1, 2)
    assert f == 2

def main():
    tuple_test0_length()
    tuple_test1_indexing()
    tuple_test2_to_string()
    tuple_test3_add()
    tuple_test4_sub()
    tuple_test5_count()
    tuple_test6_index()
    tuple_test7_mul()
    tuple_test8_in()
    print("tuple_test")
