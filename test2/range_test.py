
def range_test0_type():
    assert type(range(10)) == range
    assert type(range(1, 10)) == range
    assert type(range(1, 10, 2)) == range

def range_test1_in():
    assert 1 in range(2)
    assert not(10 in range(2, 10, 2))
    assert not(2 in range(0, 10, 3))
    assert 3 in range(0, 10, 3)

    e = free
    assert 1 in range(e, 10)
    assert e == 0 # One possible solution

    e = free
    assert 10 in range(0, e)
    assert e == 20 # One possible solution

    s = free
    assert 3 in range(0, 10, s)
    assert s == 3 # One possible solution

def range_test2_index():
    assert range(10)[0] == 0
    assert range(10)[2] == 2
    assert range(0, 10, 3)[1] == 3
    assert range(0, 10, 3)[-1] == 9

def range_test3_len():
    assert len(range(10)) == 10
    assert len(range(2, 10)) == 8
    assert len(range(2, 10, 4)) == 2
    assert len(range(0, 10, 3)) == 4
    assert len(range(-2, -10, -3)) == 3

def range_test4_iterator():
    i = iter(range(0, 3, 1))
    e0 = i.__next__()
    e1 = i.__next__()
    e2 = i.__next__()
    assert e0 == 0
    assert e1 == 1
    assert e2 == 2

def main():
    range_test0_type()
    range_test1_in()
    range_test2_index()
    range_test3_len()
    range_test4_iterator()
    print("range_test")