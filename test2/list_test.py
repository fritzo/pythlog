
def list_test0_length_of_list():
    l0 = []
    assert len(l0) == 0

    l1 = [1, 2, 3]
    assert len(l1) == 3

def list_test1_index_list():
    l = [1, 2, 3, 4]
    assert l[0] == 1
    assert l[1] == 2
    assert l[2] == 3
    assert l[3] == 4
    assert l[-1] == 4
    assert l[-2] == 3
    assert l[-3] == 2
    assert l[-4] == 1

def list_test2_append_element():
    l = [1]
    l.append(2)
    assert len(l) == 2
    assert l == [1, 2]

def list_test3_extend_list():
    l = [1]
    l.extend([2, 3])
    assert len(l) == 3
    assert l == [1, 2, 3]

def list_test4_add_list():
    l0 = [1]
    l1 = [2, 3]
    l2 = [4]
    assert l0 + l1 + l2 == [1, 2, 3, 4]

    l = free
    assert l0 + l == [1, 2]
    assert l == [2]

def list_test5_subtract_list():
    l0 = [1, 2, 3]
    assert l0 - [3] == [1, 2]

    l = free
    assert l0 - l == [1]
    assert l == [2, 3]

def list_test6_assign_element():
    l = [1, 1]
    l[0] = 0
    assert l == [0, 1]

    i = free
    l[i] = 0
    assert i == 1

def list_test7_multiply_referenced():
    l0 = [1]
    l1 = l0
    l0[0] = 0
    assert l1[0] == 0

def list_test8_in():
    l = [1, 2, 3, 4]
    assert 1 in l
    assert 3 in l
    assert not (5 in l)

    f = free
    assert f in l
    assert f == 1

def list_test9_type():
    assert type([]) == list

def pattern_func(x: [len(it) == 2]):
    return 2

def pattern_func(x: [len(it) == 3]):
    return 3

def list_test10_pattern():
    assert [1 in it] == [1]
    assert [1 in it and len(it) > 3] != [0, 1, 2]
    assert [1 in it and len(it) > 3] == [0, 1, 2, 3]

    l = [[1], [-1, 0, 1], [0, 1, 2], [1, 2, 3]]
    p = list(len(it) > 1 and it[0] == 1)
    assert l.index(p) == 3

    assert len([len(it) < 4] + [len(it) < 2]) < 6

    l = [it[0] == 1 and it[1] == 2]
    assert l[0] == 1
    assert l[1] == 2
    
    elem = 10
    l = [it[0] == elem]
    assert str(l[0]) == "10"

    e0 = free
    e1 = free
    l0 = [it[0] == e0 and it[1] == e1]
    l1 = [it[0] == e1 and it[1] == e0]
    assert l0 == [1, 2]
    assert str(l1[0]) == "2"
    assert str(l1[1]) == "1"

    l = [it.index(0) < it.index(10)]
    assert l == [1, 2, 0, 4, 5, 10]

    assert pattern_func([1, 2]) == 2
    assert pattern_func([1, 2, 3]) == 3

def list_test11_index():
    assert [1, 2, 3].index(1) == 0
    assert [0, 1, 2, 3].index(1) == 1
    assert [0, 1, [2], 3].index([2]) == 2
    assert [0, 1, 2, 1].index(1) == 3

def list_test12_count():
    assert [1, 2, 1].count(1) == 2

def main():
    list_test0_length_of_list()
    list_test1_index_list()
    list_test2_append_element()
    list_test3_extend_list()
    list_test4_add_list()
    list_test5_subtract_list()
    list_test6_assign_element()
    list_test7_multiply_referenced()
    list_test8_in()
    list_test9_type()
    list_test10_pattern()
    list_test11_index()
    list_test12_count()
    print("list_test")