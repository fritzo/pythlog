
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

def list_test6_insert():
    l = [1, 3, 4]
    l.insert(1, 2)
    assert str(l) == "[1, 2, 3, 4]"

    l = [1, 2, 4]
    l.insert(-1, 3)
    assert str(l) == "[1, 2, 3, 4]"
    
    assert None == [].insert(0, 0)

def list_test7_pop():
    l = [1, 2, 3]
    assert 3 == l.pop()
    assert l == [1, 2]

    assert 2 == l.pop()
    assert l == [1]

    assert 1 == l.pop()
    assert l == []

def list_test8_remove():
    l = [1, 2, 3, 2, 1]

    l.remove(1)    
    assert l == [2, 3, 2, 1]
    l.remove(3)
    assert l == [2, 2, 1]
    l.remove(2)
    assert l == [2, 1]
    l.remove(1)
    assert l == [2]
    l.remove(2)
    assert l == []

    assert None == [1].remove(1)

def list_test9_reverse():
    l = [1, 2, 3]
    l.reverse()
    assert l == [3, 2, 1]

    assert None == [].reverse()

def list_test10_sort():
    l = [6, 9, 81]
    l.sort()
    assert l == [6, 9, 81]

    assert None == [].sort()

def list_test11_assign_element():
    l = [1, 1]
    l[0] = 0
    assert l == [0, 1]

    i = free
    l[i] = 0
    assert i == 1

def list_test12_multiply_referenced():
    l0 = [1]
    l1 = l0
    l0[0] = 0
    assert l1[0] == 0

def list_test13_in():
    l = [1, 2, 3, 4]
    assert 1 in l
    assert 3 in l
    assert not (5 in l)

    f = free
    assert f in l
    assert f == 1

def list_test14_type():
    assert type([]) == list

    l = free
    assert len(l) == 0
    write(l)
    assert type(l) == list
#    assert str(l) == "[]"

def list_test15_index():
    assert [1, 2, 3].index(1) == 0
    assert [0, 1, 2, 3].index(1) == 1
    assert [0, 1, [2], 3].index([2]) == 2
    assert [0, 1, 2, 1].index(1) == 3

    f = free
    assert [0, 1, 2, 3].index(f) == 2
    assert str(f) == "2"

def list_test16_count():
    assert [1, 2, 1].count(1) == 2

def list_test17_sum():
    assert sum([1, 2, 3]) == 6
    assert sum(["a", "b", "c"], "") == "abc"

    # Doesn't work. Why?
    #f = free; assert sum([f, f]) == 4; assert str(f) == 2

def list_test18_iter():
    lst = [1, 2, 3, 4]
    i = iter(lst)
    l0 = i.__next__()
    l1 = i.__next__()
    l2 = i.__next__()
    l3 = i.__next__()
    l4 = i.__next__()
    assert l0 == 1
    assert l1 == 2
    assert l2 == 3
    assert l3 == 4
    assert l4 == StopIteration

def list_test19_iter_in_for_loop():
    lst = [1, 2, 3, 4]
    res = []
    for el in lst:
        res.append(el)
    assert str(res) == str(lst)

def list_test20_iter_in_while_loop():
    lst = [1, 2, 3, 4]
    res = []
    i = iter(lst)
    el = i.__next__()
    while el != StopIteration:
        res.append(el)
        el = i.__next__()
    assert str(res) == str(lst)

def pattern_func(x: [len(it) == 2]):
    return 2

def pattern_func(x: [len(it) == 3]):
    return 3

def list_test21_pattern_matches_list_literal():
    assert list(1 in it) == [1]
    assert [1 in it and len(it) > 3] != [0, 1, 2]
    assert [1 in it and len(it) > 3] == [0, 1, 2, 3]

    l = [it.index(0) < it.index(10)]
    assert l == [1, 2, 0, 4, 5, 10]

    assert pattern_func([1, 2]) == 2
    assert pattern_func([1, 2, 3]) == 3

    l = [sum(it) == 6]
    assert l == [1, 2, 3]

def list_test22_pattern_captures_locals():        
    elem = 10
    l = [it[0] == elem]
    assert str(l[0]) == "10"

    l0 = [it[0] == !e0 and it[1] == !e1]
    l1 = [it[0] == e1 and it[1] == e0]
    assert l0 == [1, 2]
    assert str(l1[0]) == "2"
    assert str(l1[1]) == "1"

def list_test23_pattern_find_element():
    l = [[1], [-1, 0, 1], [0, 1, 2], [1, 2, 3]]
    p = [len(it) > 1 and it[0] == 1]
    assert l.index(p) == 3
    
    l = [[1], [2], [3], [4], [5]]
    assert l.index([3 in it]) == 2

def list_test24_pattern_index():    
    l = [it[0] == 1 and it[1] == 2]
    assert l[0] == 1
    assert l[1] == 2

def list_test25_pattern_len():
    assert len([len(it) == 4] + [len(it) == 2]) == 6
    assert len([len(it) < 4] + [len(it) < 2]) < 6
    # Doesn't work.
#    assert len([len(it) < 4] + [len(it) < 2]) == [1, 2, 3]

def list_test26_pattern_match_against_other_pattern():
    p0 = [len(it) == 1]
    p1 = [1 in it]
    assert p0 == p1
    assert str(p0) == str(p1) # == "[1]"

def list_test27_pattern_pop():
    p = [1 in it and len(it) == 2]
    l = p.pop()
    assert l != 1
    assert str(p) == "[1]" # The remaining element must be 1

def list_test28_pattern_add():
    p = [1 in it and len(it) == 1] + [2 in it and len(it) == 1]
    assert str(p) == "[1, 2]"

def main():
    list_test0_length_of_list()
    list_test1_index_list()
    list_test2_append_element()
    list_test3_extend_list()
    list_test4_add_list()
    list_test5_subtract_list()
    list_test6_insert()
    list_test7_pop()
    list_test8_remove()
    list_test9_reverse()
    list_test10_sort()
    list_test11_assign_element()
    list_test12_multiply_referenced()
    list_test13_in()
    list_test14_type()
    list_test15_index()
    list_test16_count()
    list_test17_sum()
    list_test18_iter()
    list_test19_iter_in_for_loop()
    list_test20_iter_in_while_loop()
    list_test21_pattern_matches_list_literal()
    list_test22_pattern_captures_locals()
    list_test23_pattern_find_element()
    list_test24_pattern_index()
    list_test25_pattern_len()
    list_test26_pattern_match_against_other_pattern()
    list_test27_pattern_pop()
    list_test28_pattern_add()
    print("list_test")
