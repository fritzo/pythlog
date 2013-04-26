
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

def main():
    list_test0_length_of_list()
    list_test1_index_list()
    list_test2_append_element()
    list_test3_extend_list()
    list_test4_add_list()
    list_test5_subtract_list()
    print(1)