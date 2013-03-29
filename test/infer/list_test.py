
# test0_list_concat/1 [] []
def test0_list_concat(a):
    return [] + a

# test1_list_contains/1 [] []
def test1_list_contains(a):
    return a in []

# test2_list_getitem/1 [] [0]
def test2_list_getitem(a):
    return a[0]

# test3_list_setitem/2 [1] [2]
def test3_list_setitem(a, b):
    a[0] = b

# test4_list_delitem/1 [1] []
def test4_list_delitem(a):
    del a[0]

# test5_list_eq/1 [] []
def test5_list_eq(a):
    return a == []

# test6_list_len/0 [] []
def test6_list_len():
    return len([])

# test7_list_append/1 [1] []
def test7_list_append(a):
    a.append(1)
    a.append(2)

# test8_list_modify_and_return/1 [1] []
def test8_list_modify_and_return(a):
    a.append(1)
    return a