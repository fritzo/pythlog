
# test0_recursion_infinite/1 [] []
def test0_recursion_infinite(a):
    test0_recursion_infinite(a)

# test1_recursion_infinite_with_mutation/1 [1] []
def test1_recursion_infinite_with_mutation(a):
    a.append(1)
    test1_recursion_infinite_with_mutation(a)

# test2_recursion_infinite_with_freeze/1 [] [1]
def test2_recursion_infinite_with_freeze(a):
    b = []
    b[1] = a
    test2_recursion_infinite_with_freeze(a)