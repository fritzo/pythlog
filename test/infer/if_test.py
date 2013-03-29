
# test0_if/2 [1] [2]
def test0_if(a, b):
    if 1:
        a.append(b)
    return 0


# test1_if_else/2 [1] [2]
def test1_if_else(a, b):
    if 1:
        return 0
    else:
        a.append(b)

# test2_if_elif_else/3 [1,3] [2]
def test2_if_elif_else(a, b, c):
    if 1:
        a.append(1)
    elif 1:
        a.append(b)
    else:
        c.append(1)

# test3_if_with_multiple_returns/2 [] [0]
def test3_if_with_multiple_returns(a, b):
    if 1:
        return a[1]
    else:
        return b[1]

def foo(a):
    a.append(1)
    return True

# test4_if_condition_mutates/1 [1] []
def test4_if_condition_mutates(a):
    if foo(a):
        pass