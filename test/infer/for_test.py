
# test0_for_loop_mutates_in_loop/2 [2] []
def test0_for_loop_mutates_in_loop(a, b):
    for i in a:
        b.append(i)

# test1_for_loop_return_element/1 [] [0]
def test1_for_loop_return_element(a):
    for i in a:
        return i

