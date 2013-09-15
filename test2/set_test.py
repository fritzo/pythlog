

def set_test0_len():
    assert len(set()) == 0
    assert len({1}) == 1
    assert len({1, 2}) == 2
    assert len({1, 2, 3, 1}) == 3

def set_test1_construct_from_list():
    l = [1, 2, 1, 1]
    assert len(set(l)) == 2
    assert set(l) == {2, 1}

def set_test2_stringify():
    assert str({1, 2, 3}) == "{1, 2, 3}"
    assert str({3, 2, 1, 1}) == "{1, 2, 3}"
    type(x) == set
    assert str(x) == "?set"

def set_test3_in():
    assert 2 in {1, 2, 3}
    assert not 2 in {1, 3}

def set_test4_issubset():
    assert {1, 2}.issubset({1, 2, 3})
    assert {1, 2, 3}.issubset({1, 2, 3})
    assert not {1, 2, 3}.issubset({1, 2})

    assert {1, 2} <= {1, 2, 3}
    assert {1, 2, 3} <= {1, 2, 3}
    assert not {1, 2, 3} <= {1, 2}

    assert {1, 2} < {1, 2, 3}
    assert not {1, 2, 3} < {1, 2, 3}
    assert not {1, 2, 3} < {1, 2}

def set_test5_isdisjoint():
    assert {1, 2}.isdisjoint({3, 4})
    assert not {1}.isdisjoint({1, 2})




def set_test20_equations():
    type(x) == set
    assert 1 in x
    assert 2 in x
    len(x) == 2
    assert str(x) == "{1, 2}"

# TODO: Not powerful enough to solve this:
#    len(y) = 3
#    assert 3 in y
#    assert x < y
#    assert str(x) == "{1, 2, 3}"

def main():
    set_test0_len()
    set_test1_construct_from_list()
    set_test2_stringify()
    set_test3_in()
    set_test4_issubset()
    set_test5_isdisjoint()

    set_test20_equations()
    print("set_test")
