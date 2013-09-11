

def var_test0():
    e = free
    assert e == 20
    assert str(e) == "20"

    assert !e == 10
    assert str(e) == "10"

def main():
    var_test0()
    print("var_test")
