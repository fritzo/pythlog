

def for_test0_range():
    l = []
    for x in range(3):
        l.append(x)
    assert l == [0, 1, 2]

def main():
    for_test0_range()
    print("for_test")