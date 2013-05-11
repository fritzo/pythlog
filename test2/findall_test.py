def func(): return 0
def func(): return 1
def func(): return 2
def func(): return 3
def func(): return 3

def findall_test0_get_all_from_nondet_function():
    solutions = [all x in x == func()]
    assert solutions == [0, 1, 2, 3, 3]

def findall_test1_ints():
    assert [all x in x * x == 4] == [-2, 2]

def main():
    findall_test0_get_all_from_nondet_function()
    findall_test1_ints()
    print("findall_test")