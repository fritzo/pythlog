def int_test0_add():
    assert 1 + 2 == 3
    assert 6 + 9 == 15
    x = free
    assert x + 8 == 28
    assert x == 20


def int_test1_sub():
    assert 7 - 2 == 5
    x = free
    assert x - 8 == 20
    assert x == 28


def int_test2_mult():
    assert 7 * 2 == 14
    x = free
    assert x * 8 == 48
    assert x == 6

# TODO: test truediv
def int_test3_div():
    assert 6 // 2 == 3
    x = free
    assert x // 6 == 7
    assert x == 42


def int_test4_mod():
    assert 5 % 2 == 1
    assert -5 % 2 == 1
    assert 5 % -2 == -1
    assert -5 % -2 == -1

    x = free
    assert x % 2 == 1
    assert x == 3


def int_test5_exp():
    assert 2 ** 10 == 1024

    x = free
    assert x ** 10 == 1024
    assert x == 2

    y = free
    assert 2 ** y == 1024
    assert y == 10


def int_test6_shift():
    assert 6 >> 1 == 3
    assert -82 << 2 == -328

    x = free
    assert x << 8 == 512
    assert x == 2

    y = free
    assert 2 << y == 128
    assert y == 6


def int_test7_bit_inverse():
    assert ~67 == -68
    assert ~-76 == 75

    x = free
    assert ~x == 99
    assert x == -100


def int_test8_bit_and():
    assert 64372483 & 473982479238 == 59124482
    assert -749382 & 43792 == 32784
    assert -239223 & -98126524 == -98299648

    x = free
    assert 0xf & x == 0x2
    assert x == 2


def int_test9_bit_or():
    assert 4327948 | 3894723 == 8089551
    assert -4327948 | 3894723 == -4194825
    assert -4327948 | -3894723 == -133123

    x = free
    assert 0x2 | x == 0x3
    assert x == 1

    y = free
    assert 0x2 | y == 0x3
    assert y == 3


def int_test10_bit_xor():
    assert 436272 ^ 134643 == 304579
    assert -374323452 ^ 23125653245 == -23423798791

    x = free
    assert 0x2 ^ x == 0x3
    assert x == 1

def int_test11_type():
    x = free
    assert type(x) == int
    assert x == 1

def int_test12_comparisons():
    assert 1 < 2
    assert 1 <= 2
    assert 2 == 2
    assert 2 >= 2
    assert 3 > 2


def fac(n):
    assert n >= 0
    if n == 0:
        return 1
    else:
        return fac(n - 1) * n

def int_test20_factorial():
    assert fac(7) == 5040
    n = free
    assert 5040 == fac(n)
    assert n == 7

def wfac(n):
    assert n >= 0
    prod = 1
    while n > 0:
        prod = prod * n
        n = n - 1
    return prod

def int_test21_factorial_using_while():
    assert wfac(7) == 5040
    n = free
    assert 5040 == wfac(n)
    assert n == 7


def main():
    int_test0_add()
    int_test1_sub()
    int_test2_mult()
    int_test3_div()
    int_test4_mod()
    int_test5_exp()
    int_test6_shift()
    int_test7_bit_inverse()
    int_test8_bit_and()
    int_test9_bit_or()
    int_test10_bit_xor()
    int_test11_type()
    int_test12_comparisons()
    int_test20_factorial()
    int_test21_factorial_using_while()
    print("int_test")


# 12 - 20