# Assignments on the form:
#   - x + 1 = 0
#   - f(a, b, c) = g()
# etc, are called "match assignments" because they are syntactic sugar for:
#   - x = free; assert x + 1 == 0

def match_assign_test0_simple_arithmetic_match():
    a + 1 = 2
    assert str(a) == "1"

    (b + a) * 2 = 6
    assert str(b) == "2"
    
def main():
    match_assign_test0_simple_arithmetic_match()
    print("match_assign_test")
