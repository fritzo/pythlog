
class WithFunctions:
    def pure(self, a, b):
        return a * b

    def mutating_and_freezing(self, a, b):
        a.append(b)

# test0_class_call_pure_function/2 [] []
def test0_class_call_pure_function(a, b):
    c = WithFunctions()
    c.pure(a, b)

# test1_class_call_mutating_and_freezing_function/2 [1] [2]
def test1_class_call_mutating_and_freezing_function(a, b):
    c = WithFunctions()
    c.mutating_and_freezing(a, b)

# test2_class_determines_class_from_func_name/3 [1] [2]
def test2_class_determines_class_from_func_name(a, b, c):
    c.mutating_and_freezing(a, b)