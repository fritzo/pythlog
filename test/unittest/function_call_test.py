import unittest
import pythlog

class FunctionCallTest(unittest.TestCase):
	def test_call_function_without_arguments(self):
		code = pythlog.compile_module("""
def f():
	g()
def g():
	pass
""")
		exp = """f_f(ReturnValue) :- 
    f_g(T0),
    ReturnValue = pl_None."""
		print code
		self.assertTrue(exp in code)

	def test_call_function_with_arguments(self):
		code = pythlog.compile_module("""
def f():
	g(1, 2)
def g(a, b):
	pass
""")
		exp = """f_f(ReturnValue) :- 
    f_g(pl_int(1), pl_int(2), T0),
    ReturnValue = pl_None."""
		self.assertTrue(exp in code)


	def test_call_function_with_arguments_and_return_value(self):
		code = pythlog.compile_module("""
def f():
	x = g(1, 2)
def g(a, b):
	return 1
""")
		exp = """f_f(ReturnValue) :- 
    f_g(pl_int(1), pl_int(2), T0),
    V_x_0 = T0,
    ReturnValue = pl_None."""
		self.assertTrue(exp in code)