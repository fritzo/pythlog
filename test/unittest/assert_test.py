import unittest
import pythlog

def compile_stmt(stmt):
	code = pythlog.compile_module("""
def f(a, b):
	%s
""" % stmt)
	stmts = code.strip().split('\n')[1:-1]
	return [s[:-1].strip() for s in stmts] # drop trailing ',' and '.'

class AssertTest(unittest.TestCase):
	def test_assert_without_message(self):
		self.assertEquals(['pl_assert(pl_int(1), pl_None)'],
			              compile_stmt("assert 1"))
		self.assertEquals(['pl_assert(pl_bool(1), pl_None)'],
			              compile_stmt("assert True"))
		self.assertEquals(['pl_assert(pl_bool(0), pl_None)'],
			              compile_stmt("assert False"))

	def test_assert_with_message(self):
		self.assertEquals(['pl_assert(pl_int(1), pl_seq(str, "msg"))'],
			              compile_stmt("assert 1, 'msg'"))
