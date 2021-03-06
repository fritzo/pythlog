

def variable_initialized_in_both_branches(x):
  if x == 0:
    r = 0
  else:
    r = 1
  return r

def if_test0_variable_initialized_in_both_branches():
    assert 0 == variable_initialized_in_both_branches(0)
    assert 1 == variable_initialized_in_both_branches(1)

def variable_assigned_in_then_branch(x):
  r = 1
  if x == 0:
    r = 0
  return r

def if_test1_variable_assigned_in_then_branch():
    assert 0 == variable_assigned_in_then_branch(0)
    assert 1 == variable_assigned_in_then_branch(1)

def variable_assigned_in_else_branch(x):
  r = 0
  if x == 0:
    pass
  else:
    r = 1
  return r

def if_test2_variable_assigned_in_else_branch():
    assert 0 == variable_assigned_in_else_branch(0)
    assert 1 == variable_assigned_in_else_branch(1)

def variable_assigned_only_in_then_branch(x):
  if x == 0:
    r = 0
  return r

def if_test3_variable_assigned_only_in_then_branch():
    assert 0 == variable_assigned_only_in_then_branch(0)
    # TODO: how to test that the function fails when if arg!=0?

def variable_assigned_only_in_else_branch(x):
  if x != 0:
    pass
  else:
    r = 0
  return r

def if_test4_variable_assigned_only_in_else_branch():
    assert 0 == variable_assigned_only_in_else_branch(0)
    # TODO: how to test that the function fails when if arg!=0?

def return_in_both_branches(x):
  if x == 0:
    return 0
  else:
    return 1

def if_test5_return_in_both_branches():
    assert 0 == return_in_both_branches(0)
    assert 1 == return_in_both_branches(1)

def return_in_then_branch(x):
  if x == 0:
    return 0
  return 1

def if_test6_return_in_then_branch():
    assert 0 == return_in_then_branch(0)
    assert 1 == return_in_then_branch(1)

def return_in_else_branch(x):
  if x == 0:
    pass
  else:
    return 1
  return 0

def if_test7_return_in_else_branch():
    assert 0 == return_in_else_branch(0)
    assert 1 == return_in_else_branch(1)

def nested_if(a, b):
  if a == 0:
    if b == 0:
      return 0
    else:
      return 1
  else:
    if b == 1:
      return 2
    else:
      return 3

def if_test8_nested_if():
  assert 0 == nested_if(0, 0)
  assert 1 == nested_if(0, 1)
  assert 2 == nested_if(1, 0)
  assert 3 == nested_if(1, 1)

def main():
    if_test0_variable_initialized_in_both_branches()
    if_test1_variable_assigned_in_then_branch()
    if_test2_variable_assigned_in_else_branch()
    if_test3_variable_assigned_only_in_then_branch()
    if_test4_variable_assigned_only_in_else_branch()
    if_test5_return_in_both_branches()
    if_test6_return_in_then_branch()
    if_test7_return_in_else_branch()
    if_test8_nested_if()
    print("if_test")