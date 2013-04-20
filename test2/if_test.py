

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



def main():
    if_test0_variable_initialized_in_both_branches()
    if_test1_variable_assigned_in_then_branch()
    if_test2_variable_assigned_in_else_branch()
    if_test3_variable_assigned_only_in_then_branch()
    if_test4_variable_assigned_only_in_else_branch()
    print(1)