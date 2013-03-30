

# test0_localvar_mutates/1 [1] []
def test0_localvar_mutates(a):
    var = a
    var.append(1)

# test1_localvar_freezes/1 [] [1]
def test1_localvar_freezes(a):
    var = []
    var.append(a)

def mutate(a):
    a.append(1)

def freeze(a):
    return [a]

# test2_localvar_assigned_to_function/1 [1] []
def test2_localvar_assigned_to_function(a):
    f = mutate
    f(a)

# test3_localvar_assigned_to_function_in_if/1 [1] [1]
def test3_localvar_assigned_to_function_in_if(a):
    f = mutate
    if 1:
        f = freeze
    f(a)

# test4_localvar_assigned_to_function_in_if_else/1 [1] [1]
def test4_localvar_assigned_to_function_in_if_else(a):
    if 1:
        f = mutate
    else:
        f = freeze
    f(a)

# test5_localvar_assigned_to_function_in_for/1 [1] [1]
def test5_localvar_assigned_to_function_in_for(a):
    f = freeze
    for i in [1]:
        f = mutate
    f(a)