
def while_test0_while_reading_and_writing_and_modifying_locals():
    i = 0
    j = 0
    k = 1
    while i < 10:
        i = i + k
        j = i
    assert i == 10
    assert j == 10
    assert k == 1

def while_test1_conditional_assignment():
    i = 0
    j = 0
    while i < 10:
        i = i + 1
        if i < 5:
            j = i
    assert j == 4

def while_test2_write_more_than_one_update_to_variable():
    i = 0
    j = 0
    while i < 10:
        i = i + 1
        i = i + 1
        j = j + 1
    assert j == 5

def while_test3_several_conditional_assignments():
    i = 0
    j = 0
    while i < 4:
        if i == 0:
            j = j + 1
        elif i == 1:
            j = j + 2
        elif i == 2:
            j = j + 3
        elif i == 3:
            j = j + 4
        i = i + 1
    assert j == 10

def while_test4_solve_number_of_iterations():
    i = free
    j = 0
    while i > 0:
        j = j + 1
        i = i - 1
    assert i == 10
    assert j == 10

def main():
    while_test0_while_reading_and_writing_and_modifying_locals()
    while_test1_conditional_assignment()
    while_test2_write_more_than_one_update_to_variable()
    while_test3_several_conditional_assignments()
    while_test4_solve_number_of_iterations()
    print("while_test")