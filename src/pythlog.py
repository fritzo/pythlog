#!/usr/bin/python

import ast

pythlog_builtlins = "".split()
class FindGlobalSymbols(ast.NodeVisitor):
    def __init__(self):
        self._symbols = set()

    def global_symbols(self):
        return set(list(self._symbols) + dir(__builtins__) + pythlog_builtlins)

    def visit_FunctionDef(self, node):
        self._symbols.add(node.name)

    def visit_ClassDef(self, node):
        self._symbols.add(node.name)

class Allocator:
    def __init__(self):
        self._counter = 0

    def _alloc_number(self):
        num = self._counter
        self._counter += 1
        return num

    def tempvar(self):
        return "T%s" % self._alloc_number()

class ModuleTranslator(ast.NodeVisitor):
    def __init__(self, global_symbols):
        self._global_symbols = global_symbols
        self._predicates = []
        self._allocator = Allocator()

    def code(self):
        return "\n\n".join(self._predicates)

    def visit_FunctionDef(self, node):
        ft = FunctionTranslator(node,
                               self._allocator,
                               self._global_symbols)
        code = ft.predicates()
        self._predicates.append(code)

    def visit_ClassDef(self, node):
        ct = ClassTranslator(node,
                            self._allocator,
                            self._global_symbols)
        self._predicates.append(ct.predicates(node.body))


class StatementTranslator(ast.NodeVisitor):
    def __init__(self, allocator, globals):
        self._allocator = allocator
        self._globals = globals
        self._code = []

    # Expression nodes
    def visit_Name(self, node):
        return node.id

    def visit_Num(self, node):
        return "t_int(%s)" % node.n

    def visit_BinOp(self, node):
        lhs = self.visit(node.left)
        rhs = self.visit(node.right)
        op = node.op.__class__.__name__.lower()
        result = self._allocator.tempvar()
        self._code.append('i_binop(%s, %s, %s, %s)' % (op, lhs, rhs, result))
        return result

    def visit_Compare(self, node):
        assert len(node.ops) == 1
        assert len(node.comparators) == 1
        op = node.ops[0].__class__.__name__.lower()
        lhs = self.visit(node.left)
        rhs = self.visit(node.comparators[0])
        result = self._allocator.tempvar()
        self._code.append('i_compare(%s, %s, %s, %s)' % (op, lhs, rhs, result))
        return result

    def visit_Call(self, node):
        result = self._allocator.tempvar()
        if node.func.id == 'print':
            func = 'i_print'
        else:
            func = node.func.id

        args = ", ".join(self.visit(a) for a in node.args)
        self._code.append("%s([%s], Io, %s)" % (func, args, result))
        return result

    # Statement nodes

    def visit_Assert(self, node):
        test = self.visit(node.test)
        self._code.append('i_assert(%s)' % test)
        return self._code

    def visit_Assign(self, node):
        assert len(node.targets) == 1
        target = self.visit(node.targets[0])
        value = self.visit(node.value)
        self._code.append('i_assign(%s, %s)' % (target, value))
        return self._code

    def visit_Return(self, node):
        value = self.visit(node.value)
        self._code.append('i_return(Result, %s)' % value)
        return self._code

    def visit_Expr(self, node):
        self.visit(node.value)
        return self._code

class FunctionTranslator(ast.NodeVisitor):
    def __init__(self, node, allocator, globals):
        self._node = node
        self._allocator = allocator
        self._globals = globals
        self._code = []

    def predicates(self):
        for stmt in self._node.body:
            st = StatementTranslator(self._allocator, self._globals)
#            print(ast.dump(stmt))
            self._code.extend(st.visit(stmt))

        args = ", ".join("L%s0" % a.arg for a in self._node.args.args)
        head = "'%s'([%s], Io, Result) :-\n  " % (self._node.name, args)
        return head + ",\n  ".join(self._code) + "."


class SsaRewriter(ast.NodeTransformer):
    def __init__(self):
        self._locals = {}

    def _new_local(self, id):
        if id in self._locals:
            index = int(self._locals[id][len(id) + 1:]) + 1
        else:
            index = 0
        next_id = "L%s%s" % (id, index)
        self._locals[id] = next_id
        return next_id

    def visit_FunctionDef(self, node):
        self._locals = {}
        for a in node.args.args:
            self._new_local(a.arg)
        return self.generic_visit(node)

    def visit_Name(self, node):
        if node.id == 'free':
            id = '_'
        elif node.ctx.__class__ == ast.Store:
            id = self._new_local(node.id)
        elif node.id in self._locals:
            id = self._locals[node.id]
        else:
            return node # Not a local var
        new = ast.Name(id=id, ctx=node.ctx)
        return ast.copy_location(new, node)

    def visit_Assign(self, node):
        value = self.visit(node.value)
        targets = [self.visit(t) for t in node.targets]
        new = ast.Assign(targets=targets, value=value)
        return ast.copy_location(new, node)

def ssa_form(parse_tree):
    rewriter = SsaRewriter()
    return rewriter.visit(parse_tree)

def global_symbols(parse_tree):
    symbol_finder = FindGlobalSymbols()
    symbol_finder.visit(parse_tree)
    return symbol_finder.global_symbols()

def compile_module(module_code):
    parse_tree = ast.parse(module_code)
    ssa_tree = ssa_form(parse_tree)
    compiler = ModuleTranslator(global_symbols(ssa_tree))
    compiler.visit(ssa_tree)
    return BUILTINS + compiler.code()


def main():
    import argparse

    parser = argparse.ArgumentParser(description='pythlog')
    parser.add_argument('infile', metavar='INFILE',
                        type=argparse.FileType('r'),
                        help='input file')
    parser.add_argument('-o', dest='outfile',
                        type=argparse.FileType('w'),
                        default='a.out.pl',
                        help='output file')

    args = parser.parse_args()
    outfile = args.outfile
    outfile.write(compile_module(args.infile.read()))
    outfile.close()


BUILTINS = """
?- use_module(library(clpfd)).

i_assign(Var, Var).

i_assert(t_bool(1)).

i_binop(add, t_int(L), t_int(R), t_int(Result)) :-
    Result #= L + R.
i_binop(sub, t_int(L), t_int(R), t_int(Result)) :-
    Result #= L - R.
i_binop(mult, t_int(L), t_int(R), t_int(Result)) :-
    Result #= L * R.
i_binop(div, t_int(L), t_int(R), t_int(Result)) :-
    Result #= L / R.


i_compare(ne, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #\= R).
i_compare(eq, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #= R).
i_compare(lt, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #< R).
i_compare(gt, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #> R).
i_compare(ge, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #>= R).
i_compare(le, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #=< R).

i_print(Objects, Io, t_None) :-
    to_print_string(Objects, [], Str),
    io(List) = Io,
    append(List, [Str], Result),
    setarg(1, Io, Result).

i_return(Var, Var).


to_print_string([], Acc, t_str(Acc)).
to_print_string([H|T], Acc, Result) :-
    f_str(H, t_str(HStr)),
    append(Acc, HStr, NextAcc),
    to_print_string(T, NextAcc, Result).

f_repr(t_int(I), t_str(Repr)) :-
    integer(I), !,
    number_codes(I, Repr).
f_repr(t_int(_), t_str("?int")).

f_str(Object, Str) :-
    f_repr(Object, Str).


io_write([]).
io_write([H|T]) :-
    t_str(S) = H,
    string_to_list(Str, S),
    write(Str), nl,
    io_write(T).

start :-
    main([], Io, _) -> 
     (io(Printed) = Io, io_write(Printed))
     ; (write('Goal ''main'' failed.'), nl)
    ,
    halt.

:- style_check(-singleton).
:- style_check(-discontiguous).

"""


if __name__ == '__main__':
    main()