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

    def globalsym(self):
        return "g_%s" % self._alloc_number()

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
        self._predicates.extend(ft.predicates())

    def visit_ClassDef(self, node):
        ct = ClassTranslator(node,
                            self._allocator,
                            self._global_symbols)
        self._predicates.append(ct.predicates(node.body))


class StatementTranslator(ast.NodeVisitor):
    def __init__(self, allocator, globals):
        self._allocator = allocator
        self._globals = globals
        self._predicates = []
        self._code = []
        self._code_prefix = ['(true']
        self._code_suffix = ['true)']

    def code(self):
        return self._code_prefix + self._code + self._code_suffix

    def predicates(self):
        """Get any additional predicates created while translating the
        statement (needed for ifs, loops, etc)."""
        return self._predicates

    # Expression nodes
    def visit_Name(self, node):
        return node.id

    def visit_Num(self, node):
        return "t_int(%s)" % node.n

    def visit_UnaryOp(self, node):
        operand = self.visit(node.operand)
        op = node.op.__class__.__name__.lower()
        result = self._allocator.tempvar()
        self._code.append('i_unary%s(%s, %s)' % (op, operand, result))
        return result


    def visit_BinOp(self, node):
        lhs = self.visit(node.left)
        rhs = self.visit(node.right)
        op = node.op.__class__.__name__.lower()
        result = self._allocator.tempvar()
        self._code.append('i_bin%s(%s, %s, %s)' % (op, lhs, rhs, result))
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
        args = ", ".join(self.visit(a) for a in node.args)
        self._code.append("%s([%s], Io, %s)" % (node.func.id, args, result))
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
        self._code.append('i_return(Result, %s), DidReturn = 1' % value)
        return self._code

    def visit_Expr(self, node):
        self.visit(node.value)
        return self._code

    def visit_If(self, node):
        self._code_prefix = ['(true']
        test = self.visit(node.test)

        st = StatementTranslator(self._allocator, self._globals)
        for stmt in node.body:
            st.visit(stmt)
        body = st.code()
        self._predicates.extend(st.predicates())

        st = StatementTranslator(self._allocator, self._globals)
        for stmt in node.orelse:
            st.visit(stmt)
        orelse = st.code()
        self._predicates.extend(st.predicates())


        name = self._allocator.globalsym()
        args = node.invars + node.outvars + ['Io', 'Result', 'DidReturn']

        self._predicates.append(generate_predicate(name,
                                                   ['t_bool(1)'] + args,
                                                   body))
        self._predicates.append(generate_predicate(name,
                                                   ['t_bool(0)'] + args,
                                                   orelse))
        self._code.append('%s(%s, %s)' % (name, test, ", ".join(args)))
        self._code.extend(self._code_suffix)
        self._code.append('((DidReturn \= 1)-> (true')
        self._code_suffix = ['true); true)']
        return self._code

def generate_predicate(name, args, body):
    head = "%s(%s)" % (name, ", ".join(args))
    if len(body) == 0:
        return head + "."   
    suffix = " :-\n  " + ",\n  ".join(body) + "."
    return head + suffix

class FunctionTranslator(ast.NodeVisitor):
    def __init__(self, node, allocator, globals):
        self._node = node
        self._allocator = allocator
        self._globals = globals
        self._code = []
        self._predicates = []

    def predicates(self):
        st = StatementTranslator(self._allocator, self._globals)
        for stmt in self._node.body:
#            print(ast.dump(stmt))
            st.visit(stmt)
        self._code.extend(st.code())
        self._predicates.extend(st.predicates())

        func_args = "[%s]" % (", ".join(a.arg for a in self._node.args.args))
        pred_args = [func_args, 'Io', 'Result']
        self._predicates.append(generate_predicate(self._node.name,
                                                   pred_args,
                                                   self._code))
        return self._predicates

class SsaRewriter(ast.NodeTransformer):
    def __init__(self):
        self._locals = {}

    def _localvar_index(self, id, locs):
        return int(locs[id][len(id) + 1:])

    def _localvar_name(self, id, idx):
        return "L%s%s" % (id, idx)

    def _new_local(self, id):
        if id in self._locals:
            index = self._localvar_index(id, self._locals) + 1
        else:
            index = 0
        next_id = self._localvar_name(id, index)
        self._locals[id] = next_id
        return next_id

    def visit_arg(self, node):
        newarg = self._new_local(node.arg)
        return ast.copy_location(ast.arg(arg=newarg,
                                         annotation=node.annotation),
                                 node)

    def visit_FunctionDef(self, node):
        """
        Makes the name of functions not clash with Prolog names by prefixing
        the function name with 'g_' (read: global_). 
        """
        self._locals = {} # New function started => clear locals
        args = self.visit(node.args) # Must be visited before the body
        body = [self.visit(stmt) for stmt in node.body]
        new = ast.FunctionDef(name="g_" + node.name,
                              args=args,
                              body=body)
        return ast.copy_location(new, node)

    def visit_If(self, node):
        """
        Canonifies if statemenets such that if the then-body assignes a local,
        so does the else-body (by simply doing a copy if necessary). Furthermore,
        this function adds a field to the if-node called 'outvars', which is the
        set of variables assigned by this node.
        """
        test = self.visit(node.test)
        locals_before = self._locals.copy()
        body = [self.visit(stmt) for stmt in node.body]
        locals_after_body = self._locals.copy()
        self._locals = locals_before.copy()
        orelse = [self.visit(stmt) for stmt in node.orelse]
        locals_after_orelse = self._locals.copy()

        all_vars = set(locals_after_body.keys()).union(set(locals_after_orelse.keys()))
        self._locals = locals_before.copy()
        outvars = []
        for var in all_vars:
            if (var not in locals_before and
                var in locals_after_body and
                var in locals_after_body): # Var added in both branches
                body_idx = self._localvar_index(var, locals_after_body)
                orelse_idx = self._localvar_index(var, locals_after_orelse)
                if body_idx > orelse_idx:
                    max_idx, not_max_idx = body_idx, orelse_idx
                else:
                    max_idx, not_max_idx = orelse_idx, body_idx
                out_var = self._localvar_name(var, max_idx)
                in_var = self._localvar_name(var, not_max_idx)
                assign = ast.Assign(targets=[ast.Name(id=out_var, ctx=ast.Store())],
                                    value=ast.Name(id=in_var, ctx=ast.Load()))
                if max_idx > body_idx:
                    body.append(assign)
                elif max_idx > orelse_idx:
                    orelse.append(assign)
                self._locals[var] = out_var
                outvars.append(out_var)
        new = ast.If(test=test, body=body, orelse=orelse)
        new._fields = new._fields + ('outvars', 'invars')
        new.outvars = outvars
        new.invars = list(locals_before.values())
        return ast.copy_location(new, node)

    def visit_Name(self, node):
        if node.id == 'free':
            id = self._new_local(node.id)
        elif node.ctx.__class__ == ast.Store:
            id = self._new_local(node.id)
        elif node.id in self._locals:
            id = self._locals[node.id]
        else:
            id = "g_" + node.id # Not a local var => global name
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


BITS = 64
def int_and_body():
    idxs = range(0, BITS) # Bits
    l_bits = ", ".join("L%s" % b for b in idxs)
    r_bits = ", ".join("R%s" % b for b in idxs)
    bits = "[%s, %s] ins 0..1" % (l_bits, r_bits)
    l = "Ls #= " + " + ".join("L%s * %s" % (b, 2**b) for b in idxs)
    r = "Rs #= " + " + ".join("R%s * %s" % (b, 2**b) for b in idxs)
    result = "Result #= " + " + ".join("L%s * R%s * %s" % (b, b, 2**b) for b in idxs)
    return ",\n    ".join([
            'no_sign(L, Ls), no_sign(R, Rs)',
            bits, l, r, result])

BUILTINS = """
?- use_module(library(clpfd)).

i_assign(Var, Var).

i_assert(t_bool(1)).

% TODO: These cuts should not be here.
i_binadd(L, R, Result) :-
    m___add__(L, [R], _, Result), !.
i_binadd(L, R, Result) :-
    m___radd__(R, [L], _, Result), !.
i_binsub(L, R, Result) :-
    m___sub__(L, [R], _, Result), !.
i_binsub(L, R, Result) :-
    m___rsub__(R, [L], _, Result), !.
i_binmult(L, R, Result) :-
    m___mult__(L, [R], _, Result), !.
i_binmult(L, R, Result) :-
    m___rmult__(R, [L], _, Result), !.
i_bindiv(L, R, Result) :-
    m___div__(L, [R], _, Result), !.
i_bindiv(L, R, Result) :-
    m___rdiv__(R, [L], _, Result), !.
i_binmod(L, R, Result) :-
    m___mod__(L, [R], _, Result), !.
i_binmod(L, R, Result) :-
    m___rmod__(R, [L], _, Result), !.
i_binpow(L, R, Result) :-
    m___pow__(L, [R], _, Result), !.
i_binpow(L, R, Result) :-
    m___rpow__(R, [L], _, Result), !.
i_binrshift(L, R, Result) :-
    m___rshift__(L, [R], _, Result), !.
i_binrshift(L, R, Result) :-
    m___rrshift__(R, [L], _, Result), !.
i_binlshift(L, R, Result) :-
    m___lshift__(L, [R], _, Result), !.
i_binlshift(L, R, Result) :-
    m___rlshift__(R, [L], _, Result), !.
i_binbitand(L, R, Result) :-
    m___and__(L, [R], _, Result), !.
i_binbitand(L, R, Result) :-
    m___rand__(R, [L], _, Result), !.

i_unaryusub(I, Result) :-
    m___neg__(I, [], _, Result).
i_unaryinvert(I, Result) :-
    m___invert__(I, [], _, Result).

i_compare(ne, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #\= R).
i_compare(eq, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #= R).
i_compare(lt, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #< R).
i_compare(gt, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #> R).
i_compare(gte, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #>= R).
i_compare(lte, t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #=< R).

i_return(Var, Var).


m___add__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L + R.
m___sub__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L - R.
m___mult__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L * R.
m___div__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L / R.
m___mod__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    L #< 0, R #< 0, !,
    Result #= -(-L mod -R).
m___mod__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L mod R.
m___pow__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L ^ R.
m___rshift__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L / (2 ^ R).
m___lshift__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L * (2 ^ R).
m___and__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    {int_and_body}.
m___neg__(t_int(I), [], _Io, t_int(-I)).
m___invert__(t_int(I), [], _Io, t_int(Result)) :-
    Result #= -I -1.


no_sign(I, Is) :-
    I #< 0, !,
    Is #= {max_bit_val} + I.
no_sign(I, I).

to_print_string([], Acc, t_str(Acc)).
to_print_string([H|T], Acc, Result) :-
    g_str(H, t_str(HStr)),
    append(Acc, HStr, NextAcc),
    to_print_string(T, NextAcc, Result).


g_print(Objects, Io, t_None) :-
    to_print_string(Objects, [], Str),
    io(List) = Io,
    append(List, [Str], Result),
    setarg(1, Io, Result).

g_repr(t_int(I), t_str(Repr)) :-
    integer(I), !,
    number_codes(I, Repr).
g_repr(t_int(_), t_str("?int")).

g_str(Object, Str) :-
    g_repr(Object, Str).


io_write([]).
io_write([H|T]) :-
    t_str(S) = H,
    string_to_list(Str, S),
    write(Str), nl,
    io_write(T).

start :-
    g_main([], Io, _) -> 
     (io(Printed) = Io, io_write(Printed))
     ; (write('Goal ''main'' failed.'), nl)
    ,
    halt.

:- style_check(-singleton).
:- style_check(-discontiguous).

""".format(int_and_body=int_and_body(), max_bit_val=2**BITS)

if __name__ == '__main__':
    main()