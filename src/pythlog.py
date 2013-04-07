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
        predicates = translate_function(node,
                                        self._allocator,
                                        self._global_symbols)
        self._predicates.extend(predicates)

    def visit_ClassDef(self, node):
        predicates = translate_class(node,
                                     self._allocator,
                                     self._global_symbols)
        self._predicates.extend(predicates)


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
        op = type(node.op).__name__.lower()
        result = self._allocator.tempvar()
        self._code.append('i_unary%s(%s, %s)' % (op, operand, result))
        return result


    def visit_BinOp(self, node):
        lhs = self.visit(node.left)
        rhs = self.visit(node.right)
        op = type(node.op).__name__.lower()
        result = self._allocator.tempvar()
        self._code.append('i_bin%s(%s, %s, %s)' % (op, lhs, rhs, result))
        return result

    def visit_Compare(self, node):
        assert len(node.ops) == 1
        assert len(node.comparators) == 1
        op = type(node.ops[0]).__name__.lower()
        lhs = self.visit(node.left)
        rhs = self.visit(node.comparators[0])
        result = self._allocator.tempvar()
        self._code.append('i_compare(%s, %s, %s, %s)' % (op, lhs, rhs, result))
        return result

    def visit_Call(self, node):
        result = self._allocator.tempvar()
        pred_args = []
        if node.implicit is not None:
            pred_args.append(self.visit(node.implicit))
        call_args = "[" + ", ".join(self.visit(a) for a in node.args) + "]"
        pred_args.extend([call_args, 'Io', result])
        self._code.append("%s(%s)" % (node.func.id, ", ".join(pred_args)))
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


def translate_function(func_node, allocator, globals):
    """
    Translates a function (described by the AST node 'func_node') into
    Prolog code. Returns a list of predicates.
    """
    code = []
    predicates = []

    if func_node.constructs is None:
        result = 'Result'
    else:
        result = 't_object(%s)' % func_node.constructs

    if func_node.args.implicit is None:
        pred_args = []
    else:
        pred_args = [func_node.args.implicit.arg]
        anno_tr = StatementTranslator(allocator, globals)
        anno_result = anno_tr.visit(func_node.args.implicit.annotation)
        code.extend(anno_tr.code())
        code.append('%s = %s' % (func_node.args.implicit.arg, anno_result))

    st = StatementTranslator(allocator, globals)
    for stmt in func_node.body:
#            print(ast.dump(stmt))
        st.visit(stmt)
    code.extend(st.code())
    predicates.extend(st.predicates())

    func_args = "[%s]" % (", ".join(a.arg for a in func_node.args.args))
    pred_args.extend([func_args, 'Io', result])
    predicates.append(generate_predicate(func_node.name,
                                         pred_args,
                                         code))
    return predicates

def translate_class(class_node, allocator, globals):
    """
    Translates a class (described by the AST node 'class_node') into Prolog
    code. Returns a list of predicates.
    """
    predicates = []
#    print(ast.dump(class_node))
    for decl in class_node.body:
        predicates.extend(translate_function(decl, allocator, globals))
    return predicates


EMPTY_CTOR_DEF = ast.FunctionDef(name='__init__',
                                 args=ast.arguments(args=[ast.arg(arg='self',
                                                                  annotation=None)],
                                                    vararg=None,
                                                    varargannotation=None,
                                                    kwonlyargs=[],
                                                    kwarg=None,
                                                    kwargannotation=None,
                                                    defaults=[],
                                                    kw_defaults=[]),
                                 body=[ast.Pass()],
                                 decorator_list=[],
                                 returns=None)

def implicit_arg_annotation(class_node):
    return ast.Call(func=ast.Name(id=class_node.name, ctx=ast.Load()),
                    args=[],
                    keywords=[],
                    starargs=None,
                    kwargs=None)

class SsaRewriter(ast.NodeTransformer):
    """
    Rewrites code to use single static assignment form. Also, fixes names of
    classes and functions such that they don't conflict with Prolog builtins.

    Furthermore, some irregularities are rewritten into a canonical form, such
    as not all classes having an explicit constructor.
    """
    def __init__(self):
        self._locals = {}
        self._class_has_ctor = False
        self._current_class_node = None # None => not in class decl
        # Constructors need some special handling
        self._currently_translating_constructor = False

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
        """
        Add the functions arguments to the dict of local variables.
        """
        newarg = self._new_local(node.arg)
        return ast.copy_location(ast.arg(arg=newarg,
                                         annotation=node.annotation),
                                 node)

    def visit_arguments(self, node):
        """
        Rewrite the argument list of a function such that it's clear if and
        argument is an "implicit" argument (the 'self' argument of methods).
        An Implicit argument is moved to a new attribute of the AST node
        called 'implicit'. Constructors are modeled by returning a new
        object, thus, constructors does not have an implicit argument.
        """
        implicit = None 
        unvisited_args = node.args
        if self._current_class_node is not None:
            unvisited_args = node.args[1:]
            if not self._currently_translating_constructor:
                implicit = self.visit(node.args[0])
                implicit.annotation = self.visit(implicit_arg_annotation(self._current_class_node))
        new = ast.arguments(args=[self.visit(a) for a in unvisited_args],
                            vararg=node.vararg,
                            varargannotation=node.varargannotation,
                            kwonlyargs=node.kwonlyargs,
                            kwarg=node.kwarg,
                            kwargannotation=node.kwargannotation,
                            defaults=node.defaults,
                            kw_defaults=node.kw_defaults)
        new._fields = new._fields + ('implicit', )
        new.implicit = implicit

        return ast.copy_location(new, node)

    def _predicate_name_for_function_name(self, node):
        """
        Get the prolog name for a python function name, taking into account
        if the function is part of a class declaration.
        """
        if self._current_class_node is not None:
            if node.name == '__init__':
                return 'g_' + self._current_class_node.name # ctor name
            else:
                return "m_" + node.name #  method name
        else:
            return "g_" + node.name # global function

    def visit_FunctionDef(self, node):
        """
        Makes the name of functions not clash with Prolog names by prefixing
        the function name with 'g_' (read: global_). Also, clears some state
        needed for translating functions.
        """
        if self._current_class_node is not None and node.name == '__init__':
            self._class_has_ctor = True
            self._currently_translating_constructor = True
            constructs = "t_" + self._current_class_node.name
        else:
            self._currently_translating_constructor = False
            constructs = None

        pred_name = self._predicate_name_for_function_name(node)
        self._locals = {} # New function started => clear locals
        args = self.visit(node.args) # Must be visited before the body
        body = [self.visit(stmt) for stmt in node.body]
        new = ast.FunctionDef(name=pred_name,
                              args=args,
                              body=body)

        new.constructs = constructs
        new._fields = new._fields + ('constructs', )
        return ast.copy_location(new, node)

    def visit_ClassDef(self, node):
        """
        Makes the name of classes not class Prolog names by predicates by
        prefixing the class name wit a 'g_' (read: global_).
        Also, add an empty constructor if no one was explicitly defined.
        """
        self._class_has_ctor = False # This may be modified when visiting the body
        self._current_class_node = node
        body = [self.visit(decl) for decl in node.body]
        if not self._class_has_ctor:
            body.append(self.visit(EMPTY_CTOR_DEF))
        self._current_class_node = None
        new = ast.ClassDef(name='t_' + node.name,
                           bases=node.bases,
                           keywords=node.keywords,
                           starargs=node.starargs,
                           kwargs=node.kwargs,
                           body=body,
                           decorator_list=node.decorator_list)
        return ast.copy_location(new, node)



    def visit_If(self, node):
        """
        Canonifies if statemenets such that if the then-body assignes a local,
        so does the else-body (by simply doing a copy if necessary). Furthermore,
        this function adds a field to the if-node called 'outvars', which is the
        set of variables assigned by this node.
        """
        # TODO: Horrible mess of code... rewrite!
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
        elif type(node.ctx) == ast.Store:
            id = self._new_local(node.id)
        elif node.id in self._locals:
            id = self._locals[node.id]
        else:
            id = "g_" + node.id # Not a local var => global name
        new = ast.Name(id=id, ctx=node.ctx)
        return ast.copy_location(new, node)

    def visit_Call(self, node):
        """
        Translates call of the type 'object.method(args)' into
        'method(implicit=object, args)'.
        """
        if type(node.func) == ast.Attribute:
            func = ast.Name(id='m_' + node.func.attr, ctx=ast.Load())
            implicit = self.visit(node.func.value)
        else:
            func = self.visit(node.func)
            implicit = None
        new = ast.Call(func=func,
                       args=[self.visit(a) for a in node.args],
                       keywords=node.keywords,
                       starargs=node.starargs,
                       kwargs=node.kwargs)
        new.implicit = implicit
        new._fields = new._fields + ('implicit', )

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
    result = "UnsignedResult #= " + " + ".join("L%s * R%s * %s" % (b, b, 2**b) for b in idxs)
    return ",\n    ".join([
            bits, l, r, result])


def int_or_body():
    idxs = range(0, BITS) # Bits
    l_bits = ", ".join("L%s" % b for b in idxs)
    r_bits = ", ".join("R%s" % b for b in idxs)
    bits = "[%s, %s] ins 0..1" % (l_bits, r_bits)
    l = "Ls #= " + " + ".join("L%s * %s" % (b, 2**b) for b in idxs)
    r = "Rs #= " + " + ".join("R%s * %s" % (b, 2**b) for b in idxs)
    result = "UnsignedResult #= " + " + ".join("(L%s + R%s - L%s * R%s) * %s" % (b, b, b, b, 2**b) for b in idxs)
    return ",\n    ".join([
            bits, l, r, result])

def int_xor_body():
    idxs = range(0, BITS) # Bits
    l_bits = ", ".join("L%s" % b for b in idxs)
    r_bits = ", ".join("R%s" % b for b in idxs)
    bits = "[%s, %s] ins 0..1" % (l_bits, r_bits)
    l = "Ls #= " + " + ".join("L%s * %s" % (b, 2**b) for b in idxs)
    r = "Rs #= " + " + ".join("R%s * %s" % (b, 2**b) for b in idxs)
    result = "UnsignedResult #= " + " + ".join("(L%s + R%s - 2 * L%s * R%s) * %s" % (b, b, b, b, 2**b) for b in idxs)
    return ",\n    ".join([
            bits, l, r, result])



BUILTINS = """
?- use_module(library(clpfd)).

i_assign(Var, Var).

i_assert(t_bool(1)).


i_binadd(L, R, Result) :-
    m___add__(L, [R], _, Result).
i_binadd(L, R, Result) :-
    m___radd__(R, [L], _, Result).
i_binsub(L, R, Result) :-
    m___sub__(L, [R], _, Result).
i_binsub(L, R, Result) :-
    m___rsub__(R, [L], _, Result).
i_binmult(L, R, Result) :-
    m___mul__(L, [R], _, Result).
i_binmult(L, R, Result) :-
    m___rmul__(R, [L], _, Result).
i_bindiv(L, R, Result) :-
    m___truediv__(L, [R], _, Result).
i_bindiv(L, R, Result) :-
    m___rtruediv__(R, [L], _, Result).
i_binfloordiv(L, R, Result) :-
    m___floordiv__(L, [R], _, Result).
i_binfloordiv(L, R, Result) :-
    m___rfloordiv__(R, [L], _, Result).
i_binmod(L, R, Result) :-
    m___mod__(L, [R], _, Result).
i_binmod(L, R, Result) :-
    m___rmod__(R, [L], _, Result).
i_binpow(L, R, Result) :-
    m___pow__(L, [R], _, Result).
i_binpow(L, R, Result) :-
    m___rpow__(R, [L], _, Result).
i_binrshift(L, R, Result) :-
    m___rshift__(L, [R], _, Result).
i_binrshift(L, R, Result) :-
    m___rrshift__(R, [L], _, Result).
i_binlshift(L, R, Result) :-
    m___lshift__(L, [R], _, Result).
i_binlshift(L, R, Result) :-
    m___rlshift__(R, [L], _, Result).
i_binbitand(L, R, Result) :-
    m___and__(L, [R], _, Result).
i_binbitand(L, R, Result) :-
    m___rand__(R, [L], _, Result).
i_binbitor(L, R, Result) :-
    m___or__(L, [R], _, Result).
i_binbitor(L, R, Result) :-
    m___ror__(R, [L], _, Result).
i_binbitxor(L, R, Result) :-
    m___xor__(L, [R], _, Result).
i_binbitxor(L, R, Result) :-
    m___rxor__(R, [L], _, Result).

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
m___mul__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    Result #= L * R.
m___rmul__(t_int(R), [t_int(L)], _Io, t_int(Result)) :-
    Result #= L * R.
m___floordiv__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
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
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_and_body},
    fix_sign(UnsignedResult, Result).
m___rand__(t_int(R), [t_int(L)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_and_body},
    fix_sign(UnsignedResult, Result).
m___or__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_or_body},
    fix_sign(UnsignedResult, Result).
m___ror__(t_int(R), [t_int(L)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_or_body},
    fix_sign(UnsignedResult, Result).
m___xor__(t_int(L), [t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_xor_body},
    fix_sign(UnsignedResult, Result).
m___rxor__(t_int(R), [t_int(L)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_xor_body},
    fix_sign(UnsignedResult, Result).
m___neg__(t_int(I), [], _Io, t_int(-I)).
m___invert__(t_int(I), [], _Io, t_int(Result)) :-
    Result #= -I -1.


no_sign(I, Is) :-
    I #< 0,
    Is #= {max_bit_val} + I.
no_sign(I, I).
fix_sign(I, I) :-
    I #< {max_bit_val} / 2.
fix_sign(I, Result) :-
    I #> {max_bit_val} / 2,
    Result #= I - {max_bit_val}.

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

""".format(int_and_body=int_and_body(),
           int_or_body=int_or_body(),
           int_xor_body=int_xor_body(),
           max_bit_val=2**BITS)

if __name__ == '__main__':
    main()