#!/usr/bin/python

import ast


class NodeTransformer(ast.NodeTransformer):
    """
    Base class with some convinience methods for instanciating nodes.
    """
    def visit(self, node):
        """
        Like the ast.NodeTransformer.visit function except for the natural
        extension of handling list.
        """
        if node is None:
            return node
        elif type(node) == list:
            return [self.visit(n) for n in node]
        elif type(node) == str:
            return node
        elif type(node) == set:
            return {self.visit(n) for n in node}
        else:
            return ast.NodeTransformer.visit(self, node)

    def copy_node(self, old_node, new_type=None, **overrides):
        """
        Copies a node and adds/overrides some attributes. Optionally
        a new type can be used for the new node.
        """
        new_node = (new_type or type(old_node))()
        new_fields = tuple(set(overrides.keys()).difference(new_node._fields))
        new_node._fields = old_node._fields + new_fields
        for attr in new_node._fields:
            if attr in overrides:
                value = overrides[attr]
            elif hasattr(old_node, attr):
                value = self.visit(getattr(old_node, attr))
            setattr(new_node, attr, value)

        return ast.copy_location(new_node, old_node)


class NodeVisitor(ast.NodeVisitor):
    def visit(self, node):
        """
        Like the ast.NodeVisitor.visit function except for the natural
        extension of handling list.
        """
        if type(node) == list:
            newlist = []
            for n in node:
                res = self.visit(n)
                if type(res) in (list, tuple):
                    newlist.extend(res)
                else:
                    newlist.append(res)
            return newlist
        elif type(node) not in (type(None), str):
            return ast.NodeTransformer.visit(self, node)


PYTHLOG_BUILTINS = {'write'}
class FindGlobalSymbols(ast.NodeVisitor):
    def __init__(self):
        self._symbols = set()

    def global_symbols(self):
        return self._symbols.union(PYTHLOG_BUILTINS).union(dir(__builtins__))

    def visit_FunctionDef(self, node):
        self._symbols.add(node.name)

    def visit_ClassDef(self, node):
        self._symbols.add(node.name)

def global_symbols(parse_tree):
    """
    Returns a set of the string representing the global symbols of the module.
    """
    symbol_finder = FindGlobalSymbols()
    symbol_finder.visit(parse_tree)
    return symbol_finder.global_symbols()

class FindAllNames(ast.NodeVisitor):
    def __init__(self):
        self.names = set()
        self.local_names = set()

    def visit_Name(self, node):
        self.names.add(node.id)

    def visit_LocalName(self, node):
        self.local_names.add(node.id)

def all_names_in(parse_tree):
    """
    Finds all names (ast.Name) in a given parse tree.
    """
    f = FindAllNames()
    f.visit(parse_tree)
    return f.names

def all_local_names_in(parse_tree):
    """
    Find all local names (LocalName) in a given parse tree.
    """
    f = FindAllNames()
    f.visit(parse_tree)
    return f.local_names

class FindInheritance(NodeVisitor):
    """
    Returns a dict (str:list[str]) representing the list of classes every class
    in the module inherits from.
    """
    def __init__(self):
        self._inheritance = {}

    def inheritance(self):
        return self._inheritance

    def visit_ClassDef(self, node):
        self._inheritance[node.name] = [b.id for b in node.bases]


class PrettyPrinter(NodeVisitor):
    def __init__(self):
        self._lines = []
        self._indent_level = 0

    def text(self, tree):
        self.visit(tree)
        return "\n".join(self._lines)

    def _line(self, text):
        self._lines.append(("  " * self._indent_level) + text)

    def _indent(self):
        self._indent_level += 1

    def _deindent(self):
        self._indent_level -= 1

    def visit_List(self, node):
        return "[" + ", ".join(self.visit(e) for e in node.elts) + "]"

    def visit_Num(self, node):
        return str(node.n)

    def visit_LocalName(self, node):
        return node.id

    def visit_GlobalName(self, node):
        return node.id

    def visit_Name(self, node):
        return node.id

    def visit_Compare(self, node):
        assert len(node.ops) == 1
        assert len(node.comparators) == 1
        op = {'Eq':'==', 'GtE':'>=', 'NotEq':'!='}[type(node.ops[0]).__name__]
        lhs = self.visit(node.left)
        rhs = self.visit(node.comparators[0])
        return "%s %s %s" % (lhs, op, rhs)

    def visit_BinOp(self, node):
        return "binop"

    def visit_While(self, node):
        test = self.visit(node.test)
        self._line('while %s:' % test)
        self._indent()
        body = self.visit(node.body)
        self._deindent()

    def visit_Attribute(self, node):
        return "%s.%s" % (self.visit(node.value), node.attr)

    def visit_Pass(self, node):
        self._line("pass")

    def visit_Assign(self, node):
        targets = ", ".join(self.visit(t) for t in node.targets)
        value = self.visit(node.value)
        self._line("%s = %s" % (targets, value))

    def visit_Expr(self, node):
        self._line(self.visit(node.value))

    def visit_Call(self, node):
        args = ", ".join(self.visit(a) for a in node.args)
        func = self.visit(node.func)
        return "%s(%s)" % (func, args)

    def visit_Return(self, node):
        self._line('return %s' % self.visit(node.value))

    def visit_Assert(self, node):
        self._line('assert %s' % self.visit(node.test))

    def visit_FunctionDef(self, node):
        arglist = ", ".join(a.arg for a in node.args.args)
        self._line('def %s(%s):' % (node.name, arglist))
        self._indent()
        self.visit(node.body)
        self._deindent()
        self._line('')

    def visit_If(self, node):
        test = self.visit(node.test)
        self._line('if %s:' % test)
        self._indent()
        if len(node.body) == 0:
            self._line('pass')
        else:
            self.visit(node.body)
        self._deindent()
        if len(node.orelse) > 0:
            self._line('else:')
            self._indent()
            self.visit(node.orelse)
            self._deindent()


def prettyprint(node):
    """Print an AST node as Python code."""
    return PrettyPrinter().text(node)


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
    def __init__(self, allocator):
        self._predicates = []
        self._allocator = allocator

    def code(self):
        return "\n\n".join(self._predicates)

    def visit_FunctionDef(self, node):
        predicates = translate_function(node,
                                        self._allocator)
        self._predicates.extend(predicates)

    def visit_ClassDef(self, node):
        predicates = translate_class(node,
                                     self._allocator)
        self._predicates.extend(predicates)


class StatementTranslator(ast.NodeVisitor):
    def __init__(self, allocator):
        self._allocator = allocator
        self._predicates = []
        self._code = []

    def _emit_list_unify_hook(self, module, node, local_vars):
        # TODO: Consider making attributed variables part of the IR and move
        # this to a seperate rewrite step.
        s = StatementTranslator(self._allocator)
        s.visit(assert_expr(node.expr, True))
        prologue = ['It = t_%s(Other)' % node.type,
                    '[%s] = Attr' % ', '.join(local_vars)]
        code = prologue + s.code()
        hook = module + ":attr_unify_hook(Attr, Other) :-\n  " + ",\n  ".join(code) + "."
        self._predicates.append(hook)

    def _emit_lazy_list_predicate(self, node_elt, result, iterator):
        s = StatementTranslator(self._allocator)
        s.visit(node_elt)
        pred_name = self._allocator.globalsym()
        sign = "%s(%s, %s)" % (pred_name, result, iterator)
        pred = sign + " :-\n  " + ",\n  ".join(s.code()) + "."
        self._predicates.append(pred)
        return pred_name

    def code(self):
        return self._code

    def predicates(self):
        """Get any additional predicates created while translating the
        statement (needed for ifs, loops, etc)."""
        return self._predicates

    # Expression nodes
    def visit_LocalName(self, node):
        return node.id

    def visit_GlobalName(self, node):
        return node.id

    def visit_Free(self, node):
        return '_'

    def visit_Bool(self, node):
        return "t_bool(%s)" % int(node.b)

    def visit_Tuple(self, node):
        elts = ", ".join(self.visit(e) for e in node.elts)
        return "t_tuple([%s])" % elts

    def visit_Num(self, node):
        return "t_int(%s)" % node.n

    def visit_Str(self, node):
        return 't_str("%s")' % node.s

    def visit_Index(self, node):
        return self.visit(node.value)

    def visit_NewObject(self, node):
        return "t_object(%s, [], _)" % node.type

    def visit_List(self, node):
        elts = ", ".join(self.visit(e) for e in node.elts)
        return "t_list([%s])" % elts

    def visit_Set(self, node):
        elts = ", ".join(self.visit(e) for e in node.elts)
        result = self._allocator.tempvar()
        self._code.append("list_to_ord_set([%s], %s)" % (elts, result))
        return "t_set(%s)" % result

    def visit_FindAll(self, node):
        result = self._allocator.tempvar()
        target = self.visit(node.target)
        s = StatementTranslator(self._allocator)
        s.visit(assert_expr(node.expr, True))
        expr = ", ".join(s._code + ['label_var(%s)' % target])
        self._code.append('findall(%s, (%s), %s)' % (target, expr, result))
        return "t_list(%s)" % result

    def _emit_next_value_for_lazy_list(self, generator, target):
        pred_name = self._allocator.globalsym()
        s = StatementTranslator(self._allocator)
        s.visit(assignment(target, callfunc('m___next__', ['Iter'])))
        if len(generator.ifs) == 1:
            s.visit(assignment('Comp', generator.ifs[0]))
            s._code.append("((Comp = t_bool(1)) -> (Result = %s) ; (%s([Iter], Io, Result)))" % (target, pred_name))
        else:
            s._code.append('Result = %s' % target)            
        head = '%s([Iter], Io, Result)' % pred_name
        code = head + " :-\n  " + ",\n  ".join(s.code()) + "."
        self._predicates.append(code)
        return pred_name

    def visit_GeneratorExp(self, node):
        # TODO: Consider doing a bit of rewriting in an earlier stage to
        # simplify this code.
        assert len(node.generators) == 1
        target = node.generators[0].target[0]
        step_iter_pred = self._emit_next_value_for_lazy_list(node.generators[0], target)
        pred_name = self._allocator.globalsym()
        pred_args = ['List', 'Iter', 'Io'] + list(all_local_names_in(node)- {target})

        s = StatementTranslator(self._allocator)
        s.visit(assignment(target, callfunc(step_iter_pred, ['Iter'])))
        s._code.append('%s \== g_StopIteration -> ([H|T] = List' % target)
        s.visit(assignment('H', node.elt))
        s._code.append('freeze(T, %s(T, %s)) ) ; List = []' % (pred_name, ", ".join(pred_args[1:])))

        head = "%s(%s)" % (pred_name, ", ".join(pred_args))
        self._predicates.append(head + " :-\n  " + ",\n  ".join(s.code()) + ".")

        result = self._allocator.tempvar()
        iter = self.visit(callfunc('g_iter', [self.visit(node.generators[0].iter)]))
        self._code.append('%s(%s, %s, %s)' % (pred_name, result, iter, ", ".join(pred_args[2:])))
        return 't_list(%s)' % result

    def visit_Pattern(self, node):
        result = self._allocator.tempvar()
        module = self._allocator.globalsym()
        local_vars = all_local_names_in(node.expr)
        self._emit_list_unify_hook(module, node, local_vars)
        self._code.append('put_attr(%s, %s, [%s])' % (result, module, ", ".join(local_vars)))
        return "t_%s(%s)" % (node.type, result)

    def visit_It(self, node):
        return "It"

    def visit_Subscript(self, node):
        result = self._allocator.tempvar()
        value = self.visit(node.value)
        slice = self.visit(node.slice)
        self._code.append('m___getitem__([%s, %s], Io, %s)' % (value, slice, result))
        return result

    def visit_UnaryOp(self, node):
        operand = self.visit(node.operand)
        op = type(node.op).__name__.lower()
        result = self._allocator.tempvar()
        self._code.append('i_unary%s(%s, %s)' % (op, operand, result))
        return result

    def visit_Attribute(self, node):
        result = self._allocator.tempvar()
        value = self.visit(node.value)
        self._code.append("i_getattr(%s, '%s', %s)" % (value, node.attr, result))
        return result

    def visit_BinOp(self, node):
        lhs = self.visit(node.left)
        rhs = self.visit(node.right)
        op = type(node.op).__name__.lower()
        result = self._allocator.tempvar()
        self._code.append('i_bin%s(%s, %s, %s)' % (op, lhs, rhs, result))
        return result

    def visit_BoolOp(self, node):
        assert len(node.values) >= 2
        values = [self.visit(v) for v in node.values]
        op = type(node.op).__name__.lower()
        result = values[0]
        for v in values[1:]:
            next_result = self._allocator.tempvar()
            self._code.append('i_bool%s(%s, %s, %s)' % (
                op, result, v, next_result))
            result = next_result
        return result
         

    def visit_Compare(self, node):
        assert len(node.ops) == 1, len(node.ops)
        assert len(node.comparators) == 1
        op = type(node.ops[0]).__name__.lower()
        lhs = self.visit(node.left)
        rhs = self.visit(node.comparators[0])
        result = self._allocator.tempvar()
        self._code.append('i_cmp%s(%s, %s, %s)' % (op, lhs, rhs, result))
        return result

    def visit_Call(self, node):
        result = self._allocator.tempvar()
        call_args = "[" + ", ".join(self.visit(a) for a in node.args) + "]"
        pred_args = [call_args, 'Io', result]
        self._code.append("%s(%s)" % (node.func.id, ", ".join(pred_args)))
        return result

    # Statement nodes

    def visit_Assert(self, node):
        test = self.visit(node.test)
        self._code.append('i_assert(%s)' % test)
        return self._code

    def visit_Assign(self, node):
        assert len(node.targets) == 1, ast.dump(node)
        value = self.visit(node.value)
        if type(node.targets[0]) == LocalName:
            target = self.visit(node.targets[0])
            self._code.append('i_assign(%s, %s)' % (target, value))
        elif type(node.targets[0]) == ast.Attribute:
            target = self.visit(node.targets[0].value)
            attr = node.targets[0].attr
            self._code.append("i_setattr(%s, '%s', %s)" % (target, attr, value))    
        elif type(node.targets[0]) == ast.Subscript:
            target = self.visit(node.targets[0].value)
            slice = self.visit(node.targets[0].slice)
            self._code.append("m___setitem__([%s, %s, %s], Io, Result)" % (target, slice, value))
        else:
            assert False, ast.dump(node)
        return self._code

    def visit_Return(self, node):
        value = self.visit(node.value)
        self._code.append('i_return(Result, %s)' % value)
        return self._code

    def visit_Expr(self, node):
        self.visit(node.value)
        return self._code

def generate_predicate(name, args, body):
    head = "%s(%s)" % (name, ", ".join(args))
    if len(body) == 0:
        return head + "."   
    suffix = " :-\n  " + ",\n  ".join(body) + "."
    return head + suffix


def translate_function(func_node, allocator):
    """
    Translates a function (described by the AST node 'func_node') into
    Prolog code. Returns a list of predicates (one function might translate
    into several predicates).
    """
    code = []
    predicates = []

    st = StatementTranslator(allocator)
    for stmt in func_node.body:
        st.visit(stmt)
    code.extend(st.code())
    predicates.extend(st.predicates())

    func_args = "[%s]" % (", ".join(a.arg for a in func_node.args.args))
    pred_args = [func_args, 'Io', 'Result']

    predicates.append(generate_predicate(func_node.name,
                                         pred_args,
                                         code))
    return predicates

def translate_class(class_node, allocator):
    """
    Translates a class (described by the AST node 'class_node') into Prolog
    code. Returns a list of predicates.
    """
    predicates = []
    for decl in class_node.body:
        if type(decl) != ast.Pass:
            predicates.extend(translate_function(decl, allocator))
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
                                 body=[],
                                 decorator_list=[],
                                 returns=None)

class NewObject(ast.expr):
    """
    Represents the ast for constructing a new object. 
    """
    def __init__(self, type):
        ast.expr.__init__(self, type=type)
        self._fields = ('type', )

class GlobalName(ast.expr):
    """
    Represents the ast for a global name (e.g., reference to a class). 
    """
    def __init__(self, **kwargs):
        ast.expr.__init__(self, **kwargs)
        self._fields = ('id', 'ctx')

class LocalName(ast.expr):
    """
    Represents the ast for a local name (local variable). 
    """
    def __init__(self, **kwargs):
        ast.expr.__init__(self, **kwargs)
        self._fields = ('id', 'ctx')

class Free(ast.expr):
    """
    Represents the ast for the keyword 'free'.
    """
    def __init__(self):
        ast.expr.__init__(self)

class Match(ast.expr_context):
    """
    Plays the same role as ast.Load and ast.Store but with different semantics.
    If the variable is (partly) bound, then will be matched. If the variable doesn't
    exists in the scope, it is instroduced as a new free variable and then matched.
    """
    def __init__(self):
        ast.expr_context.__init__(self)

class MatchStmt(ast.stmt):
    """
    Match statement node, e.g., x == 9, foo(i) <= bar(z), etc.
    """
    def __init__(self, left=None, op=None, right=None):
        ast.stmt.__init__(self, left=left, op=op, right=right)
        self._fields = ('left', 'op', 'right')
        

class Assign(ast.Assign):
    """
    Assignment of the style x = 9.
    """
    def __init__(self, targets=None, value=None):
        ast.Assign.__init__(self, targets=targets, value=value)

class It(ast.expr):
    """
    Represents the ast for the keyword 'it'.
    """
    def __init__(self):
        ast.expr.__init__(self) 
        
class Pattern(ast.expr):
    """
    Represents the ast for a pattern literal, e.g., [1 in it].
    """
    def __init__(self, type, expr):
        ast.expr.__init__(self, type=type, expr=expr)
        self._fields = ('type', 'expr')

class FindAll(ast.expr):
    """
    Represents the ast for a findall-expression ([all x in foo])
    """
    def __init__(self, target, expr):
        ast.expr.__init__(self, target=target, expr=expr)
        self._fields = ('target', 'expr')

class Bool(ast.expr):
    """
    Represents the literals 'True' and 'False'.
    """
    def __init__(self, b):
        ast.expr.__init__(self, b=b)
        self._fields = ('b', )

def maxmin(x, y):
    if x > y:
        return x, y
    return y, x

class RewriteMatchAssignment(NodeTransformer):
    def __init__(self):
        self._locals_and_args = set()
        self._body = []

    def visit_FunctionDef(self, node):
        self._locals_and_args = set(a.arg for a in node.args.args)
        self._body = []
        for stmt in node.body:
            self._body.append(self.visit(stmt))
        return self.copy_node(node, body=self._body)

    def visit_LocalName(self, node):
        self._locals_and_args.add(node.id)
        return node

    def visit_Assign(self, node):
        if not (type(node.value) == GlobalName and node.value.id == 'uninitialized'):
            self.visit(node.targets[0])
        return node

    def visit_MatchStmt(self, node):
        matched_vars = set()
        class FindVars(NodeTransformer):
            def visit_LocalName(self, node):
                if type(node.ctx) == Match:
                    matched_vars.add(node.id)
                return node
        FindVars().visit(node)
        introduced_vars = matched_vars - self._locals_and_args
        self._locals_and_args.update(introduced_vars)
        for var in introduced_vars:
            self._body.append(assignment(var, Free()))
        return assert_op(node.left, node.op, node.right)

class SsaRewriter(NodeTransformer):
    """
    Rewrites code to use single static assignment form.
    """
    def __init__(self, allocator):
        self._allocator = allocator
        self._locals = {}
        self._extra_predicates = []
        self._loaded_vars = {}
        self._stored_vars = {}

    def _localvar_index(self, id, locs):
        return int(locs[id][len(id) + 1:])

    def _localvar_name(self, id, idx):
        return "L%s%s" % (id, idx)

    def _register_var_used(self, key, ssa_var):
        self._loaded_vars[key] = ssa_var

    def _new_local(self, id):
        if id in self._locals:
            index = self._localvar_index(id, self._locals) + 1
        else:
            index = 0
        next_id = self._localvar_name(id, index)
        self._locals[id] = next_id
        return next_id

    def _checkpoint_rw_vars(self):
        checkpoint = self._loaded_vars, self._stored_vars
        self._loaded_vars = {}
        self._stored_vars = {}
        return checkpoint

    def _restore_checkpoint_rw_vars(self, checkpoint):
        r, w = checkpoint
        r.update(self._loaded_vars)
        w.update(self._stored_vars)
        self._loaded_vars = r
        self._stored_vars = w

    def visit_Module(self, node):
        body = self.visit(node.body)
        return self.copy_node(node, body=body + self._extra_predicates)

    def visit_arg(self, node):
        """
        Add the functions arguments to the dict of local variables.
        """
        newarg = self._new_local(node.arg)
        return self.copy_node(node, arg=newarg)

    def visit_FunctionDef(self, node):
        self._locals = {} # New function started => clear locals
        args = self.visit(node.args) # Must be visited before the body
        body = [self.visit(stmt) for stmt in node.body]
        return self.copy_node(node, args=args, body=body)

    def _extract_expression_to_function(self, node):
        """
        Extracts the expression described by the ast node 'node' into
        a new function. Returns a 'Call' ast node that invokes the extracted
        function such that 'node' can be replaced by the returned node without
        changing the behaviour of the function containing 'node'.
        """
        checkpoint = self._checkpoint_rw_vars()
        return_stmt = ast.Return(value=self.visit(node))
        func_name = self._allocator.globalsym()
        arg_names = list(self._loaded_vars.values()) + list(self._stored_vars.values())

        func = function_def(func_name, arg_names, [return_stmt])
        self._extra_predicates.append(func)
        self._restore_checkpoint_rw_vars(checkpoint)
        return function_call(func_name, arg_names)

    def visit_comprehension(self, node):
        # Needs special handling since target is a ast.Name(..., ast.Store()).
        # Otherwise comprehension target can't be easily mapped to the
        # comprehension expression (GeneratorExp.elt).
        return ast.comprehension(target=[self._locals[node.target.id]],
                                 iter=self.visit(node.iter),
                                 ifs=self.visit(node.ifs))

    def visit_While(self, node):
        """
        Rewrites a while statement into a call to a recursive function. This
        rewrite emits two new functions. One is the recursive function that
        implements the body of the while; the other is the condition of the
        while.
        The recursive function has three sets/kinds of arguments:
            1. local variables read in body of the while
            2. local variables modified in the body of the while ("accumulators")
            3. the final result of the modified local variables.
        """
        # TODO: The current implementation causes unecessary many checks of the
        # loop condition.
        checkpoint = self._checkpoint_rw_vars()
        locals_before = self._locals.copy()
        test = self._extract_expression_to_function(node.test)
        body = [self.visit(stmt) for stmt in node.body]

        in_names = list(locals_before[k] for k in self._loaded_vars.keys())
        out_names = list(self._stored_vars.values())
        acc_names = ["Acc" + a for a in out_names]
        final_names = ["Final" + a for a in out_names]
        func_name = self._allocator.globalsym() + "while"
    
        def_names = in_names + acc_names + final_names
        end_body = [assignment(d, s) for d, s in zip(final_names, acc_names)]
        func = function_def(func_name,
                            def_names,
                            [assert_expr(test, False)] + end_body)
        self._extra_predicates.append(func)

        recursive_args = [self._stored_vars.get(k, v) for k, v in self._loaded_vars.items()] + out_names + final_names
        recursive_call = function_call(func_name, recursive_args)

        func = function_def(func_name,
                            def_names,
                            [assert_expr(test, True)] + body + [recursive_call])
        self._extra_predicates.append(func)


        call_names = in_names + acc_names + out_names
        self._restore_checkpoint_rw_vars(checkpoint)
        return function_call(func_name, call_names)

    def visit_If(self, node):
        """
        TODO: write this
        """
        # TODO: Horrible mess of code... rewrite!
        test = self.visit(node.test)

        checkpoint = self._checkpoint_rw_vars()
        locals_before = self._locals.copy()
        body = [self.visit(stmt) for stmt in node.body]
        locals_after_body = self._locals.copy()

        self._locals = locals_before.copy()
        orelse = [self.visit(stmt) for stmt in node.orelse]
        locals_after_orelse = self._locals.copy()
        self._locals = locals_before.copy()

        all_vars = set(self._loaded_vars.keys()).union(self._stored_vars.keys())
        outvars = set()
        invars = set(self._loaded_vars.values()) # Variables read by the bodies.
        for var in all_vars:
            body_idx = self._localvar_index(var, locals_after_body)
            orelse_idx = self._localvar_index(var, locals_after_orelse)
            max_idx, min_idx = maxmin(body_idx, orelse_idx)

            out_var = self._localvar_name(var, max_idx)
            in_var = self._localvar_name(var, min_idx)
            assign = assignment(out_var, in_var)
            self._register_var_used(var, in_var)
            if max_idx > body_idx:
                body.append(assign)
            elif max_idx > orelse_idx:
                orelse.append(assign)
            self._locals[var] = out_var
            if max_idx != body_idx:
                outvars.add(out_var)
                invars.add(in_var)
        func_name = self._allocator.globalsym() + "if"
        expr_var = self._allocator.tempvar()
        arg_names = [expr_var] + list(invars) + list(outvars)
        then_func = function_def(func_name, arg_names, [assert_expr(expr_var, True)] + body)
        else_func = function_def(func_name, arg_names, [assert_expr(expr_var, False)] + orelse)

        self._extra_predicates.append(then_func)
        self._extra_predicates.append(else_func)

        self._restore_checkpoint_rw_vars(checkpoint)
        return function_call(func_name, [test] + arg_names[1:])

    def visit_LocalName(self, node):
        if type(node.ctx) == ast.Store:
            id = self._new_local(node.id)
            self._stored_vars[node.id] = id
        elif node.id in self._locals:
            id = self._locals[node.id]
            self._loaded_vars[node.id] = id
        else:
            assert False, node.id
        return self.copy_node(node, id=id)

    def visit_Assign(self, node):
        # NOTE: 'value' and 'targets' must be visited in this order due to
        # side-effects in the visit-method.
        value = self.visit(node.value)
        targets = [self.visit(t) for t in node.targets]
        return self.copy_node(node, targets=targets, value=value)


def string_to_ast_node(arg, store=False):
    if type(arg) == str:
        ctx = ast.Store() if store else ast.Load()
        return LocalName(id=arg, ctx=ctx)
    return arg

def function_call(func_name, args):
    """
    Calls 'func_name' (string) with arguments 'args' (list of string or ast
    nodes).
    """
    return ast.Call(func=GlobalName(id=func_name, ctx=ast.Load()),
                 args=[string_to_ast_node(arg) for arg in args],
                 keywords=[],
                 starargs=None,
                 kwargs=None)


def function_def(func_name, arg_names, body):
    """
    Creates a function definition ast object. Function is named 'func_name'
    and it takes 'arg_names' arguments (a list of strings). The argument
    'body' is a list of ast nodes.
    """
    arg_list = [ast.arg(arg=name, annotation=None) for name in arg_names]
    args = ast.arguments(args=arg_list,
                         vararg=None,
                         varargannotation=None,
                         kwonlyargs=[],
                         kwarg=None,
                         kwargannotation=None,
                         defaults=[],
                         kw_defaults=[])
    return ast.FunctionDef(name=func_name,
                           args=args,
                           body=body,
                           decorator_list=[],
                           returns=None)


def assert_self_type(type_name):
    """
    Get the ast for assert the type of 'self'. Implemented as:
        assert isinstance(self, type_name)
    """
    return [ast.Assert(test=ast.Call(func=ast.Name(id='isinstance', ctx=ast.Load()), args=[ast.Name(id='self', ctx=ast.Load()), ast.Name(id=type_name, ctx=ast.Load())], keywords=[], starargs=None, kwargs=None), msg=None)]

UNINITIALIZED = GlobalName(id='uninitialized', ctx=ast.Load())
def assign_uninitialized(var_name):
    """
    Get the ast for assigning 'var_name' to 'uninitialized'. Implemented as:
        var_name = uninitialized
    """
    # 'uninitialized' is guaranteed to be a symbol that does not refer to
    # anything because the SymbolResolver prefixes all global symbols with
    # either g_ or m_.
    return assignment(var_name, value=UNINITIALIZED)

def callfunc(funcname, arglist):
    args = [string_to_ast_node(a) for a in arglist]
    return ast.Call(func=string_to_ast_node(funcname),
                    args=args,
                    keywords=[],
                    starargs=None,
                    kwargs=None)

def assignment(dst, value):
    """
    Get the ast for assigning 'value' to 'dst' (ast node or
    string). If strings are provided, then they are wrapped in ast node
    LocalName.
    """
    return Assign(targets=[string_to_ast_node(dst, store=True)],
                  value=string_to_ast_node(value))

def assert_op(lhs, op, rhs):
    """
    Assert that 'lhs' has a certain relation 'rhs' as specified by
    'op', e.g., equal, not equal, less than, etc.
    """
    return ast.Assert(test=ast.Compare(left=lhs, ops=[op], comparators=[rhs]), msg=None)

def assert_equal(lhs, rhs):
    """
    Assert that 'lhs' is equal to 'rhs'.
    """
    return assert_op(lhs, ast.Eq(), rhs)

def assert_expr(expr, true_false):
    """
    Assert that a expression (a ast node or variable given a string) has
    either value 'True' or value 'False'.
    """
    if type(expr) == str:
        expr = LocalName(id=expr, ctx=ast.Load())
    return assert_equal(expr, Bool(b=true_false))

def ctor_return_stmt():
    """
    Get the ast for the return statement used when translating constructor
    into normal function.
    """ 
    return ast.Return(value=LocalName(id='self', ctx=ast.Load()))

def ctor_self_init(class_name):
    """
    Get the ast for the initialization of 'self' in the constructor.
    """
    return assignment('self', NewObject(class_name))

class SymbolResolver(NodeTransformer):
    """
    Fixes names of classes and functions such that they don't conflict
    with Prolog builtins. Method calls are rewritten into normal function calls,
    and an 'assert type(self) == CorrectType' is added to method bodies. 

    Furthermore, some irregularities are rewritten into a canonical form, such
    as not all classes having an explicit constructor.
    """
    def __init__(self, globals):
        self._globals = globals
        self._current_class_def = None
        self._class_has_ctor = False
        self._locals_and_args = set()
        self._in_ctor = False

    def visit_Name(self, node):
        if node.id == 'free':
            return Free()
        elif node.id in ('True', 'False'):
            return Bool(b=eval(node.id))
        # TODO: Global symbol might be chosen instead of local when there is a
        # local symbol and a global symbol wit the same name. This is wrong.
        if node.id in self._globals:
            return GlobalName(id='g_' + node.id, ctx=node.ctx)
            
        self._locals_and_args.add(node.id)
        return LocalName(id=node.id, ctx=node.ctx)

    def visit_FunctionDef(self, node):
        self._locals_and_args = set()
        self._in_ctor = self._current_class_def is not None and node.name == '__init__'
        self._class_has_ctor |= self._in_ctor

        visited_body = self.visit(node.body) # Populates self._locals_and_args
        local_vars = self._locals_and_args - set(a.arg for a in node.args.args) - {'free'}
        var_inits = [assign_uninitialized(v) for v in local_vars]
        body = var_inits + visited_body
        if self._current_class_def is not None:
            if node.name == '__init__':
                new_args = self.copy_node(node.args, args=node.args.args[1:])
                name = 'g_' + self._current_class_def.name
                return self.copy_node(node,
                                      name=name,
                                      args=new_args,
                                      body=[ctor_self_init(name)] + body + [ctor_return_stmt()])
            else:
                assert_types = self.visit(assert_self_type(self._current_class_def.name))
                return self.copy_node(node,
                                      name='m_' + node.name,
                                      body=assert_types + body)
        else:
            return self.copy_node(node,
                                  name='g_' + node.name,
                                  body=body)

    def visit_ClassDef(self, node):
        self._current_class_def = node
        self._class_has_ctor = False
        new_node = self.copy_node(node, name='g_' + node.name)

        if not self._class_has_ctor:
            new_node.body.append(self.visit(EMPTY_CTOR_DEF))

        self._current_class_def = None
        return new_node

    def visit_Call(self, node):
        """
        Translates call of the type 'object.method(a, b, c, ...)' into
        'm_method(object, a, b, c, ...)'.
        """
        if type(node.func) == ast.Attribute:
            self_arg = [self.visit(node.func.value)]
            func = GlobalName(id='m_' + node.func.attr, ctx=ast.Load())
        else:
            self_arg = []
            func = self.visit(node.func)
        return self.copy_node(node, func=func, args=self_arg + self.visit(node.args))

class SingleReturnRewriter(NodeTransformer):
    def __init__(self):
        self._return_seen = False

    def visit_stmts(self, node):
        uncond = []
        cond = []
        add_to = uncond
        for n in node:
            res = self.visit(n)
            if type(res) in (list, tuple):
                add_to.extend(res)
            else:
                add_to.append(res)
            if type(n) == ast.If:
                add_to = cond

        if len(cond) > 0:
            uncond.append(ast.If(test=ast.Compare(left=LocalName(id='0has_returned', ctx=ast.Load()), ops=[ast.Eq()], comparators=[ast.Num(n=0)]), body=cond, orelse=[]))
        return uncond

    def visit_Return(self, node):
        self._return_seen = True
        return [assignment('0return', node.value),
                assignment('0has_returned', ast.Num(n=1))]

    def visit_If(self, node):
        return self.copy_node(node,
                              body=self.visit_stmts(node.body),
                              orelse=self.visit_stmts(node.orelse))

    def visit_FunctionDef(self, node):
        self._return_seen = False

        body = [assignment('0has_returned', ast.Num(n=0)),
                assign_uninitialized('0return')]
        body.extend(self.visit_stmts(node.body))

        if self._return_seen:
            return_stmt = [ast.Return(value=LocalName(id='0return', ctx=ast.Load()))]
        else:
            return_stmt = [ast.Return(value=GlobalName(id='g_None', ctx=ast.Load()))]

        new = self.copy_node(node, body=body + return_stmt)
        return new

class ArgListMatchToAssert(NodeTransformer):
    def __init__(self, globals):
        self._globals = globals
        self._func_prolog = []
        self._current_args = []

    def visit_arg(self, node):
        if node.annotation is None:
            return node

        new_free_vars = all_names_in(node.annotation) - self._globals - self._current_args
        for var in new_free_vars:
            self._func_prolog.append(assignment(var, ast.Name('free', ast.Load())))
        self._func_prolog.append(assert_equal(ast.Name(node.arg, ast.Load()), node.annotation))

        return self.copy_node(node, annotation=None)

    def visit_FunctionDef(self, node):
        self._current_args = set(a.arg for a in node.args.args)
        self._func_prolog = []
        args = self.visit(node.args)
        return self.copy_node(node, args=args, body=self._func_prolog + node.body)


def is_pattern_expr(node):
    ok = (isinstance(node, ast.Compare) or isinstance(node, ast.BoolOp))
    return ok and ("it" in all_names_in(node))

class IdentifyPatternLiterals(NodeTransformer):
    def __init__(self, globals):
        pass

    def visit_Name(self, node):
        if node.id == 'it':
            return It()
        return node

    def visit_List(self, node):
        if len(node.elts) == 1 and is_pattern_expr(node.elts[0]):
            return Pattern(type='list', expr=self.visit(node.elts[0]))
        return self.copy_node(node)

    def visit_Call(self, node):
        if type(node.func) == ast.Name and len(node.args) == 1 and is_pattern_expr(node.args[0]):
            return Pattern(type=node.func.id, expr=self.visit(node.args[0]))
        return self.copy_node(node)


class ForToWhileLoop(NodeTransformer):
    def __init__(self):
        self._counter = 0

    def visit_For(self, node):
        iter_name = '%sfor_iter' % self._counter
        assign_iter = assignment(ast.Name(iter_name, ast.Store()),
                                 callfunc(ast.Name('iter', ast.Load()),
                                          [node.iter]))
        def next_value(): # Have to do it like this to avoid sharing of nodes
            return assignment(node.target,
                              callfunc(ast.Name('next', ast.Load()),
                                       [ast.Name(iter_name, ast.Load())]))
        body = node.body + [next_value()]
        load_target = ast.Name(node.target.id, ast.Load())
        test = ast.Compare(left=load_target,
                           ops=[ast.NotEq()],
                           comparators=[ast.Name(id='StopIteration', ctx=ast.Load())])

        self._counter += 1
        return [assign_iter, next_value(), ast.While(test=test, body=body, orelse=[])]

class IdentifyFindallExpressions(NodeTransformer):
    def visit_List(self, node):
        # This massive if finds [__findallexpr__ and ...]
        if (len(node.elts) == 1 and
            type(node.elts[0]) == ast.BoolOp and
            type(node.elts[0].op) == ast.And and
            type(node.elts[0].values[0]) == ast.Name and
            node.elts[0].values[0].id == '__findallexpr__'):
            values = node.elts[0].values[1:]
            assert type(values[0]) == ast.Compare
            assert type(values[0].ops[0]) == ast.In
            target = ast.Name(values[0].left.id, ast.Store())
            left = values[0].comparators[0]
            ops = values[0].ops[1:]
            comparators = values[0].comparators[1:]
            values[0] = ast.Compare(left=left, ops=ops, comparators=comparators)
            return FindAll(target=target, expr=values[0])
        else:
            return self.copy_node(node, elts=self.visit(node.elts))


class ReplaceLoadWith(NodeTransformer):
    def __init__(self, node):
        self._node = node
    def visit_Load(self, _node):
        return self._node

class IdentifyMatchStmts(NodeTransformer):
    def visit_Expr(self, node):
        if (type(node.value) == ast.Compare):
            op = node.value.ops[0]
            left = ReplaceLoadWith(Match()).visit(node.value.left)
            right = ReplaceLoadWith(Match()).visit(node.value.comparators[0]) 
            return MatchStmt(left=left, op=op, right=right)
        else:
            return node

class IdentifyImplicitFreeVars(NodeTransformer):
    def __init__(self):
        self._current_func_body = []

    def visit_FunctionDef(self, node):
        self._current_func_body = []
        for stmt in node.body:
            trans_stmt = self.visit(stmt)
            self._current_func_body.append(trans_stmt)
        return self.copy_node(node, body=self._current_func_body)

    def visit_Attribute(self, node):
        # Finds __newfreevar__.whatever
        if type(node.value) == ast.Name and node.value.id == '__newfreevar__':
            assign_stmt = Assign(targets=[ast.Name(node.attr, ast.Store())],
                                 value=Free())
            self._current_func_body.append(assign_stmt)
            return ast.Name(node.attr, ast.Load())
        else:
            return node

def rewrite_match_assignment(parse_tree):
    """
    rewrites match assignments (e.g., y + x + 2 = 4) to
        y = free
        assert y + x + 2 == 4
    where y is a previously unassigned variable and x is an assigned variable.
    """
    return RewriteMatchAssignment().visit(parse_tree)

def ssa_form(parse_tree, allocator):
    """Rewrite functions into static single assignment form"""
    return SsaRewriter(allocator).visit(parse_tree)

def define_type_dependent_builtins(parse_tree):
    """
    Defines builtin functions that are dependent on the defined types.
    These functions are:
      - isinstance
    """
    i = FindInheritance()
    i.visit(parse_tree)
    inheritance = i.inheritance()
    templ = """
def isinstance(obj, type_object):
    assert type(obj) == %s
    return type_object in [%s]
    """
    extras = []
    for cls, supers in inheritance.items():
        extras.append(ast.parse(templ % (cls, ", ".join([cls] + supers))))

    return ast.Module(body=parse_tree.body + extras)

def for_to_while_loop(parse_tree):
    """Rewrites for loops into while loops."""
    return ForToWhileLoop().visit(parse_tree)

def resolve_global_symbols(parse_tree, global_symbols):
    """
    Rename classes and functions such that they don't conflict with prolog
    builtins.

    Also, unifies function and methods (including constructors/__init__)
    calls/definintion, such that the only distinctiong is the name (functions
    always start with 'g_', method always start with 'm_', and the name of
    constructor is the name of the class). Method calls are rewritten into
    normal function calls, and an 'assert type(self) == CorrectType' is added
    to method bodies.

    Furthermore, some irregularities are rewritten into a canonical form, such
    as not all classes having an explicit constructor.
    """
    return SymbolResolver(global_symbols).visit(parse_tree)

def single_return_point(parse_tree):
    """
    Rewrites functions with multiple return statements to only having one
    return statement by adding if:s and a new boolean variable saying if any
    more statements should be executed (if the function is "done"). Example:
        def foo(a):
            if a == 1:
                return 0
            return 1
    is rewritten to:
        def foo(a):
            has_returned = 0
            if a == 1:
                has_returned = 1
                return_value = 0
            if has_returned == 0:
                has_returned = 1
                return_value = 1
            return return_value
    This rewrite is necessary as Prolog do not have early exits from
    predicates.
    """
    return SingleReturnRewriter().visit(parse_tree)

def arg_list_match_to_assert(parse_tree, global_symbols):
    """
    Rewrites
        func(x: Class(a, b)): pass
    to
        func(x): a = free; b = free; assert x == Class(a, b)
    """
    return ArgListMatchToAssert(global_symbols).visit(parse_tree)

def identify_pattern_literals(parse_tree, global_symbols):
    """
    Identifies pattern literals (e.g., [8 in it]) such that they
    get a dedicated ast node.
    """
    return IdentifyPatternLiterals(global_symbols).visit(parse_tree)

def identify_findall_expression(parse_tree):
    """
    Identifies a magic sequence of tokens that the preprocessor emits for
    findall-expressions and instantiates dedicated ast nodes for findall-
    expressions.
    """
    return IdentifyFindallExpressions().visit(parse_tree)

def identify_match_stmts(parse_tree):
    """
    Identifies statements of the kind "x == 2", "x + 5 < z", "foo(a, b) != c", etc
    (that is ast.Expr(ast.Compare(...)) nodes). These statements are rewritten into
    MatchStmt nodes to be rewritten into ast.Assert in later stages.
    """
    return IdentifyMatchStmts().visit(parse_tree)

def identify_implicit_free_vars(parse_tree):
    """
    Identifies a magic sequence of tokens that the preprocessor emits for
    implicitly defined free variables (!var).
    """
    return IdentifyImplicitFreeVars().visit(parse_tree)

def preprocess(text):
    """
    Rewrites the sequence:
       [all foo in bar]
    to:
       [__findallexpr__ and foo in bar]
    which is later identified as a findall-expression after full parsing.
    """
    from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
    from io import BytesIO
    result = []
    last_was_bracket = False
    g = tokenize(BytesIO(text.encode('utf-8')).readline) # tokenize the string
    for toknum, tokval, _, _, _  in g:
        if last_was_bracket and toknum == NAME and tokval == 'all':
            result.append((NAME, '__findallexpr__ and'))
        elif tokval == '!':
            result.append((NAME, '__newfreevar__.'))
        else:
            result.append((toknum, tokval))
        last_was_bracket = (toknum == OP and tokval == '[')
    return untokenize(result).decode('utf-8')

def compile_module(module_code):
    allocator = Allocator()
    pp_code = preprocess(module_code)
    parse_tree = ast.parse(pp_code)
    global_syms = global_symbols(parse_tree)
    parse_tree = identify_match_stmts(parse_tree)
    parse_tree = identify_findall_expression(parse_tree)
    parse_tree = identify_implicit_free_vars(parse_tree)
    parse_tree = define_type_dependent_builtins(parse_tree)
    parse_tree = for_to_while_loop(parse_tree)
    parse_tree = arg_list_match_to_assert(parse_tree, global_syms)
    parse_tree = identify_pattern_literals(parse_tree, global_syms)
    parse_tree = resolve_global_symbols(parse_tree, global_syms)
    parse_tree = rewrite_match_assignment(parse_tree)
    parse_tree = single_return_point(parse_tree)
    parse_tree = ssa_form(parse_tree, allocator)

    compiler = ModuleTranslator(allocator)
    compiler.visit(parse_tree)
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
?- use_module(library(ordsets)).

i_assign(Var, Var).

i_assert(t_bool(1)).


i_binadd(L, R, Result) :-
    m___add__([L, R], _, Result).
i_binadd(L, R, Result) :-
    m___radd__([R, L], _, Result).
i_binsub(L, R, Result) :-
    m___sub__([L, R], _, Result).
i_binsub(L, R, Result) :-
    m___rsub__([R, L], _, Result).
i_binmult(L, R, Result) :-
    m___mul__([L, R], _, Result).
i_binmult(L, R, Result) :-
    m___rmul__([R, L], _, Result).
i_bindiv(L, R, Result) :-
    m___truediv__([L, R], _, Result).
i_bindiv(L, R, Result) :-
    m___rtruediv__([R, L], _, Result).
i_binfloordiv(L, R, Result) :-
    m___floordiv__([L, R], _, Result).
i_binfloordiv(L, R, Result) :-
    m___rfloordiv__([R, L], _, Result).
i_binmod(L, R, Result) :-
    m___mod__([L, R], _, Result).
i_binmod(L, R, Result) :-
    m___rmod__([R, L], _, Result).
i_binpow(L, R, Result) :-
    m___pow__([L, R], _, Result).
i_binpow(L, R, Result) :-
    m___rpow__([R, L], _, Result).
i_binrshift(L, R, Result) :-
    m___rshift__([L, R], _, Result).
i_binrshift(L, R, Result) :-
    m___rrshift__([R, L], _, Result).
i_binlshift(L, R, Result) :-
    m___lshift__([L, R], _, Result).
i_binlshift(L, R, Result) :-
    m___rlshift__([R, L], _, Result).
i_binbitand(L, R, Result) :-
    m___and__([L, R], _, Result).
i_binbitand(L, R, Result) :-
    m___rand__([R, L], _, Result).
i_binbitor(L, R, Result) :-
    m___or__([L, R], _, Result).
i_binbitor(L, R, Result) :-
    m___ror__([R, L], _, Result).
i_binbitxor(L, R, Result) :-
    m___xor__([L, R], _, Result).
i_binbitxor(L, R, Result) :-
    m___rxor__([R, L], _, Result).
i_boolor(t_bool(1), t_bool(_), t_bool(1)).
i_boolor(t_bool(_), t_bool(1), t_bool(1)).
i_booland(t_bool(1), t_bool(1), t_bool(1)).
i_booland(t_bool(_), t_bool(0), t_bool(0)).
i_booland(t_bool(0), t_bool(_), t_bool(0)).

i_unaryusub(I, Result) :-
    m___neg__([I], _, Result).
i_unaryinvert(I, Result) :-
    m___invert__([I], _, Result).
i_unarynot(t_bool(0), t_bool(1)).
i_unarynot(t_bool(1), t_bool(0)).

i_cmpne(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #\= R).
i_cmpeq(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #= R).
i_cmpeq(O, O, t_bool(1)) :-
    O \= t_int(_).
i_cmpeq(L, R, t_bool(0)) :-
    L \= R. % TODO: reify unification to boolean result!
i_cmpnoteq(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #\= R).
i_cmpnoteq(O, O, t_bool(0)).
i_cmpnoteq(L, R, t_bool(1)) :-
    L \= R. % TODO: reify unification to boolean result!
i_cmplt(Lhs, Rhs, Result) :-
    m___lt__([Lhs, Rhs], _Io, Result).
i_cmpgt(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #> R).
i_cmpgte(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #>= R).
i_cmplte(Lhs, Rhs, Result) :-
    m___le__([Lhs, Rhs], _Io, Result).

i_cmplte(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #=< R).
i_cmpin(Object0, Object1, Result) :-
    m___contains__([Object1, Object0], _Io, Result).

m___contains__([t_range(Start, Stop, Step), t_int(I)], _Io, t_bool(R)) :-
    R #<==> (I #< Stop #/\ I #>= Start #/\ Start + Step * _ #= I).
m___contains__([t_list(Elts), Object], _Io, t_bool(1)) :-
    nth0(_, Elts, Object).
m___contains__([t_list(Elts), Object], _Io, t_bool(0)) :-
    not(nth0(_, Elts, Object)).
m___contains__([t_str(Full), t_str(Sub)], _Io, t_bool(R)) :-
    append(_, Sub, T0), append(T0, _, Full), !, R = 1.
m___contains__([t_str(_), t_str(_)], _Io, t_bool(0)).
m___contains__([t_tuple(Elts), Object], _Io, t_bool(R)) :-
    nth0(_, Elts, Object), R = 1.
m___contains__([t_tuple(_), _], _Io, t_bool(0)).
m___contains__([t_set(Elts), Object], _Io, t_bool(R)) :-
    ord_memberchk(Object, Elts), !, R = 1.
m___contains__([t_set(_), _], _Io, t_bool(0)).


i_return(Var, Var).

% TODO: Representing __dict__ like a list of key=value-pair is bad for performance.
i_getattr(t_object(_Type, Attrs, _Ref), Attr, Value) :-
    nth0(_, Attrs, Attr=Value), !.

i_setattr(Object, Attr, Value) :-
    Object = t_object(_Type, Attrs, _Ref),
    update_attr(0, Attr, Value, Attrs, NewAttrs),
    setarg(2, Object, NewAttrs).

update_attr(1, _, _, [], []).
update_attr(0, Name, Value, [], [Name=Value]).
update_attr(IsAssigned, Name, Value, [AttrName=AttrValue|Attrs], [AttrName=UpdatedValue|NewAttrs]) :-
    (Name == AttrName ->
        (UpdatedValue=Value, NextIsAssigned = 1) ;
        (UpdatedValue=AttrValue, NextIsAssigned = IsAssigned)
    ),
    update_attr(NextIsAssigned, Name, Value, Attrs, NewAttrs).

m___add__([t_str(L), t_str(R)], _, t_str(Result)) :-
    append(L, R, Result).
m___add__([t_list(L), t_list(R)], _, t_list(Result)) :-
    append(L, R, Result).
m___add__([t_tuple(L), t_tuple(R)], _, t_tuple(Result)) :-
    append(L, R, Result).
m___add__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L + R.
m___radd__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___sub__([t_str(L), t_str(R)], _, t_str(Result)) :-
    append(Result, R, L).
m___sub__([t_list(L), t_list(R)], _, t_list(Result)) :-
    append(Result, R, L).
m___sub__([t_tuple(L), t_tuple(R)], _, t_tuple(Result)) :-
    append(Result, R, L).
m___sub__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L - R.
m___rsub__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___mul__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L * R.
m___rmul__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___floordiv__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L / R.
m___rfloordiv__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___mod__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    L #< 0, R #< 0, !,
    Result #= -(-L mod -R).
m___mod__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L mod R.
m___rmod__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___pow__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L ^ R.
m___rpow__([u, u], _Io, u).
m___rshift__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L / (2 ^ R).
m___rrshift__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___lshift__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L * (2 ^ R).
m___rlshift__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___and__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_and_body},
    fix_sign(UnsignedResult, Result).
m___rand__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___or__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_or_body},
    fix_sign(UnsignedResult, Result).
m___ror__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___xor__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_xor_body},
    fix_sign(UnsignedResult, Result).
m___rxor__([u, u], _Io, u). % Place holder to get fail instead of exception.
m___neg__([t_int(I)], _Io, t_int(Neg)) :-
    Neg #= -I.
m___invert__([t_int(I)], _Io, t_int(Result)) :-
    Result #= -I -1.
m___invert__([t_str(InChars)], _Io, t_str(OutChars)) :-
    questionmark_to_freevar(InChars, OutChars).

m___lt__([t_int(L), t_int(R)], _Io, t_bool(Result)) :-
    Result #<==> (L #< R).
m___lt__([t_set(L), t_set(R)], Io, t_bool(Result)) :-
    L \= R,
    m_issubset([t_set(L), t_set(R)], Io, t_bool(1)),
    !, Result = 1.
m___lt__([t_set(_), t_set(_)], _Io, t_bool(0)).
m___le__([t_int(L), t_int(R)], _Io, t_bool(Result)) :-
    Result #<==> (L #=< R).
m___le__([t_set(L), t_set(R)], Io, Result) :-
    m_issubset([t_set(L), t_set(R)], Io, Result).

% '?' == 63
questionmark_to_freevar([63|InChars], [_|OutChars]) :-
    !, questionmark_to_freevar(InChars, OutChars).
questionmark_to_freevar([H|InChars], [H|OutChars]) :-
    questionmark_to_freevar(InChars, OutChars).
questionmark_to_freevar([], []).
    
no_sign(I, Is) :-
    I #< 0,
    Is #= {max_bit_val} + I.
no_sign(I, I).
fix_sign(I, I) :-
    I #< {max_bit_val} / 2.
fix_sign(I, Result) :-
    I #> {max_bit_val} / 2,
    Result #= I - {max_bit_val}.

m___setitem__([List, t_int(Idx), Item], _Io, g_None) :-
    List = t_list(Es),
    nth0_replace(Es, Idx, Item, NewEs),
    setarg(1, List, NewEs).
nth0_replace([_|T], 0, Item, [Item|T]).
nth0_replace([H|T], Idx, Item, [H|R]) :-
    Idx #> 0,
    NextIdx #= Idx - 1,
    nth0_replace(T, NextIdx, Item, R).

m___getitem__([t_range(Start, Stop, Step), t_int(I)], _Io, Result) :-
    I #>= 0,
    Start + Step * I #= R,
    R #< Stop,
    Result = t_int(R).
m___getitem__([t_list(Elts), t_int(I)], _Io, Result) :-
    nth0(I, Elts, Result).
m___getitem__([t_tuple(Elts), t_int(I)], _Io, Result) :-
    nth0(I, Elts, Result).
m___getitem__([t_str(Chars), t_int(I)], _Io, t_str([Char])) :-
    nth0(I, Chars, Char).
m___getitem__([Object, t_int(NegIdx)], Io, Result) :-
    NegIdx #< 0,
    g_len([Object], Io, t_int(Length)),
    PosIdx #= Length + NegIdx,
    PosIdx #>= 0,
    m___getitem__([Object, t_int(PosIdx)], Io, Result).

m_append([List, Element], _Io, g_None) :-
    List = t_list(Es),
    append(Es, [Element], NewEs),
    setarg(1, List, NewEs).
m_extend([List, t_list(XList)], _Io, g_None) :-
    List = t_list(Es),
    append(Es, XList, NewEs),
    setarg(1, List, NewEs).

m_endswith([t_str(Str), t_str(Suffix)], _Io, t_bool(R)) :-
    append(_, Suffix, Str), !, R = 1.
m_endswith([t_str(_), t_str(_)], _Io, t_bool(0)).
m_startswith([t_str(Str), t_str(Prefix)], _Io, t_bool(R)) :-
    append(Prefix, _, Str), !, R = 1.
m_startswith([t_str(_), t_str(_)], _Io, t_bool(0)).
m_join([t_str(Sep), t_list(Strs)], _Io, t_str(Result)) :-
    join_strs(Sep, Strs, [], Result).

m_count([t_tuple(Es), E], _Io, t_int(R)) :-
    count_elem(Es, E, 0, R).
m_count([t_list(Es), E], _Io, t_int(R)) :-
    count_elem(Es, E, 0, R).
m_index([t_tuple(Es), E], _Io, t_int(R)) :-
    nth0(R, Es, E).
m_index([t_list(Es), E], _Io, t_int(R)) :-
    nth0(R, Es, E).
m_insert([List, t_int(Idx), E], _Io, g_None) :-
    t_list(Es) = List,
    list_insert(Es, Idx, E, OutEs),
    setarg(1, List, OutEs).
m_pop([List], _Io, Result) :-
    t_list(Es) = List,
    append(ButLast, [Result], Es),
    setarg(1, List, ButLast).
m_remove([List, E], _Io, g_None) :-
    t_list(Es) = List,
    select(E, Es, Res),
    setarg(1, List, Res).
m_reverse([List], _Io, g_None) :-
    t_list(Es) = List,
    reverse(Es, Res),
    setarg(1, List, Res).
m_sort([List], _Io, g_None) :-
    t_list(Es) = List,
    sort(Es, Res),
    setarg(1, List, Res).

count_elem([], _, Result, Result).
count_elem([H|T], E, Acc, Result) :-
    H \= T,
    count_elem(T, E, Acc, Result).
count_elem([H|T], H, Acc, Result) :-
    NextAcc is Acc + 1,
    count_elem(T, H, NextAcc, Result).

list_mul(_, 0, _, []).
list_mul(_, 1, Acc, Acc).
list_mul(Es, R, Acc, Result) :-
    append(Acc, Es, NextAcc),
    NextR #= R - 1,
    list_mul(Es, NextR, NextAcc, Result).

list_insert(Es, 0, E, [E|Es]).
list_insert([H|Es], Idx, E, [H|OutEs]) :-
    Idx #> 0,
    NextIdx #= Idx - 1,
    list_insert(Es, NextIdx, E, OutEs).
list_insert(Es, NegIdx, E, OutEs) :-
    NegIdx #< 0,
    length(Es, Length),
    PosIdx #= Length + NegIdx,
    list_insert(Es, PosIdx, E, OutEs).

m_issubset([t_set(L), t_set(R)], _Io, t_bool(Res)) :-
   ord_subset(L, R), !, Res = 1.
m_issubset([t_set(_), t_set(_)], _Io, t_bool(0)).
m_isdisjoint([t_set(L), t_set(R)], _Io, t_bool(Res)) :-
   ord_disjoint(L, R), !, Res = 1.
m_isdisjoint([t_set(_), t_set(_)], _Io, t_bool(0)).

join_strs(_, [t_str(H)], Acc, Result) :-
    !,
    append(Acc, H, Result).
join_strs(Sep, [t_str(H)|T], Acc, Result) :-
    append(Acc, H, T0),
    append(T0, Sep, NextAcc),
    join_strs(Sep, T, NextAcc, Result).
join_strs(_, [], Result, Result).

to_print_string([], Acc, t_str(Acc)).
to_print_string([H|T], Acc, Result) :-

    g_str([H], _Io, t_str(HStr)),
    append(Acc, HStr, NextAcc),
    to_print_string(T, NextAcc, Result).

g_abs([t_int(I)], _Io, t_int(R)) :-
    R #= abs(I).

g_write(Objects, _Io, g_None) :-
    write(Objects), nl.
g_print(Objects, Io, g_None) :-
    to_print_string(Objects, [], Str),
    io(List) = Io,
    append(List, [Str], Result),
    setarg(1, Io, Result).

m___str__([Object], _Io, t_str("?object")) :- var(Object), !.
m___str__([t_str(Chars)], _Io, t_str(Chars)).
m___repr__([Object], _Io, t_str("?object")) :- var(Object), !.
m___repr__([t_bool(B)], _Io, t_str("False")) :- B == 0.
m___repr__([t_bool(B)], _Io, t_str("True")) :- B == 1.
m___repr__([t_bool(B)], _Io, t_str("?bool")) :- var(B).
m___repr__([t_int(I)], _Io, t_str(Repr)) :-
    integer(I), !,
    number_codes(I, Repr).
m___repr__([t_int(I)], _Io, t_str(Result)) :-
    fd_dom(I, Dom), term_to_atom(Dom, DomA), name(DomA, DomS),
    append("?int:", DomS, Result).
m___repr__([t_object(Type, _Attrs, _Ref)], _Io, t_str("?object")) :-
    var(Type), !.
m___repr__([t_object(Type, Attrs, _Ref)], _Io, t_str(Repr)) :-
    name(Type, [_, _|StrType]), % skip leading 'g_'
    attr_list(Attrs, AttrList),
    append(StrType, "(", T0),
    repr_list(AttrList, T0, T1),
    append(T1, ")", Repr).
m___repr__([t_list(Es)], _Io, t_str(Repr)) :-
    repr_list(Es, "[", Res),
    append(Res, "]", Repr).
m___repr__([t_tuple(Es)], _Io, t_str(Repr)) :-
    repr_list(Es, "(", Res),
    append(Res, ")", Repr).
m___repr__([t_set(Es)], _Io, t_str(R)) :-
    var(Es), !, R = "?set".
m___repr__([t_set(Es)], _Io, t_str(Repr)) :-
    repr_list(Es, "{{", Res),
    append(Res, "}}", Repr).

g_str(ArgList, Io, Str) :-
    m___str__(ArgList, Io, Str), !.
g_str(ArgList, Io, Str) :-
    m___repr__(ArgList, Io, Str).
g_repr(ArgList, Io, Str) :-
    m___repr__(ArgList, Io, Str).

attr_list([], []).
attr_list([_Name=Value|T], [Value|AT]) :-
    attr_list(T, AT).
repr_list([], Acc, Acc).
repr_list([H], Acc, Res) :-
    !,
    m___repr__([H], _Io, t_str(R)),
    append(Acc, R, Res).
repr_list([H|T], Acc, Res) :-
    m___repr__([H], _Io, t_str(R)),
    append(Acc, R, Tmp),
    append(Tmp, ", ", NextAcc),
    repr_list(T, NextAcc, Res).


g_type([t_set(_)], _Io, g_set).
g_type([t_bool(_)], _Io, g_bool).
g_type([t_str(_)], _Io, g_str).
g_type([t_list(_)], _Io, g_list).
g_type([t_int(_)], _Io, g_int).
g_type([t_range(_, _, _)], _Io, g_range).
g_type([t_object(Type, _Attrs, _Ref)], _Io, Type).
g_len([Object], Io, Result) :-
    m___len__([Object], Io, Result).
g_sum([t_list(Es)], _Io, Result) :-
    list_sum(Es, t_int(0), Result).
g_sum([t_list(Es), Start], _Io, Result) :-
    list_sum(Es, Start, Result).
g_iter([Object], Io, Result) :-
    m___iter__([Object], Io, Result).
g_next(Args, Io, Result) :-
    m___next__(Args, Io, Result).
g_range([t_int(Stop)], _Io, t_range(0, Stop, 1)).
g_range([t_int(Start), t_int(Stop)], _Io, t_range(Start, Stop, 1)).
g_range([t_int(Start), t_int(Stop), t_int(Step)], _Io, t_range(Start, Stop, Step)).
g_set([], _Io, t_set([])).
g_set(Args, Io, t_set(Elts)) :-
    g_iter(Args, Io, Iter),
    iter_to_ordset(Iter, Io, [], Elts).
iter_to_ordset(Iter, Io, Acc, Elts) :-
    g_next([Iter], Io, Elem),
    Elem \= g_StopIteration,
    ord_add_element(Acc, Elem, NextAcc),
    iter_to_ordset(Iter, Io, NextAcc, Elts).
iter_to_ordset(Iter, Io, Acc, Acc) :-
    g_next([Iter], Io, Elem),
    Elem == g_StopIteration.

list_sum([], Acc, Acc).
list_sum([H|T], Acc, Result) :-
    m___add__([Acc, H], _Io, NextAcc),
    list_sum(T, NextAcc, Result).

label_var(t_int(I)) :-
    label([I]).
label_var(X) :-
    X \= t_int(_).

% A, B, Max, Min
sort2(A, B, A, B) :- A #> B.
sort2(A, B, B, A) :- A #=< B.

m___len__([t_set(Elts)], _Io, t_int(L)) :-
    length(Elts, L).
m___len__([t_list(Elts)], _Io, t_int(L)) :-
    length(Elts, L).
m___len__([t_str(Elts)], _Io, t_int(L)) :-
    length(Elts, L).
m___len__([t_tuple(Elts)], _Io, t_int(L)) :-
    length(Elts, L).
m___len__([t_range(Start, Stop, Step)], _Io, t_int(L)) :-
    sort2(Start, Stop, Low, High),
    L #= (Low - High - 1) / abs(Step) + 1.
m___iter__([t_list(Elts)], _Io, t_list_iterator(Elts)).
m___iter__([t_range(L, H, S)], _Io, t_range_iterator(L, H, S)).

m___next__([Iter], _Io, Result) :-
    Iter = t_list_iterator(Elts),
    [Result|Rest] = Elts,
    setarg(1, Iter, Rest).
m___next__([t_list_iterator([])], _Io, g_StopIteration).
m___next__([Iter], _Io, t_int(L)) :-
    t_range_iterator(L, H, S) = Iter,
    L #< H,
    NextL #= S + L,
    setarg(1, Iter, NextL).
m___next__([t_range_iterator(L, H, _)], _Io, g_StopIteration) :-
    L #>= H.


io_write([]).
io_write([H|T]) :-
    t_str(S) = H,
    string_to_list(Str, S),
    write(Str), nl,
    io_write(T).

start :-
    g_main([], Io, _) -> 
     (io(Printed) = Io, io_write(Printed))
     ; nl, (write('   *** Goal ''main'' failed. ***'), nl)
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
