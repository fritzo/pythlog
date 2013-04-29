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


pythlog_builtlins = "write".split()
class FindGlobalSymbols(ast.NodeVisitor):
    def __init__(self):
        self._symbols = set()

    def global_symbols(self):
        return set(list(self._symbols) + dir(__builtins__) + pythlog_builtlins)

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

    def visit_Name(self, node):
        self.names.add(node.id)

def all_names_in(parse_tree):
    """
    Finds all names in a given parse tree.
    """
    f = FindAllNames()
    f.visit(parse_tree)
    return f.names

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

    def visit_Num(self, node):
        return str(node.n)

    def visit_LocalName(self, node):
        return node.id

    def visit_GlobalName(self, node):
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

    def visit_Num(self, node):
        return "t_int(%s)" % node.n

    def visit_Index(self, node):
        return self.visit(node.value)

    def visit_NewObject(self, node):
        return "t_object(%s, [], _)" % node.type

    def visit_List(self, node):
        elts = ", ".join(self.visit(e) for e in node.elts)
        return "t_list([%s])" % elts

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
        assert len(node.ops) == 1
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

    def visit_If(self, node):
        # TODO: Move the translation from if to predicate dispatch to a rewrite stage.
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
        args = node.invars + node.outvars + ['Io']

        self._predicates.append(generate_predicate(name,
                                                   ['t_bool(1)'] + args,
                                                   body))
        self._predicates.append(generate_predicate(name,
                                                   ['t_bool(0)'] + args,
                                                   orelse))
        self._code.append('%s(%s, %s)' % (name, test, ", ".join(args)))
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
    Prolog code. Returns a list of predicates (one function might translate
    into several predicates).
    """
    code = []
    predicates = []

    st = StatementTranslator(allocator, globals)
    for stmt in func_node.body:
#            print(ast.dump(stmt))
        st.visit(stmt)
    code.extend(st.code())
    predicates.extend(st.predicates())

    func_args = "[%s]" % (", ".join(a.arg for a in func_node.args.args))
    pred_args = [func_args, 'Io', 'Result']

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
        if type(decl) != ast.Pass:
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

def maxmin(x, y):
    if x > y:
        return x, y
    return y, x

class SsaRewriter(NodeTransformer):
    """
    Rewrites code to use single static assignment form.
    """
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
        self._locals = locals_before.copy()

        all_vars = set(locals_after_body.keys()).union(set(locals_after_orelse.keys()))
        outvars = []
        for var in all_vars:
            body_idx = self._localvar_index(var, locals_after_body)
            orelse_idx = self._localvar_index(var, locals_after_orelse)
            max_idx, min_idx = maxmin(body_idx, orelse_idx)

            out_var = self._localvar_name(var, max_idx)
            in_var = self._localvar_name(var, min_idx)
            assign = assignment(out_var, LocalName(id=in_var, ctx=ast.Load()))
            if max_idx > body_idx:
                body.append(assign)
            elif max_idx > orelse_idx:
                orelse.append(assign)
            self._locals[var] = out_var
            outvars.append(out_var)

        return self.copy_node(node,
                              test=test,
                              body=body,
                              orelse=orelse,
                              outvars=outvars,
                              invars=list(locals_before.values()))

    def visit_LocalName(self, node):
        if type(node.ctx) == ast.Store:
            id = self._new_local(node.id)
        elif node.id in self._locals:
            id = self._locals[node.id]
        else:
            assert False, node.id
        return self.copy_node(node, id=id)

    def visit_Assign(self, node):
        # NOTE: 'value' and 'targets' must be visited in this order due to
        # side-effects in the visit-method.
        value = self.visit(node.value)
        targets = [self.visit(t) for t in node.targets]
        return self.copy_node(node, targets=targets, value=value)

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

def assignment(var_name, value):
    """
    Get the ast for assigning 'var_name' to 'value'.
    """
    return ast.Assign(targets=[LocalName(id=var_name, ctx=ast.Store())], value=value)

def assert_equal(lhs, rhs):
    return ast.Assert(test=ast.Compare(left=lhs, ops=[ast.Eq()], comparators=[rhs]), msg=None)

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

def ssa_form(parse_tree):
    """Rewrite functions into static single assignment form"""
    return SsaRewriter().visit(parse_tree)

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


def resolve_global_symbols(parse_tree):
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
    return SymbolResolver(global_symbols(parse_tree)).visit(parse_tree)

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

def arg_list_match_to_assert(parse_tree):
    """
    Rewrites
        func(x: Class(a, b)): pass
    to
        func(x): a = free; b = free; assert x == Class(a, b)
    """
    return ArgListMatchToAssert(global_symbols(parse_tree)).visit(parse_tree)

def compile_module(module_code):
    parse_tree = ast.parse(module_code)
    parse_tree = define_type_dependent_builtins(parse_tree)
    parse_tree = arg_list_match_to_assert(parse_tree)
    parse_tree = resolve_global_symbols(parse_tree)
#    print(prettyprint(parse_tree))
    parse_tree = single_return_point(parse_tree)
    #print(prettyprint(parse_tree))
    parse_tree = ssa_form(parse_tree)
    #print(prettyprint(parse_tree))

    compiler = ModuleTranslator(global_symbols(parse_tree))
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
i_cmpeq(O, O, t_bool(1)).
i_cmpeq(L, R, t_bool(0)) :-
    L \= R. % TODO: reify unification to boolean result!
i_cmpnoteq(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #\= R).
i_cmpnoteq(O, O, t_bool(0)).
i_cmpnoteq(L, R, t_bool(1)) :-
    L \= R. % TODO: reify unification to boolean result!
i_cmplt(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #< R).
i_cmpgt(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #> R).
i_cmpgte(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #>= R).
i_cmplte(t_int(L), t_int(R), t_bool(Result)) :-
    Result #<==> (L #=< R).
i_cmpin(Object, t_list(Elts), t_bool(1)) :-
    nth0(_, Elts, Object).
i_cmpin(Object, t_list(Elts), t_bool(0)) :-
    not(nth0(_, Elts, Object)).


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

m___add__([t_list(L), t_list(R)], _, t_list(Result)) :-
    append(L, R, Result).
m___add__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L + R.
m___radd__([t_int(R), t_int(L)], _Io, t_int(Result)) :-
    Result #= L + R.
m___sub__([t_list(L), t_list(R)], _, t_list(Result)) :-
    append(Result, R, L).
m___sub__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L - R.
m___mul__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L * R.
m___rmul__([t_int(R), t_int(L)], _Io, t_int(Result)) :-
    Result #= L * R.
m___floordiv__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L / R.
m___mod__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    L #< 0, R #< 0, !,
    Result #= -(-L mod -R).
m___mod__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L mod R.
m___pow__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L ^ R.
m___rshift__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L / (2 ^ R).
m___lshift__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    Result #= L * (2 ^ R).
m___and__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_and_body},
    fix_sign(UnsignedResult, Result).
m___rand__([t_int(R), t_int(L)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_and_body},
    fix_sign(UnsignedResult, Result).
m___or__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_or_body},
    fix_sign(UnsignedResult, Result).
m___ror__([t_int(R), t_int(L)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_or_body},
    fix_sign(UnsignedResult, Result).
m___xor__([t_int(L), t_int(R)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_xor_body},
    fix_sign(UnsignedResult, Result).
m___rxor__([t_int(R), t_int(L)], _Io, t_int(Result)) :-
    no_sign(L, Ls),
    no_sign(R, Rs),
    {int_xor_body},
    fix_sign(UnsignedResult, Result).
m___neg__([t_int(I)], _Io, t_int(Neg)) :-
    Neg #= -I.
m___invert__([t_int(I)], _Io, t_int(Result)) :-
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

m___setitem__([List, t_int(Idx), Item], _Io, g_None) :-
    List = t_list(Es),
    nth0_replace(Es, Idx, Item, NewEs),
    setarg(1, List, NewEs).
nth0_replace([_|T], 0, Item, [Item|T]).
nth0_replace([H|T], Idx, Item, [H|R]) :-
    Idx #> 0,
    NextIdx #= Idx - 1,
    nth0_replace(T, NextIdx, Item, R).

m___getitem__([t_list(Elts), t_int(I)], _Io, Result) :-
    nth0(I, Elts, Result).
m_append([List, Element], _Io, g_None) :-
    List = t_list(Es),
    append(Es, [Element], NewEs),
    setarg(1, List, NewEs).
m_extend([List, t_list(XList)], _Io, g_None) :-
    List = t_list(Es),
    append(Es, XList, NewEs),
    setarg(1, List, NewEs).

to_print_string([], Acc, t_str(Acc)).
to_print_string([H|T], Acc, Result) :-
    g_str([H], t_str(HStr)),
    append(Acc, HStr, NextAcc),
    to_print_string(T, NextAcc, Result).

g_write(Objects, _Io, g_None) :-
    write(Objects), nl.
g_print(Objects, Io, g_None) :-
    to_print_string(Objects, [], Str),
    io(List) = Io,
    append(List, [Str], Result),
    setarg(1, Io, Result).

m___str__(u, u).
m___repr__([t_bool(0)], t_str("False")).
m___repr__([t_bool(1)], t_str("True")).
m___repr__([t_int(I)], t_str(Repr)) :-
    integer(I), !,
    number_codes(I, Repr).
m___repr__([t_int(I)], t_str(Result)) :-
    fd_dom(I, Dom), term_to_atom(Dom, DomA), name(DomA, DomS),
    append("?int:", DomS, Result).
m___repr__([t_object(Type, _Attrs, _Ref)], t_str("?object")) :-
    var(Type), !.
m___repr__([t_object(Type, Attrs, _Ref)], t_str(Repr)) :-
    name(Type, [_, _|StrType]), % skip leading 'g_'
    attr_list(Attrs, AttrList),
    append(StrType, "(", T0),
    repr_list(AttrList, T0, T1),
    append(T1, ")", Repr).
m___repr__([t_list(Es)], t_str(Repr)) :-
    repr_list(Es, "[", Res),
    append(Res, "]", Repr).

g_str(ArgList, Str) :-
    m___str__(ArgList, Str), !.
g_str(ArgList, Str) :-
    m___repr__(ArgList, Str).
g_repr(ArgList, Str) :-
    m___repr__(ArgList, Str).

attr_list([], []).
attr_list([_Name=Value|T], [Value|AT]) :-
    attr_list(T, AT).
repr_list([], Acc, Acc).
repr_list([H], Acc, Res) :-
    !,
    m___repr__([H], t_str(R)),
    append(Acc, R, Res).
repr_list([H|T], Acc, Res) :-
    m___repr__([H], t_str(R)),
    append(Acc, R, Tmp),
    append(Tmp, ", ", NextAcc),
    repr_list(T, NextAcc, Res).



g_type([t_int(_)], _Io, g_int).
g_type([t_object(Type, _Attrs, _Ref)], _Io, Type).
g_len([t_list(Elts)], _Io, t_int(L)) :-
    length(Elts, L).


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