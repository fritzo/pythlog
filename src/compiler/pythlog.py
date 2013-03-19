#!/usr/bin/python

import ast

class FindGlobalSymbols(ast.NodeVisitor):
    def __init__(self):
        self._symbols = set()

    def symbols(self):
        return self._symbols.union(dir(__builtins__))

    def visit_FunctionDef(self, node):
        self._symbols.add(node.name)

    def visit_ClassDef(self, node):
        self._symbols.add(node.name)

class ModuleCompiler(ast.NodeVisitor):
    def __init__(self, global_symbols):
        self._global_symbols = global_symbols
        self._predicates = []
        self._unique_number = 0

    def predicates(self):
        return self._predicates

    def _alloc_unique_number(self):
        num = self._unique_number
        self._unique_number += 1
        return num

    def visit_Module(self, node):
        for definition in node.body:
            self.visit(definition)

    def visit_FunctionDef(self, node):
        args = [a.id for a in node.args.args]
        compiler = FunctionCompiler(node.name, args,
                                    self._alloc_unique_number(),
                                    self._global_symbols)
        self._predicates.extend(compiler.predicates(node.body))

    def visit_ClassDef(self, node):
        compiler = ClassCompiler(node.name,
                                 self._alloc_unique_number(),
                                 self._global_symbols)
        self._predicates.extend(compiler.predicates(node.body))


class TemporaryVariables:
    def __init__(self):
        self._id = 0

    def _new_temp_var(self):
        var = "T%s" % self._id
        self._id += 1
        return var

class AccessedVariables(ast.NodeVisitor):
    def __init__(self):
        self._vars_loaded = set()
        self._vars_stored = set()

    def vars(self, stmts):
        for stmt in stmts:
            self.visit(stmt)
        return self._vars_loaded, self._vars_stored

    def visit_Name(self, node):
        if node.id in ['True', 'False']:
            return
        if node.ctx.__class__ == ast.Store().__class__:
            self._vars_stored.add(node.id)
        else:
            self._vars_loaded.add(node.id)

def accessed_variables(*stmt_lists):
    loaded = set()
    stored = set()
    for stmt_list in stmt_lists:
         l, s = AccessedVariables().vars(stmt_list)
         loaded.update(l) # union
         stored.update(s) # union
    return loaded, stored

class AccessedAttributes(ast.NodeVisitor):
    def __init__(self):
        self._attrs = set()
    def visit_Attribute(self, node):
        if node.ctx.__class__ == ast.Store and node.value.id == 'self': # TODO: Too much hardcoding
            self._attrs.add(node.attr)
    def attributes(self):
        return self._attrs

def written_self_attrs(stmt_list):
    visitor = AccessedAttributes()
    for stmt in stmt_list:
        visitor.visit(stmt)
    return visitor.attributes()

def new_value_for_var(varname, local_vars):
    if varname in local_vars:
        current_val = local_vars[varname]
        idx = int(current_val[current_val.rfind('_') + 1:]) + 1
    else:
        idx = 0
    value = "V_%s_%s" % (varname, idx)
    local_vars[varname] = value
    return value

class ExprVisitor(ast.NodeVisitor):
    def __init__(self, local_vars, temp_vars, stmts, global_symbols,
                 name_allocator, out_predicates, attrs_writable,
                 io_manager, free_vars):
        self._local_vars = local_vars
        self._temp_vars = temp_vars
        self._stmts = stmts
        self._global_symbols = global_symbols
        self._name_allocator = name_allocator
        self._out_predicates = out_predicates
        self._attrs_writable = attrs_writable
        self._io_manager = io_manager
        self._free_vars = free_vars # Whether or not free variables are allowed
        self._free_vars_used = False

    def _expr_builder(self, stmts):
        return ExprVisitor(self._local_vars,
                            self._temp_vars,
                            stmts,
                            self._global_symbols,
                            self._name_allocator,
                            self._out_predicates,
                            self._attrs_writable,
                            self._io_manager,
                            self._free_vars)


    def free_vars_used(self):
        return self._free_vars_used

    def _new_var(self, varname):
        return new_value_for_var(varname, self._local_vars)

    def _new_temp_var(self):
        return self._temp_vars._new_temp_var()

    def visit_Name(self, node):
        if node.id == 'True':
            return "pl_bool(1)"
        elif node.id == 'False':
            return "pl_bool(0)"
        elif node.id == 'None':
            return "pl_None"
        elif node.ctx.__class__ == ast.Store().__class__:
            return self._new_var(node.id)
        elif node.id in self._local_vars:
            return self._local_vars[node.id]
        elif node.id in self._global_symbols:
            return "f_%s" % node.id # TODO: name mangling should not be done here
        elif self._free_vars:
            self._free_vars_used = True
            return self._new_var(node.id)
        assert False, node.id

    def visit_Str(self, node):
        return 'pl_seq(str, "%s")' % node.s

    def visit_Num(self, node):
        return "pl_int(%s)" % node.n

    def visit_List(self, node):
        elements = ", ".join(self.visit(e) for e in node.elts)
        return "pl_seq(list, [%s])" % elements

    def visit_ListComp(self, node):
        assert len(node.generators) == 1
        assert len(node.generators[0].ifs) == 0
        pred_name = self._name_allocator.for_pred_name()
        iterator = self.visit(node.generators[0].iter)

        expr_stmts = []
        expr_builder = self._expr_builder(expr_stmts)
        gen_var = expr_builder.visit(node.generators[0].target)

#        cond_stmts = []
#        if len(node.generators[0].ifs) > 0:
#            cond_builder = expr_builder._expr_builder(cond_stmts)
#            condition = cond_builder.visit(node.generators[0].ifs[0])
#            cond_pred_name = self._name_allocator.cond_pred_name()
#            print predicate_for_function(cond_pred_name, [], cond_builder._stmts)
#            print condition
#            print cond_builder._stmts
#        else:
#            condition = []

        elem_expr = expr_builder.visit(node.elt)
        self._out_predicates.append("%s([], [], IO, IO)." % pred_name)
        pred_args = ["[%s|Rest]" % gen_var,
                     "[%s|%s]" % (elem_expr, iterator)]
        pred_io = expr_builder._io_manager.current_io_var_name()
        expr_stmts.append("%s(Rest, %s, %s, OutIO)" % (pred_name, iterator, pred_io)) 
        self._out_predicates.append(predicate_for_function(pred_name, pred_args, expr_stmts))

        result = self._new_temp_var()
        unwrapped_iter = self._new_temp_var()
        unwrapped_result = self._new_temp_var()
        io0, io1 = self._io_manager.new_io_var_name()
        self._stmts.append("%s = pl_seq(_, %s)" % (iterator, unwrapped_iter))
        self._stmts.append("%s(%s, %s, %s, %s)" % (pred_name, unwrapped_iter, unwrapped_result, io0, io1))
        self._stmts.append("%s = pl_seq(list, %s)" % (result, unwrapped_result))
        return result

    def visit_Attribute(self, node):
        if not self._attrs_writable:
            assert node.ctx.__class__ == ast.Load().__class__, ast.dump(node)
        if node.value.id == 'self': # TODO: Too much hardcoding
            return "V_%s_%s" % (node.value.id, node.attr)
        obj = self.visit(node.value)
        attrname = node.attr
        result = self._new_temp_var()
        self._stmts.append("pl_getattr(%s, f_%s, %s)" % (obj, attrname, result))
        return result

    def visit_BinOp(self, node):
        lhs = self.visit(node.left)
        op = "pl_" + node.op.__class__.__name__.lower()
        rhs = self.visit(node.right)
        result = self._new_temp_var()
        stmt = "%s(%s, %s, %s)" % (op, lhs, rhs, result)
        self._stmts.append(stmt)
        return result

    def visit_UnaryOp(self, node):
        operand = self.visit(node.operand)
        op = "pl_" + node.op.__class__.__name__.lower()
        result = self._new_temp_var()
        stmt = "%s(%s, %s)" % (op, operand, result)
        self._stmts.append(stmt)
        return result

    def visit_BoolOp(self, node):
        op = "pl_" + node.op.__class__.__name__.lower()
        values = [self.visit(v) for v in node.values]
        last_result = values[0]
        for v in values[1:]:
            result = self._new_temp_var()
            stmt = "%s(%s, %s, %s)" % (op, last_result, v, result)
            self._stmts.append(stmt)
            last_result = result
        return result

    def visit_Compare(self, node):
        assert len(node.ops) == 1
        assert len(node.comparators) == 1
        lhs = self.visit(node.left)
        op = "pl_" + node.ops[0].__class__.__name__.lower()
        rhs = self.visit(node.comparators[0])
        assert lhs is not None, ast.dump(node.left)
        assert rhs is not None, ast.dump(node.comparators[0])
        result = self._new_temp_var()
        stmt = "%s(%s, %s, %s)" % (op, lhs, rhs, result)
        self._stmts.append(stmt)
        return result

    def visit_Index(self, node):
        return self.visit(node.value)

    def visit_Subscript(self, node):
        value = self.visit(node.value)
        arg = self.visit(node.slice)
        result = self._new_temp_var()
        stmt = "pl_subscript(%s, %s, %s)" % (value, arg, result)
        self._stmts.append(stmt)
        return result
        
    def visit_Call(self, node):
        func_args = [self.visit(a) for a in node.args]
        func = self.visit(node.func)
        result = self._new_temp_var()
        io0, io1 = self._io_manager.new_io_var_name()
        call_args = func_args + [result, io0, io1]
        self._stmts.append("%s(%s)" % (func, ", ".join(call_args)))
        return result


class ReturnStmtVisitor(ast.NodeVisitor):
    def __init__(self):
        self._has_return_stmt = False

    def visit_Return(self, _):
        self._has_return_stmt = True

# TODO: This is not fully correct. If the end of the function is reached
# without executing an return statement, the function returns None.
def has_return_stmt(stmt_list):
    v = ReturnStmtVisitor()
    for stmt in stmt_list:
        v.visit(stmt)
    return v._has_return_stmt

class StatementListCompiler(ast.NodeVisitor):
    def __init__(self, local_vars, temp_vars, name_allocator,
                 global_symbols, attrs_writable=False):
        self._local_vars = local_vars
        self._temp_vars = temp_vars
        self._name_allocator = name_allocator
        self._global_symbols = global_symbols
        self._attrs_writable = attrs_writable
        self._out_stmts = []
        self._out_predicates = []
        self._io_manager = IoManager()

    def _expr_visitor(self, free_vars=False):
        return ExprVisitor(self._local_vars,
                           self._temp_vars,
                           self._out_stmts,
                           self._global_symbols,
                           self._name_allocator,
                           self._out_predicates,
                           self._attrs_writable,
                           self._io_manager,
                           free_vars)

    def _add_stmt(self, stmt):
        self._out_stmts.append(stmt)

    def visit_stmt_list(self, stmts):
        for stmt in stmts:
            self.visit(stmt)
        return self._out_stmts, self._out_predicates

    def visit_Print(self, node):
        expr = self._expr_visitor()
        values = ", ".join(expr.visit(v) for v in node.values)
        io0, io1 = self._io_manager.new_io_var_name()
        self._add_stmt('pl_print([%s], %s, %s, %s)' % (values, int(node.nl), io0, io1))

    def visit_Expr(self, node):
        self._expr_visitor().visit(node.value)

    def visit_Assert(self, node):
        expr_visitor = self._expr_visitor(free_vars=True)
        expr = expr_visitor.visit(node.test)
        assert expr is not None, ast.dump(node)
        if node.msg is not None:
            msg = self._expr_visitor().visit(node.msg)
        else:
            msg = "pl_None"
        if expr_visitor.free_vars_used():
            self._add_stmt("pl_solve(%s)" % expr)
        else:
            io0, io1 = self._io_manager.new_io_var_name()
            self._add_stmt("pl_assert(%s, %s, %s, %s)" % (expr, msg, io0, io1))

    def visit_Assign(self, node):
        assert len(node.targets) == 1
        expr = self._expr_visitor()
        src = expr.visit(node.value)
        dst = expr.visit(node.targets[0])
        self._add_stmt("%s = %s" % (dst, src))

    def visit_Return(self, node):
        expr = self._expr_visitor()
        value = expr.visit(node.value)
        self._add_stmt("ReturnValue = %s" % value)

    def visit_If(self, node):
        pred_name = self._name_allocator.if_pred_name()
        expr = self._expr_visitor()
        test_var = expr.visit(node.test)

        vars_loaded, vars_stored = accessed_variables(node.body, node.orelse)
        vars_loaded.difference_update(self._global_symbols)

        then_compiler = StatementListCompiler(self._local_vars.copy(),
                                              self._temp_vars,
                                              self._name_allocator,
                                              self._global_symbols)
        then_stmts, then_extra_preds = then_compiler.visit_stmt_list(node.body)
        vars_not_written_in_then = vars_stored - accessed_variables(node.body)[1]

        else_compiler = StatementListCompiler(self._local_vars.copy(),
                                              self._temp_vars,
                                              self._name_allocator,
                                              self._global_symbols)
        else_stmts, else_extra_preds = else_compiler.visit_stmt_list(node.orelse)
        vars_not_written_in_else = vars_stored - accessed_variables(node.orelse)[1]

        old_local_vars = self._local_vars.copy()
        for varname in vars_stored:
            new_value_for_var(varname, self._local_vars)

        then_stmts.insert(0, 'pl_bool(True, pl_bool(1))')
        for varname in vars_not_written_in_then:
            then_stmts.append("%s = %s" % (self._local_vars[varname], old_local_vars[varname]))
        then_stmts.append('OutIO = %s' % then_compiler._io_manager.current_io_var_name())

        else_stmts.insert(0, 'pl_bool(False, pl_bool(0))')
        for varname in vars_not_written_in_else:
            else_stmts.append("%s = %s" % (self._local_vars[varname], old_local_vars[varname]))
        else_stmts.append('OutIO = %s' % else_compiler._io_manager.current_io_var_name())
        
        
        inout_args = ([old_local_vars[v] for v in list(vars_loaded.union(vars_stored))] + 
                      [self._local_vars[v] for v in list(vars_stored)])

        then_pred_args = ["True"] + inout_args
        else_pred_args = ["False"] + inout_args
        io0, io1 = self._io_manager.new_io_var_name()
        call_args = [test_var] + inout_args + [io0, io1]

        self._add_stmt("%s(%s)" % (pred_name, ", ".join(call_args)))
        self._out_predicates.extend(else_extra_preds)
        self._out_predicates.extend(then_extra_preds)
        self._out_predicates.append(predicate_for_function(pred_name, then_pred_args, then_stmts))
        self._out_predicates.append(predicate_for_function(pred_name, else_pred_args, else_stmts))



def predicate_for_function(pred_name, pred_args, pred_stmts, io=True):
    args = pred_args + ['InIO', 'OutIO'] * io
    head = "%s(%s)" % (pred_name, ", ".join(args))
    if len(pred_stmts) == 0:
        return head + "."
    else:
        return head + " :- \n" + ",\n".join(["    %s" % s for s in pred_stmts]) + "."

class NameAllocator:
    def __init__(self, base_name):
        self._base_name = base_name
        self._count = 0

    def _alloc_num(self):
        num = self._count
        self._count += 1
        return num

    def if_pred_name(self):
        return self._base_name + "_if%s" % self._alloc_num()

    def for_pred_name(self):
        return self._base_name + "_for%s" % self._alloc_num()

    def cond_pred_name(self):
        return self._base_name + "_cond%s" % self._alloc_num()

class IoManager:
    def __init__(self):
        self._io_var_count = 0

    def current_io_var_name(self):
        if self._io_var_count == 0:
            return "InIO"
        else:
            return "IO%s" % self._io_var_count

    def new_io_var_name(self):
        io0 = self.current_io_var_name()
        self._io_var_count += 1
        io1 = self.current_io_var_name()
        return io0, io1

class FunctionCompiler(ast.NodeVisitor):
    def __init__(self, name, args, id, global_symbols):
        self._name = name
        self._args = args
        local_vars = dict((a, "V_%s_0" % a) for a in args)
        temp_vars = TemporaryVariables()
        allocator = NameAllocator("%s_%s" % (name, id))
        self._stmts_list_compiler = StatementListCompiler(local_vars,
                                                          temp_vars,
                                                          allocator,
                                                          global_symbols)

    def predicates(self, stmt_list):
        pred_stmts, predicates = self._stmts_list_compiler.visit_stmt_list(stmt_list)
        if not(has_return_stmt(stmt_list)):
            pred_stmts.append('ReturnValue = pl_None')
        pred_stmts.append('OutIO = %s' % self._stmts_list_compiler._io_manager.current_io_var_name())
        pred_args = ["V_%s_0" % a for a in self._args] + ["ReturnValue"]
        pred_name = "f_%s" % self._name
        return predicates + [predicate_for_function(pred_name, pred_args, pred_stmts)]

class ClassCompiler(ast.NodeVisitor):
    def __init__(self, class_name, id, global_symbols):
        self._class_name = class_name
        self._id = id
        self._global_symbols = global_symbols
        self._out_predicates = []
        self._attrs = []

    def predicates(self, stmt_list):
        for stmt in stmt_list:
            self.visit(stmt)

        return self._out_predicates

    def visit_CtorDef(self, node):
        self._attrs = sorted(written_self_attrs(node.body))
        args = [a.id for a in node.args.args[1:]]
        local_vars = dict((a, "V_%s_0" % a) for a in args)
        temp_vars = TemporaryVariables()
        allocator = NameAllocator("%s_%s" % (self._class_name, self._id))
        compiler = StatementListCompiler(local_vars,
                                         temp_vars,
                                         allocator,
                                         self._global_symbols,
                                         attrs_writable=True)

        pred_stmts, predicates = compiler.visit_stmt_list(node.body)
        fields = [local_vars[a] for a in self._attrs]
        return_stmt = "ReturnValue = pl_object(f_%s, [%s])" % (self._class_name, ", ".join(fields))
        pred_stmts.append(return_stmt)
        pred_args = ["V_%s_0" % a for a in args] + ["ReturnValue"]
        pred_name = "f_%s" % self._class_name
        pred = predicate_for_function(pred_name, pred_args, pred_stmts)
        self._out_predicates.append(pred)

        for attr in self._attrs:
            obj = "pl_object(f_%s, [%s])" % (self._class_name, ", ".join(fields))
            args = [obj, "f_" + attr, local_vars[attr]]
            pred = predicate_for_function("pl_getattr", args, [], io=False)
            self._out_predicates.append(pred)

    def visit_FunctionDef(self, node):
        if node.name == '__init__':
            self.visit_CtorDef(node)
            return
        print ast.dump(node)


def compile_module(module_code):
    parse_tree = ast.parse(module_code)
    symbol_finder = FindGlobalSymbols()
    symbol_finder.visit(parse_tree)

    compiler = ModuleCompiler(symbol_finder.symbols())
    compiler.visit(parse_tree)
    return "\n\n".join(compiler.predicates()) + '\n'


def main():
    import argparse

    parser = argparse.ArgumentParser(description='pythlog')
    parser.add_argument('infiles', metavar='INFILES',
                        type=argparse.FileType('r'), nargs='+',
                        help='input files')
    parser.add_argument('-o', dest='outfile',
                        type=argparse.FileType('w'),
                        default='a.out.pl',
                        help='output file')

    args = parser.parse_args()
    outfile = args.outfile
    outfile.write(":- style_check(-singleton).\n\n")
    for f in args.infiles:
        outfile.write(compile_module(f.read()))
    outfile.close()

if __name__ == '__main__':
    main()