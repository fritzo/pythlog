#!/usr/bin/python3.2

import ast
import os
import subprocess
import tempfile

class ProperyInferenceVisitor(ast.NodeVisitor):
    def __init__(self, filename):
        self._filename = filename
        self._var_counter = 0
        self._pred_body = []
        self._predicates = []
        self._localvars = {}
        self._predicate_names = []
        self._function_hash = None

    def code(self):
        code = "module_functions([%s]).\n\n" % ", ".join(
            "'%s'/%s" % (name, arity) for name, arity in self._predicate_names)

        names = set(n[0] for n in self._predicate_names)
        gbls = "\n".join("getglobal('%s', func('%s'))." % (n, n) for n in names)
        return code + "\n\n".join(self._predicates) + "\n\n" + gbls + "\n"

    def add_stmt(self, stmt):
        self._pred_body.append("  " + stmt)

    def predicate_end(self, predicate_head):
        stack_check = '  not(member(%s, Stack)), (\n' % self._function_hash
        body = stack_check + ",\n".join(self._pred_body) + "\n  ); true"
        self._predicates.append(predicate_head + "\n" + body + ".")
        self._pred_body = []
        self._var_counter = 0
        self._localvars = {}

    def _is_localvar(self, varname):
        return varname in self._localvars

    def _add_localvar(self, varname):
        if self._is_localvar(varname):
            current_name = self._localvars[varname]
            next_idx = int(current_name.split('_')[0][1:]) + 1
            self._localvars[varname] = "L%s_%s" % (next_idx, varname)
        else:
            self._localvars[varname] = "L0_" + varname

    def _tmpvar(self):
        n = self._var_counter
        self._var_counter += 1
        return "T%s" % n

    def _context_vars(self, node):
        return "'%s':%s:%s, [%s|Stack]" % (
            self._filename,
            node.lineno, node.col_offset + 1,
            self._function_hash)

    # Statement nodes

    def visit_FunctionDef(self, node):
        self._function_hash = hash(ast.dump(node, include_attributes=True))
        self._localvars = {a.arg:"L0_" + a.arg for a in node.args.args}

        for stmt in node.body:
            self.visit(stmt)

        name = node.name
        args = ", ".join(self._localvars[a.arg] for a in node.args.args)
        head = "invoke(func('%s'), [%s], Return, _, Stack) :-" % (name, args)
        self.predicate_end(head)

        descr = (name, len(node.args.args))
        if descr not in self._predicate_names:
            self._predicate_names.append(descr)

    def visit_Return(self, node):
        value = self.visit(node.value)
        self.add_stmt("return(Return, %s)" % value)

    def visit_Delete(self, node):
        assert len(node.targets) == 1
        target = node.targets[0]
        value = self.visit(target.value)
        slice = self.visit(target.slice.value)
        ctx = self._context_vars(node)
        self.add_stmt("delitem(%s, %s, %s)" % (value, slice, ctx))

    def visit_If(self, node):
        self.visit(node.test)
        for stmt in node.body:
            self.visit(stmt)
        for stmt in node.orelse:
            self.visit(stmt)

    def visit_For(self, node):
        target = self.visit(node.target)
        iterable = self.visit(node.iter)
        ctx = self._context_vars(node)

        self.add_stmt("for(%s, %s, %s)" % (iterable, target, ctx))

        for stmt in node.body:
            self.visit(stmt)

    def visit_Expr(self, node):
        self.visit(node.value)

    def visit_Assign(self, node):
        assert len(node.targets) == 1
        value = self.visit(node.value)
        target = node.targets[0]
        if target.__class__ == ast.Subscript:
            list = self.visit(target.value)
            slice = self.visit(target.slice.value)
            ctx = self._context_vars(node)
            self.add_stmt("setitem(%s, %s, %s, %s)" % (list, slice, value, ctx))
        else:
            self.add_stmt("%s = %s" % (self.visit(target), value))

    # Expression nodes
    def visit_Num(self, node):
        return "_:int"

    def visit_List(self, node):
        return "_:list"

    def visit_BinOp(self, node):
        lhs = self.visit(node.left)
        rhs = self.visit(node.right)
        op = node.op.__class__.__name__.lower()
        result = self._tmpvar()
        ctx = self._context_vars(node)
        self.add_stmt('binop(%s, %s, %s, %s, %s)' % (op, lhs, rhs, result, ctx))
        return result

    def visit_Compare(self, node):
        assert len(node.comparators) == 1
        assert len(node.ops) == 1
        op = node.ops[0].__class__.__name__.lower()
        lhs = self.visit(node.left)
        rhs = self.visit(node.comparators[0])
        result = self._tmpvar()
        ctx = self._context_vars(node)
        self.add_stmt("cmp(%s, %s, %s, %s, %s)" % (op, lhs, rhs, result, ctx))
        return result

    def visit_Call(self, node):
        name = self.visit(node.func)
        args = ", ".join(self.visit(a) for a in node.args)
        result = self._tmpvar()
        ctx = self._context_vars(node)
        self.add_stmt("invoke(%s, [%s], %s, %s)" % (name, args, result, ctx))
        return result

    def visit_Attribute(self, node):
        value = self.visit(node.value)
        result = self._tmpvar()
        attrname = node.attr
        self.add_stmt("getattr(%s, '%s', %s)" % (value, attrname, result))
        return result

    def visit_Name(self, node):
        if node.id in ['True', 'False']:
            return "_:bool"
        if node.ctx.__class__ == ast.Store:
            self._add_localvar(node.id)
        if self._is_localvar(node.id):
            return self._localvars[node.id]
        result = self._tmpvar()
        self.add_stmt("getglobal('%s', %s)" % (node.id, result))
        return result

    def visit_Subscript(self, node):
        value = self.visit(node.value)
        slice = self.visit(node.slice.value)
        result = self._tmpvar()
        ctx = self._context_vars(node)
        self.add_stmt("getitem(%s, %s, %s, %s)" % (value, slice, result, ctx))
        return result

def generate_prolog_code(code, filename):
    """
    Generates prolog code that infer the types of the functions in the module.
    When the goal 'main(FileName)' is reached the types has been written to
    the file FileName.
    """
    visitor = ProperyInferenceVisitor(filename)
    visitor.visit(ast.parse(code))

    base = os.path.dirname(os.path.abspath(__file__))
    framework = open(os.path.join(base, "pythlog-infer-framework.pl"))
    return framework.read() + visitor.code()


def infer_types(code, infile, outfile, directory):
    prolog_code = generate_prolog_code(code, infile)

    prolog_file = os.path.join(directory, "pythlog-infer.pl")
    with open(prolog_file, 'w') as dest:
        dest.write(prolog_code)
    
    prolog = "swipl -q -s %s -g \"main('%s'), halt\"" % (prolog_file, outfile)
    subprocess.call(prolog, shell=True)

def main():
    import argparse
    parser = argparse.ArgumentParser(description='pythlog-infer: type inferer for pythlog.')
    parser.add_argument('infile', metavar='INFILES',
                        type=argparse.FileType('r'),
                        help='input file')
    parser.add_argument('-o', dest='outfile',
                        default=None,
                        help='output file')

    args = parser.parse_args()
    if args.outfile is None:
        outfile = args.infile.name + ".infer"
    else:
        outfile = args.outfile
    with tempfile.TemporaryDirectory() as directory:
        infer_types(args.infile.read(),
                    args.infile.name,
                    outfile,
                    "/tmp")

if __name__ == '__main__':
    main()