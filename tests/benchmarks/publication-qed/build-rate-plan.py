"""Compile a grouped SPINAS export with operand tables for repeated C++ arithmetic.

This benchmark-only build keeps set_momenta, helicity dispatch, precision
selection and the spin sum from process.cpp. The group of operations in each
temporary assignment is compiled by the C++ compiler. Its integer subscripts
are read from a table. Use this build only with the label 'operand tables'.
"""
import argparse
from array import array
from pathlib import Path
import re
import subprocess
import sys

HERE = Path(__file__).resolve().parent
COMPONENT = re.compile(r'(template<class Scalar> Scalar|cdouble) qed_rate::component_(\d+)\(const Indices& indices\) const \{\n')
ASSIGN = re.compile(r'    (?:const (?:Scalar|cdouble) v(\d+) = |return )(.+);\n')
OPERAND = re.compile(r'\bv(\d+)|\b(indices|chains|invariants|parameters|derived)\[(\d+)\]')
INT = re.compile(r'(?<![\w.])-?\d+(?![\w.])')
LONG_SUM = re.compile(r'v\d+(?: \+ v\d+){5,}\Z')


class Table:
    def __init__(self, file):
        self.file = file
        self.words = 0
        self.codes = {}

    def emit(self, operation, operands):
        index = self.codes.setdefault(operation, len(self.codes))
        array('i', [index, *operands]).tofile(self.file)
        self.words += 1 + len(operands)


def component(lines, table):
    start = table.words
    count = 0
    returned = False
    for line in lines:
        if line == '}\n':
            if not returned:
                raise ValueError('Missing component return')
            return start, count
        m = ASSIGN.fullmatch(line)
        if returned or not m or (m[1] and int(m[1]) != count):
            raise ValueError('Unexpected generated assignment: ' + line[:100])
        returned = m[1] is None
        expr = m[2]
        if LONG_SUM.fullmatch(expr):
            operands = [int(v[1:]) for v in expr.split(' + ')]
            table.emit('sum', [len(operands), *operands])
        else:
            operands = []
            def sub(m):
                i = len(operands)
                operands.append(int(m[1] or m[3]))
                return f'{m[2] or "v"}[a[{i}]]'
            code = OPERAND.sub(sub, expr)
            table.emit((len(operands), code), operands)
        count += 1
    raise ValueError('Truncated component')


def compact(source, output):
    output.mkdir(parents=True, exist_ok=True)
    words = output/'operands.i32'
    assert array('i').itemsize == 4
    with words.open('wb') as binary, (source/'process.cpp').open() as raw, (output/'plan.cpp').open('w') as cpp:
        operations = Table(binary)
        setup = Table(binary)
        print('#include <vector>\nextern "C" const int hepcat_rate_operands[];', file=cpp)
        for line in raw:
            if '    invariants[0] =' in line or line.startswith(('    chains[0]', '    for (int ls :','    for (int rs :')):
                # All subsequent generated invariant and chain assignments end at ready=true.
                start = setup.words
                while line.strip() != 'ready = true;':
                    block = line
                    while not block.rstrip().endswith(';'):
                        block += next(raw)
                    if 'invariants[' not in block and 'chains[' not in block:
                        raise ValueError('Unexpected setup: '+block[:100])
                    operands = []
                    def slot(m):
                        operands.append(int(m[0]))
                        return f'a[{len(operands)-1}]'
                    code = INT.sub(slot, block)
                    setup.emit((len(operands), code), operands)
                    line=next(raw)
                print(f'    const int* pc = hepcat_rate_operands + {start};\n    while (pc < hepcat_rate_operands + {setup.words}) {{\n        const int op=*pc++; const int* a=pc; switch(op) {{',file=cpp)
                for (length, code), i in setup.codes.items():
                    print(f'case {i}: {{ {code} }} pc += {length}; break;',file=cpp)
                print('default: throw std::logic_error("Invalid setup operation"); }}',file=cpp)
                operations.words = setup.words
                cpp.write(line)
            elif (m := COMPONENT.fullmatch(line)):
                start, count = component(raw, operations)
                ty = 'Scalar' if m[1].startswith('template') else 'cdouble'
                cpp.write(line)
                print(f'    return rate_plan<{ty}>(indices, {start}, {count});\n}}',file=cpp)
            else:
                cpp.write(line)
        print('namespace hepcat_generated {\ntemplate<class Scalar> Scalar qed_rate::rate_plan(const Indices& indices, unsigned start, unsigned count) const {',file=cpp)
        print('    static thread_local std::vector<Scalar> v; v.resize(count);\n    const int* pc=hepcat_rate_operands+start;\n    for(unsigned dest=0;dest<count;++dest) {\n        const int op=*pc++; const int* a=pc;\n        switch(op){',file=cpp)
        for code, i in operations.codes.items():
            if code=='sum':
                print(f'case {i}: {{ int n=*pc++; Scalar s=v[*pc++]; while(--n) {{s=s+v[*pc++];}} v[dest]=s; break; }}',file=cpp)
            else:
                length, expr = code
                print(f'case {i}: v[dest] = {expr}; pc+={length}; break;',file=cpp)
        print('default: throw std::logic_error("Invalid arithmetic operation"); } } return v[count-1]; } }',file=cpp)
    header=(source/'process.hpp').read_text()
    match='    Parameters parameters;'
    if header.count(match)!=1:raise ValueError('Missing class parameters')
    (output/'process.hpp').write_text(header.replace(match,'    template<class Scalar> Scalar rate_plan(const Indices&, unsigned, unsigned) const;\n'+match))
    asm = ('.section __TEXT,__const\n' if sys.platform=='darwin' else '.section .rodata\n')
    label=('_' if sys.platform=='darwin' else '')+'hepcat_rate_operands'
    (output/'data.S').write_text(asm+f'.p2align 2\n.globl {label}\n{label}:\n.incbin "{words.resolve()}"\n')


def main():
    p=argparse.ArgumentParser(description=__doc__)
    for name in ('source','output','spinas_include','spinas_library'):
        p.add_argument(name,type=Path)
    args=p.parse_args()
    compact(args.source,args.output)
    # The class layout is identical in the bridge and in the table evaluator.
    cmd=['clang++','-O2','-std=c++17','-fPIC','-dynamiclib' if sys.platform=='darwin' else '-shared',
         '-I'+str(args.output),'-I'+str(args.source),'-I'+str(args.spinas_include),
         args.output/'plan.cpp',args.output/'data.S',HERE/'rate-spinas-bridge.cpp',
         args.spinas_library,'-o',args.output/'rate.dylib']
    subprocess.run(cmd,check=True)

if __name__=='__main__':
    main()
