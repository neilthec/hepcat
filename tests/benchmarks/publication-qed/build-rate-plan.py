"""Compile a grouped SPINAS export with operand tables for repeated C++ arithmetic.

This benchmark-only build keeps set_momenta, helicity dispatch, precision
selection and the spin sum from process.cpp. The group of operations in each
temporary assignment is compiled by the C++ compiler. Its integer subscripts
are read from a table. Use this build only with the label 'operand tables'.
"""
import argparse
import json
from array import array
from pathlib import Path
import re
import subprocess
import sys

HERE = Path(__file__).resolve().parent
COMPONENT = re.compile(r'(template<class Scalar> Scalar|cdouble) qed_rate::component_(\d+)\(const Indices& indices\) const \{\n')
ASSIGN = re.compile(r'    (?:const (?:Scalar|cdouble) v(\d+) = |return )(.+);\n')
OPERAND = re.compile(r'\bv(\d+)|\b(indices|chains|partials|invariants|coefficients|parameters|derived)\[(\d+)\]')
INT = re.compile(r'(?<![\w.])-?\d+(?![\w.])')
LONG_SUM = re.compile(r'v\d+(?: \+ v\d+){5,}\Z')


class Table:
    def __init__(self, file):
        self.file = file
        self.words = 0
        self.codes = {}

    def emit(self, operation, operands, dest=None):
        index = self.codes.setdefault(operation, len(self.codes))
        header = [index]
        if dest is not None:
            # Keep the destination and opcode in the original one-word header.
            # Rare large programs can still address a full signed 32-bit slot.
            header = [dest * 256 + index] if index < 256 and dest < (1 << 23) else [~index, dest]
        array('i', [*header, *operands]).tofile(self.file)
        self.words += len(header) + len(operands)


def component(lines, table):
    start = table.words
    count = 0
    returned = False
    # Scan one component twice. Only last-use positions and slot numbers need
    # to stay in memory when the generated expression spans millions of bytes.
    offset = lines.tell()
    last_use = array('i')
    for line in iter(lines.readline, ''):
        if line == '}\n':
            break
        m = ASSIGN.fullmatch(line)
        if not m or (m[1] is not None and int(m[1]) != len(last_use)):
            raise ValueError('Unexpected generated assignment: ' + line[:100])
        for match in re.finditer(r'\bv(\d+)\b', m[2]):
            last_use[int(match[1])] = len(last_use)
        last_use.append(-1)
    lines.seek(offset)
    slots = array('i')
    free = []
    peak = 0
    for line in iter(lines.readline, ''):
        if line == '}\n':
            if not returned:
                raise ValueError('Missing component return')
            return start, count, peak, slots[-1]
        m = ASSIGN.fullmatch(line)
        if returned or not m:
            raise ValueError('Unexpected component body: ' + line[:100])
        returned = m[1] is None
        expr = m[2]

        def read(index):
            if last_use[index] == count:
                free.append(slots[index])
                last_use[index] = -1
            return slots[index]

        if LONG_SUM.fullmatch(expr):
            items = [read(int(v[1:])) for v in expr.split(' + ')]
            operands = [len(items), *items]
            operation = 'sum'
        else:
            operands = []
            def sub(m):
                i = len(operands)
                operands.append(read(int(m[1])) if m[1] is not None else int(m[3]))
                return f'{m[2] or "v"}[a[{i}]]'
            code = OPERAND.sub(sub, expr)
            operation = (len(operands), code)
        if free:
            dest = free.pop()
        else:
            dest = peak
            peak += 1
        slots.append(dest)
        table.emit(operation, operands, dest)
        if not returned and last_use[count] == -1:
            free.append(dest)
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
        stats = []
        for line in iter(raw.readline, ''):
            if '    invariants[0] =' in line or line.startswith(('    chains[0]', '    coefficients[0]', '    sums[', '    insertions[', '    massless_bras[', '    for (int ls :','    for (int rs :')):
                # All subsequent generated invariant and chain assignments end at ready=true.
                start = setup.words
                while line.strip() != 'ready = true;':
                    if not line:
                        raise ValueError('Truncated set_momenta')
                    if not line.strip():
                        line = raw.readline()
                        continue
                    block = line
                    while not block.rstrip().endswith(';'):
                        continuation = raw.readline()
                        if not continuation:
                            raise ValueError('Truncated setup statement')
                        block += continuation
                    if not any(name + '[' in block for name in ('invariants', 'chains', 'partials', 'coefficients', 'sums', 'insertions', 'massive_bras', 'massless_bras')):
                        raise ValueError('Unexpected setup: '+block[:100])
                    operands = []
                    def slot(m):
                        operands.append(int(m[0]))
                        return f'a[{len(operands)-1}]'
                    code = INT.sub(slot, block)
                    setup.emit((len(operands), code), operands)
                    line=raw.readline()
                print(f'    const int* pc = hepcat_rate_operands + {start};\n    while (pc < hepcat_rate_operands + {setup.words}) {{\n        const int op=*pc++; const int* a=pc; switch(op) {{',file=cpp)
                for (length, code), i in setup.codes.items():
                    print(f'case {i}: {{ {code} }} pc += {length}; break;',file=cpp)
                print('default: throw std::logic_error("Invalid setup operation"); }}',file=cpp)
                operations.words = setup.words
                cpp.write(line)
            elif (m := COMPONENT.fullmatch(line)):
                start, count, peak, result = component(raw, operations)
                stats.append(dict(component=int(m[2]), instructions=count, slots=peak))
                ty = 'Scalar' if m[1].startswith('template') else 'cdouble'
                cpp.write(line)
                print(f'    return rate_plan<{ty}>(indices, {start}, {count}, {peak}, {result});\n}}',file=cpp)
            else:
                cpp.write(line)
        print('namespace hepcat_generated {\ntemplate<class Scalar> Scalar qed_rate::rate_plan(const Indices& indices, unsigned start, unsigned count, unsigned slots, unsigned result) const {',file=cpp)
        print('    static thread_local std::vector<Scalar> v; if (v.size() < slots) v.resize(slots);\n    const int* pc=hepcat_rate_operands+start;\n    for(unsigned step=0;step<count;++step) {\n        const int header=*pc++;\n        const int op = header >= 0 ? header & 255 : ~header;\n        const int dest = header >= 0 ? header >> 8 : *pc++;\n        const int* a=pc;\n        switch(op){',file=cpp)
        for code, i in operations.codes.items():
            if code=='sum':
                print(f'case {i}: {{ int n=*pc++; Scalar s=v[*pc++]; while(--n) {{s=s+v[*pc++];}} v[dest]=s; break; }}',file=cpp)
            else:
                length, expr = code
                print(f'case {i}: v[dest] = {expr}; pc+={length}; break;',file=cpp)
        print('default: throw std::logic_error("Invalid arithmetic operation"); } } return v[result]; } }',file=cpp)
    (output/'plan-stats.json').write_text(json.dumps(stats) + '\n')
    header=(source/'process.hpp').read_text()
    match='    Parameters parameters;'
    if header.count(match)!=1:raise ValueError('Missing class parameters')
    (output/'process.hpp').write_text(header.replace(match,'    template<class Scalar> Scalar rate_plan(const Indices&, unsigned, unsigned, unsigned, unsigned) const;\n'+match))
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
