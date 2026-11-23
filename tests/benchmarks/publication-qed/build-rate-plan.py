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
import shutil
import tempfile

HERE = Path(__file__).resolve().parent
COMPONENT = re.compile(r'(template<class Scalar> Scalar|cdouble) qed_rate::component_(\d+)\(const Indices& indices\) const \{\n')
STAGE = re.compile(r'(template<class Scalar> Scalar|cdouble) qed_rate::component_(\d+)_stage_(\d+)\(const Indices& indices, (?:Scalar|cdouble)\* shared\) const \{\n')
ASSIGN = re.compile(r'    (?:const (?:Scalar|cdouble) v(\d+) = |return )(.+);\n')
OPERAND = re.compile(r'\bv(\d+)|\b(indices|chains|partials|invariants|coefficients|parameters|derived|shared|spin_bits|spin_pairs)\[(\d+)\]')
INT = re.compile(r'(?<![\w.])-?\d+(?![\w.])')
LONG_SUM = re.compile(r'v\d+(?: \+ v\d+){5,}\Z')

SPIN_BIT = r'\(indices\[(\d+)\]\[0\] > 0\)'
CHAIN_SLOT = re.compile(r'(chains\[\d+\])\[2 \* (?:0|' + SPIN_BIT + r') \+ (?:0|' + SPIN_BIT + r')\]')


def index_spin_pairs(expression, stride):
    def pair(match):
        left = int(match[2]) + 1 if match[2] else 0
        right = int(match[3]) + 1 if match[3] else 0
        index = str(left * stride + right)
        return match[1] + ('[spin_pairs[' + index + ']]' if left or right else '[0]')
    return re.sub(SPIN_BIT, lambda m: 'spin_bits[' + str(int(m[1]) + 1) + ']',
                  CHAIN_SLOT.sub(pair, expression))


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


def component(lines, table, reuse=True, shared_slots=None, base=0, stride=0):
    start = table.words
    count = 0
    returned = False
    # Scan one component twice. Only last-use positions and slot numbers need
    # to stay in memory when the generated expression spans millions of bytes.
    offset = lines.tell()
    last_use = array('i')
    if reuse:
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
            return (start, count, peak, slots[-1]) if reuse else (start, count, count, count-1)
        m = ASSIGN.fullmatch(line)
        if returned or not m or (m[1] is not None and int(m[1]) != count):
            raise ValueError('Unexpected component body: ' + line[:100])
        returned = m[1] is None
        expr = index_spin_pairs(m[2], stride) if stride else m[2]
        if shared_slots is not None and expr.startswith('shared['):
            write = re.match(r'shared\[(\d+)\] = ', expr)
            if write:
                shared_slots[int(write[1])] = base + count
                expr = expr[write.end():]

        def read(index):
            if not reuse:
                return index
            if last_use[index] == count:
                free.append(slots[index])
                last_use[index] = -1
            return slots[index]

        if LONG_SUM.fullmatch(expr):
            items = [base + read(int(v[1:])) for v in expr.split(' + ')]
            operands = [len(items), *items]
            operation = 'sum'
        else:
            operands = []
            def sub(m):
                i = len(operands)
                group = m[2] or "v"
                if m[1] is not None:
                    value = base + read(int(m[1]))
                elif group == "shared" and shared_slots is not None:
                    value = shared_slots[int(m[3])]
                    group = "v"
                else:
                    value = int(m[3])
                operands.append(value)
                return f'{group}[a[{i}]]'
            code = OPERAND.sub(sub, expr)
            operation = (len(operands), code)
        if reuse:
            if free:
                dest = free.pop()
            else:
                dest = peak
                peak += 1
            slots.append(dest)
            table.emit(operation, operands, dest)
            if not returned and last_use[count] == -1:
                free.append(dest)
        else:
            table.emit(operation, operands)
        count += 1
    raise ValueError('Truncated component')


def narrow_stage(table, start, narrow):
    """Move a completed stage to the 16-bit stream if every word fits."""
    wide = table.file
    wide.seek(4 * start)
    words = array('i')
    words.fromfile(wide, table.words - start)
    if min(words) < 0 or max(words) > 65535:
        return None
    offset = narrow.tell() // 2
    array('H', words).tofile(narrow)
    wide.seek(4 * start)
    wide.truncate()
    table.words = start
    return offset


def compact(source, output, reuse=False):
    output.mkdir(parents=True, exist_ok=True)
    header = (source/'process.hpp').read_text()
    leg_stride = 1 + int(re.search(r'leg_count = (\d+)', header)[1])
    words = output/'operands.i32'
    assert array('i').itemsize == 4
    with words.open('w+b') as binary, tempfile.TemporaryFile() as narrow, (source/'process.cpp').open() as raw, (output/'plan.cpp').open('w') as cpp:
        operations = Table(binary)
        setup = Table(binary)
        print('#include <vector>\nextern "C" const int hepcat_rate_operands[];', file=cpp)
        stats = []
        totals = {}
        producers = {}
        current_component = None
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
            elif (m := COMPONENT.fullmatch(line) or STAGE.fullmatch(line)):
                staged = m.re is STAGE
                unified = staged and not reuse
                index = int(m[2])
                base = totals.get(index, 0) if unified else 0
                # Shared reads can directly address a producer in an earlier stage.
                if unified and base == 0:
                    producers.clear()
                start, count, peak, result = component(raw, operations, reuse, producers if unified else None, base, leg_stride if staged else 0)
                if unified:
                    totals[index] = base + count
                record = dict(component=int(m[2]), instructions=count, slots=peak)
                short_start = None
                if staged:
                    record['stage'] = int(m[3])
                    if not reuse and count <= 65536:
                        short_start = narrow_stage(operations, start, narrow)
                    record['operand_bits'] = 16 if short_start is not None else 32
                stats.append(record)
                ty = 'Scalar' if m[1].startswith('template') else 'cdouble'
                cpp.write(line)
                if short_start is not None:
                    ty += ', unsigned short'
                    start = short_start
                workspace = (f', shared, {base}' if unified else ', shared') if staged else ''
                print(f'    return rate_plan<{ty}>(indices, {start}, {count}, {peak}, {result}{workspace});\n}}',file=cpp)
            else:
                if totals and '::component_' in line:
                    wrapper = re.search(r'::component_(\d+)(?:_sum)?\(', line)
                    current_component = int(wrapper[1]) if wrapper else None
                if current_component in totals and 'auto* shared = spin_workspace<' in line:
                    line = re.sub(r'>\(\d+\);', f'>({totals[current_component]});', line)
                cpp.write(line)
        wide_bytes = binary.tell()
        has_narrow = narrow.tell() != 0
        template = 'template<class Scalar, class Word>' if has_narrow else 'template<class Scalar>'
        if has_narrow:
            print(f'// Narrow operand stream byte offset = {wide_bytes}', file=cpp)
            narrow.seek(0)
            shutil.copyfileobj(narrow, binary)
            print('#include <type_traits>\nextern "C" const unsigned short hepcat_rate_narrow[];', file=cpp)
            print('template<class Scalar> static std::vector<Scalar>& rate_scratch() { static thread_local std::vector<Scalar> v; return v; }', file=cpp)
        has_stages = any('stage' in record for record in stats)
        shared_arg = ', Scalar* shared' if has_stages else ''
        if totals: shared_arg += ', unsigned base'
        print('namespace hepcat_generated {\n' + template + ' Scalar qed_rate::rate_plan(const Indices& indices, unsigned start, unsigned count, unsigned slots, unsigned result' + shared_arg + ') const {',file=cpp)
        scratch = 'auto& v=rate_scratch<Scalar>();' if has_narrow else 'static thread_local std::vector<Scalar> v;'
        if totals:
            if not has_narrow:
                scratch = 'static thread_local std::vector<Scalar> local;'
            else:
                scratch = 'auto& local=rate_scratch<Scalar>();'
            print('    ' + scratch + ' Scalar* v=shared; if (!v) { if (local.size()<slots) local.resize(slots); v=local.data(); }', file=cpp)
        else:
            print('    ' + scratch + ' if (v.size() < slots) v.resize(slots);', file=cpp)
        if has_stages:
            print('    int spin_bits[leg_count+1]{}; int spin_pairs[(leg_count+1)*(leg_count+1)];',file=cpp)
            print('    for (size_t i=0;i<leg_count;++i) spin_bits[i+1] = indices[i][0] > 0;',file=cpp)
            print('    for (size_t i=0;i<=leg_count;++i) for (size_t j=0;j<=leg_count;++j) spin_pairs[i*(leg_count+1)+j] = 2*spin_bits[i]+spin_bits[j];',file=cpp)
        if has_narrow:
            print('    const Word* pc;\n    if constexpr (std::is_same_v<Word, unsigned short>) pc=hepcat_rate_narrow+start;\n    else pc=hepcat_rate_operands+start;',file=cpp)
        else:
            print('    const int* pc=hepcat_rate_operands+start;', file=cpp)
        print('    for(unsigned step=0;step<count;++step) {',file=cpp)
        if reuse:
            print('        const int header=*pc++;\n        const int op = header >= 0 ? header & 255 : ~header;\n        const int dest = header >= 0 ? header >> 8 : *pc++;',file=cpp)
        else:
            dest = 'base+step' if totals else 'step'
            print('        const int op=*pc++; const unsigned dest=' + dest + ';',file=cpp)
        word = 'Word' if has_narrow else 'int'
        print('        const ' + word + '* a=pc;\n        switch(op){',file=cpp)
        for code, i in operations.codes.items():
            if code=='sum':
                print(f'case {i}: {{ int n=*pc++; Scalar s=v[*pc++]; while(--n) {{s=s+v[*pc++];}} v[dest]=s; break; }}',file=cpp)
            else:
                length, expr = code
                print(f'case {i}: v[dest] = {expr}; pc+={length}; break;',file=cpp)
        result = 'base+result' if totals else 'result'
        print('default: throw std::logic_error("Invalid arithmetic operation"); } } return v[' + result + ']; } }',file=cpp)
    (output/'plan-stats.json').write_text(json.dumps(stats) + '\n')
    match='    Parameters parameters;'
    if header.count(match)!=1:raise ValueError('Missing class parameters')
    shared_parameter = ', Scalar* = nullptr' if has_stages else ''
    if totals: shared_parameter += ', unsigned = 0'
    declaration = 'template<class Scalar, class Word=int>' if has_narrow else 'template<class Scalar>'
    (output/'process.hpp').write_text(header.replace(match,'    ' + declaration + ' Scalar rate_plan(const Indices&, unsigned, unsigned, unsigned, unsigned' + shared_parameter + ') const;\n'+match))
    asm = ('.section __TEXT,__const\n' if sys.platform=='darwin' else '.section .rodata\n')
    label=('_' if sys.platform=='darwin' else '')+'hepcat_rate_operands'
    asm += f'.p2align 2\n.globl {label}\n{label}:\n.incbin "{words.resolve()}"'
    if has_narrow:
        small_label = ('_' if sys.platform == 'darwin' else '') + 'hepcat_rate_narrow'
        asm += f',0,{wide_bytes}\n.p2align 1\n.globl {small_label}\n{small_label}:\n.incbin "{words.resolve()}",{wide_bytes}'
    (output/'data.S').write_text(asm + '\n')


def main():
    p=argparse.ArgumentParser(description=__doc__)
    for name in ('source','output','spinas_include','spinas_library'):
        p.add_argument(name,type=Path)
    p.add_argument('--slot-layout', choices=['sequential', 'compact'], default='sequential',
                   help='Sequential writes use implicit destinations. Compact storage reuses slots after their last read.')
    args=p.parse_args()
    compact(args.source,args.output, args.slot_layout == 'compact')
    # The class layout is identical in the bridge and in the table evaluator.
    cmd=['clang++','-O2','-std=c++17','-fPIC','-dynamiclib' if sys.platform=='darwin' else '-shared',
         '-I'+str(args.output),'-I'+str(args.source),'-I'+str(args.spinas_include),
         args.output/'plan.cpp',args.output/'data.S',HERE/'rate-spinas-bridge.cpp',
         args.spinas_library,'-o',args.output/'rate.dylib']
    subprocess.run(cmd,check=True)

if __name__=='__main__':
    main()
