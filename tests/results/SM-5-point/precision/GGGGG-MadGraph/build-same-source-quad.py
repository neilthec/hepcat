"""Compile explicit generated object members, avoiding make's include-generation rules."""
from pathlib import Path
import json
import shutil
import subprocess

HERE = Path(__file__).resolve().parent
original = HERE / 'reference-53-retry-1'
promoted = HERE / 'reference-113'
shutil.copyfile(original / 'process/Source/cuts.inc', promoted / 'process/Source/cuts.inc')
compiler = '/opt/homebrew/bin/gfortran-16'
flags = ['-w', '-fPIC', '-ffixed-line-length-none', '-freal-8-real-16']
commands = []
with (HERE / 'quad-same-source-build.log').open('w') as log:
    def execute(command, directory):
        subprocess.run(command, cwd=directory, stdout=log, stderr=subprocess.STDOUT,
                       check=True, timeout=60)
        commands.append({'Command': command, 'Directory': str(directory)})
    for library, subdirectory in [('libmodel.a', 'MODEL'), ('libdhelas.a', 'DHELAS')]:
        members = subprocess.check_output(['ar', 't', str(original / 'process/lib' / library)], text=True).splitlines()
        objects = [name for name in members if name.endswith('.o')]
        assert objects and len(set(objects)) == len(objects)
        directory = promoted / 'process/Source' / subdirectory
        for member in objects:
            source = Path(member).with_suffix('.f').name
            assert (directory / source).is_file()
            execute([compiler, *flags, '-c', '-o', member, source], directory)
        destination = HERE / library.replace('.a', '-113-same-source.a')
        execute(['ar', 'rcs', str(destination), *(str(directory / member) for member in objects)], HERE)
    matrix = next((promoted / 'process/SubProcesses').glob('P*/matrix.f'))
    execute([compiler, *flags, '-c', '-o', 'matrix.o', 'matrix.f'], matrix.parent)
    execute([compiler, '-o', str(HERE / 'quad-stream-same-source'), str(HERE / 'quad-stream.f90'),
             str(matrix.parent / 'matrix.o'), str(HERE / 'libmodel-113-same-source.a'),
             str(HERE / 'libdhelas-113-same-source.a')], HERE)
(HERE / 'quad-same-source-build.json').write_text(json.dumps(commands, indent=2) + '\n')
if not (HERE / 'madgraph-113-stream-before-include-restoration.txt').exists():
    shutil.copyfile(HERE / 'madgraph-113-stream.txt', HERE / 'madgraph-113-stream-before-include-restoration.txt')
with (HERE / 'madgraph-113-stream.txt').open('w') as output:
    subprocess.run([str(HERE / 'quad-stream-same-source'), str(HERE / 'points-1000.f64')],
                   stdout=output, stderr=subprocess.STDOUT, check=True, timeout=60)
unchanged = (HERE / 'madgraph-113-stream.txt').read_bytes() == (HERE / 'madgraph-113-stream-before-include-restoration.txt').read_bytes()
print('ALL32000_RAW_QUAD_OUTPUTS_UNCHANGED_BY_INCLUDE_RESTORATION', unchanged)
