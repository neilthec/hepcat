"""Replay both independent squared amplitudes at the same binary128 two-body phase-space point."""
import csv
from decimal import Decimal, localcontext
import hashlib
from pathlib import Path
import re
import shutil
import sys

COMPONENT_LIMIT = 16 * sys.float_info.epsilon


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def chart_points(source, destination, masses):
    """Keep the sampled beam momentum and scattering direction, reconstruct the dependent components."""
    changes = {}
    with localcontext() as ctx, source.open(newline='') as inp, destination.open('w', newline='') as out:
        ctx.prec = 80
        m = [Decimal(float(x)) for x in masses]
        keys = [f'p{i}_{j}' for i in range(1, 5) for j in range(4)]
        rows = csv.DictReader(inp)
        writer = csv.writer(out)
        writer.writerow(['point', *keys])
        for row in rows:
            old = [Decimal(float(row[k])) for k in keys]
            beam = old[3]
            e1, e2 = [(beam**2 + x**2).sqrt() for x in m[:2]]
            energy = e1 + e2
            if beam <= 0 or energy <= m[2] + m[3]:
                raise ValueError('Invalid incoming beam or two-body threshold.')
            radius = ((energy-m[2]-m[3])*(energy+m[2]+m[3])*
                      (energy-m[2]+m[3])*(energy+m[2]-m[3])).sqrt() / (2*energy)
            direction = old[9:12]
            norm = sum(x*x for x in direction).sqrt()
            if not norm:
                raise ValueError('Degenerate scattering direction.')
            vec = [radius*x/norm for x in direction]
            q = [e1, 0, 0, beam, e2, 0, 0, -beam, (radius**2+m[2]**2).sqrt(), *vec,
                 (radius**2+m[3]**2).sqrt(), *[-x for x in vec]]
            index = int(row['point'])
            shift = max(abs(x-y) for x, y in zip(old, q)) / energy
            if shift > COMPONENT_LIMIT or index in changes:
                raise ValueError('Chart adjustment exceeds input rounding or point ID is duplicated.')
            changes[index] = float(shift)
            writer.writerow([index, *[format(Decimal(x), '.48e') for x in q]])
    if not changes:
        raise ValueError('No saved outlier points.')
    return changes


def read_values(path, key):
    with path.open(newline='') as f:
        result = {}
        for row in csv.DictReader(f):
            index = int(row['point'])
            if index in result or not Decimal(row[key]).is_finite() or Decimal(row[key]) < 0:
                raise ValueError('Invalid or duplicate squared amplitude.')
            result[index] = row[key]
        return result


def build_spinas(chart, export, include, config, manifest, gxx, run):
    # Preserve the generated process algebra and all SPINAS algorithms. GCC's
    # binary128 elementary functions live in libquadmath on both macOS and Linux.
    target = chart / 'spinas'
    target.mkdir()
    header = Path(__file__).with_name('spinas-quad.hpp')
    shutil.copyfile(header, target / header.name)
    math = re.compile(r'\bstd::(abs|acos|arg|atan2|copysign|cos|floor|fma|frexp|hypot|ldexp|polar|sin|sqrt|to_string)\b')
    sources = sorted(include.glob('*.h'))
    units = ['cmatrix', 'cvector', 'particle', 'sproduct', 'propagator']
    sources += [include.parent / 'source' / (name + '.cpp') for name in units]
    sources += [export / name for name in ('spinas-runtime.hpp', 'process.hpp', 'process.cpp')]
    hashes = {}
    for source in sources:
        if source.name in hashes:
            raise ValueError('Ambiguous SPINAS source name.')
        hashes[source.name] = digest(source)
        text = source.read_text()
        if source.name == 'types.h':
            text, n = re.subn(r'#if\s+defined\s+WITH_LONG_DOUBLE\s+typedef long double ldouble;'
                              r'\s+#else\s+typedef double ldouble;\s+#endif',
                              'using ldouble = hepcat_quad::real;', text)
            if n != 1:
                raise ValueError('Unrecognized SPINAS scalar type declaration.')
        (target / source.name).write_text(math.sub(r'hepcat_quad::\1', text))
    order = ','.join(str(x-1) for x in config['LegOrder'])
    widths = ''.join(f'parameters.at({int(i)}) = 0; ' for i in manifest.get('WidthIndices', []))
    normalize = repr(float(config.get('SPINASNormalizationFactor', 1)))
    driver = '''#include "process.hpp"
#include <fstream>
#include <sstream>
using Process = hepcat_generated::@CLASS@;
int main(int argc, char** argv) {
    if (argc != 3) return 1;
    Process process;
    process.set_widths_enabled(false);
    auto parameters = process.get_parameters();
    @WIDTHS@
    process.set_parameters(parameters);
    const std::array<int,4> order{@ORDER@};
    std::ifstream in(argv[1]);
    std::ofstream out(argv[2]);
    std::string line, value;
    std::getline(in,line);
    out << "point,spinas\\n";
    while (std::getline(in,line)) {
        std::stringstream row(line);
        std::getline(row,value,',');
        auto id = std::stoul(value);
        Process::Momenta p{};
        for (int slot : order) for (auto& x : p[slot]) {
            if (!std::getline(row,value,',')) return 2;
            x = strtoflt128(value.c_str(),nullptr);
        }
        process.set_momenta(p);
        const auto result = process.amp2() * @NORMALIZATION@;
        if (!finiteq(result) || result<0) return 3;
        out << id << ',' << hepcat_quad::to_string(result) << '\\n';
    }
    return !out || std::numeric_limits<ldouble>::digits != 113;
}
'''
    (target / 'main.cpp').write_text(driver.replace('@CLASS@', manifest['Class']).replace('@ORDER@', order)
                                      .replace('@WIDTHS@', widths).replace('@NORMALIZATION@', normalize))
    executable = target / 'evaluate'
    run([gxx, '-std=gnu++23', '-I'+str(target), '-include', 'spinas-quad.hpp',
         *[target/(n+'.cpp') for n in units], target / 'process.cpp', target / 'main.cpp',
         '-o', executable, '-lquadmath'], target, chart/'build-spinas.log')
    return executable, hashes


def build_calchep(chart, old, commands, run):
    # Record the wide accumulator just before CalcHEP's public double return.
    # The probe also checks that converting it back exactly reproduces that ABI.
    source = old / 'reference-GCC128'
    sqme = (source / 'sqme.c').read_text()
    include = (source / 'sqme.inc').read_text()
    if sqme.count('"sqme.inc"') != 1 or include.count('return (double)result;') != 1:
        raise ValueError('Unsupported CalcHEP return instrumentation.')
    (chart / 'sqme.c').write_text(sqme.replace('"sqme.inc"', '"chart-sqme.inc"'))
    (chart / 'chart-sqme.inc').write_text('REAL hepcat_last_squared;\n' +
        include.replace('return (double)result;', 'hepcat_last_squared=result; return (double)result;'))
    library = chart / 'chart.so'
    link = [str(x) for x in commands[-2]]
    link[link.index(str(source/'sqme.c'))] = str(chart/'sqme.c')
    link[link.index('-o')+1] = str(library)
    run(link, chart, chart/'build-calchep.log')
    original = (old/'replay.c').read_text()
    before, marker, after = original.partition(' FILE*input=fopen(argv[2]')
    if not marker:
        raise ValueError('Unsupported CalcHEP validation bridge.')
    match = re.search(r'interface->sqme\(1,([^,]+),momenta,NULL,&error\)', after)
    if not match:
        raise ValueError('Missing CalcHEP coupling.')
    probe = before + '''
 REAL* wide=dlsym(handle,"hepcat_last_squared");
 if(!wide || FLT128_MANT_DIG!=113) return 12;
 FILE* in=fopen(argv[2],"r");FILE* out=fopen(argv[4],"w");
 if(!in || !out) return 13;
 fgets(line,sizeof line,in); fputs("point,calchep\\n",out);
 while(fgets(line,sizeof line,in)){
   char* end=NULL; unsigned id=strtoul(line,&end,10); REAL physical[16],momenta[16];
   for(int i=0;i<16;i++){if(*end!=',')return 14;physical[i]=strtoflt128(end+1,&end);}
   for(int i=0;i<4;i++)for(int j=0;j<4;j++)momenta[4*i+j]=physical[4*order[i]+j];
   int error=0;*wide=nanq("");
   double result=interface->sqme(1,@GG@,momenta,NULL,&error);
   if(error || !finiteq(*wide) || *wide<0 || (double)*wide!=result) return 15;
   char number[128];quadmath_snprintf(number,sizeof number,"%.36Qg",*wide);
   fprintf(out,"%u,%s\\n",id,number);
 }
 return fclose(out)!=0;
}
'''
    (chart/'probe.c').write_text(probe.replace('@GG@', match.group(1)))
    command = [str(x) for x in commands[-1]]
    command[command.index(str(old/'replay.c'))] = str(chart/'probe.c')
    executable = chart/'evaluate'
    command[command.index('-o')+1] = str(executable)
    run(command, chart, chart/'build-probe.log')
    return executable, library


def replay(output, export, spinas_header, manifest, config, raw_outliers, gcc, commands, seed, tolerance, run):
    chart = output/'same-chart'
    chart.mkdir()
    order = config['LegOrder']
    masses = [manifest['Masses'][i-1] for i in order]
    points = output/'calchep-chart.csv'
    shifts = chart_points(output/'points.csv', points, masses)
    source_hash = digest(points)
    gxx = str(Path(gcc).with_name(re.sub(r'gcc(?!.*gcc)', 'g++', Path(gcc).name)))
    spinas, hashes = build_spinas(chart, export, Path(spinas_header), config, manifest, gxx, run)
    cpp_input = digest(points)
    run([spinas, points, chart/'spinas.csv'], chart, chart/'spinas.log')
    ch, library = build_calchep(chart, output, commands, run)
    ch_input = digest(points)
    run([ch,library,points,output/'parameters.csv',chart/'calchep.csv',str(tolerance),'4'],
        chart, chart/'calchep.log')
    if cpp_input != ch_input or cpp_input != source_hash or digest(points) != source_hash:
        raise ValueError('The common chart input changed.')
    values = [read_values(file, key) for file,key in [
        (output/'points.csv','hepcat'),(output/'points.csv','calchep'),(output/'GCC128.csv','replayed_calchep'),
        (chart/'spinas.csv','spinas'),(chart/'calchep.csv','calchep')]]
    if any(v.keys() != shifts.keys() for v in values):
        raise ValueError('Incomplete precision replay.')
    rows=[]
    with localcontext() as ctx:
        ctx.prec=80
        for index,shift in shifts.items():
            native,reference,unprojected,left,right = [v[index] for v in values]
            a,b = Decimal(left),Decimal(right)
            scale=max(a,b)
            relative=abs(a-b)/scale if scale else Decimal(0)
            original=Decimal(native)
            scale=max(a,original)
            native_change=abs(a-original)/scale if scale else Decimal(0)
            rows.append(dict(Point=index,RawSPINAS=native,RawCalcHEP=reference,UnprojectedCalcHEP=unprojected,
                             SPINAS=left,CalcHEP=right,RelativeError=float(relative),ComponentChangeOverECM=shift,
                             RawToRefinedSPINASRelativeChange=float(native_change)))
    failures=sum(max(r['RelativeError'], r['RawToRefinedSPINASRelativeChange'])>tolerance for r in rows)
    return dict(Status='FAIL' if failures else 'PASS',Seed=seed,Masses=masses,RequestedPoints=len(rows),ComparedPoints=len(rows),
                Mismatches=failures, Errors=0, RelativeTolerance=tolerance, SPINASMantissaBits=113,
                CalcHEPMantissaBits=113, MaximumComponentChangeOverECM=max(shifts.values()),
                ComponentChangeLimitOverECM=COMPONENT_LIMIT, MaximumRelativeError=max(r['RelativeError'] for r in rows),
                MaximumSPINASRelativeChange=max(r['RawToRefinedSPINASRelativeChange'] for r in rows),
                SourceOutliersSHA256=digest(raw_outliers), InputSHA256=source_hash, SPINASInputSHA256=cpp_input,
                CalcHEPInputSHA256=ch_input, SPINASInputSourcesSHA256=hashes, SPINASCompiler=gxx,
                Rows=rows)
