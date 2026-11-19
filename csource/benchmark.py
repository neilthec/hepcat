"""Generate the two fixtures, time operations in fresh processes, and verify results."""
import argparse
import ctypes
import hashlib
import json
import os
from pathlib import Path
import shutil
import statistics
import subprocess
import sys
import tempfile
import time


if sys.platform == "darwin":
    _info = ctypes.CDLL("/usr/lib/libproc.dylib", use_errno=True).proc_pidinfo
    _info.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_uint64,
                      ctypes.c_void_p, ctypes.c_int]
    _info.restype = ctypes.c_int


def resident_bytes(pid):
    if sys.platform == "darwin":
        # The two leading uint64 fields of proc_taskinfo are virtual and resident size.
        buffer = (ctypes.c_uint64 * 64)()
        if _info(pid, 4, 0, buffer, ctypes.sizeof(buffer)) < 16:
            raise OSError(f"proc_pidinfo could not read pid {pid}")
        return buffer[1]
    with open(f"/proc/{pid}/status", encoding="ascii") as status:
        for line in status:
            if line.startswith("VmRSS:"):
                return 1024 * int(line.split()[1])
    raise RuntimeError("process resident memory is unavailable")


def measure(command, environment, folder, log):
    with tempfile.TemporaryDirectory(dir=folder) as tmp, log.open("w+") as output:
        phase = Path(tmp) / "phase"
        env = dict(environment, HEPCAT_BENCH_PHASE=str(phase))
        proc = subprocess.Popen(command, env=env, stdout=output, stderr=subprocess.STDOUT)
        deadline = time.monotonic() + 900
        ready, go, done, ack = [phase.with_suffix("." + s) for s in ("ready","go","done","ack")]
        try:
            while not ready.exists():
                if proc.poll() is not None or time.monotonic() > deadline:
                    raise RuntimeError(f"missing start signal, see {log}")
                time.sleep(.005)
            baseline = peak = resident_bytes(proc.pid)
            samples = 0
            go.touch()
            while True:
                peak = max(peak, resident_bytes(proc.pid))
                samples += 1
                if done.exists():
                    break
                if time.monotonic() > deadline or proc.poll() is not None:
                    raise RuntimeError(f"missing finish signal, see {log}")
                time.sleep(.00025)
            ack.touch()
            if proc.wait(60):
                raise RuntimeError(f"benchmark failed, see {log}")
        finally:
            if proc.poll() is None:
                proc.kill()
                proc.wait()
        output.seek(0)
        return (dict(BaselineRSSBytes=baseline, SampledPeakRSSBytes=peak,
                     AdditionalRSSBytes=peak-baseline, RSSSamples=samples),output.read())


def main():
    here = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=here.parent)
    parser.add_argument("--kernel", default="WolframKernel")
    parser.add_argument("--binary", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--cases", nargs="+", choices=["qed","wwww"], default=["qed","wwww"])
    parser.add_argument("--modes", nargs="+", choices=["generate","reduce","prepare","native","validate"],
                        default=["generate","reduce","prepare","native","validate"])
    args = parser.parse_args()
    if args.repeats<1:
        parser.error("--repeats must be positive")
    out=args.out.resolve()
    out.mkdir(parents=True,exist_ok=True)
    for case in args.cases:
        env=dict(os.environ, HEPCAT_ROOT=str(args.root.resolve()),HEPCAT_BENCH_DIR=str(out),
                 HEPCAT_BENCH_CASE=case)
        for mode in args.modes:
            env["HEPCAT_BENCH_MODE"]=mode
            wolfram=[args.kernel,"-script",str(here/"benchmark.wls")]
            if mode in ("generate","validate"):
                subprocess.run(wolfram,env=env,check=True)
                continue
            rows=[]
            for rep in range(1,args.repeats+1):
                name=f"{case}-{mode}-{rep}"
                command=([str(args.binary.resolve()), str(out/f"{case}-input.txt"),
                          str(out/f"{case}-native.txt")] if mode=="native" else wolfram)
                mem,stdout=measure(command,env,out,out/(name+".log"))
                row=(json.loads(stdout) if mode=="native"
                     else json.loads((out/f"{case}-{mode}.json").read_text()))
                output_path = (out/f"{case}-native.txt" if mode=="native" else out/f"{case}-{mode}.wxf")
                output_hash = hashlib.sha256(output_path.read_bytes()).hexdigest()
                # Wolfram preparation uses time budgets, so different runs can
                # legitimately return different expressions. Validate each one.
                shutil.copyfile(output_path, out/(name+output_path.suffix))
                rows.append(dict(row, OutputSHA256=output_hash, **mem))
                (out/(name+".json")).write_text(json.dumps(rows[-1],indent=2)+"\n")
            record=dict(Case=case, Method=mode, Repeats=args.repeats, Runs=rows,
                        MedianSeconds=statistics.median(r["Seconds"] for r in rows),
                        MaxSampledRSSBytes=max(r["SampledPeakRSSBytes"] for r in rows))
            (out/f"{case}-{mode}-runs.json").write_text(json.dumps(record,indent=2)+"\n")
            print(case,mode,record["MedianSeconds"],record["MaxSampledRSSBytes"],flush=True)


if __name__=="__main__":
    main()
