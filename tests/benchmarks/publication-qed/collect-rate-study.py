"""Combine native construction receipts, source inventories and complete-rate sweeps."""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import tarfile


def construction_runs(root):
    newest = {}
    for path in root.rglob("construction.tsv"):
        for row in csv.DictReader(path.read_text().splitlines(), delimiter="\t"):
            family, h = row.get("family", ""), row.get("helicity", "")
            if (not family or set(family) - {"e", "A"} or row.get("points") != "100000"
                    or row.get("status") not in {"PASS", "VALID_SCORE_UNAVAILABLE"}
                    or row["preference"] not in {"physical", "spurious"}):
                continue
            key = family, row["preference"], min(h.count("+"), h.count("-"))
            if key not in newest or path.stat().st_mtime > newest[key][0]:
                newest[key] = (path.stat().st_mtime, {
                    "helicity": h, "seconds": float(row["seconds"]), "status": row["status"],
                    "receipt": str(path.relative_to(root)),
                })
    return newest


def inventory(files=(), archive=None):
    # Combine file names, byte lengths and per-file SHA-256 in a stable order.
    parts = []
    def add(name, length, stream):
        checksum = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1 << 20), b""):
            checksum.update(chunk)
        parts.append((name, length, checksum.hexdigest()))
    if archive:
        with tarfile.open(archive) as tar:
            for member in tar:
                if member.isfile() and member.name.endswith(".c"):
                    with tar.extractfile(member) as stream:
                        add(Path(member.name).name, member.size, stream)
    else:
        for path in files:
            with path.open("rb") as stream:
                add(path.name, path.stat().st_size, stream)
    if not parts:
        raise ValueError("Empty source inventory")
    manifest = json.dumps(sorted(parts), separators=(",", ":")).encode()
    return {"files": len(parts), "bytes": sum(v[1] for v in parts),
            "sha256_file_manifest": hashlib.sha256(manifest).hexdigest()}


def physical_cuts(pairs, photons):
    charges = [1, -1] * pairs + [0] * photons
    def admits_vertex(side):
        return abs(sum(side)) == 1 or (sum(side) == 0 and 1 in side)
    count = 0
    for mask in range(1, 1 << len(charges), 2):
        left = [c for i,c in enumerate(charges) if (mask >> i) & 1]
        right = [c for i,c in enumerate(charges) if not ((mask >> i) & 1)]
        count += min(len(left),len(right)) >= 2 and admits_vertex(left) and admits_vertex(right)
    return count


def read_rate(path, points, sweeps):
    data = {}
    for line in path.read_text().splitlines():
        key, val = line.split()
        data.setdefault(key, []).append(float(val))
    weights = data["value"]
    if (len(weights) != points or len(data["seconds_per_point"]) != sweeps or
            "checksum" not in data or not all(math.isfinite(v) and v>0
            for v in weights + data["seconds_per_point"])):
        raise ValueError(f"Incomplete or invalid timing receipt in {path}")
    return {"seconds_per_point": data["seconds_per_point"],
            "cpu_seconds_per_point": data.get("cpu_seconds_per_point", []),
            "passes_per_sweep": int(data["passes_per_sweep"][0]), "point_weights": weights}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for arg in ("work", "construction_root", "inventory_map", "timings", "output"):
        parser.add_argument(arg,type=Path)
    parser.add_argument("--points",type=int,default=24)
    parser.add_argument("--sweeps",type=int,default=3)
    parser.add_argument("--base-seed",type=int,default=896013)
    args=parser.parse_args()
    receipts=construction_runs(args.construction_root)
    rows=[]
    for f, sources in json.loads(args.inventory_map.read_text()).items():
        p,g=f.count("e")//2,f.count("A")
        n=len(f)
        mg=Path(sources["MadGraph"])
        meta=json.loads((mg/"result.json").read_text())
        assert meta["Masses"] == [1]*(2*p)+[0]*g
        diagrams = (math.factorial(g) if p==1 else math.factorial(p)*math.factorial(p-2)
                    *math.comb(3*p-3,p-2)*math.prod(range(3*p-2,3*p-2+g)))
        assert diagrams==int(meta["DiagramCount"]), (f, diagrams, meta["DiagramCount"])
        row={"family":f,"legs":n,"pairs":p,"photons":g,"labeled":2**g,"bose":g+1,
             "parity_reps":1+g//2,"diagrams":diagrams,"cuts":physical_cuts(p,g),
             "construction":{},"sources":{},"rate":{},
             "MadGraph_optimization":meta["OptimizationLevel"],
             "MadGraph_version":meta["MadGraphVersion"], "seed":args.base_seed+1000*n+100*p}
        for pol in ("physical","spurious"):
            row["construction"][pol]=[receipts[f,pol,h][1] for h in range(1+g//2)]
            direct=args.work/"spinas"/pol/f
            row["sources"][pol]=inventory(direct/x for x in ("process.hpp","process.cpp"))
            table=args.work/"plan"/pol/f
            if (table/"rate.dylib").is_file():
                row["sources"]["table_"+pol]=inventory(table/x for x in ("plan.cpp","process.hpp","operands.i32"))
        row["sources"]["mg"]=inventory(mg.glob("process/SubProcesses/P*/matrix*.f"))
        if sources.get("CalcHEP"):
            ch=Path(sources["CalcHEP"])
            if ch.is_dir():
                assert (ch/"sqme.c").is_file()
                row["sources"]["ch"]=inventory(ch.glob("*.c"))
            else:
                row["sources"]["ch"]=inventory(archive=ch)
        if "ch_generation" in sources:
            row["ch_generation"]=sources["ch_generation"]
        row["source_bytes"]={key:value["bytes"] for key,value in row["sources"].items()}
        for engine in row["sources"]:
            path=args.timings/f"{f}-{engine}.txt"
            if path.exists():
                row["rate"][engine]=read_rate(path,args.points,args.sweeps)
        mg_values=row["rate"]["mg"]["point_weights"]
        for engine,rate in row["rate"].items():
            values=rate["point_weights"]
            err=max(abs(v-w)/max(v,w) for v,w in zip(values,mg_values))
            rate["max_relative_to_mg"]=err
            if err>1e-7:
                raise ValueError(f"{f} {engine}: relative difference {err:g}")
        for pol in ("physical","spurious"):
            direct,table=row["rate"].get(pol),row["rate"].get("table_"+pol)
            if direct and table and direct["point_weights"] != table["point_weights"]:
                raise ValueError(f"Operand table changed a direct result: {f} {pol}")
        rows.append(row)
    assert len(rows)==18 and sum(r["labeled"] for r in rows)==333
    args.output.write_text(json.dumps(rows,indent=1)+"\n")


if __name__=="__main__":
    main()
