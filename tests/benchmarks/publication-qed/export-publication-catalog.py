"""Export signed publication reference expressions and their measured Wolfram scores.

Usage: python3 export-publication-catalog.py PUBLICATION_CHECKOUT OUTPUT.tsv --kernel /path/to/WolframKernel
"""

import argparse
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import zipfile

WXF = "tests/benchmarks/reproduction/100k/catalog/input-expressions.wxf"
EXPECTED_WXF = "be9bdbce817965e227352301a49e8b96ce501b8cf6240673fce0a70d86ba2339"
EXPECTED_NOTEBOOK = "557691155ce5c706a18aab079942541c92a629233a656fc64078fe9d444b090e"

WOLFRAM = r'''
root = Environment["HEPCAT_PUBLICATION_ROOT"];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
forms = Import[Environment["HEPCAT_PUBLICATION_WXF"], "WXF"];
If[Length[forms] =!= 38, Quit[2]];
notebook = ToExpression[Import[FileNameJoin[{root,"tests","QED-6-point-construction.nb"}],"Text"],
                        InputForm, HoldComplete];
definitions = Cases[notebook, Cell[s_String,"Input",___] /;
  (StringContainsQ[s,"qedAllPositivePhysicalFormula[model_List"] ||
   StringContainsQ[s,"qedAllPositiveSpuriousFormula[model_List"]) :> s, Infinity];
If[Length[definitions] =!= 2, Quit[2]];
Scan[ToExpression,definitions];
model = ReadModel[FileNameJoin[{root,"models"}], 2];
rows = {{"form","family","helicity","weighted","leaves","expression"}};
emit[form_, legs_, expr_] := Module[{family, helicity, wire},
  family = StringJoin[legs /. {"e"->"e","E"->"e","A.+"->"A","A.-"->"A"}];
  helicity = StringJoin[Cases[legs,"A.+"|"A.-"] /. {"A.+"->"+","A.-"->"-"}];
  If[helicity === "", helicity = "massive"];
  wire = ToString[FullForm[expr /. p_ModelParameter :>
    ModelParameter[ModelParameterName[p],"PUBLICATION","Real"]], OutputForm, PageWidth->Infinity];
  wire = StringReplace[wire, {"ConstructiveDiagrams`"->"", " "->""}];
  AppendTo[rows,{form,family,helicity,ToString[SpinorExpressionComplexity[expr]],ToString[LeafCount[expr]],wire}];
];
Scan[(emit[#["form"],#["particles"],#["expression"]])&, forms];
Do[
  reversed = SelectFirst[forms, #["form"] === pole && #["particles"] === {"e","E","A.+","A.-"}&];
  emit[pole, {"e","E","A.-","A.+"}, ConstructiveDiagrams`Private`relabelLegs[reversed["expression"],<|3->4,4->3|>]];
  emit[pole, Join[{"e","E"},ConstantArray["A.+",5]],
    If[pole==="physical",qedAllPositivePhysicalFormula[model,5],qedAllPositiveSpuriousFormula[model,5]][[2]]],
  {pole,{"physical","spurious"}}
];
If[Length[rows] =!= 43 || !DuplicateFreeQ[Rest[rows][[All,1;;3]]], Quit[3]];
data = (StringRiffle[#,"\t"]<>"\n" & /@ Prepend[SortBy[Rest[rows],Take[#,3]&],First[rows]]);
Export[Environment["HEPCAT_PUBLICATION_OUTPUT"],StringJoin[data],"Text"];
If[!FileExistsQ[Environment["HEPCAT_PUBLICATION_OUTPUT"]],Quit[4]];
'''

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("checkout", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--kernel", default=shutil.which("WolframKernel") or "WolframKernel")
    args = parser.parse_args()
    root = args.checkout.resolve()
    notebook = root / "tests/QED-6-point-construction.nb"
    if hashlib.sha256(notebook.read_bytes()).hexdigest() != EXPECTED_NOTEBOOK:
        parser.error("unexpected publication notebook, expected the aa676a9 revision")
    with zipfile.ZipFile(root / "tests/benchmarks/benchmarks.zip") as archive:
        wxf = archive.read(WXF)
    if hashlib.sha256(wxf).hexdigest() != EXPECTED_WXF:
        parser.error("unexpected publication expression bundle")
    with tempfile.TemporaryDirectory() as temporary:
        work = Path(temporary)
        expr = work / "input-expressions.wxf"
        expr.write_bytes(wxf)
        script = work / "export.wls"
        script.write_text(WOLFRAM)
        result = work / "catalog.tsv"
        env = dict(os.environ, HEPCAT_PUBLICATION_ROOT=str(root), HEPCAT_PUBLICATION_WXF=str(expr),
                   HEPCAT_PUBLICATION_OUTPUT=str(result))
        subprocess.run([args.kernel, "-script", str(script)], env=env, check=True)
        args.output.write_bytes(result.read_bytes())

if __name__ == "__main__":
    main()
