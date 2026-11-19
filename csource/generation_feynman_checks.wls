(* Compare the native CalcHEP trees with the independent Wolfram attachment.
   Set HEPCAT_BATCH_BINARY to the built hepcat-reduce-amplitude executable. *)
$HistoryLength = 0;
base = DirectoryName[$InputFileName];
root = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[base]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{base,"NativeAmplitude.wl"}]];
binary = Environment["HEPCAT_BATCH_BINARY"];
If[!StringQ[binary] || !FileExistsQ[binary], Print["Set HEPCAT_BATCH_BINARY to the native executable."]; Exit[1]];
directory = FileNameJoin[{root,"tests","reference-models","calchep"}];
sm = ReadModel[directory,1]; qed = ReadModel[directory,2];
fail[label_,detail_] := (Print["FAIL ",label," ",Short[detail,2]]; Throw[False]);

(* For four vectors retain off-shell propagators symbolically. Contract Lorentz
   tensors against dense independent external four-vectors and conserved exact
   test momenta. This compares the attached rules before gauge or spin reduction.
   In the gluon case also perform the full external SU(3) tensor comparison. *)
contract[row_,point_] := Module[{expr=Last[row],v,k,moment,metric={1,-1,-1,-1},summands,oneTerm},
  k=Partition[Prime[Range[12]+point],4];k=Append[k,-Total[k]];
  v=Partition[Prime[Range[16]+17+point],4];
  moment[Mom[leg_Integer]]:=k[[leg]];
  moment[Mom[Multiparticle[legs__Integer]]]:=Total[k[[{legs}]]];
  (* First eliminate the metric tensors as Kronecker contractions. This keeps
     the number of remaining four-component sums small in each monomial. *)
  oneTerm[term_] := Module[{fs=If[Head[term]===Times,List@@term,{term}],pos,i,j,tensors,scalars,grouped,vector},
    While[(pos=FirstPosition[fs,_LorentzMetric,{0},{1}])=!={0},
      {i,j}=List@@fs[[First[pos]]];fs=Delete[fs,pos];
      If[i===j,AppendTo[fs,4],fs=fs/.j->i]];
    fs=Flatten[Replace[fs,Power[x:(_PolarizationVector|_LorentzMomentum),2]:>{x,x},{1}]];
    tensors=Cases[fs,_PolarizationVector|_LorentzMomentum];scalars=DeleteCases[fs,_PolarizationVector|_LorentzMomentum];
    grouped=GatherBy[tensors,#[[2]]&];If[!AllTrue[grouped,Length[#]===2&],fail["uncontracted Lorentz index",grouped]];
    vector[PolarizationVector[leg_,_,_,_]]:=v[[leg]];
    vector[LorentzMomentum[p_,_]]:=moment[p];
    (Times@@scalars) (Times@@(Function[g,Total[metric vector[First[g]] vector[Last[g]]]]/@grouped))];
  summands=Expand[expr];Total[oneTerm/@If[Head[summands]===Plus,List@@summands,{summands}]]];

colorCoefficients[expression_,representations_] := Module[{parts},
  parts=ConstructiveDiagrams`Private`colorExpressionTerms[expression];
  If[FailureQ[parts],fail["color decomposition",parts]];
  Total[Function[t,Last[t] ConstructiveDiagrams`Private`colorTensorVector[First[t],representations]]/@parts]];

run[proc_,num_,mode_] := Catch[Module[{m=If[num==1,sm,qed],native,expected,left,right,diff,names},
  native=GenerateNativeAmplitude[proc,directory,num,binary];
  If[!AssociationQ[native],fail["native generation",native]];
  expected=DiagramAmplitudes[CreateDiagrams[proc,m]];
  If[!ListQ[expected] || Length[expected]=!=Length[native["Diagrams"]], fail["diagram count",proc]];
  If[!AllTrue[native["Diagrams"],FreeQ[First[#],_Multiparticle]&],fail["unexpected internal shell",proc]];
  If[mode==="Spinor",
    left=FeynmanToSpinorProducts[expected];right=FeynmanToSpinorProducts[native["Diagrams"]];
    If[!ListQ[left] || !ListQ[right] || !FreeQ[{left,right},_Failure],fail["spinor conversion",proc]];
    diff=Total[Last/@left]-Total[Last/@right];
    If[!FreeQ[diff,_ColorDelta|_ColorGenerator],diff=colorCoefficients[diff,{3,-3,-3,3}]];
    If[!AllTrue[Flatten[{diff}],Together[Expand[#]]===0&],fail["signed spinor formula",proc]],
    Do[left=Total[contract[#,point]&/@expected];right=Total[contract[#,point]&/@native["Diagrams"]];
      diff=Expand[left-right];
      If[!FreeQ[diff,_ColorStructure],diff=colorCoefficients[diff,{8,8,8,8}]];
      If[!AllTrue[Flatten[{diff}],Together[#]===0&],fail["Lorentz/color tensor",{proc,point}]],{point,{0,23}}]];
  Print[{proc,"diagrams",Length[expected],mode,"PASS"}];True]];
results={run[{"e","E","A.+","A.-"},2,"Spinor"],run[{"h","h","h","h"},1,"Spinor"],
  run[{"e","E","E","e"},2,"Spinor"],run[{"W+","W-","h","h"},1,"Spinor"],
  run[{"e","E","A.+","A.+","A.-"},2,"Spinor"],run[{"u","U","C","c"},1,"Spinor"],
  run[{"Z","Z","W+","W-"},1,"Tensor"],run[{"G.+","G.+","G.-","G.-"},1,"Tensor"]};
If[results=!=ConstantArray[True,8],Exit[1]];
Print["Native Feynman generation checks passed."];
