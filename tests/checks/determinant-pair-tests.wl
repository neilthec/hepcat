(* Generic numerator determinants. No model-specific amplitude or fit is loaded. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, expression_] := (checks++; If[!TrueQ[expression], AppendTo[failures,name]; Print["FAIL: ",name]]);
dpReduce = ConstructiveDiagrams`Private`preparationDeterminantPairs;
dpA[i_] := Spinor["Helicity","Angle",i]; dpS[i_] := Spinor["Helicity","Square",i];
dpCore[p_,q_,b_,c_,kind_:"Square"] := Module[{spin,opposite,word,bracket,x,y,sign},
 spin[i_]:=Spinor["Helicity",kind,i];opposite[i_]:=Spinor["Helicity",If[kind==="Square","Angle","Square"],i];
 word=If[kind==="Square",{Mom[p],Mom[q]},{Mom[q],Mom[p]}];sign=If[kind==="Square",1,-1];
 bracket=SpinorChain[spin[b],spin[c]];x[i_]:=SpinorChain[spin[i],Sequence@@word,spin[i]];
 y[i_,j_]:=SpinorChain[spin[i],Mom[p],opposite[j]];
 sign bracket(2MomProd[q,c] y[c,b]x[b]-2MomProd[q,b]y[b,c]x[c])-
 bracket^2(MomProd[p,p](2MomProd[q,b])(2MomProd[q,c])-MomProd[q,q](2MomProd[p,b])(2MomProd[p,c]))-
 2MomProd[b,c]x[b]x[c]
];
dpRules={Mass[1]->dpM1,Mass[2]->dpM2,Mass[3]->0,Mass[4]->0,Mass[5]->0};
dpActual=dpCore[1,2,4,5]/.{Mass[1]->dpMass,Mass[2]->dpMass};
dpActualRules={Mass[1]->dpMass,Mass[2]->dpMass,Mass[4]->0,Mass[5]->0};
dpBefore=Hash[{dpActual,dpActualRules},"SHA256"];
dpActualReport=dpReduce[dpActual,dpActualRules];
check["actual cross-pair numerator reduces exactly",dpActualReport["Expression"]===0&&Length[dpActualReport["Edits"]]===1];
check["input expression and mass rules are not mutated",Hash[{dpActual,dpActualRules},"SHA256"]===dpBefore];
check["determinant edits declare no internal-shell assumptions",AllTrue[dpActualReport["Edits"],#["InternalOnShellConditions"]==={}&]];
dpUnequal=dpReduce[dpCore[1,2,3,4],dpRules];
check["unequal external masses remain supported",dpUnequal["Expression"]===0];
dpRelabel=dpReduce[dpCore[7,11,4,19],{Mass[7]->dpM1,Mass[11]->dpM2,Mass[4]->0,Mass[19]->0}];
check["momenta and massless labels are arbitrary",dpRelabel["Expression"]===0&&Length[dpRelabel["Edits"]]===1];
dpAngle=dpReduce[dpCore[1,2,3,4,"Angle"],dpRules];
check["angle dual has its reversed momentum word and sign",dpAngle["Expression"]===0&&Length[dpAngle["Edits"]]===1&&First[dpAngle["Edits"]]["Chirality"]==="Angle"];
dpReverse=dpCore[1,2,3,4]/.HoldPattern[SpinorChain[s:Spinor["Helicity","Square",_],momentum_Mom,a:Spinor["Helicity","Angle",_]]]:>SpinorChain[a,momentum,s];
check["one-momentum transpose conventions both match",dpReduce[dpReverse,dpRules]["Expression"]===0];
dpCarrier=SpinorChain[Spinor["Spin","Angle",7],dpA[6]];
check["common symbolic spinor factors are retained",dpReduce[dpCarrier dpCore[1,2,3,4],dpRules]["Expression"]===0];
dpMassive=dpReduce[dpCore[1,2,3,4],{Mass[1]->dpM1,Mass[2]->dpM2,Mass[3]->dpMassive,Mass[4]->0}];
check["massive bracket endpoints are not assumed massless",dpMassive["Edits"]==={}&&dpMassive["Expression"]===dpCore[1,2,3,4]];
check["undeclared endpoints do not infer masslessness",dpReduce[dpCore[1,2,3,4],{}]["Edits"]==={}];
dpMissing=First[List@@Expand[dpCore[1,2,3,4]]];
check["one unmatched monomial is left unchanged",dpReduce[dpMissing,dpRules]["Expression"]===dpMissing];
dpInternal=Multiparticle[8,9];
dpInternalReport=dpReduce[dpCore[1,2,3,4]+Mass[dpInternal]^2,Append[dpRules,Mass[dpInternal]->dpWrongMass]];
check["composite internal mass rules never enter numerator reduction",dpInternalReport["Expression"]===Mass[dpInternal]^2];
dpDenominator=dpCore[1,2,3,4]/PropDen[Mom[Multiparticle[1,3]],dpM1];
dpDenReport=dpReduce[dpDenominator,dpRules];
check["kinematic inverse factors require the caller to extract a numerator",dpDenReport["Status"]==="SKIPPED"&&dpDenReport["Expression"]===dpDenominator];
dpExplosive=(SpinorChain[dpA[3],dpA[4]]+SpinorChain[dpS[3],dpS[4]])^1000;
dpGuard=dpReduce[dpExplosive,dpRules];
check["prospective expansion guard preserves a factored large power",dpGuard["Status"]==="SKIPPED"&&dpGuard["Expression"]===dpExplosive];
check["zero pair budget does not normalize or expand input",dpReduce[dpActual,dpActualRules,0]["Expression"]===dpActual];
dpZeroTime=dpReduce[dpActual,dpActualRules,8,0];
check["zero time returns the untouched checkpoint",dpZeroTime["Expression"]===dpActual&&dpZeroTime["Edits"]==={}&&TrueQ[dpZeroTime["TimedOut"]]];
dpTiny=dpReduce[dpActual,dpActualRules,8,10^-12];
dpReplay[report_,original_,rules_] := Fold[ConstructiveDiagrams`Private`canonicalChains[ConstructiveDiagrams`Private`withProtectedPropagators[Expand[(#1-#2["Subtract"]+#2["Insert"])/.rules],Expand]]&,original,report["Edits"]];
check["early timeout cannot publish a partial edit",If[dpTiny["Edits"]==={},dpTiny["Expression"]===dpActual,dpReplay[dpTiny,dpActual,dpActualRules]===dpTiny["Expression"]]];
check["completed edit logs replay exactly",dpReplay[dpActualReport,dpActual,dpActualRules]===dpActualReport["Expression"]];
check["a completed reduction is idempotent",dpReduce[dpActualReport["Expression"],dpActualRules]["Expression"]===dpActualReport["Expression"]];
check["reversed square momentum labels are supported",dpReduce[dpCore[11,7,4,19],{Mass[11]->dpM1,Mass[7]->dpM2,Mass[4]->0,Mass[19]->0}]["Expression"]===0];
check["reversed angle momentum labels are supported",dpReduce[dpCore[11,7,4,19,"Angle"],{Mass[11]->dpM1,Mass[7]->dpM2,Mass[4]->0,Mass[19]->0}]["Expression"]===0];
dpConflicting=dpReduce[dpCore[1,2,3,4],Append[dpRules,Mass[3]->dpNonzero]];
check["conflicting endpoint mass rules are declined",dpConflicting["Status"]==="SKIPPED"&&dpConflicting["Edits"]==={}&&dpConflicting["Expression"]===dpCore[1,2,3,4]];
dpNonscalar=dpReduce[dpCore[1,2,3,4],{Mass[1]->{dpM1,dpM2},Mass[2]->dpM2,Mass[3]->0,Mass[4]->0}];
check["nonscalar external masses are declined",dpNonscalar["Status"]==="SKIPPED"&&dpNonscalar["Edits"]==={}];
check["consistent duplicate mass rules are accepted",dpReduce[dpCore[1,2,3,4],Append[dpRules,Mass[3]->0]]["Expression"]===0];
(* Independent polynomial certificates: no momentum conservation or fixed P/Q virtualities. *)
dpP={{dpP11,dpP12},{dpP21,dpP22}};dpQ={{dpQ11,dpQ12},{dpQ21,dpQ22}};
dpLB={dpLB1,dpLB2};dpSB={1,dpSB2};dpLC={dpLC1,dpLC2};dpSC={dpSC1,1};
dpChart=<|"Momenta"-><|1->dpP,2->dpQ,3->Outer[Times,dpLB,dpSB],4->Outer[Times,dpLC,dpSC]|>,
 "Angle"-><|3->dpLB,4->dpLC|>,"Square"-><|3->dpSB,4->dpSC|>,
 "Masses"-><|1->Sqrt[Det[dpP]],2->Sqrt[Det[dpQ]],3->0,4->0|>|>;
check["square identity holds for arbitrary independent momentum matrices",Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[dpCore[1,2,3,4],dpChart,{}]]]===0];
check["angle identity holds for arbitrary independent momentum matrices",Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[dpCore[1,2,3,4,"Angle"],dpChart,{}]]]===0];
If[checks=!=26,AppendTo[failures,"Not every determinant-pair assertion executed."]];
Print["DETERMINANT PAIR CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:determinant-pair-tests.wl"]];
Quit[If[failures==={},0,1]];
