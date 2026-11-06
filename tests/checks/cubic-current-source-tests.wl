(* Minimal cubic current continuations derive their vertices from the model. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
sm = ReadModel[FileNameJoin[{root, "models"}], 1];
toy = sm;
toy[[1]] = Select[toy[[1]], MemberQ[{"e", "A.+", "h"}, #[[2]]] &];
toy[[2]] = Select[toy[[2]], MemberQ[{"A.+", "A.-", "h"}, #[[3]]] &&
  Take[#, 2] === {"e", "E"} &];
toy = toy /. {"e" -> "f", "E" -> "F", "A.+" -> "B.+", "A.-" -> "B.-", "h" -> "s"};
saved = toy;
mass = ModelParameter[toy, "Me"]; scalarMass = ModelParameter[toy, "Mh"];
external = {Mass[1] -> mass, Mass[2] -> mass, Mass[3] -> 0, Mass[4] -> scalarMass};
normalize[process_] := <|"Expression" -> 0, "ExternalMassRules" -> external,
  "ExternalParticles" -> process|>;
source = ConstructiveDiagrams`Private`completionHECubicBilinearSource[
  normalize[{"f", "F", "B.+", "s"}], toy, "AllPhysical", 8];
check["renamed model particles use the same general certified cubic source",
  AssociationQ[source] && source["PoleInventoryMatched"] &&
    source["Source"] === "CertifiedMinimalScalarPseudoscalarAndVectorCubicCurrents"];
check["the scalar current matches the actual three-point bilinear exactly",
  source["ScalarVertexMatch"]["ExactSymbolicMatch"] &&
    Expand[source["ScalarVertexMatch"]["OriginalAmplitude"] -
      source["ScalarVertexMatch"]["ReconstructedAmplitude"]] === 0];
check["the cubic continuation uses no numerical fit or internal shell condition",
  source["InternalOnShellConditions"] === {} &&
    source["ScalarVertexMatch"]["NumericalParameterAssignments"] === {} &&
    source["ScalarVertexMatch"]["InternalOnShellConditions"] === {} &&
    source["RequiresFullAmplitudeValidation"]];
check["the proposed physical form resolves its auxiliary references",
  FreeQ[source["Direction"], Spinor["Xi", ___] | _DiracChain | _DiracWord] &&
    !FreeQ[source["Direction"], _PropDen]];
negative = ConstructiveDiagrams`Private`completionHECubicBilinearSource[
  normalize[{"f", "F", "B.-", "s"}], toy, "AllPhysical", 8];
check["the opposite massless-vector helicity uses the same vertex certificate",
  AssociationQ[negative] && negative["ScalarVertexMatch"] === source["ScalarVertexMatch"] &&
    negative["PoleInventoryMatched"]];
pseudo = toy;
scalarPosition = First[FirstPosition[pseudo[[2]], row_List /; row[[3]] === "s"]];
pseudo[[2, scalarPosition, -1]] = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] -
  2 SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]];
pseudoSource = ConstructiveDiagrams`Private`completionHECubicBilinearSource[
  normalize[{"f", "F", "B.+", "s"}], pseudo, "AllPhysical", 8];
check["independent angle and square coefficients recover scalar and pseudoscalar currents",
  AssociationQ[pseudoSource] && pseudoSource["ScalarVertexMatch"]["ExactSymbolicMatch"] &&
    Expand[pseudoSource["ScalarVertexMatch"]["OriginalAmplitude"] -
      pseudoSource["ScalarVertexMatch"]["ReconstructedAmplitude"]] === 0];
nonminimal = pseudo;
nonminimal[[2, scalarPosition, -1]] *= MomProd[1, 2];
check["a derivative scalar vertex is not silently replaced by a minimal Yukawa current",
  MissingQ[ConstructiveDiagrams`Private`completionHECubicBilinearSource[
    normalize[{"f", "F", "B.+", "s"}], nonminimal, "AllPhysical", 8]]];
asymmetric = toy;
asymmetric[[2, First[FirstPosition[asymmetric[[2]], row_List /; row[[3]] === "B.-"]], -2]] *= 2;
check["different vector-helicity couplings cannot acquire the standard minimal-vector certificate",
  MissingQ[ConstructiveDiagrams`Private`completionHECubicBilinearSource[
    normalize[{"f", "F", "B.+", "s"}], asymmetric, "AllPhysical", 8]]];
check["an explicit generic-reference request does not trigger fixed-reference current replacement",
  AllTrue[{None, "Xi", "IndependentXi"}, MissingQ[
    ConstructiveDiagrams`Private`completionHECubicBilinearSource[
      normalize[{"f", "F", "B.+", "s"}], toy, #, 8]] &]];
check["the source builder never modifies the supplied model",
  toy === saved];
colored = toy;
colored[[1]] = (If[#[[5]] === 1, ReplacePart[#, 8 -> 3], #] &) /@ colored[[1]];
check["a color-stripped scalar current is unavailable without an exact projection certificate",
  MissingQ[ConstructiveDiagrams`Private`completionHECubicBilinearSource[
    normalize[{"f", "F", "B.+", "s"}], colored, "AllPhysical", 8]]];
coloredProcess = {"f", "F", "B.+", "s"};
coloredDiagrams = CreateDiagrams[coloredProcess, colored];
coloredPairs = DiagramAmplitudes[coloredDiagrams];
coloredChannels = FactorizationAmplitudeChannels[coloredProcess, colored];
representations = ConstructiveDiagrams`Private`colorParticleRepresentation[#, colored] & /@ coloredProcess;
projection = ConstructiveDiagrams`Private`colorProjectionData[coloredPairs, coloredChannels, representations];
certificate = ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate[
  coloredDiagrams, colored, projection, representations];
coloredSource = Block[{ConstructiveDiagrams`Private`$colorConstructingProjection = True,
    ConstructiveDiagrams`Private`$colorConstructionInput = <|"Diagrams" -> coloredDiagrams,
      "ColorProjectionCertificate" -> certificate|>},
  ConstructiveDiagrams`Private`completionHECubicBilinearSource[
    normalize[coloredProcess], colored, "AllPhysical", 8]];
check["a rank-one fundamental color flow permits only its certified projected current coefficient",
  AssociationQ[coloredSource] && coloredSource["ColorProjectionCertificate"] === certificate &&
    coloredSource["ColorProjectionCoordinate"] === certificate["CommonDiagramCoordinate"] &&
    coloredSource["PoleInventoryMatched"]];
rephasedCertificate = Join[certificate, <|"Basis" -> I certificate["Basis"],
  "CommonDiagramCoordinate" -> -I certificate["CommonDiagramCoordinate"]|>];
rephasedSource = Block[{ConstructiveDiagrams`Private`$colorConstructingProjection = True,
    ConstructiveDiagrams`Private`$colorConstructionInput = <|"Diagrams" -> coloredDiagrams,
      "ColorProjectionCertificate" -> rephasedCertificate|>},
  ConstructiveDiagrams`Private`completionHECubicBilinearSource[
    normalize[coloredProcess], colored, "AllPhysical", 8]];
check["an exact color-basis phase is propagated to the numerical amplitude coefficient",
  AssociationQ[rephasedSource] && rephasedSource["ColorProjectionCoordinate"] === -I &&
    Together[rephasedSource["Direction"] + I coloredSource["Direction"]] === 0];
Print[checks, " cubic-current source checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:cubic-current-source-tests.wl"];
Exit[0];
