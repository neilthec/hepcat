(* Reuse exact retained affine coefficients without trusting inconsistent metadata. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
raModel = ReadModel[FileNameJoin[{root, "models"}], 3];
raMass = ModelParameter[raModel, "Me"];
raRules = Thread[Mass /@ Range[4] -> {raMass, raMass, 0, 0}];
raCoordinates = {Unique["retainedCoordinate"], Unique["retainedCoordinate"]};
raDirections = {(MomProd[1, 3] + MomProd[1, 4])^8,
  SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]/
    PropDen[Mom[Multiparticle[1, 3]], raMass]};
raFamily = 7 + raCoordinates.raDirections;
raCoefficientCalls = 0;
raGood = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>,
    CoefficientArrays = Function[{value, variables}, raCoefficientCalls++; $Failed]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, raDirections]];
check["matching literal affine metadata avoids expanding the full family with CoefficientArrays",
  raCoefficientCalls === 0];
check["an exhausted proof budget retains the exact supplied directions and center",
  raGood["FamilyAmplitude"] === raFamily && raGood["RemainingBasis"] === raDirections &&
    raGood["FreeParameters"] === raCoordinates && raGood["RemovedParameters"] === {}];
raCoefficientCalls = 0;
raRejectedMetadata = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>,
    CoefficientArrays = Function[{value, variables}, raCoefficientCalls++; $Failed]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, {1, 2}]];
check["incorrect retained coefficients fall back to ordinary coefficient inspection",
  raCoefficientCalls > 0 && raRejectedMetadata["FamilyAmplitude"] === raFamily &&
    raRejectedMetadata["FreeParameters"] === raCoordinates];
raWrong = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, {1, 2}]];
check["ordinary fallback extracts the actual coefficients rather than the incorrect metadata",
  raWrong["RemainingBasis"] === raDirections && raWrong["FamilyAmplitude"] === raFamily];
raShort = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
    <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, Take[raDirections, 1]]];
check["a truncated coefficient list cannot drop a family coordinate",
  raShort["FreeParameters"] === raCoordinates && raShort["RemainingBasis"] === raDirections];
raNonlinear = 7 + (Times @@ raCoordinates) raDirections[[1]];
raDependent = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
    <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raNonlinear, raCoordinates, raRules, raModel, 0,
    {raCoordinates[[2]] raDirections[[1]], 0}]];
check["coordinate-dependent metadata cannot make a nonlinear family appear affine",
  raDependent["AffineFamily"] === False && raDependent["FamilyAmplitude"] === raNonlinear &&
    raDependent["FreeParameters"] === raCoordinates && raDependent["RemovedParameters"] === {}];
raZeroCoordinate = Unique["retainedZero"];
raZero = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
    <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    7, {raZeroCoordinate}, raRules, raModel, 1, {0}]];
check["an exactly zero retained direction is removed only after its zero certificate",
  raZero["RemainingBasis"] === {} && raZero["FreeParameters"] === {} &&
    raZero["RemovedParameters"] === {raZeroCoordinate} &&
    AllTrue[raZero["Certificates"], #["Status"] === "PROVED_ZERO" &]];
check["proof or metadata handling never modifies the original supplied coefficients",
  raDirections === {(MomProd[1, 3] + MomProd[1, 4])^8,
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]/
      PropDen[Mom[Multiparticle[1, 3]], raMass]}];
Print["RETAINED AFFINE COEFFICIENT CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:retained-affine-coefficients-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
