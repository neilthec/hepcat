(* Notebook/launcher presentation helpers, independent of the physics package. *)
BeginPackage["HEPCATTrainingNotebook`"];
trainingIntegerSetting::usage = "trainingIntegerSetting[name, default] reads an integer launcher override.";
trainingRunSummary::usage = "trainingRunSummary[report, settings] formats the training report as readable text.";
Begin["`Private`"];

trainingIntegerSetting[name_String, default_Integer] := Module[{value = Environment["HEPCAT_TRAIN_" <> name]},
  If[StringQ[value] && StringMatchQ[value, DigitCharacter ..], FromDigits[value], default]
];

trainingRunSummary[report_, settings_Association] := Module[{number, duration, rate, count, hold},
  If[!AssociationQ[report], Return["TRAINING FAILED\nNo successful training report was returned. See the messages above."]];
  number[x_] := ToString[NumberForm[N[x], {12, 1}], OutputForm];
  duration[x_] := number[x] <> " s (" <> number[x/60] <> " min)";
  count[key_] := ToString[Lookup[report, key, 0]];
  hold = Lookup[report, "HoldoutScrambles", 0];
  rate[key_] := If[hold == 0, "not measured (validation disabled)",
    count[key] <> " / " <> ToString[hold] <> " (" <> number[100 Lookup[report, key, 0]/hold] <> "%)"];
  StringRiffle[{
    "TRAINING COMPLETE",
    "Data: " <> ToString[settings["Amplitudes"]] <> " amplitudes, " <>
      ToString[settings["Scrambles"]] <> " training scrambles each, depth " <> ToString[settings["Steps"]],
    "Training: up to " <> ToString[settings["Rounds"]] <> " rounds; batch size " <> count["TrainingBatchSize"] <>
      "; " <> count["Kernels"] <> " symbolic workers",
    "Examples: " <> count["TrainingStates"] <> " expression states, " <> count["TrainingRows"] <> " candidate edits",
    "Reverse steps used: " <> count["ReverseSteps"] <> "; skipped: " <> count["SkippedReverseSteps"],
    "Cached generation jobs reused: " <> count["CacheHits"] <> " / " <> ToString[Length[Lookup[report, "GenerationJobs", {}]]],
    "",
    "Elapsed pipeline time: " <> duration[report["TotalWallSeconds"]],
    "  Worker setup: " <> duration[report["WorkerSetupSeconds"]],
    "  Data generation: " <> duration[report["DataGenerationSeconds"]],
    "  NN training: " <> duration[report["TrainingSeconds"]],
    "  Validation: " <> duration[report["ValidationSeconds"]],
    "Training CPU time: " <> number[report["TrainingCPUSeconds"]] <> " s (not elapsed time)",
    "",
    "Validation: fresh scrambles of the SAME starting amplitudes, not unseen processes.",
    "  Made simpler: " <> rate["HoldoutSimpler"],
    "  Recovered original form: " <> rate["HoldoutRecovered"],
    "Model saved: " <> ToString[report["ModelPath"]],
    "Full details remain in the notebook variable report. Shell time includes Wolfram startup."
  }, "\n"]
];

End[];
EndPackage[];
