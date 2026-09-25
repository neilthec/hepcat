(* Evaluate the notebook's text input cells without starting a front end. *)
trainingNotebookPath = FileNameJoin[{DirectoryName[$InputFileName], "train-unscrambling.nb"}];
SetEnvironment["HEPCAT_TRAIN_ROOT" -> DirectoryName[DirectoryName[$InputFileName]]];
trainingNotebookExpression = Get[trainingNotebookPath];
trainingNotebookInputs = Cases[trainingNotebookExpression,
  Cell[HEPCATTrainingRunner`Private`input_String, "Input", ___] :> HEPCATTrainingRunner`Private`input, Infinity];
If[trainingNotebookInputs === {} || !FreeQ[trainingNotebookExpression, Cell[BoxData[_], "Input", ___]],
  Print["Headless training expects text Input cells in train-unscrambling.nb."]; Quit[1]];
Scan[ToExpression, trainingNotebookInputs];
Quit[If[AssociationQ[Global`report], 0, 1]];
