(* ::Package:: *)

(* Reference Amplitudes
   Copied from SM-4-point.nb and SM-4-point-test.nb, without evaluating diagrams.
   337 reference occurrences, 195 distinct {mass rules, amplitude} pairs.
   Exact duplicates share Sources; algebraically equivalent forms are retained.
   Main-notebook input numbers index the extracted title/section/input cells
   from zero. Source mass/coupling conventions are intentionally unchanged.
   These are channel references as well as channel sums, not 195 processes.
   Only external masses are supplied: no extra internal on-shell assumptions.
   A separate synthetic five-point section supplies algebraic training fixtures,
   not diagram-derived physical amplitudes. Its internal-channel rules are explicit.
*)

Join[{
(* ::Subsection::Closed:: *)
(* Reference 1 *)
<|"Name" -> "SM-4-point-test.nb / u U -> C c / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> C c / A", "SM-4-point.nb / input 34 / target 1", 
   "SM-4-point.nb / input 38 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mc, 
   Mass[4] -> Mc}, "Amplitude" -> 
  (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 2 *)
<|"Name" -> "SM-4-point-test.nb / u U -> C c / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> C c / h", "SM-4-point.nb / input 45 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mc, Mass[4] -> Mc}, 
 "Amplitude" -> (EE^2*Mc*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 3 *)
<|"Name" -> "SM-4-point-test.nb / u U -> C c / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> C c / Z", "SM-4-point.nb / input 48 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mc, Mass[4] -> Mc}, 
 "Amplitude" -> -1/2*(EE^2*(gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLu - gRu)^2*Mc*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 4 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / A", "Sources" -> {"SM-4-point-test.nb / u U -> U u / A"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2]) + 
   (8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 5 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / G", "Sources" -> {"SM-4-point-test.nb / u U -> U u / G"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (-2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2] + 
   (2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 6 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / h", "Sources" -> {"SM-4-point-test.nb / u U -> U u / h"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
      (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])) + 
   (EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 7 *)
<|"Name" -> "SM-4-point-test.nb / u U -> U u / Z", "Sources" -> {"SM-4-point-test.nb / u U -> U u / Z"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) - 
   (EE^2*(gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*(-(gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLu*gRu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 8 *)
<|"Name" -> "SM-4-point-test.nb / u U -> S s / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> S s / A", "SM-4-point.nb / input 88 / target 1", 
   "SM-4-point.nb / input 93 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Ms, 
   Mass[4] -> Ms}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 9 *)
<|"Name" -> "SM-4-point-test.nb / u U -> S s / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> S s / h", "SM-4-point.nb / input 96 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (EE^2*Ms*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 10 *)
<|"Name" -> "SM-4-point-test.nb / u U -> S s / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> S s / Z", "SM-4-point.nb / input 99 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd*gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLd - gRd)*(gLu - gRu)*Ms*Mu*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 11 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / A", "SM-4-point.nb / input 108 / target 1", 
   "SM-4-point.nb / input 112 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, 
   Mass[4] -> Md}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 12 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / h", "SM-4-point.nb / input 115 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*Md*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 13 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / Z", "SM-4-point.nb / input 118 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd*gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLd - gRd)*(gLu - gRu)*Md*Mu*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 14 *)
<|"Name" -> "SM-4-point-test.nb / u U -> D d / W+", 
 "Sources" -> {"SM-4-point-test.nb / u U -> D d / W+", "SM-4-point.nb / input 121 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      (Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] - 
        Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
       (-(Mu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]) + 
        Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 15 *)
<|"Name" -> "SM-4-point-test.nb / d D -> S s / A", 
 "Sources" -> {"SM-4-point-test.nb / d D -> S s / A", "SM-4-point.nb / input 132 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 16 *)
<|"Name" -> "SM-4-point-test.nb / d D -> S s / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> S s / h", "SM-4-point.nb / input 139 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (EE^2*Md*Ms*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 17 *)
<|"Name" -> "SM-4-point-test.nb / d D -> S s / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> S s / Z", "SM-4-point.nb / input 142 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLd - gRd)^2*Md*Ms*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 18 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / A", "Sources" -> {"SM-4-point-test.nb / d D -> D d / A"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2]) + 
   (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 19 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / G", "Sources" -> {"SM-4-point-test.nb / d D -> D d / G"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (-2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2] + 
   (2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 20 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / h", "Sources" -> {"SM-4-point-test.nb / d D -> D d / h"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
      (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])) + 
   (EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 21 *)
<|"Name" -> "SM-4-point-test.nb / d D -> D d / Z", "Sources" -> {"SM-4-point-test.nb / d D -> D d / Z"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) - 
   (EE^2*(gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*(-(gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*gRd*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 22 *)
<|"Name" -> "SM-4-point-test.nb / u U -> E e / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> E e / A", "SM-4-point.nb / input 179 / target 1", 
   "SM-4-point.nb / input 183 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Me, 
   Mass[4] -> Me}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(3*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 23 *)
<|"Name" -> "SM-4-point-test.nb / u U -> E e / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> E e / h", "SM-4-point.nb / input 186 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*Me*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 24 *)
<|"Name" -> "SM-4-point-test.nb / u U -> E e / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> E e / Z", "SM-4-point.nb / input 189 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRe*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLe*gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLe - gRe)*(gLu - gRu)*Me*Mu*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 25 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Ne ne / Z", "SM-4-point.nb / input 197 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      gLu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 26 *)
<|"Name" -> "SM-4-point-test.nb / d D -> E e / A", 
 "Sources" -> {"SM-4-point-test.nb / d D -> E e / A", "SM-4-point.nb / input 204 / target 1", 
   "SM-4-point.nb / input 208 / target 1"}, "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Me, 
   Mass[4] -> Me}, "Amplitude" -> 
  (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(3*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 27 *)
<|"Name" -> "SM-4-point-test.nb / d D -> E e / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> E e / h", "SM-4-point.nb / input 211 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*Md*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 28 *)
<|"Name" -> "SM-4-point-test.nb / d D -> E e / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> E e / Z", "SM-4-point.nb / input 214 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd*gLe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLd*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - (EE^2*(gLd - gRd)*(gLe - gRe)*Md*Me*
     (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 29 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Ne ne / Z", "SM-4-point.nb / input 221 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      gLd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 30 *)
<|"Name" -> "SM-4-point-test.nb / e E -> M m / A", 
 "Sources" -> {"SM-4-point-test.nb / e E -> M m / A", "SM-4-point.nb / input 228 / target 1", 
   "SM-4-point.nb / input 232 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, 
   Mass[4] -> Mm}, "Amplitude" -> 
  (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2]|>,

(* ::Subsection::Closed:: *)
(* Reference 31 *)
<|"Name" -> "SM-4-point-test.nb / e E -> M m / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> M m / h", "SM-4-point.nb / input 235 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, Mass[4] -> Mm}, 
 "Amplitude" -> (EE^2*Me*Mm*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 32 *)
<|"Name" -> "SM-4-point-test.nb / e E -> M m / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> M m / Z", "SM-4-point.nb / input 238 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mm, Mass[4] -> Mm}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLe - gRe)^2*Me*Mm*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 33 *)
<|"Name" -> "SM-4-point-test.nb / e E -> E e / A", "Sources" -> {"SM-4-point-test.nb / e E -> E e / A"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2] + 
   (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 34 *)
<|"Name" -> "SM-4-point-test.nb / e E -> E e / h", "Sources" -> {"SM-4-point-test.nb / e E -> E e / h"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
      (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])) + 
   (EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 35 *)
<|"Name" -> "SM-4-point-test.nb / e E -> E e / Z", "Sources" -> {"SM-4-point-test.nb / e E -> E e / Z"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) - 
   (EE^2*(gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*(-(gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLe*gRe*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 36 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Ne ne / Z", "SM-4-point.nb / input 267 / target 1", 
   "SM-4-point.nb / input 272 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, 
   Mass[4] -> 0}, "Amplitude" -> 
  -1/2*(EE^2*(gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      gLe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 37 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Ne ne / W+", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Ne ne / W+", "SM-4-point.nb / input 275 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(Me^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 38 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Nm nm / Z", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> Nm nm / Z", "SM-4-point.nb / input 280 / target 1", 
   "SM-4-point.nb / input 285 / target 1"}, "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> 0, 
   Mass[4] -> 0}, "Amplitude" -> 
  -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 39 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Ne ne / Z", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> Ne ne / Z"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 40 *)
<|"Name" -> "SM-4-point-test.nb / u D -> T b / W+", "Sources" -> {"SM-4-point-test.nb / u D -> T b / W+"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> Mtp, Mass[4] -> Mb}, 
 "Amplitude" -> -1/2*(EE^2*(Md*Mtp*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      Mtp*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] - 
      Mb*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      Mb*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 41 *)
<|"Name" -> "SM-4-point-test.nb / e E -> h h / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> h h / h", "SM-4-point.nb / input 321 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Me*Mh^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 42 *)
<|"Name" -> "SM-4-point-test.nb / e E -> h h / e", "Sources" -> {"SM-4-point-test.nb / e E -> h h / e"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/4*(EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
     (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3])) - 
   (EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (4*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 43 *)
<|"Name" -> "SM-4-point-test.nb / u U -> h h / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> h h / h", "SM-4-point.nb / input 331 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Mh^2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 44 *)
<|"Name" -> "SM-4-point-test.nb / u U -> h h / u", "Sources" -> {"SM-4-point-test.nb / u U -> h h / u"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/4*(EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
     (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3])) - 
   (EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (4*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 45 *)
<|"Name" -> "SM-4-point-test.nb / d D -> h h / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> h h / h", "SM-4-point.nb / input 341 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Md*Mh^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 46 *)
<|"Name" -> "SM-4-point-test.nb / d D -> h h / d", "Sources" -> {"SM-4-point-test.nb / d D -> h h / d"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/4*(EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
       SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
     (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3])) - 
   (EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (4*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 47 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z h / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z h / Z", "SM-4-point.nb / input 382 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLe*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*gRe*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      Me*(-SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 48 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z h / e:T", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z h / e:T", "SM-4-point.nb / input 386 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gRe*Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3])) + 
   (EE^2*gLe*Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 49 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z h / e:U", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z h / e:U", "SM-4-point.nb / input 388 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLe*Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) + 
   (EE^2*gRe*Me*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 50 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z h / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z h / Z", "SM-4-point.nb / input 398 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLu*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*gRu*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      Mu*(-SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 51 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z h / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z h / u:T", "SM-4-point.nb / input 402 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gRu*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3])) + 
   (EE^2*gLu*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 52 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z h / u:U", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z h / u:U", "SM-4-point.nb / input 404 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLu*Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) + 
   (EE^2*gRu*Mu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 53 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z h / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z h / Z", "SM-4-point.nb / input 409 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLd*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) + 
   (EE^2*gRd*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      Md*(-SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 54 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z h / d:T", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z h / d:T", "SM-4-point.nb / input 413 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gRd*Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3])) + 
   (EE^2*gLd*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 55 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z h / d:U", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z h / d:U", "SM-4-point.nb / input 415 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*gLd*Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) + 
   (EE^2*gRd*Md*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (2*Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 56 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z Z / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> Z Z / h", "SM-4-point.nb / input 538 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*Me*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 57 *)
<|"Name" -> "SM-4-point-test.nb / e E -> Z Z / e", "Sources" -> {"SM-4-point-test.nb / e E -> Z Z / e"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 58 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z Z / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> Z Z / h", "SM-4-point.nb / input 555 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 59 *)
<|"Name" -> "SM-4-point-test.nb / u U -> Z Z / u", "Sources" -> {"SM-4-point-test.nb / u U -> Z Z / u"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 60 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z Z / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> Z Z / h", "SM-4-point.nb / input 567 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 61 *)
<|"Name" -> "SM-4-point-test.nb / d D -> Z Z / d", "Sources" -> {"SM-4-point-test.nb / d D -> Z Z / d"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 62 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / h", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / h", "SM-4-point.nb / input 591 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*Me*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 63 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / Z", "SM-4-point.nb / input 597 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*gRe*(2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
        (Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
         2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
     (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2])) - 
   (EE^2*gLe*(2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (-(Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]) + 
        Me*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        2*SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]])))/
    (2*MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 64 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / A", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / A", "SM-4-point.nb / input 600 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(MW*Mandelstahm[1, 2]) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     (SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 65 *)
<|"Name" -> "SM-4-point-test.nb / e E -> W- W+ / ne:U", 
 "Sources" -> {"SM-4-point-test.nb / e E -> W- W+ / ne:U", "SM-4-point.nb / input 594 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 66 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / h", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / h", "SM-4-point.nb / input 617 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 67 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / Z", "SM-4-point.nb / input 624 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*gRu*(2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
        (Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
         2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
     (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2])) - 
   (EE^2*gLu*(2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (-(Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]) + 
        Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        2*SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]])))/
    (2*MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 68 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / A", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / A", "SM-4-point.nb / input 628 / target 1", 
   "SM-4-point.nb / input 636 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> 
  (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(3*MW*Mandelstahm[1, 2]) + 
   (4*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     (SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (3*MW^2*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 69 *)
<|"Name" -> "SM-4-point-test.nb / u U -> W- W+ / d:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> W- W+ / d:T", "SM-4-point.nb / input 621 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
    (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
   (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 70 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / h", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / h", "SM-4-point.nb / input 642 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(Mh^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 71 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / Z", "SM-4-point.nb / input 648 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*gRd*(2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       2*MW*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
        (Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
         2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
     (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2])) - 
   (EE^2*gLd*(2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (-(Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]) + 
        Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        2*SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]])))/
    (2*MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 72 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / A", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / A", "SM-4-point.nb / input 651 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(3*MW*Mandelstahm[1, 2]) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     (SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (3*MW^2*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 73 *)
<|"Name" -> "SM-4-point-test.nb / d D -> W- W+ / u:U", 
 "Sources" -> {"SM-4-point-test.nb / d D -> W- W+ / u:U", "SM-4-point.nb / input 645 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 74 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> W- W+ / Z", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> W- W+ / Z", "SM-4-point.nb / input 612 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
       (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[3], Spinor["Helicity", "Angle", 1]]))/
    (MW^2*SW^2*(MZ^2 - Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 75 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> W- W+ / e:T", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> W- W+ / e:T", "SM-4-point.nb / input 609 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
    (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
   (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 76 *)
<|"Name" -> "SM-4-point-test.nb / ne E -> W- h / W+", 
 "Sources" -> {"SM-4-point-test.nb / ne E -> W- h / W+", "SM-4-point.nb / input 762 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      Me*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 77 *)
<|"Name" -> "SM-4-point-test.nb / ne E -> W- h / e:T", 
 "Sources" -> {"SM-4-point-test.nb / ne E -> W- h / e:T", "SM-4-point.nb / input 766 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Me, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*Me*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 78 *)
<|"Name" -> "SM-4-point-test.nb / u D -> W- h / W+", 
 "Sources" -> {"SM-4-point-test.nb / u D -> W- h / W+", "SM-4-point.nb / input 771 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      (Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
        Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 79 *)
<|"Name" -> "SM-4-point-test.nb / u D -> W- h / d:T", 
 "Sources" -> {"SM-4-point-test.nb / u D -> W- h / d:T", "SM-4-point.nb / input 775 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     (2*Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 80 *)
<|"Name" -> "SM-4-point-test.nb / u D -> W- h / u:U", 
 "Sources" -> {"SM-4-point-test.nb / u D -> W- h / u:U", "SM-4-point.nb / input 778 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MW, Mass[4] -> Mh}, 
 "Amplitude" -> -1/2*(EE^2*Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (2*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 81 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Z h / Z", "Sources" -> {"SM-4-point-test.nb / ne Ne -> Z h / Z"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*MZ^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]])/
   (Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 82 *)
<|"Name" -> "SM-4-point-test.nb / ne Ne -> Z Z / ne", 
 "Sources" -> {"SM-4-point-test.nb / ne Ne -> Z Z / ne"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*Mandelstahm[1, 3]) - (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*Mandelstahm[1, 4])|>,

(* ::Subsection::Closed:: *)
(* Reference 83 *)
<|"Name" -> "SM-4-point-test.nb / e E -> A.+ h / e:T", 
 "Sources" -> {"SM-4-point-test.nb / e E -> A.+ h / e:T", "SM-4-point.nb / input 354 / target 1", 
   "SM-4-point.nb / input 358 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, 
   Mass[4] -> Mh}, "Amplitude" -> 
  (EE^2*Me*(SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
      ((2*Me^2 - Mh^2)*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]] + 
       Me*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]) + 
     Me*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
   (Sqrt[2]*MW*SW*(Me^2 - Mandelstahm[1, 3])*(Me^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 84 *)
<|"Name" -> "SM-4-point-test.nb / u U -> A.+ h / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> A.+ h / u:T", "SM-4-point.nb / input 363 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mh}, 
 "Amplitude" -> 
  -1/3*(Sqrt[2]*EE^2*Mu*(SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
       (-((Mh^2 - 2*Mu^2)*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]) + 
        Mu*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]) + 
      Mu*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]] - 
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
    (MW*SW*(Mu^2 - Mandelstahm[1, 3])*(Mu^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 85 *)
<|"Name" -> "SM-4-point-test.nb / d D -> A.+ h / d:T", 
 "Sources" -> {"SM-4-point-test.nb / d D -> A.+ h / d:T", "SM-4-point.nb / input 368 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*Md*(SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
      ((2*Md^2 - Mh^2)*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]] + 
       Md*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]) + 
     Md*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
   (3*Sqrt[2]*MW*SW*(Md^2 - Mandelstahm[1, 3])*(Md^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 86 *)
<|"Name" -> "SM-4-point-test.nb / e E -> A.+ A.+ / e:T", 
 "Sources" -> {"SM-4-point-test.nb / e E -> A.+ A.+ / e:T", "SM-4-point.nb / input 425 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (2*EE^2*Me*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   ((-Me^2 + Mandelstahm[1, 3])*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 87 *)
<|"Name" -> "SM-4-point-test.nb / e E -> A.+ A.- / e", 
 "Sources" -> {"SM-4-point-test.nb / e E -> A.+ A.- / e", "SM-4-point.nb / input 430 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   ((-Me^2 + Mandelstahm[1, 3])*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 88 *)
<|"Name" -> "SM-4-point-test.nb / u U -> A.+ A.+ / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> A.+ A.+ / u:T", "SM-4-point.nb / input 435 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (8*EE^2*Mu*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   (9*(-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 89 *)
<|"Name" -> "SM-4-point-test.nb / u U -> A.+ A.- / u", 
 "Sources" -> {"SM-4-point-test.nb / u U -> A.+ A.- / u", "SM-4-point.nb / input 439 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   (9*(-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 90 *)
<|"Name" -> "SM-4-point-test.nb / d D -> A.+ A.+ / d:T", 
 "Sources" -> {"SM-4-point-test.nb / d D -> A.+ A.+ / d:T", "SM-4-point.nb / input 444 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (2*EE^2*Md*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   (9*(-Md^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 91 *)
<|"Name" -> "SM-4-point-test.nb / d D -> A.+ A.- / d", 
 "Sources" -> {"SM-4-point-test.nb / d D -> A.+ A.- / d", "SM-4-point.nb / input 448 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   (9*(-Md^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 92 *)
<|"Name" -> "SM-4-point-test.nb / u U -> G.+ G.+ / u:T", 
 "Sources" -> {"SM-4-point-test.nb / u U -> G.+ G.+ / u:T", "SM-4-point.nb / input 454 / target 1", 
   "SM-4-point.nb / input 457 / target 1"}, "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, 
   Mass[4] -> 0}, "Amplitude" -> 
  (2*GG^2*Mu*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
   ((-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 93 *)
<|"Name" -> "SM-4-point-test.nb / A.+ e -> Z E / e:S", 
 "Sources" -> {"SM-4-point-test.nb / A.+ e -> Z E / e:S", "SM-4-point.nb / input 499 / target 1", 
   "SM-4-point.nb / input 502 / target 1"}, "Masses" -> {Mass[1] -> 0, Mass[2] -> Me, Mass[3] -> MZ, 
   Mass[4] -> Me}, "Amplitude" -> 
  -((EE^2*gLe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
       Me*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
     (MW*SW*(Me^2 - Mandelstahm[1, 2])*(Me^2 - Mandelstahm[1, 4]))) - 
   (EE^2*gRe*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Me*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW*SW*(Me^2 - Mandelstahm[1, 2])*(Me^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 94 *)
<|"Name" -> "SM-4-point-test.nb / A.+ u -> Z U / u:S", 
 "Sources" -> {"SM-4-point-test.nb / A.+ u -> Z U / u:S", "SM-4-point.nb / input 508 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mu}, 
 "Amplitude" -> (2*EE^2*gLu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
      Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
    (3*MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4])) + 
   (2*EE^2*gRu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (3*MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 95 *)
<|"Name" -> "SM-4-point-test.nb / A.+ d -> Z D / d:S", 
 "Sources" -> {"SM-4-point-test.nb / A.+ d -> Z D / d:S", "SM-4-point.nb / input 524 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Md}, 
 "Amplitude" -> -1/3*(EE^2*gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
       Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
     (MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4])) - 
   (EE^2*gRd*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (3*MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 96 *)
<|"Name" -> "SM-4-point-test.nb / h G.+ -> U u / u:T", 
 "Sources" -> {"SM-4-point-test.nb / h G.+ -> U u / u:T", "SM-4-point.nb / input 659 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> 0, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -((EE*GG*Mu*(-(Mh^2*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]) + 
      Mu*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 3]] + 
      Mu*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Mom[3], Spinor["Helicity", "Square", 2]]))/
    (Sqrt[2]*MW*SW*(-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 97 *)
<|"Name" -> "SM-4-point-test.nb / h G.+ -> D d / d:T", 
 "Sources" -> {"SM-4-point-test.nb / h G.+ -> D d / d:T", "SM-4-point.nb / input 667 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> 0, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -((EE*GG*Md*(-(Mh^2*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]) + 
      Md*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 3]] + 
      Md*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 2], Mom[1], Mom[3], Spinor["Helicity", "Square", 2]]))/
    (Sqrt[2]*MW*SW*(-Md^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 98 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> Z U / u:S", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> Z U / u:S", "SM-4-point.nb / input 679 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> Mu}, 
 "Amplitude" -> (EE*GG*gLu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
      Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
    (MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4])) + 
   (EE*GG*gRu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW*SW*(Mu^2 - Mandelstahm[1, 2])*(Mu^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 99 *)
<|"Name" -> "SM-4-point-test.nb / G.+ d -> Z D / d:S", 
 "Sources" -> {"SM-4-point-test.nb / G.+ d -> Z D / d:S", "SM-4-point.nb / input 687 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> Md}, 
 "Amplitude" -> (EE*GG*gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     (MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] - 
      Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]]))/
    (MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4])) + 
   (EE*GG*gRd*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     (-(Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]]) + 
      MZ*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]]))/
    (MW*SW*(Md^2 - Mandelstahm[1, 2])*(Md^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 100 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> A.+ U / u", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> A.+ U / u", "SM-4-point.nb / input 699 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (4*EE*GG*Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   (3*(-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 101 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> A.- U / u", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> A.- U / u", "SM-4-point.nb / input 705 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (4*EE*GG*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   (3*(-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 102 *)
<|"Name" -> "SM-4-point-test.nb / G.+ d -> A.+ D / d", 
 "Sources" -> {"SM-4-point-test.nb / G.+ d -> A.+ D / d", "SM-4-point.nb / input 712 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE*GG*Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   (3*(-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 103 *)
<|"Name" -> "SM-4-point-test.nb / G.+ u -> G.+ U / u", 
 "Sources" -> {"SM-4-point-test.nb / G.+ u -> G.+ U / u", "SM-4-point.nb / input 723 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (2*GG^2*Mu*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   ((-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 104 *)
<|"Name" -> "SM-4-point-test.nb / A.+ W+ -> U d / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ W+ -> U d / W+", "SM-4-point.nb / input 818 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MW, Mass[3] -> Mu, Mass[4] -> Md}, 
 "Amplitude" -> (Sqrt[2]*EE^2*(2/(3*(-Mu^2 + Mandelstahm[1, 3])) - 1/(3*(-Md^2 + Mandelstahm[1, 4])))*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (-(MW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
      (Md^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]] - 
       Mu*SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])))/
   (MW*SW*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 105 *)
<|"Name" -> "SM-4-point-test.nb / A.+ W+ -> Ne e / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ W+ -> Ne e / W+", "SM-4-point-test.nb / A.+ W+ -> Ne e / e:U", 
   "SM-4-point.nb / input 825 / target 1", "SM-4-point.nb / input 828 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MW, Mass[3] -> 0, Mass[4] -> Me}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     (Me^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]] - 
      MW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]]))/
    (MW*SW*(-MW^2 + Mandelstahm[1, 2])*(-Me^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 106 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> A.+ W- / W+", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> A.+ W- / W+", "SM-4-point-test.nb / E ne -> A.+ W- / e:T", 
   "SM-4-point.nb / input 845 / target 1"}, "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> 0, 
   Mass[4] -> MW}, "Amplitude" -> 
  (Sqrt[2]*EE^2*(Me*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 2]] + 
     MW*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 4]])/
   (MW*SW*(-MW^2 + Mandelstahm[1, 2])*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 107 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> Z W- / W+", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> Z W- / W+", "SM-4-point.nb / input 869 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*(2*MW^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      2*MW^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
        MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
       (Me*(2*MW^2 - MZ^2)*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 2]] + 
        2*MW^2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Helicity", "Angle", 2]])))/
    (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 108 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> Z W- / e:T", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> Z W- / e:T", "SM-4-point.nb / input 872 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (gRe*Me*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     gLe*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
   (Sqrt[2]*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 109 *)
<|"Name" -> "SM-4-point-test.nb / E ne -> Z W- / ne:U", 
 "Sources" -> {"SM-4-point-test.nb / E ne -> Z W- / ne:U", "SM-4-point.nb / input 875 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (Sqrt[2]*MW^2*SW^2*Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 110 *)
<|"Name" -> "SM-4-point-test.nb / U d -> Z W+ / W+", 
 "Sources" -> {"SM-4-point-test.nb / U d -> Z W+ / W+", "SM-4-point.nb / input 887 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
      (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
       MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
     2*MW^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MW*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
       MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
      ((2*MW^2 - MZ^2)*(Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
         Md*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
       2*MW^2*SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]])))/
   (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 111 *)
<|"Name" -> "SM-4-point-test.nb / U d -> Z W+ / u:T", 
 "Sources" -> {"SM-4-point-test.nb / U d -> Z W+ / u:T"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (gRu*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     gLu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
   (Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 112 *)
<|"Name" -> "SM-4-point-test.nb / U d -> Z W+ / d:U", 
 "Sources" -> {"SM-4-point-test.nb / U d -> Z W+ / d:U"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (gRd*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       (MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
        SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]])))/
    (Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 113 *)
<|"Name" -> "SM-4-point-test.nb / G.+ W+ -> U d / u:T", 
 "Sources" -> {"SM-4-point-test.nb / G.+ W+ -> U d / u:T", "SM-4-point-test.nb / G.+ W+ -> U d / d:U", 
   "SM-4-point.nb / input 909 / target 1", "SM-4-point.nb / input 913 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MW, Mass[3] -> Mu, Mass[4] -> Md}, 
 "Amplitude" -> -((Sqrt[2]*EE*GG*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     (-(MW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]]) + 
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       (Md^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]] - 
        Mu*SpinorChain[Spinor["Helicity", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])))/
    (MW*SW*(-Mu^2 + Mandelstahm[1, 3])*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 114 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / h:S", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / h:S", "SM-4-point.nb / input 929 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-9*EE^2*Mh^4)/(4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 115 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / h:T", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / h:T", "SM-4-point.nb / input 932 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-9*EE^2*Mh^4)/(4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 116 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / h:U", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / h:U", "SM-4-point.nb / input 935 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-9*EE^2*Mh^4)/(4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 117 *)
<|"Name" -> "SM-4-point-test.nb / h h -> h h / contact", 
 "Sources" -> {"SM-4-point-test.nb / h h -> h h / contact", "SM-4-point.nb / input 939 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> (-3*EE^2*Mh^2)/(4*MW^2*SW^2)|>,

(* ::Subsection::Closed:: *)
(* Reference 118 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / h:S", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / h:S", "SM-4-point.nb / input 950 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (-3*EE^2*Mh^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 119 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / Z:T", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / Z:T", "SM-4-point.nb / input 954 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] + 
      MZ*(SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 120 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / Z:U", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / Z:U", "SM-4-point.nb / input 957 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*(2*MZ^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] - 
      MZ*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 121 *)
<|"Name" -> "SM-4-point-test.nb / h h -> Z Z / contact", 
 "Sources" -> {"SM-4-point-test.nb / h h -> Z Z / contact", "SM-4-point.nb / input 946 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(2*MW^2*SW^2)|>,

(* ::Subsection::Closed:: *)
(* Reference 122 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / h:S", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / h:S", "SM-4-point.nb / input 968 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-3*EE^2*Mh^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
   (2*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 123 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / W+:T", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / W+:T", "SM-4-point.nb / input 972 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] + 
      MW*(SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 124 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / W+:U", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / W+:U", "SM-4-point.nb / input 975 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]] - 
      MW*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]] + 
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 125 *)
<|"Name" -> "SM-4-point-test.nb / h h -> W- W+ / contact", 
 "Sources" -> {"SM-4-point-test.nb / h h -> W- W+ / contact", "SM-4-point.nb / input 964 / target 1"}, 
 "Masses" -> {Mass[1] -> Mh, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(2*MW^2*SW^2)|>,

(* ::Subsection::Closed:: *)
(* Reference 126 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> Z Z / h:S", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> Z Z / h:S", "SM-4-point.nb / input 1081 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 127 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> Z Z / h:T", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> Z Z / h:T", "SM-4-point.nb / input 1084 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])))|>,

(* ::Subsection::Closed:: *)
(* Reference 128 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> Z Z / h:U", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> Z Z / h:U", "SM-4-point.nb / input 1087 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 129 *)
<|"Name" -> "SM-4-point-test.nb / Z Z -> W- W+ / h:S", 
 "Sources" -> {"SM-4-point-test.nb / Z Z -> W- W+ / h:S", "SM-4-point.nb / input 1093 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 130 *)
<|"Name" -> "SM-4-point-test.nb / W+ W- -> W+ W- / h:S", 
 "Sources" -> {"SM-4-point-test.nb / W+ W- -> W+ W- / h:S"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2])))|>,

(* ::Subsection::Closed:: *)
(* Reference 131 *)
<|"Name" -> "SM-4-point-test.nb / W+ W- -> W+ W- / h:U", 
 "Sources" -> {"SM-4-point-test.nb / W+ W- -> W+ W- / h:U", "SM-4-point-test.nb / W+ W+ -> W- W- / h:U", 
   "SM-4-point.nb / input 1121 / target 1"}, "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 132 *)
<|"Name" -> "SM-4-point-test.nb / W+ W- -> W- W+ / h:T", 
 "Sources" -> {"SM-4-point-test.nb / W+ W- -> W- W+ / h:T", "SM-4-point-test.nb / W+ W+ -> W- W- / h:T", 
   "SM-4-point.nb / input 1118 / target 1"}, "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3])))|>,

(* ::Subsection::Closed:: *)
(* Reference 133 *)
<|"Name" -> "SM-4-point-test.nb / A.+ h -> W- W+ / W+:T", 
 "Sources" -> {"SM-4-point-test.nb / A.+ h -> W- W+ / W+:T", "SM-4-point.nb / input 982 / target 1", 
   "SM-4-point.nb / input 985 / target 1"}, "Masses" -> {Mass[1] -> 0, Mass[2] -> Mh, Mass[3] -> MW, 
   Mass[4] -> MW}, "Amplitude" -> (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], 
     Spinor["Spin", "Angle", 4]]*(Mh^2*SpinorChain[Spinor["Helicity", "Square", 1], 
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 1], 
       Spinor["Spin", "Square", 4]] - 
     MW*(SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 3]] + 
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Spin", "Angle", 4]])))/
   (MW*SW*(-MW^2 + Mandelstahm[1, 3])*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 134 *)
<|"Name" -> "SM-4-point-test.nb / Z h -> W- W+ / Z:S", 
 "Sources" -> {"SM-4-point-test.nb / Z h -> W- W+ / Z:S", "SM-4-point.nb / input 992 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> 
  (EE^2*(2*MW*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]) + 
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 1]] - 
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 1]]))/
   (Sqrt[2]*MW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 135 *)
<|"Name" -> "SM-4-point-test.nb / Z h -> W- W+ / W+:T", 
 "Sources" -> {"SM-4-point-test.nb / Z h -> W- W+ / W+:T"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
     (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 3]))) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 4]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 4]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 4]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 136 *)
<|"Name" -> "SM-4-point-test.nb / Z h -> W- W+ / W+:U", 
 "Sources" -> {"SM-4-point-test.nb / Z h -> W- W+ / W+:U"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
     (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 4]))) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (Sqrt[2]*EE^2*MW*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 3]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]])/
    (Sqrt[2]*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (Sqrt[2]*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 3]])/
    (MZ^2*SW^2*(MW^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 137 *)
<|"Name" -> "SM-4-point-test.nb / A.+ A.+ -> W- W+ / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ A.+ -> W- W+ / W+", "SM-4-point.nb / input 1009 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 2]]^2*
    SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]^2)/
   ((-MW^2 + Mandelstahm[1, 3])*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 138 *)
<|"Name" -> "SM-4-point-test.nb / A.+ A.- -> W- W+ / W+", 
 "Sources" -> {"SM-4-point-test.nb / A.+ A.- -> W- W+ / W+", "SM-4-point.nb / input 1019 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*(SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]] + 
      SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]])^2)/
   ((-MW^2 + Mandelstahm[1, 3])*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 139 *)
<|"Name" -> "SM-4-point-test.nb / A.+ Z -> W- W+ / W+:U", 
 "Sources" -> {"SM-4-point-test.nb / A.+ Z -> W- W+ / W+:U", "SM-4-point.nb / input 1074 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*(CW^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]^2*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]^2 + 
     (-1 + 2*CW^2)*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
     CW^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]^2*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]^2 - 
     CW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     CW*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     CW^2*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]^2*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]^2))/
   (CW*SW*(-(CW^2*MZ^2) + Mandelstahm[1, 3])*(-(CW^2*MZ^2) + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 140 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / Z:T", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / Z:T", "SM-4-point.nb / input 1131 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 141 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / Z:U", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / Z:U", "SM-4-point.nb / input 1140 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*CW^2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MZ^2*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4])) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW*MZ^3*SW^2*(MZ^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 142 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / A:T", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / A:T", "SM-4-point.nb / input 1172 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> ((-2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] - 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] - 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3])/MW^2 + 
   (((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3])*
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]] + 
     ((EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 3] + 
       (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 3])*
      SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/MW^3|>,

(* ::Subsection::Closed:: *)
(* Reference 143 *)
<|"Name" -> "SM-4-point-test.nb / W+ W+ -> W- W- / A:U", 
 "Sources" -> {"SM-4-point-test.nb / W+ W+ -> W- W- / A:U", "SM-4-point.nb / input 1184 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> ((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] - 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] - 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] - 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4] + 
     (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/Mandelstahm[1, 4])/MW^2 + 
   ((EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4] + 
     (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/Mandelstahm[1, 4])/MW^3|>,

(* ::Subsection::Closed:: *)
(* Reference 144 *)
<|"Name" -> "SM-4-point.nb / input 57 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 57 / target 1", "SM-4-point.nb / input 63 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (-8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 145 *)
<|"Name" -> "SM-4-point.nb / input 59 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 59 / target 1", "SM-4-point.nb / input 66 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (8*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 146 *)
<|"Name" -> "SM-4-point.nb / input 69 / target 1", "Sources" -> {"SM-4-point.nb / input 69 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 147 *)
<|"Name" -> "SM-4-point.nb / input 71 / target 1", "Sources" -> {"SM-4-point.nb / input 71 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 148 *)
<|"Name" -> "SM-4-point.nb / input 74 / target 1", "Sources" -> {"SM-4-point.nb / input 74 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> -1/2*(EE^2*(gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLu*gRu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 149 *)
<|"Name" -> "SM-4-point.nb / input 76 / target 1", "Sources" -> {"SM-4-point.nb / input 76 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mu, Mass[4] -> Mu}, 
 "Amplitude" -> (EE^2*(gLu - gRu)^2*Mu^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) + 
   (EE^2*(-(gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLu*gRu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 150 *)
<|"Name" -> "SM-4-point.nb / input 136 / target 1", "Sources" -> {"SM-4-point.nb / input 136 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Ms, Mass[4] -> Ms}, 
 "Amplitude" -> (4*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 151 *)
<|"Name" -> "SM-4-point.nb / input 151 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 151 / target 1", "SM-4-point.nb / input 157 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 2])|>,

(* ::Subsection::Closed:: *)
(* Reference 152 *)
<|"Name" -> "SM-4-point.nb / input 153 / target 1", "Sources" -> {"SM-4-point.nb / input 153 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/(9*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 153 *)
<|"Name" -> "SM-4-point.nb / input 160 / target 1", "Sources" -> {"SM-4-point.nb / input 160 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 154 *)
<|"Name" -> "SM-4-point.nb / input 162 / target 1", "Sources" -> {"SM-4-point.nb / input 162 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 155 *)
<|"Name" -> "SM-4-point.nb / input 165 / target 1", "Sources" -> {"SM-4-point.nb / input 165 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> -1/2*(EE^2*(gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLd*gRd*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 156 *)
<|"Name" -> "SM-4-point.nb / input 167 / target 1", "Sources" -> {"SM-4-point.nb / input 167 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Md, Mass[4] -> Md}, 
 "Amplitude" -> (EE^2*(gLd - gRd)^2*Md^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) + 
   (EE^2*(-(gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*gRd*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 157 *)
<|"Name" -> "SM-4-point.nb / input 243 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 243 / target 1", "SM-4-point.nb / input 249 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (-2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 2]|>,

(* ::Subsection::Closed:: *)
(* Reference 158 *)
<|"Name" -> "SM-4-point.nb / input 245 / target 1", 
 "Sources" -> {"SM-4-point.nb / input 245 / target 1", "SM-4-point.nb / input 252 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (2*EE^2*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] - 
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] - 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/Mandelstahm[1, 3]|>,

(* ::Subsection::Closed:: *)
(* Reference 159 *)
<|"Name" -> "SM-4-point.nb / input 255 / target 1", "Sources" -> {"SM-4-point.nb / input 255 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
    (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
   (4*MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 160 *)
<|"Name" -> "SM-4-point.nb / input 257 / target 1", "Sources" -> {"SM-4-point.nb / input 257 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]] + 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-Mh^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 161 *)
<|"Name" -> "SM-4-point.nb / input 260 / target 1", "Sources" -> {"SM-4-point.nb / input 260 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> -1/2*(EE^2*(gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
       gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
       gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       gLe*gRe*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
     (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2])) - 
   (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] - 
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])*
     (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 162 *)
<|"Name" -> "SM-4-point.nb / input 262 / target 1", "Sources" -> {"SM-4-point.nb / input 262 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Me, Mass[4] -> Me}, 
 "Amplitude" -> (EE^2*(gLe - gRe)^2*Me^2*(SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 3]] - SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])*
     (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]))/
    (4*CW^2*MZ^2*SW^2*(-MZ^2 + Mandelstahm[1, 3])) + 
   (EE^2*(-(gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]) - 
      gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLe*gRe*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
         SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
         SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])))/
    (2*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 163 *)
<|"Name" -> "SM-4-point.nb / input 287 / target 1", "Sources" -> {"SM-4-point.nb / input 287 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Helicity", "Square", 3]])/
    (CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 164 *)
<|"Name" -> "SM-4-point.nb / input 295 / target 1", "Sources" -> {"SM-4-point.nb / input 295 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Md, Mass[3] -> Mt, Mass[4] -> Mb}, 
 "Amplitude" -> -1/2*(EE^2*(Md*Mt*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      Mt*Mu*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] - 
      Mb*Md*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      Mb*Mu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 165 *)
<|"Name" -> "SM-4-point.nb / input 302 / target 1", "Sources" -> {"SM-4-point.nb / input 302 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Mu, Mass[3] -> Ml, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(Ml*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle", 4]] - 
      Md*Ml*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]] + 
      2*MW^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 166 *)
<|"Name" -> "SM-4-point.nb / input 310 / target 1", "Sources" -> {"SM-4-point.nb / input 310 / target 1"}, 
 "Masses" -> {Mass[1] -> Mm, Mass[2] -> 0, Mass[3] -> Me, Mass[4] -> 0}, 
 "Amplitude" -> -1/2*(EE^2*(2*MW^2*SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]] - 
      Me*Mm*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 2]]))/
    (MW^2*SW^2*(-MW^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 167 *)
<|"Name" -> "SM-4-point.nb / input 324 / target 1", "Sources" -> {"SM-4-point.nb / input 324 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 168 *)
<|"Name" -> "SM-4-point.nb / input 326 / target 1", "Sources" -> {"SM-4-point.nb / input 326 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Me^2*(2*Me*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 169 *)
<|"Name" -> "SM-4-point.nb / input 334 / target 1", "Sources" -> {"SM-4-point.nb / input 334 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 170 *)
<|"Name" -> "SM-4-point.nb / input 336 / target 1", "Sources" -> {"SM-4-point.nb / input 336 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Mu^2*(2*Mu*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 171 *)
<|"Name" -> "SM-4-point.nb / input 344 / target 1", "Sources" -> {"SM-4-point.nb / input 344 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[3], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 172 *)
<|"Name" -> "SM-4-point.nb / input 346 / target 1", "Sources" -> {"SM-4-point.nb / input 346 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> Mh, Mass[4] -> Mh}, 
 "Amplitude" -> -1/4*(EE^2*Md^2*(2*Md*(SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]] + 
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]) + 
      SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 2]] + 
      SpinorChain[Spinor["Spin", "Square", 2], Mom[4], Spinor["Spin", "Angle", 1]]))/
    (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 173 *)
<|"Name" -> "SM-4-point.nb / input 393 / target 1", "Sources" -> {"SM-4-point.nb / input 393 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> Mh}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]])/
   (Sqrt[2]*CW^2*SW^2*(-MZ^2 + Mandelstahm[1, 2]))|>,

(* ::Subsection::Closed:: *)
(* Reference 174 *)
<|"Name" -> "SM-4-point.nb / input 463 / target 1", "Sources" -> {"SM-4-point.nb / input 463 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> 0}, 
 "Amplitude" -> (-2*GG^2*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]] + 
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]])*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]])/
   ((-Mu^2 + Mandelstahm[1, 3])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 175 *)
<|"Name" -> "SM-4-point.nb / input 541 / target 1", "Sources" -> {"SM-4-point.nb / input 541 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Me^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 176 *)
<|"Name" -> "SM-4-point.nb / input 543 / target 1", "Sources" -> {"SM-4-point.nb / input 543 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLe*gRe*Me*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRe^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLe^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Me^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 177 *)
<|"Name" -> "SM-4-point.nb / input 548 / target 1", "Sources" -> {"SM-4-point.nb / input 548 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (MW^2*SW^2*Mandelstahm[1, 4])|>,

(* ::Subsection::Closed:: *)
(* Reference 178 *)
<|"Name" -> "SM-4-point.nb / input 550 / target 1", "Sources" -> {"SM-4-point.nb / input 550 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> 0, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 2], Spinor["Spin", "Square", 4]]*
    (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
     SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
   (2*MW^2*SW^2*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 179 *)
<|"Name" -> "SM-4-point.nb / input 559 / target 1", "Sources" -> {"SM-4-point.nb / input 559 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Mu^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 180 *)
<|"Name" -> "SM-4-point.nb / input 562 / target 1", "Sources" -> {"SM-4-point.nb / input 562 / target 1"}, 
 "Masses" -> {Mass[1] -> Mu, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLu*gRu*Mu*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRu^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLu^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 181 *)
<|"Name" -> "SM-4-point.nb / input 570 / target 1", "Sources" -> {"SM-4-point.nb / input 570 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(Md^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 182 *)
<|"Name" -> "SM-4-point.nb / input 572 / target 1", "Sources" -> {"SM-4-point.nb / input 572 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Md, Mass[3] -> MZ, Mass[4] -> MZ}, 
 "Amplitude" -> -1/2*(EE^2*gLd*gRd*Md*(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
        SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]))/
     (MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gRd^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])) - 
   (EE^2*gLd^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     (MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]] - 
      SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]]))/
    (2*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 183 *)
<|"Name" -> "SM-4-point.nb / input 716 / target 1", "Sources" -> {"SM-4-point.nb / input 716 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (-2*EE*GG*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   (3*(-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 184 *)
<|"Name" -> "SM-4-point.nb / input 727 / target 1", "Sources" -> {"SM-4-point.nb / input 727 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Mu, Mass[3] -> 0, Mass[4] -> Mu}, 
 "Amplitude" -> (2*GG^2*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   ((-Mu^2 + Mandelstahm[1, 2])*(-Mu^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 185 *)
<|"Name" -> "SM-4-point.nb / input 739 / target 1", "Sources" -> {"SM-4-point.nb / input 739 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (2*GG^2*Md*SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
   ((-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 186 *)
<|"Name" -> "SM-4-point.nb / input 743 / target 1", "Sources" -> {"SM-4-point.nb / input 743 / target 1"}, 
 "Masses" -> {Mass[1] -> 0, Mass[2] -> Md, Mass[3] -> 0, Mass[4] -> Md}, 
 "Amplitude" -> (2*GG^2*(-(SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]) + 
     SpinorChain[Spinor["Helicity", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 3]]*
      SpinorChain[Spinor["Helicity", "Square", 1], Mom[2], Spinor["Helicity", "Angle", 3]]))/
   ((-Md^2 + Mandelstahm[1, 2])*(-Md^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 187 *)
<|"Name" -> "SM-4-point.nb / input 839 / target 1", "Sources" -> {"SM-4-point.nb / input 839 / target 1"}, 
 "Masses" -> {Mass[1] -> Me, Mass[2] -> 0, Mass[3] -> 0, Mass[4] -> MW}, 
 "Amplitude" -> -((Sqrt[2]*EE^2*(Me*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 4]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 2]] + 
      MW*SpinorChain[Spinor["Helicity", "Angle", 2], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]])*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 4]])/
    (MW*SW*(-MW^2 + Mandelstahm[1, 2])*(-Me^2 + Mandelstahm[1, 3])))|>,

(* ::Subsection::Closed:: *)
(* Reference 188 *)
<|"Name" -> "SM-4-point.nb / input 893 / target 1", "Sources" -> {"SM-4-point.nb / input 893 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
    (gRu*Mu*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     gLu*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      (MZ*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
       SpinorChain[Spinor["Spin", "Square", 4], Mom[1], Spinor["Spin", "Angle", 3]])))/
   (Sqrt[2]*MW^2*SW^2*(-Mu^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 189 *)
<|"Name" -> "SM-4-point.nb / input 898 / target 1", "Sources" -> {"SM-4-point.nb / input 898 / target 1"}, 
 "Masses" -> {Mass[1] -> Md, Mass[2] -> Mu, Mass[3] -> MZ, Mass[4] -> MW}, 
 "Amplitude" -> -((EE^2*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     (gRd*Md*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]] + 
      gLd*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
       (MW*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
        SpinorChain[Spinor["Spin", "Square", 3], Mom[1], Spinor["Spin", "Angle", 4]])))/
    (Sqrt[2]*MW^2*SW^2*(-Md^2 + Mandelstahm[1, 4])))|>,

(* ::Subsection::Closed:: *)
(* Reference 190 *)
<|"Name" -> "SM-4-point.nb / input 997 / target 1", "Sources" -> {"SM-4-point.nb / input 997 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*(2*MW^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] - 
     MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 4], Mom[2], Spinor["Spin", "Angle", 4]] + 
     2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]]))/
   (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 191 *)
<|"Name" -> "SM-4-point.nb / input 1002 / target 1", "Sources" -> {"SM-4-point.nb / input 1002 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> Mh, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (EE^2*(2*MW^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] + 
     2*MW^2*MZ*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
      SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]] + 
     MZ^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Mom[2], Spinor["Spin", "Angle", 3]] + 
     2*MW^2*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
      SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
      SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]]))/
   (Sqrt[2]*MW^2*MZ^2*SW^2*(-MW^2 + Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 192 *)
<|"Name" -> "SM-4-point.nb / input 1103 / target 1", "Sources" -> {"SM-4-point.nb / input 1103 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) - 
   (2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 3]))|>,

(* ::Subsection::Closed:: *)
(* Reference 193 *)
<|"Name" -> "SM-4-point.nb / input 1112 / target 1", "Sources" -> {"SM-4-point.nb / input 1112 / target 1"}, 
 "Masses" -> {Mass[1] -> MZ, Mass[2] -> MZ, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) - 
   (CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/
    (2*MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (2*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (MW^2*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (3*CW^3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4])) + 
   (CW^4*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*MW^3*SW^2*(MW^2 - Mandelstahm[1, 4]))|>,

(* ::Subsection::Closed:: *)
(* Reference 194 *)
<|"Name" -> "SM-4-point.nb / input 1150 / target 1", "Sources" -> {"SM-4-point.nb / input 1150 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (-2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 3]) - 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 3]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[3], Spinor["Spin", "Angle", 4]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[3], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 3])|>,

(* ::Subsection::Closed:: *)
(* Reference 195 *)
<|"Name" -> "SM-4-point.nb / input 1159 / target 1", "Sources" -> {"SM-4-point.nb / input 1159 / target 1"}, 
 "Masses" -> {Mass[1] -> MW, Mass[2] -> MW, Mass[3] -> MW, Mass[4] -> MW}, 
 "Amplitude" -> (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) - 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]])/(2*CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (2*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/(CW^2*MZ^2*Mandelstahm[1, 4]) + 
   (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[4], Spinor["Spin", "Angle", 3]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 2], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + (3*EE^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4]) + 
   (EE^2*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*CW^3*MZ^3*Mandelstahm[1, 4])|>
},

(* ::Subsection::Closed:: *)
(* Synthetic Five-Point Algebraic Fixtures *)
Module[{families, familyNames},
  families = {{0, 0, 0, 0, 0}, {Me, Me, 0, 0, 0}, {Me, Me, Mm, Mm, 0},
    {MW, MW, MW, MW, 0}, {Mh, MW, MZ, Me, Me}, {MW, MW, MZ, MZ, Mh}};
  familyNames = {"Massless", "FermionPair", "TwoFermionPairs", "FourVectors",
    "MixedMassive", "FiveMassive"};
  Flatten[Table[Module[{masses = families[[family]], spinor, bracket, chain, expressions,
      channelPairs, channelMasses, channelIndices, denominators, templateNames, externalRules},
    spinor[type_, leg_] := Spinor[If[masses[[leg]] === 0, "Helicity", "Spin"], type, leg];
    bracket[type_, left_, right_] := SpinorChain[spinor[type, left], spinor[type, right]];
    chain[leftType_, left_, momenta_List, rightType_, right_] :=
      SpinorChain @@ Join[{spinor[leftType, left]}, Mom /@ momenta, {spinor[rightType, right]}];
    expressions = {
      bracket["Angle", 1, 2] bracket["Angle", 3, 4] bracket["Square", 2, 5],
      bracket["Square", 1, 2] bracket["Square", 3, 4] bracket["Angle", 1, 5],
      chain["Angle", 1, {3}, "Square", 2] bracket["Angle", 4, 5],
      chain["Square", 1, {2, 3}, "Square", 4] bracket["Angle", 2, 5],
      chain["Angle", 1, {2, 5, 3}, "Square", 4],
      MomProd[2, 5] bracket["Angle", 1, 3] bracket["Square", 2, 4] +
        MomProd[3, 5] bracket["Angle", 1, 4] bracket["Square", 2, 3],
      chain["Angle", 1, {Multiparticle[2, 3]}, "Square", 4] bracket["Angle", 2, 5],
      (Mom[Multiparticle[2, 3]]^2 - Mass[2]^2 - Mass[3]^2)
        bracket["Angle", 1, 4] bracket["Square", 2, 5]
    };
    channelPairs = {{1, 2}, {2, 3}, {4, 5}}; channelMasses = {0, MZ, Mh};
    channelIndices = {{1}, {}, {2}, {3}, {1, 3}, {2}, {2, 3}, {2}};
    denominators = {PropDen[Mom[Multiparticle[1, 2]], 0], 1,
      PropDen[Mom[Multiparticle[2, 3]], MZ], PropDen[Mom[Multiparticle[4, 5]], Mh],
      PropDen[Mom[Multiparticle[1, 2]], 0] PropDen[Mom[Multiparticle[4, 5]], Mh],
      PropDen[Mom[Multiparticle[2, 3]], MZ],
      PropDen[Mom[Multiparticle[2, 3]], MZ] PropDen[Mom[Multiparticle[4, 5]], Mh],
      PropDen[Mom[Multiparticle[2, 3]], MZ]};
    templateNames = {"AngleBracketProduct", "SquareBracketProduct", "OneMomentumChain",
      "TwoMomentumChain", "ThreeMomentumChain", "DotProductSum", "InternalMomentumChain",
      "InternalMomentumSquare"};
    externalRules = Thread[(Mass /@ Range[5]) -> masses];
    Table[With[{name = "Synthetic5Point / " <> familyNames[[family]] <> " / " <> templateNames[[template]]},
      <|"Name" -> name, "Sources" -> {name}, "Synthetic" -> True, "ExternalLegCount" -> 5,
        "Family" -> "Synthetic5Point / " <> familyNames[[family]], "Template" -> templateNames[[template]],
        "Masses" -> Join[externalRules,
          (Mass[Multiparticle @@ channelPairs[[#]]] -> channelMasses[[#]] & /@ channelIndices[[template]])],
        "Amplitude" -> (-1)^(family + template) (family + template) EE^3
          expressions[[template]]/((1 + Mod[template, 3]) denominators[[template]])|>],
      {template, Length[expressions]}]
  ], {family, Length[families]}], 1]
]]
