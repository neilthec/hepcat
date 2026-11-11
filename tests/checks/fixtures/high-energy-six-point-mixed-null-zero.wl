(* Actual unresolved affine directions from the six-point one-positive,
   three-negative photon construction, saved seed842384929358529730.
   These are identity-proof fixtures, not amplitude formulas used by construction.
   Model-local mass and coupling slots are restored by the two function arguments. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


Function[{mixedIdentityMass, mixedIdentityCoupling},
 <|"ExternalMassRules" -> {ConstructiveDiagrams`Mass[1] -> mixedIdentityMass,
    ConstructiveDiagrams`Mass[2] -> mixedIdentityMass,
    ConstructiveDiagrams`Mass[3] -> 0, ConstructiveDiagrams`Mass[4] -> 0,
    ConstructiveDiagrams`Mass[5] -> 0, ConstructiveDiagrams`Mass[6] -> 0},
   "SourceDirectionPositions" -> {2, 3},
   "Directions" -> {
-4*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) +
    2*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) +
    2*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        2]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        2]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) +
    2*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    4*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    8*((Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 5], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 6], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 4]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 4], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 6], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 4]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 4], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 5], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) +
    2*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) -
    2*((Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass])) +
    2*((Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) -
    8*((Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`MomProd[2, 4]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`MomProd[2, 4]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(ConstructiveDiagrams`PropDen[
         ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass])) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) +
    4*((Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Spin",
          "Angle", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[2, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
        ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[4],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[5],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[4],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[6],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[2, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
         "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[5],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[6],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
        ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) +
    2*((Rational[-1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
           "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) +
    4*((Rational[-1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 5], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 4]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 4], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 4]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 4], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    4*((Rational[-1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    4*((Rational[-1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[1, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[1, 4]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[1, 4]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) +
    2*((Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[4],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[5],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[4],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[6],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[2, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[5],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Spin", "Square", 1],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[6],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6]]^2)/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[2, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) +
    2*((Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + Rational[1, 8]*
     ((Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass])),
2*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        2]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        2]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Spin", "Angle",
        1]]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[-1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) +
    2*((Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^2*ConstructiveDiagrams`MomProd[2, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Spinor["Spin", "Angle", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Square", 3]])/(ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
        ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass^3*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor["Spin",
          "Square", 1], ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
          ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]^2)/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[2, 4]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[2, 5]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) +
    2*((Rational[-1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
          ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 6]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 2]*
        ConstructiveDiagrams`MomProd[2, 5]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor["Helicity",
          "Square", 3]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1] +
         ConstructiveDiagrams`Mom[5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1] + ConstructiveDiagrams`Mom[5],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3, 6, 4]],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1] +
         ConstructiveDiagrams`Mom[4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3, 6, 5]],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1] +
        ConstructiveDiagrams`Mom[4], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[1, 2]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`MomProd[2, 6]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 2]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
        "Square", 1]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + Rational[1, 2]*
     ((Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 3]*mixedIdentityCoupling^4*mixedIdentityMass*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
           "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
           "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 1]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 2]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
         ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
       (ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) -
    2*((Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[2, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) +
    2*((Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 6]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 4]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 4], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`MomProd[1, 5]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
         ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Spin",
          "Square", 1]]*ConstructiveDiagrams`SpinorChain[
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
         ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 5], ConstructiveDiagrams`Mom[2],
         ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
        ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
          "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1],
         ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
          "Angle", 6]])/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass])) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1] +
         ConstructiveDiagrams`Mom[6], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[1] + ConstructiveDiagrams`Mom[6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3, 4, 5]],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1] +
         ConstructiveDiagrams`Mom[6], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[1] + ConstructiveDiagrams`Mom[6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor["Helicity",
        "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3, 5, 4]],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1] +
         ConstructiveDiagrams`Mom[4], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3, 5, 6]],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1] +
        ConstructiveDiagrams`Mom[4], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 4]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 6]],
       mixedIdentityMass]) + (Rational[1, 6]*mixedIdentityCoupling^4*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Spin", "Square", 1],
       ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
         "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1] +
         ConstructiveDiagrams`Mom[5], ConstructiveDiagrams`Spinor["Helicity",
         "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
       ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 3, 4, 6]],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
      ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
        "Helicity", "Angle", 6], ConstructiveDiagrams`Mom[1] +
        ConstructiveDiagrams`Mom[5], ConstructiveDiagrams`Mom[2],
       ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])/
     (mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[2, 6]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 3, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 4, 5]],
       mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
        ConstructiveDiagrams`Multiparticle[1, 5, 6]],
       mixedIdentityMass]) + Rational[1, 8]*
     ((Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 5], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 5, 6]],
         mixedIdentityMass]) + (Rational[-2, 3]*mixedIdentityCoupling^4*(ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]] +
         ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 5],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 6], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]])*
        (mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[1, 3, 4]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]] + mixedIdentityMass^2*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 4]] + ConstructiveDiagrams`PropDen[
           ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[2,
             3]], mixedIdentityMass]*
          ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 4]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 2],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
            ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 4], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 6]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 4]]^2*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]^2*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 5],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] -
         2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 4]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 4]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 3], ConstructiveDiagrams`Spinor["Helicity",
            "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 2], ConstructiveDiagrams`Spinor["Helicity",
            "Square", 3]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
            "Helicity", "Angle", 5]]*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 3]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Spin", "Square", 1], ConstructiveDiagrams`Spinor["Spin",
            "Square", 2]]*ConstructiveDiagrams`SpinorChain[
            ConstructiveDiagrams`Spinor["Helicity", "Square", 3],
            ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6],
           ConstructiveDiagrams`Mom[1], ConstructiveDiagrams`Mom[2],
           ConstructiveDiagrams`Spinor["Helicity", "Angle", 6]] +
         mixedIdentityMass^2*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 5]],
           mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
            ConstructiveDiagrams`Multiparticle[2, 6]],
           mixedIdentityMass]*ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
             "Helicity", "Angle", 6], ConstructiveDiagrams`Spinor["Helicity",
             "Angle", 5]]^2*ConstructiveDiagrams`SpinorChain[
           ConstructiveDiagrams`Spinor["Spin", "Square", 1],
           ConstructiveDiagrams`Spinor["Spin", "Square", 2]]*
          ConstructiveDiagrams`SpinorChain[ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3], ConstructiveDiagrams`Mom[1],
           ConstructiveDiagrams`Mom[2], ConstructiveDiagrams`Spinor[
            "Helicity", "Square", 3]]))/(mixedIdentityMass*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 3]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[2, 6]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 3, 4]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 5]],
         mixedIdentityMass]*ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[
          ConstructiveDiagrams`Multiparticle[1, 4, 6]],
         mixedIdentityMass]))}
 |>]
