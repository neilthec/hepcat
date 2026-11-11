

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

Function[{fixtureMass, fixtureCharge},
 <|"Preferred" -> (-4*fixtureCharge^4*fixtureMass*
      SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle",
        6]]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        6], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 1]])/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 1]])/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass^2*
      SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle",
         6]]^2*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Spinor["Spin", "Square", 2]]*
      (SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle",
          1]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 5]] +
       SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          1]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 6]] + fixtureMass*
        SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Spin", "Square",
          1]]))/(PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
          5], Spinor["Spin", "Angle", 1]] -
       SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Spin", "Square", 1]] -
       SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Spin", "Square", 1]]))/(PropDen[Mom[Multiparticle[1, 3]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
          6], Spinor["Spin", "Angle", 1]] -
       SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Spin", "Square", 1]] -
       SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Spin", "Square", 1]]))/(PropDen[Mom[Multiparticle[1, 3]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 2]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 2]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass^2*
      SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle",
         6]]^2*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*
      (SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle",
          1]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 5]] +
       SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          1]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 6]] + fixtureMass*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square",
          1]])*SpinorChain[Spinor["Helicity", "Square", 4],
       Spinor["Spin", "Square", 2]])/(PropDen[Mom[Multiparticle[1, 3]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Spinor["Helicity", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1],
       Spinor["Helicity", "Square", 4]]*
      (SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Angle", 6],
         Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Square",
          4], Spinor["Helicity", "Square", 6]] -
       SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle",
          1]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 6]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]])*SpinorChain[
       Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
        5]])/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 1], Spinor["Helicity", "Square", 3]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]])*SpinorChain[
       Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
        5]])/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 5]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]))/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 1], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Spinor["Helicity", "Square", 5]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 5]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 5]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] +
       2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          3], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] +
       SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 5]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]])*SpinorChain[
       Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
        6]])/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 1], Spinor["Helicity", "Square", 3]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]])*SpinorChain[
       Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
        6]])/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 1], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 5]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] +
       2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 5]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]] -
       fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 5]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[2], Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[2], Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Angle", 4],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] + fixtureMass*
        SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Helicity",
          "Square", 5]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] +
       2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 4],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Helicity",
          "Square", 5]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Angle", 3],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Helicity",
          "Square", 5]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] -
       2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 3],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Helicity",
          "Square", 5]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 6],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*SpinorChain[
       Spinor["Helicity", "Square", 4], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(2*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]] + fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]] -
       SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(2*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]] + fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]] -
       SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*
      (fixtureMass^3*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          6]] - fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[4], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[3], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*
      (fixtureMass^3*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]] - 2*fixtureMass*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          5]] - 2*fixtureMass*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[3], Spinor["Helicity", "Angle",
          5]] - fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[4], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[3], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] +
       2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[6],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 5]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[2], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[3],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*SpinorChain[
       Spinor["Helicity", "Square", 4], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Angle", 6], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Spin", "Square",
        1]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Spinor["Helicity", "Square", 6]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 6]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 6]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]] - 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] +
       fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 4],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 5]] +
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 3]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 4]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 4]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*
      (-(fixtureMass^3*SpinorChain[Spinor["Helicity", "Square", 3],
          Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
           "Angle", 1], Spinor["Helicity", "Angle", 6]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
          Spinor["Helicity", "Angle", 5]]) - 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]] + fixtureMass^3*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]] - 2*fixtureMass*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[5],
         Spinor["Helicity", "Angle", 6]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]] + fixtureMass^2*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[6], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*
      (-(fixtureMass^3*SpinorChain[Spinor["Helicity", "Square", 3],
          Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
           "Angle", 1], Spinor["Helicity", "Angle", 6]]*
         SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
          Spinor["Helicity", "Angle", 5]]) + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          6]] - fixtureMass^2*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[6],
         Spinor["Spin", "Angle", 1]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] +
       fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[6], Spinor["Spin", "Angle", 1]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 6]] +
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[6], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[6],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 6]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[3],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[6], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[6],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square",
        1]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Square", 4], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]),
  "LeafSmaller" -> (-4*fixtureCharge^4*fixtureMass*
      SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle",
        6]]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        6], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2)/(PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 1]])/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 1]])/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 2]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
         5], Spinor["Helicity", "Angle", 6]]^2*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
        "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        4], Spinor["Spin", "Square", 2]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
          5], Spinor["Spin", "Angle", 1]] + SpinorChain[
        Spinor["Helicity", "Angle", 5], Mom[3], Spinor["Spin", "Square",
         1]] + SpinorChain[Spinor["Helicity", "Angle", 5], Mom[4],
        Spinor["Spin", "Square", 1]]))/(PropDen[Mom[Multiparticle[1, 3]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*SpinorChain[
       Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
          6], Spinor["Spin", "Angle", 1]] + SpinorChain[
        Spinor["Helicity", "Angle", 6], Mom[3], Spinor["Spin", "Square",
         1]] + SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4],
        Spinor["Spin", "Square", 1]]))/(PropDen[Mom[Multiparticle[1, 3]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass^2*
      SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle",
         6]]^2*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 4], Spinor["Spin", "Square", 2]]*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Spin", "Square", 1]] + SpinorChain[Spinor["Helicity",
         "Square", 3], Mom[5], Spinor["Spin", "Angle", 1]] +
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[6],
        Spinor["Spin", "Angle", 1]]))/(PropDen[Mom[Multiparticle[1, 3]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[6],
         Spinor["Helicity", "Angle", 5]])*SpinorChain[
       Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
        5]])/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
         Spinor["Helicity", "Angle", 5]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[5],
         Spinor["Helicity", "Angle", 6]])*SpinorChain[
       Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
        6]])/(PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass^2*
      SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle",
         6]]^2*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Spinor["Spin", "Square", 2]]*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Spin", "Square", 1]] + SpinorChain[Spinor["Helicity",
         "Square", 4], Mom[5], Spinor["Spin", "Angle", 1]] +
       SpinorChain[Spinor["Helicity", "Square", 4], Mom[6],
        Spinor["Spin", "Angle", 1]]))/(PropDen[Mom[Multiparticle[1, 4]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Spinor["Helicity", "Square", 3]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1],
       Spinor["Helicity", "Square", 4]]*
      (SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin", "Angle",
          1]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1],
         Spinor["Helicity", "Square", 4]] -
       SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[6],
         Spinor["Spin", "Angle", 1]]))/(PropDen[Mom[Multiparticle[1, 4]],
       fixtureMass]*PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 1], Mom[3],
         Spinor["Helicity", "Angle", 5]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 1], Mom[3],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] +
       2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[5], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 6]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[5], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 1], Mom[3],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Mom[3], Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          3], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[5], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[5], Spinor["Helicity", "Angle", 6]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 1], Mom[4],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          3], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Mom[4], Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 5]] +
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Spin", "Angle", 1]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Spin", "Square", 2],
         Mom[1], Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 5]]*
      (fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Spin", "Angle", 1]] - 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 1], Mom[3],
         Spinor["Helicity", "Angle", 5]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 1], Mom[3],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Spin", "Square", 2],
         Mom[1], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 1], Mom[3],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 6],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*SpinorChain[
       Spinor["Helicity", "Square", 4], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(2*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]] + fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]] -
       SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 1]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
         "Square", 4]]^2*(2*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]] + fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]] -
       SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle",
          2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Helicity", "Square", 4]]*
      (SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*
      (fixtureMass^3*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          6]] - fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[4], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[3], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*
      (fixtureMass^3*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle",
          2], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]] - 2*fixtureMass*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          5]] - 2*fixtureMass*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[3], Spinor["Helicity", "Angle",
          5]] - fixtureMass^2*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[4], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]] -
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[3], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] -
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 4]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] +
       2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[6],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 5]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Angle", 5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[2], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[3],
         Spinor["Helicity", "Angle", 6]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Square",
        3], Spinor["Helicity", "Square", 4]]*SpinorChain[
       Spinor["Helicity", "Square", 4], Spinor["Spin", "Square", 2]]*
      SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Angle", 6], Mom[1], Mom[2], Spinor["Helicity", "Angle", 6]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Spin", "Square",
        1]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
       Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
        "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 5]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Spinor["Helicity", "Square", 6]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 6]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 4],
         Spinor["Helicity", "Square", 6]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]] - 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 6]] +
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] +
       SpinorChain[Spinor["Helicity", "Angle", 4], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 3]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 4]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*
      (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]])*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin",
          "Square", 2], Spinor["Helicity", "Square", 4]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]] -
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) +
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*
      (fixtureMass^3*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]] - fixtureMass^3*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]] - 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]] + 2*fixtureMass*MomProd[1, 2]*SpinorChain[Spinor["Helicity",
          "Square", 3], Spinor["Helicity", "Square", 4]]*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[5],
         Spinor["Helicity", "Angle", 6]] - 2*fixtureMass*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]] - fixtureMass^2*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[5], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[6], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Spin", "Angle", 2]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] +
       fixtureMass*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[6], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*fixtureMass*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Helicity", "Angle", 6]]*
      (fixtureMass^3*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 6]]*SpinorChain[
         Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass^3*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[1], Spinor["Helicity", "Angle",
          6]] + fixtureMass^2*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
          1], Spinor["Helicity", "Angle", 5]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Helicity", "Angle", 6]] + fixtureMass^2*
        SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity",
          "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 3],
         Spinor["Helicity", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]]*
        SpinorChain[Spinor["Helicity", "Square", 4], Mom[6],
         Spinor["Spin", "Angle", 1]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] -
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square",
          4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]] + fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]] -
       fixtureMass*SpinorChain[Spinor["Helicity", "Angle", 5],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Square",
          2], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[6], Spinor["Spin", "Angle", 1]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (2*MomProd[1, 2]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
         Spinor["Helicity", "Angle", 6]] - fixtureMass^2*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 6]] +
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[4], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[6], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[6],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
       Spinor["Helicity", "Angle", 6]]*
      (fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 4]]*SpinorChain[
         Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]] + 2*MomProd[1, 2]*
        SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
          "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 6]] -
       fixtureMass^2*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          1], Spinor["Helicity", "Square", 3]]*SpinorChain[
         Spinor["Helicity", "Square", 4], Mom[2], Spinor["Helicity", "Angle",
          5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[3],
         Spinor["Helicity", "Angle", 6]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          4]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
         Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[2], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square",
          3]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 4], Mom[3], Spinor["Helicity", "Angle", 6]]*
        SpinorChain[Spinor["Spin", "Square", 2], Mom[1],
         Spinor["Helicity", "Angle", 5]] - fixtureMass*
        SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity",
          "Angle", 5]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[1],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Helicity",
          "Square", 3], Mom[1], Mom[6], Spinor["Helicity", "Square", 4]] +
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
          6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
         Spinor["Helicity", "Angle", 5]]*SpinorChain[Spinor["Spin", "Square",
          2], Mom[1], Spinor["Helicity", "Angle", 5]]*
        SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[6],
         Spinor["Helicity", "Square", 4]]))/
     (PropDen[Mom[Multiparticle[1, 3]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 6]], fixtureMass]) -
    (4*fixtureCharge^4*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
        "Angle", 6], Spinor["Spin", "Angle", 2]]*
      SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square",
        1]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
       Spinor["Helicity", "Square", 3]]*SpinorChain[Spinor["Helicity",
        "Square", 4], Mom[1], Mom[2], Spinor["Helicity", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 5]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 6]], fixtureMass]*
      PropDen[Mom[Multiparticle[2, 4]], fixtureMass]*
      PropDen[Mom[Multiparticle[1, 3, 5]], fixtureMass])|>]
