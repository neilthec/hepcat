

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

Function[heIdentityMass,
 (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
     Spinor["Helicity", "Square", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Helicity", "Square",
       5]]^2*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square",
      3], Mom[2], Spinor["Helicity", "Angle", 6]])/
   (6*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
     Spinor["Helicity", "Square", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Helicity", "Square",
       4]]^2*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square",
      3], Mom[2], Spinor["Helicity", "Angle", 6]])/
   (6*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 4],
      Spinor["Helicity", "Square", 5]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
      Spinor["Helicity", "Angle", 6]]^2)/
   (6*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 5],
      Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
      Spinor["Helicity", "Angle", 6]]^2)/
   (6*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass]) +
  (heIdentityMass^2*SpinorChain[Spinor["Helicity", "Square", 4],
      Spinor["Helicity", "Square", 5]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1] + Mom[4] + Mom[5],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 4]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass]) +
  (heIdentityMass^2*SpinorChain[Spinor["Helicity", "Square", 5],
      Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1] + Mom[4] + Mom[5],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 4]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 5]]^2*
    SpinorChain[Spinor["Helicity", "Square", 4], Spinor["Helicity", "Square",
      6]]*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
      6]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
      6]]*SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 3]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 4],
     Spinor["Helicity", "Square", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Helicity", "Square",
       3]]^2*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square",
      4], Mom[2], Spinor["Helicity", "Angle", 6]])/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 5]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
      Spinor["Helicity", "Angle", 6]]^2)/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 5],
      Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 4], Mom[2],
      Spinor["Helicity", "Angle", 6]]^2)/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass^2*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 5]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 4], Mom[1] + Mom[3] + Mom[5],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 3]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass^2*SpinorChain[Spinor["Helicity", "Square", 5],
      Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 4], Mom[1] + Mom[3] + Mom[5],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 3]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Helicity", "Square",
      6]]*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
      6]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
      6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 3]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 4],
      Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Helicity", "Square",
      6]]*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
      6]]*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
      6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 3]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
      Spinor["Helicity", "Angle", 6]]^2)/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 4],
      Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
      Spinor["Helicity", "Angle", 6]]^2)/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass^2*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 5]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1] + Mom[3] + Mom[4],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 3]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass^2*SpinorChain[Spinor["Helicity", "Square", 4],
      Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 5]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1] + Mom[3] + Mom[4],
     Spinor["Helicity", "Angle", 6]])/(6*PropDen[Mom[Multiparticle[1, 3]],
     heIdentityMass]*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1] + Mom[3] + Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Mom[1] + Mom[3] + Mom[4], Spinor["Helicity", "Square", 5]])/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 4],
      Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1] + Mom[3] + Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Mom[1] + Mom[3] + Mom[4], Spinor["Helicity", "Square", 5]])/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 4]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Helicity", "Square", 5]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 4], Mom[1] + Mom[3] + Mom[5],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Mom[1] + Mom[3] + Mom[5], Spinor["Helicity", "Square", 4]])/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 5],
      Spinor["Helicity", "Square", 3]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 4], Mom[1] + Mom[3] + Mom[5],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Mom[1] + Mom[3] + Mom[5], Spinor["Helicity", "Square", 4]])/
   (6*PropDen[Mom[Multiparticle[1, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 4],
      Spinor["Helicity", "Square", 5]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1] + Mom[4] + Mom[5],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Mom[1] + Mom[4] + Mom[5], Spinor["Helicity", "Square", 3]])/
   (6*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass]) +
  (heIdentityMass*SpinorChain[Spinor["Helicity", "Square", 5],
      Spinor["Helicity", "Square", 4]]^2*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 6]]*
    SpinorChain[Spinor["Helicity", "Square", 3], Mom[1] + Mom[4] + Mom[5],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Mom[1] + Mom[4] + Mom[5], Spinor["Helicity", "Square", 3]])/
   (6*PropDen[Mom[Multiparticle[1, 4]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 5]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 3]], heIdentityMass]*
    PropDen[Mom[Multiparticle[2, 6]], heIdentityMass]*
    PropDen[Mom[Multiparticle[1, 4, 5]], heIdentityMass])]
