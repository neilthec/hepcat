

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

<|"Expression" -> 
  -((vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Angle", 3], 
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
       Spinor["Spin", "Square", 3]])/(Sqrt[2]*vectorIdentityParameter2*
      vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
       vectorIdentityParameter4])) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 3], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Angle", 4], 
      Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Spinor["Spin", "Square", 3]])/(2*Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*vectorIdentityParameter4*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]])/
    (Sqrt[2]*vectorIdentityParameter2^2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*vectorIdentityParameter4*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (Sqrt[2]*vectorIdentityParameter2^2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]])/(Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]])/(Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 4], 
      Spinor["Spin", "Square", 1]])/(2*Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Square", 3]])/(2*Sqrt[2]*vectorIdentityParameter2^2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) + (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 4], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Square", 1]])/(2*Sqrt[2]*vectorIdentityParameter2^2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) + (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Angle", 4], 
      Mom[Multiparticle[1, 2]], Spinor["Spin", "Square", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) + (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Angle", 4], 
      Mom[Multiparticle[1, 2]], Spinor["Spin", "Square", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[2], 
      Spinor["Spin", "Angle", 3]])/(2*Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*vectorIdentityParameter4*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Mom[2], Spinor["Spin", "Angle", 3]])/
    (2*Sqrt[2]*vectorIdentityParameter2^2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Square", 1], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Mom[2], Spinor["Spin", "Angle", 3]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[2], 
      Spinor["Spin", "Angle", 4]])/(2*Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*vectorIdentityParameter4*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Mom[2], Spinor["Spin", "Angle", 4]])/
    (2*Sqrt[2]*vectorIdentityParameter2^2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Square", 1], 
      Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Mom[2], Spinor["Spin", "Angle", 4]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Angle", 3]])/(2*Sqrt[2]*vectorIdentityParameter2^2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Mom[2], 
      Spinor["Spin", "Angle", 1]])/(2*Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*vectorIdentityParameter4*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 3], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2^2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[2], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[2], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[2], 
      Spinor["Spin", "Angle", 1]])/(2*Sqrt[2]*vectorIdentityParameter2*
     vectorIdentityParameter3^2*vectorIdentityParameter4*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 3], 
      Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1], 
      Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Square", 4], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2^2*vectorIdentityParameter3^2*
     PropDen[Mom[Multiparticle[1, 2]], vectorIdentityParameter4]) - 
   (vectorIdentityParameter1^2*SpinorChain[Spinor["Spin", "Angle", 1], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 3], 
      Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Square", 4], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[2], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 4], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) - (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[2], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 4], 
      Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) + (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Angle", 1]])/(2*Sqrt[2]*vectorIdentityParameter2^2*
     vectorIdentityParameter3^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) + (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 4], 
      Mom[Multiparticle[1, 2]], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]) + (vectorIdentityParameter1^2*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Mom[Multiparticle[1, 2]], 
      Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 4], 
      Mom[Multiparticle[1, 2]], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*vectorIdentityParameter2*vectorIdentityParameter3^2*
     vectorIdentityParameter4^2*PropDen[Mom[Multiparticle[1, 2]], 
      vectorIdentityParameter4]), "ExternalMassRules" -> 
  {Mass[1] -> vectorIdentityParameter4, Mass[2] -> vectorIdentityParameter5, 
   Mass[3] -> vectorIdentityParameter2, Mass[4] -> vectorIdentityParameter2}, 
 "Scope" -> 
  "OrdinaryExternalMassAndSpinorIdentity;NoInternalShellOrNumericalParameters\
"|>
