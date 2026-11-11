

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

Function[HECompactIdentity`argument,
 {(SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]),
  (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 4]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[1], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[3],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[2], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[1, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 3]]*
     SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
   (SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 1]]*
     SpinorChain[Spinor["Spin", "Square", 4], Spinor["Spin", "Square", 3]]*
     SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[2],
      Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity",
       "Square", 5], Mom[4], Mom[3], Spinor["Helicity", "Square", 5]])/
    (PropDen[Mom[Multiparticle[2, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[3, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 5]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[4, 6]], HECompactIdentity`argument]*
     PropDen[Mom[Multiparticle[1, 3, 5]], 0])}]
