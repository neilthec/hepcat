

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

<|"PositiveDirections" ->
  {(-2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        1], Spinor["Spin", "Angle", 2]])/(PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        1], Spinor["Spin", "Angle", 2]])/(PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        1], Spinor["Spin", "Angle", 2]])/(PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        1], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 2]],
       0]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        1], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 2]],
       0]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        1], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 2]],
       0]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 2]],
       0]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 2]],
       0]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 2]],
       0]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 3]])/(PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Angle",
        2], Spinor["Spin", "Angle", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Spin", "Angle",
        3], Spinor["Spin", "Angle", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Spin", "Angle",
        3], Spinor["Spin", "Angle", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square",
        5], Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Spin", "Angle",
        3], Spinor["Spin", "Angle", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]),
   (8*(-((Sqrt[2]*ModelParameter["EE",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
           Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
            "Square", 5], Spinor["Spin", "Square", 4]]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
            2]])/(PropDen[Mom[Multiparticle[1, 3]], 0]*
          PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]])) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 2]]*
         SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 2]]*
         SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3 +
    (8*(-((Sqrt[2]*ModelParameter["EE",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
           Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
            "Square", 5], Spinor["Spin", "Square", 4]]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
            2]])/(PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me", "c29\
0c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad", "Real"]]*
          PropDen[Mom[Multiparticle[2, 4]], 0]*PropDen[
           Mom[Multiparticle[3, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]])) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 2]]*
         SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 2]]*
         SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3 - (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Square",
        5], Mom[1], Mom[4], Spinor["Helicity", "Square", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (4*((ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3 +
    2*
     ((ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[2], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[1], Mom[3], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[2], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity",
          "Square", 5], Mom[3], Mom[4], Spinor["Helicity", "Square", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]])) -
    (4*((-2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]) -
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 4]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 3]]*
         SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 2]]*
         SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]) +
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Square", 5],
          Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity",
           "Square", 5], Spinor["Spin", "Square", 2]]*
         SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Square", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3}, "NegativeDirections" ->
  {(-2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        1], Spinor["Spin", "Square", 2]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        1], Spinor["Spin", "Square", 2]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[2, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        1], Spinor["Spin", "Square", 2]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[3, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        1], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        1], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[2, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        1], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[3, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[2, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[2, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[3, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 3]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[3, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
        2], Spinor["Spin", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square",
        3], Spinor["Spin", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square",
        3], Spinor["Spin", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
       Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square",
        3], Spinor["Spin", "Square", 4]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]),
   (8*(-((Sqrt[2]*ModelParameter["EE",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity",
            "Angle", 5], Spinor["Spin", "Angle", 4]]*SpinorChain[
           Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])/
         (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[
           Mom[Multiparticle[2, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]])) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 2]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[2, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin",
           "Square", 3], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[3, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin",
           "Square", 3], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3 +
    (8*(-((Sqrt[2]*ModelParameter["EE",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity",
            "Angle", 5], Spinor["Spin", "Angle", 4]]*SpinorChain[
           Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]])/
         (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
          PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]])) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 2]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin",
           "Square", 2], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[1, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin",
           "Square", 3], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*MomProd[4, 5]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle",
           5], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin",
           "Square", 3], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3 - (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) -
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
       ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
       Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
       Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
        5], Mom[1], Mom[4], Spinor["Helicity", "Angle", 5]])/
     (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]) +
    (4*((ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3 +
    2*
     ((ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[2], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[1], Mom[3], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[2], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) +
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]) -
      (ModelParameter["EE",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle",
          5], Mom[3], Mom[4], Spinor["Helicity", "Angle", 5]])/
       (Sqrt[2]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]])) -
    (4*((-2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 2]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 2]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
           1], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]) -
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square",
           2], Spinor["Spin", "Square", 3]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
           2], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
           2], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square",
           3], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]) +
       (2*Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Spin",
           "Angle", 1]]*SpinorChain[Spinor["Helicity", "Angle", 5],
          Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square",
           3], Spinor["Spin", "Square", 4]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) -
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]) +
       (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[2], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[3], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           4]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) + (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[1, 2]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (Sqrt[2]*PropDen[Mom[Multiparticle[1, 3]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]) - (Sqrt[2]*ModelParameter["EE",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]^3*SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
           2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
           "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[3],
          Mom[4], Spinor["Helicity", "Angle", 5]])/
        (PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]])))/3}, "ExternalMassRules" ->
  {Mass[1] -> ModelParameter["Me",
     "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
     "Real"], Mass[2] -> ModelParameter["Me",
     "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
     "Real"], Mass[3] -> ModelParameter["Me",
     "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
     "Real"], Mass[4] -> ModelParameter["Me",
     "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
     "Real"], Mass[5] -> 0}, "PreparedAmplitudes" ->
  <|"Positive" -> (-2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*(-(PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
         PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
         PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
           "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad"\
, "Real"]]*(PropDen[Mom[Multiparticle[1, 2]], 0]*SpinorChain[
            Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
           SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
             "Square", 5]] + PropDen[Mom[Multiparticle[1, 3]], 0]*
           SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
             3]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
             "Square", 5]] + PropDen[Mom[Multiparticle[1, 3]], 0]*
           SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
             3]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
             "Square", 5]] - PropDen[Mom[Multiparticle[1, 2]], 0]*
           SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle",
             2]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Helicity",
             "Square", 5]])*SpinorChain[Spinor["Spin", "Square", 4],
          Spinor["Helicity", "Square", 5]]) +
       PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[1, 5]],
         ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*SpinorChain[Spinor["Spin", "Square", 3],
         Spinor["Helicity", "Square", 5]]*
        (PropDen[Mom[Multiparticle[2, 4]], 0]*SpinorChain[
           Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
            "Square", 5]] + PropDen[Mom[Multiparticle[1, 2]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
            "Square", 5]] + PropDen[Mom[Multiparticle[2, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
            "Square", 5]] + PropDen[Mom[Multiparticle[1, 2]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
          SpinorChain[Spinor["Spin", "Square", 4], Spinor["Helicity",
            "Square", 5]]) - PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*SpinorChain[Spinor["Spin", "Square", 2],
         Spinor["Helicity", "Square", 5]]*
        (PropDen[Mom[Multiparticle[3, 4]], 0]*SpinorChain[
           Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity",
            "Square", 5]] + PropDen[Mom[Multiparticle[1, 3]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 3], Spinor["Helicity",
            "Square", 5]] + PropDen[Mom[Multiparticle[3, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 3], Spinor["Helicity",
            "Square", 5]] + PropDen[Mom[Multiparticle[1, 3]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
          SpinorChain[Spinor["Spin", "Square", 4], Spinor["Helicity",
            "Square", 5]]) - PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*SpinorChain[Spinor["Spin", "Square", 1],
         Spinor["Helicity", "Square", 5]]*
        (PropDen[Mom[Multiparticle[3, 4]], 0]*SpinorChain[
           Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity",
            "Square", 5]] - PropDen[Mom[Multiparticle[2, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 3], Spinor["Helicity",
            "Square", 5]] - PropDen[Mom[Multiparticle[2, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
          SpinorChain[Spinor["Spin", "Square", 4], Spinor["Helicity",
            "Square", 5]] - PropDen[Mom[Multiparticle[3, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
          SpinorChain[Spinor["Spin", "Square", 4], Spinor["Helicity",
            "Square", 5]]) - PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 4]], 0]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square",
            2]] - SpinorChain[Spinor["Spin", "Angle", 2],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
            1], Spinor["Spin", "Square", 4]] -
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square",
            3]] + SpinorChain[Spinor["Spin", "Angle", 1],
           Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square",
            3], Spinor["Spin", "Square", 4]])*
        (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[
           Mom[Multiparticle[2, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
           Spinor["Helicity", "Square", 5]] -
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
          PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[4],
           Spinor["Helicity", "Square", 5]]) +
       PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[2, 4]],
         0]*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
            4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
            "Square", 3]] + SpinorChain[Spinor["Spin", "Angle", 2],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
            1], Spinor["Spin", "Square", 4]] +
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square",
            3]] + SpinorChain[Spinor["Spin", "Angle", 1],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
            2], Spinor["Spin", "Square", 4]])*
        (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[
           Mom[Multiparticle[3, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[2],
           Spinor["Helicity", "Square", 5]] -
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*SpinorChain[
           Spinor["Helicity", "Square", 5], Mom[3], Mom[4],
           Spinor["Helicity", "Square", 5]])))/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]), "Negative" ->
    (-2*Sqrt[2]*ModelParameter["EE",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]^3*(PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*SpinorChain[Spinor["Spin", "Angle", 4],
         Spinor["Helicity", "Angle", 5]]*
        (PropDen[Mom[Multiparticle[1, 2]], 0]*SpinorChain[
           Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle", 5]]*
          SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square",
            2]] - PropDen[Mom[Multiparticle[1, 3]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
            "Square", 3]] - PropDen[Mom[Multiparticle[1, 2]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
            "Square", 3]] - PropDen[Mom[Multiparticle[1, 3]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
            "Square", 3]]) + PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
        PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*SpinorChain[Spinor["Spin", "Angle", 3],
         Spinor["Helicity", "Angle", 5]]*
        (PropDen[Mom[Multiparticle[1, 2]], 0]*SpinorChain[
           Spinor["Spin", "Angle", 4], Spinor["Helicity", "Angle", 5]]*
          SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square",
            2]] + PropDen[Mom[Multiparticle[1, 2]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
            "Square", 4]] + PropDen[Mom[Multiparticle[2, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
            "Square", 4]] + PropDen[Mom[Multiparticle[2, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
            "Square", 4]]) - PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
        PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*SpinorChain[Spinor["Spin", "Angle", 2],
         Spinor["Helicity", "Angle", 5]]*
        (PropDen[Mom[Multiparticle[1, 3]], 0]*SpinorChain[
           Spinor["Spin", "Angle", 4], Spinor["Helicity", "Angle", 5]]*
          SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square",
            3]] + PropDen[Mom[Multiparticle[1, 3]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
            "Square", 4]] + PropDen[Mom[Multiparticle[3, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
            "Square", 4]] + PropDen[Mom[Multiparticle[3, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
            "Square", 4]]) + PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[1, 3]], 0]*
        PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
          "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
          "Real"]]*SpinorChain[Spinor["Spin", "Angle", 1],
         Spinor["Helicity", "Angle", 5]]*
        (PropDen[Mom[Multiparticle[2, 4]], 0]*SpinorChain[
           Spinor["Spin", "Angle", 4], Spinor["Helicity", "Angle", 5]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square",
            3]] + PropDen[Mom[Multiparticle[3, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 4], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
            "Square", 3]] + PropDen[Mom[Multiparticle[2, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin",
            "Square", 4]] - PropDen[Mom[Multiparticle[3, 4]], 0]*
          SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle",
            5]]*SpinorChain[Spinor["Spin", "Square", 3], Spinor["Spin",
            "Square", 4]]) - PropDen[Mom[Multiparticle[1, 2]], 0]*
        PropDen[Mom[Multiparticle[3, 4]], 0]*
        (SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square",
            2]] - SpinorChain[Spinor["Spin", "Angle", 2],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
            1], Spinor["Spin", "Square", 4]] -
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square",
            3]] + SpinorChain[Spinor["Spin", "Angle", 1],
           Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square",
            3], Spinor["Spin", "Square", 4]])*
        (PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[
           Mom[Multiparticle[2, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[3],
           Spinor["Helicity", "Angle", 5]] -
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
          PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[2], Mom[4],
           Spinor["Helicity", "Angle", 5]]) +
       PropDen[Mom[Multiparticle[1, 3]], 0]*PropDen[Mom[Multiparticle[2, 4]],
         0]*(SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle",
            4]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin",
            "Square", 3]] + SpinorChain[Spinor["Spin", "Angle", 2],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
            1], Spinor["Spin", "Square", 4]] +
         SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
          SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square",
            3]] + SpinorChain[Spinor["Spin", "Angle", 1],
           Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square",
            2], Spinor["Spin", "Square", 4]])*
        (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[
           Mom[Multiparticle[3, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*SpinorChain[Spinor["Helicity", "Angle", 5], Mom[1], Mom[2],
           Spinor["Helicity", "Angle", 5]] -
         PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
            "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad\
", "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*SpinorChain[
           Spinor["Helicity", "Angle", 5], Mom[3], Mom[4],
           Spinor["Helicity", "Angle", 5]])))/
     (PropDen[Mom[Multiparticle[1, 2]], 0]*PropDen[Mom[Multiparticle[1, 3]],
       0]*PropDen[Mom[Multiparticle[1, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[2, 4]], 0]*
      PropDen[Mom[Multiparticle[2, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[3, 4]], 0]*
      PropDen[Mom[Multiparticle[3, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]]*PropDen[Mom[Multiparticle[4, 5]], ModelParameter["Me",
        "c290c11592fdb153523bc79fa19d6792772af2948585b673a998d4f95927eaad",
        "Real"]])|>|>
