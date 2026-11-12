(* Ordinary source fixture: canonical-product reduction of a prepared five-point channel.
   Full production logic contains no process- or fixture-specific branch.
   HEPCAT, Copyright (C) 2026 Neil Christensen, GPL-3.0-or-later. *)
Function[{fixtureModel}, Module[{fixtureParameters = AssociationMap[ModelParameter[fixtureModel, #] &, {"EE","Me","Mh","MW","SW"}]},
<|"SourceExpression" -> -1/2*(fixtureParameters["EE"]^3*fixtureParameters["Me"]*
     (3*fixtureParameters["Mh"]^2*PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
        fixtureParameters["Me"]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]] +
      2*fixtureParameters["Me"]^2*PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]],
        fixtureParameters["Mh"]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]] +
      2*fixtureParameters["Me"]^2*PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]],
        fixtureParameters["Mh"]]*SpinorChain[Spinor["Spin", "Square", 1], Spinor["Helicity", "Square", 3]]*
       SpinorChain[Spinor["Spin", "Square", 2], Spinor["Helicity", "Square", 3]] +
      PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[4], Spinor["Spin", "Angle", 2]] +
      PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[5], Spinor["Spin", "Angle", 2]] +
      3*fixtureParameters["Mh"]^2*PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*SpinorChain[Spinor["Spin", "Angle", 1],
        Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
        Spinor["Helicity", "Square", 3]] + 2*fixtureParameters["Me"]^2*PropDen[Mom[Multiparticle[1, 4]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]] -
      2*MomProd[3, 5]*PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*SpinorChain[Spinor["Spin", "Angle", 1],
        Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
        Spinor["Helicity", "Square", 3]] + 2*fixtureParameters["Me"]^2*PropDen[Mom[Multiparticle[1, 5]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*
       SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]] -
      2*MomProd[3, 4]*PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*SpinorChain[Spinor["Spin", "Angle", 1],
        Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
        Spinor["Helicity", "Square", 3]] + 3*fixtureParameters["Mh"]^2*PropDen[Mom[Multiparticle[1, 4]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]] +
      2*fixtureParameters["Me"]^2*PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*SpinorChain[Spinor["Spin", "Square", 1],
        Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
        Spinor["Helicity", "Square", 3]] + 2*fixtureParameters["Me"]^2*PropDen[Mom[Multiparticle[1, 5]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*
       SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]] +
      fixtureParameters["Me"]*PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*SpinorChain[Spinor["Spin", "Square", 1],
        Mom[4], Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
        Spinor["Helicity", "Square", 3]] + fixtureParameters["Me"]*PropDen[Mom[Multiparticle[1, 4]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*
       SpinorChain[Spinor["Spin", "Square", 1], Mom[5], Spinor["Spin", "Angle", 2]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]] +
      fixtureParameters["Me"]*PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*
       PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*SpinorChain[Spinor["Spin", "Square", 2],
        Mom[4], Spinor["Spin", "Angle", 1]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
        Spinor["Helicity", "Square", 3]] + fixtureParameters["Me"]*PropDen[Mom[Multiparticle[1, 4]],
        fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]*
       SpinorChain[Spinor["Spin", "Square", 2], Mom[5], Spinor["Spin", "Angle", 1]]*
       SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]]))/
    (Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*
     PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]),
 "ExpectedReducedSource" ->
  -((fixtureParameters["EE"]^3*fixtureParameters["Me"]^3*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square", 3],
       Spinor["Spin", "Square", 2]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
      PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]],
       fixtureParameters["Me"]])) - (Sqrt[2]*fixtureParameters["EE"]^3*fixtureParameters["Me"]^3*
     MomProd[1, 2]*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]])/
    (fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*
     PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^3*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]])/
    (Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (Sqrt[2]*fixtureParameters["EE"]^3*fixtureParameters["Me"]^3*MomProd[1, 2]*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]])/
    (fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*
     PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (3*fixtureParameters["EE"]^3*fixtureParameters["Me"]*fixtureParameters["Mh"]^2*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]])/
    (2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[2, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]) -
   (3*fixtureParameters["EE"]^3*fixtureParameters["Me"]*fixtureParameters["Mh"]^2*MomProd[1, 2]*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]])/
    (Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]*
     PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^4*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
      Spinor["Spin", "Angle", 2]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^4*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1],
      Spinor["Spin", "Angle", 2]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (3*fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*fixtureParameters["Mh"]^2*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 1]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Spin", "Angle", 2]])/
    (2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]*
     PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^4*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
      Spinor["Spin", "Angle", 1]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^4*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2],
      Spinor["Spin", "Angle", 1]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (3*fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*fixtureParameters["Mh"]^2*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]*
     PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]) +
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[4],
      Spinor["Spin", "Angle", 1]])/(2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]],
      fixtureParameters["Me"]]) + (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[5], Spinor["Spin", "Angle", 1]])/
    (2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
      Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) +
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]*MomProd[3, 4]*SpinorChain[Spinor["Spin", "Angle", 1],
      Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^3*SpinorChain[Spinor["Spin", "Angle", 1],
      Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) +
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]*MomProd[3, 5]*SpinorChain[Spinor["Spin", "Angle", 1],
      Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (3*fixtureParameters["EE"]^3*fixtureParameters["Me"]*fixtureParameters["Mh"]^2*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2], Spinor["Helicity", "Square", 3]])/
    (2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 3]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]*
     PropDen[Mom[Multiparticle[4, 5]], fixtureParameters["Mh"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*SpinorChain[Spinor["Spin", "Angle", 1], Mom[4],
      Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*SpinorChain[Spinor["Spin", "Angle", 1], Mom[5],
      Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*SpinorChain[Spinor["Spin", "Angle", 2], Mom[4],
      Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 4]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) -
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*SpinorChain[Spinor["Spin", "Angle", 2], Mom[5],
      Spinor["Spin", "Square", 1]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Mom[2],
      Spinor["Helicity", "Square", 3]])/(2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 3]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) +
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]*SpinorChain[Spinor["Spin", "Angle", 1],
      Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Mom[4],
      Spinor["Helicity", "Square", 3]])/(2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 4]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]],
      fixtureParameters["Me"]]) + (fixtureParameters["EE"]^3*fixtureParameters["Me"]*
     SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[2], Mom[5], Spinor["Helicity", "Square", 3]])/
    (2*Sqrt[2]*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 5]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]),
 "ExpectedDirection" -> (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*
     SpinorChain[Spinor["Helicity", "Square", 3], Spinor["Spin", "Square", 2]]*
     SpinorChain[Spinor["Helicity", "Square", 3], Mom[4], Spinor["Spin", "Angle", 1]])/
    (2*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*PropDen[Mom[Multiparticle[1, 4]],
      fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]], fixtureParameters["Me"]]) +
   (fixtureParameters["EE"]^3*fixtureParameters["Me"]^2*SpinorChain[Spinor["Helicity", "Square", 3],
      Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Square", 3], Mom[5],
      Spinor["Spin", "Angle", 1]])/(2*fixtureParameters["MW"]^2*fixtureParameters["SW"]^2*
     PropDen[Mom[Multiparticle[1, 5]], fixtureParameters["Me"]]*PropDen[Mom[Multiparticle[2, 3]],
      fixtureParameters["Me"]]), "ExternalMassRules" -> {Mass[1] -> fixtureParameters["Me"],
   Mass[2] -> fixtureParameters["Me"], Mass[3] -> 0, Mass[4] -> fixtureParameters["Mh"],
   Mass[5] -> fixtureParameters["Mh"]}|>
]]
