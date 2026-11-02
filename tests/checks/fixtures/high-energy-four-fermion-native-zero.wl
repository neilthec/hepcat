(* A retained affine direction from the fresh general four-fermion/two-photon
   completion. This is a regression fixture, never a production amplitude.
   Its zero certificate uses external identities and generic coherent spins. *)
Function[{HEPCATNativeIdentityFixture`mass, HEPCATNativeIdentityFixture`coupling},
 (-4*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (4*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (4*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (4*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (4*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (4*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^3*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^3*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Spinor["Spin", "Square", 1]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^3*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^3*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Spinor["Spin", "Square", 1]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^3*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^3*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Spin", "Square", 1]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Spinor["Spin", "Angle", 1]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Spinor["Spin", "Angle", 1]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Spinor["Spin", "Angle", 1]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1], Spinor["Spin", "Angle", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[3, 4]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[1], Spinor["Spin", "Angle", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[2, 4]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]])/(PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Mom[1], Spinor["Spin", "Angle", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[2], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 3],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 3],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[3], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Angle", 6],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Angle", 6],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[3],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[2],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Angle", 6],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 3],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Angle", 6],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[1],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[1], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[1], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[1], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[1], Spinor["Spin", "Square", 3]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Square", 5],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[1], Spinor["Spin", "Square", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Square", 5],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[3],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[1], Spinor["Spin", "Square", 3]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 3]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[1], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[1], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[1], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Square", 5],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[1], Spinor["Spin", "Square", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[3, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[2], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[2, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 3],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Spin", "Square", 3],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[3], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[2],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[1], Spinor["Spin", "Square", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 2]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[1], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[1], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[1], Spinor["Spin", "Square", 4]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Helicity", "Square", 5],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 2]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[1], Spinor["Spin", "Square", 4]])/(PropDen[Mom[Multiparticle[1, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[2, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (2*HEPCATNativeIdentityFixture`coupling^4*MomProd[1, 4]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 2]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Spin", "Square", 1]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[1, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 4]]*
    SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 3]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Mom[4], Spinor["Spin", "Square", 2]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[3], Mom[4],
     Spinor["Helicity", "Square", 5]])/(PropDen[Mom[Multiparticle[2, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[3, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 5]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[4, 6]],
     HEPCATNativeIdentityFixture`mass]*PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[1], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 3],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 2]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 2],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 1],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 4]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 3]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 3, 5]], 0]) +
  (HEPCATNativeIdentityFixture`coupling^4*SpinorChain[Spinor["Spin", "Angle", 1],
     Spinor["Spin", "Angle", 3]]*SpinorChain[Spinor["Spin", "Square", 2],
     Spinor["Spin", "Square", 4]]*SpinorChain[Spinor["Helicity", "Angle", 6], Mom[2], Mom[4],
     Spinor["Helicity", "Angle", 6]]*SpinorChain[Spinor["Helicity", "Square", 5], Mom[3],
     Mom[4], Spinor["Helicity", "Square", 5]])/
   (PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[3, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0]) -
  (HEPCATNativeIdentityFixture`coupling^4*HEPCATNativeIdentityFixture`mass^2*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 1]]*
    SpinorChain[Spinor["Helicity", "Angle", 6], Spinor["Spin", "Angle", 3]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Spin", "Square", 4]]*
    SpinorChain[Spinor["Helicity", "Square", 5], Mom[4], Mom[1], Spinor["Spin", "Square", 2]])/
   (PropDen[Mom[Multiparticle[1, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[2, 6]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[4, 5]], HEPCATNativeIdentityFixture`mass]*
    PropDen[Mom[Multiparticle[1, 2, 6]], 0])]
