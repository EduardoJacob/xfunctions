
#' @export

# Calculate the Resulting Force applyed at 1st Point caused by the other Points
# using Coulombs Law: F = ke Q1 Q2 /R^2

XElectricForce = function(...) {
  if (nargs() == 1) {
    ListOfCharges = (...)
  } else {
    ListOfCharges = list(...)
  }
    
  Force = c(0,0,0)
  # Coulombs Constant:
  ke = 9e9
  #ke = 1
  
  P = ListOfCharges[[1]]
  P1 = P[1:3]
  q1 = P[4]
  
  for (P in ListOfCharges[-1]) {
    P2 = P[1:3]
    q2 = P[4]
    F2 = q2 / norm2(P1-P2)
    Force = Force + F2*unit(P1-P2)
  }
  Force = ke * q1 * Force
  print(Force)
  print(norm(Force))
  ux = c(1,0,0)
  print(angle(Force,ux))
  print(angled(Force,ux))
}




