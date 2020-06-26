#' Electric Force
#'
#' Calculate the Resulting Force applyed at 1st Point caused by the other Points
#' using Coulombs Law: F = ke Q1 Q2 /R^2
#'
#'
#' @param ... A List of Points (x,y,z,Q)
#'
#' @return
#' @export
#'
#' @examples
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
} # XElectricForce


norm   = function(a) sqrt(sum(a*a)) # The norm of a vector
norm2  = function(a) sum(a*a)       # The norm squared of a vector
unit   = function(a) a/norm(a)      # The unit vector
dot    = function(a,b) sum(a*b)     # the dot product of 2 vectors
angle  = function(a,b) acos(dot(a,b)/(norm(a)*norm(b)))   # The angle in radians between 2 vectors
angled = function(a,b) angle(a,b) * 180 / pi              # The angle in degrees between 2 vectors
cross  = function(a,b) c(a[2]*b[3]-a[3]*b[2],a[3]*b[1]-a[1]*b[3],a[1]*b[2]-a[2]*b[1]) # Cross product of 2 vectors
triple = function(a,b,c) dot(a,cross(b,c)) # Triple product between 3 vectors

# rotate a vector (x,y) by the angle a in degrees
rotate = function(x,y,a) {
  a = a * pi / 180
  c = cos(a)
  s = sin(a)
  c(x*c-s*y,x*s+y*c)
}




