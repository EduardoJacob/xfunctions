#' Show Object Detailed Information
#'
#' @param v An Object
#'
#' @return
#' @export
#'
#' @examples
XShowObject = function(v) {
  cat("Object     :","\n")
  print(v)
  cat("Length     :",length(v),"\n")
  cat("Typeof     :",typeof(v),"\n")
  cat("Class      :",class(v),"\n")
  cat("Mode       :",mode(v),"\n")
  cat("Dimensions :","\n")      # Best used with Matrices or Arrays
  print(dim(v))
  cat("Levels     :","\n")   # The Levels of a Factor
  print(levels(v))
  cat("Attributes :","\n")
  print(attributes(v))
  cat("is.vector  :",is.vector(v),"\n")
  cat("is.atomic  :",is.atomic(v),"\n")
  cat("is.list    :",is.list(v),"\n")
  cat("Structure  :")
  str(v)
  cat("Names      :","\n")
  print(names(v))
  cat("Summary    :","\n")
  summary(v)
}

# v = c(1,2,3)
# XShowObject(v)
