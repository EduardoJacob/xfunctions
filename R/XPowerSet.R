
#' @export

XPowerSet = function(v) {
  # library("gtools")
  
  n = length(v)
  L = vector("list",2^n)
  index = 1
  L[[index]] = ""
  
  for (r in 1:n) {
    x = gtools::combinations(n,r,v)
    for (i in 1:nrow(x)) {
      index = index + 1
      L[[index]] = x[i,]
    }
  }
  L
}

# x = XPowerSet(letters[1:6])
# for (combination in x) print(combination)
# for (i in 1:length(x)) cat(i,":",x[[i]],"\n")





