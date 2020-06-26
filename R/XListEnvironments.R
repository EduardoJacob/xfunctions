#' List chain of Environments, starting on a given Environment
#'
#' @param env Receives an environment, defaults for globalenv()
#'
#' @return
#' @export
#'
#' @examples
XListEnvironments = function(env = globalenv()) {
  repeat {
    name <- environmentName(env)
    if (nchar(name) != 0)
      name <- paste0(name, "\n")
    else
      name <- str(env, give.attr = FALSE)
    cat(name)
    env <- parent.env(env)
    if (identical(env, emptyenv())) break
  }
  
  invisible()
} # XListEnvironments

