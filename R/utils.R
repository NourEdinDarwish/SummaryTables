# optTrue -------------------------------------------------------------------

#' Safely check if a boolean option is TRUE
#'
#' jmvcore's Options `$` accessor throws when the option does not exist.
#' This helper captures the expression unevaluated via NSE, splits it into
#' the object and name, then checks existence with `has()` before reading
#' the value with `get()`.
#'
#' @param expr An unquoted expression like `options$boldQ`
#' @return TRUE if the option exists and is TRUE, FALSE otherwise
#' @examples
#' # Instead of:
#' #   options$has("boldQ") && isTRUE(options$get("boldQ"))
#' # Write:
#' #   optTrue(options$boldQ)
optTrue <- function(expr) {
  expr <- substitute(expr)
  obj  <- eval(expr[[2]], parent.frame())
  name <- as.character(expr[[3]])
  obj$has(name) && isTRUE(obj$get(name))
}
