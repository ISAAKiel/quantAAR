# General roxygen tags
#' @useDynLib quantaar
#' @importFrom Rcpp sourceCpp

#' @export
.onUnload <- function (libpath) {
  library.dynam.unload("quantaar", libpath)
}