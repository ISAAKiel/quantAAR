#' tidyseriation
#'
#' @param x test
#' @param dist_args test
#' @param seriate_args test
#'
#' @export
tidyseriation <- function(
  x,
  dist_args = list(method = "euclidian"),
  seriate_args = list(method = "Spectral")
) {

  if (is.matrix(x) | is.dist(x)) {
    seriate_args$x <- x
  } else {
    dist_args$x <- as.matrix(x)
    seriate_args$x <- do.call(stats::dist, dist_args)
  }

  seriation_result <- do.call(seriation::seriate, seriate_args)
  seriation_order <- seriation::get_order(seriation_result)

  x_matrix_reordered <- x_matrix[seriation_order,]
  x_tibble_reordered <- tibble::as_tibble(x_matrix_reordered)

  return(x_tibble_reordered)

}