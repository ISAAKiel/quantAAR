#' tidyseriation
#'
#' @param x test
#' @param dist_method test
#' @param seriate_method test
#'
#' @export
tidyseriation <- function(x, dist_method = "euclidian", seriate_method = "Spectral") {

  x_matrix <- as.matrix(x)
  x_dist <- stats::dist(x_matrix, dist_method)
  seriation_result <- seriation::seriate(x_dist, seriate_method)
  seriation_order <- seriation::get_order(seriation_result)
  x_matrix_reordered <- x_matrix[seriation_order,]
  x_tibble_reordered <- tibble::as_tibble(x_matrix_reordered)

  return(x_tibble_reordered)

}