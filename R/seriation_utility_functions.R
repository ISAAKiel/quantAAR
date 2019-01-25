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
  seriate_args = list(method = "PCA")
) {

  x_matrix <- as.matrix(x)
  # dist_args$x <- x_matrix
  # seriate_args$x <- do.call(stats::dist, dist_args)
  seriate_args$x <- x_matrix

  seriation_result <- do.call(seriation::seriate, seriate_args)
  seriation_order_rows <- seriation::get_order(seriation_result, dim = 1)
  seriation_order_cols <- seriation::get_order(seriation_result, dim = 2)

  x_matrix_reordered <- x_matrix[seriation_order_rows, seriation_order_cols]
  x_tibble_reordered <- tibble::as_tibble(x_matrix_reordered)

  return(x_tibble_reordered)

}