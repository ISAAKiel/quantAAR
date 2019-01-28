#' tidyseriation
#'
#' @param x test
#' @param method test
#' @param control test
#' @param ... test
#'
#' @export
tidyseriation <- function(
  x,
  method = "PCA",
  control = NULL,
  ...
) {

  x_matrix <- as.matrix(x)

  seriation_result <- seriation::seriate(x_matrix, method, control, ...)
  seriation_order_rows <- seriation::get_order(seriation_result, dim = 1)
  seriation_order_cols <- seriation::get_order(seriation_result, dim = 2)

  x_matrix_reordered <- x_matrix[seriation_order_rows, seriation_order_cols]
  x_df_reordered <- as.data.frame(
    x_matrix_reordered,
    row.names = rownames(x)[seriation_order_rows]
  )

  return(x_df_reordered)

}
