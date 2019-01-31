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

  seriation_result <- seriation::seriate(
    x_matrix,
    method = method,
    control = control,
    ...
  )
  seriation_order_rows <- seriation::get_order(seriation_result, dim = 1)
  seriation_order_cols <- seriation::get_order(seriation_result, dim = 2)

  x_matrix_reordered <- x_matrix[seriation_order_rows, seriation_order_cols]
  x_tibble_reordered <- tibble::as_tibble(x_matrix_reordered)
  x_tibble_reordered$row <- rownames(x)[seriation_order_rows]
  x_tibble_reordered$row_order <- 1:nrow(x_tibble_reordered)

  x_gathered <- tidyr::gather(
    x_tibble_reordered,
    key = "col",
    value = "value",
    -"row", -"row_order"
  )

  x_gathered$col_order <- rep(1:length(seriation_order_cols), each = length(seriation_order_rows))

  x_res <- dplyr::select(
    x_gathered,
    "row",
    "row_order",
    "col",
    "col_order",
    "value"
  )

  # set factor levels
  x_res$row <- forcats::fct_inorder(x_res$row)
  x_res$col <- forcats::fct_inorder(x_res$col)

  # store method
  attr(x_res, "method") <- method

  return(x_res)

}

#' seriation2widedf
#'
#' @param x test
#'
#' @export
seriation2widedf <- function(x) {

  x_simple <- dplyr::select(
    x,
    -"row_order",
    -"col_order"
  )

  x_spread <- tidyr::spread(
    x_simple,
    key = "col",
    value = "value"
  )

  x_res <- tibble::column_to_rownames(
    x_spread,
    "row"
  )

 return(x_res)

}
