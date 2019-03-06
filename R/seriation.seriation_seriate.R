#' Seriation
#'
#' Mask function for seriation::seriate() to get the seriation results in a tidy data.frame.
#' The seriation is calculated by \code{seriation::seriate()}.
#' The result table can be transformed back to a wide format with \code{seriation2widedf}.
#'
#' @param ... Input arguments of \code{seriation::seriate}.
#'
#' @return A tibble with the seriation result in a long format.
#'
#' row: Character. Names of rows.
#'
#' row_order: Integer. Order of rows.
#'
#' col: Character. Names of columns.
#'
#' col_order: Integer. Order of columns.
#'
#' value: Numeric. Value in input matrix for respective row and col.
#'
#' @examples
#' ## seriate matrix
#' data("iris")
#' x <- iris[-5]
#'
#' ## to make the variables comparable, we scale the data
#' x <- scale(x, center = FALSE)
#' x <- as.data.frame(x)
#'
#' ## try some methods
#' order <- quantAAR::seriation.seriation_seriate(x, method = "PCA")
#'
#' ## transform back to wide format
#' seriation2widedf(order)
#'
#' @rdname seriation
#'
#' @export
seriation.seriation_seriate <- function(...) {

  # modify input
  other_params <- list()
  if ("x" %in% names(list(...))) {
    x <- list(...)$x
    if (length(list(...)) > 1) {
      other_params <- list(...)[names(list(...)) != "x"]
    }
  } else {
    x <- list(...)[[1]]
    if (length(list(...)) > 1) {
      other_params <- list(...)[2:length(list(...))]
    }
  }
  if (length(other_params) == 0) {
    other_params$method <- "PCA"
  } else if (length(other_params) == 1 & is.null(other_params[[1]])) {
    other_params$method <- "PCA"
  }

  if (is.data.frame(x)) {
    x_matrix <- as.matrix(x)
  } else {
    stop("x is not a data.frame.")
  }

  # run seriation
  seriation_result <- do.call(
    what = seriation::seriate,
    args = list(
      x = x_matrix,
      unlist(other_params)
    )
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

  return(x_res)

}

#' @rdname seriation
#'
#' @param x Data.frame. Output of tidyseriation.
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
