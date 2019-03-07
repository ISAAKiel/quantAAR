#' Seriation
#'
#' Seriation function wrappers that give the result in a long tidy data.frame.
#' The result table can be transformed to a wide format with \code{seriation2widedf}.
#'
#' @param ... Input arguments of \code{seriation::seriate}.
#'
#' @return A tibble with the seriation result in a long format.
#' Additional values are stored in object attributes. See \code{attributes(result)}.
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
#' seriation.seriation_seriate(matuskovo_material, method = "PCA")
#'
#' matuskovo_CountMatrix <- as(matuskovo_material, "CountMatrix")
#' seriation.tabula_seriate(matuskovo_CountMatrix, method = "correspondance")
#'
#' # transform back to wide format
#' spread_seriation(seriation.seriation_seriate(matuskovo_material, method = "PCA"))
#'
#' @rdname seriation
#'
#' @export
seriation.seriation_seriate <- function(...) {

  check_if_packages_are_available(c("seriation"))

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
  } else if (is.matrix(x)) {
    x_matrix <- x
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
#' @param x Data.frame. Output of the seriation wrapper functions.
#'
#' @export
spread_seriation <- function(x) {

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

#' @rdname seriation
#'
#' @export
seriation.tabula_seriate <- function(...) {

  check_if_packages_are_available(c("tabula"))

  if ("object" %in% names(list(...))) {
    object <- list(...)$object
  } else {
    object <- list(...)[[1]]
  }

  # run seriation
  seriation_result <- tabula::seriate(...)

  seriation_order_rows <- seriation_result@rows
  seriation_order_cols <- seriation_result@columns

  x_matrix_reordered <- object[seriation_order_rows, seriation_order_cols]
  x_tibble_reordered <- tibble::as_tibble(x_matrix_reordered)
  x_tibble_reordered$row <- rownames(object)[seriation_order_rows]
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

