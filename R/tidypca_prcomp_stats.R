#' Mask function for stats::prcomp() to get the PCA results in a tidy data.frame
#'
#' \code{tidypca()} delivers the coordinates of objects and variables of a principal components
#' analyse as a tidy data.frame. The PCA is calculated by \code{stats::prcomp()}.
#' See \code{?stats::prcomp} for further information.
#'
#' @param ... Input arguments of \code{stats::prcomp}.
#'
#' @return A tibble with the PCA results for variables (columns) and objects (rows).
#' The standard deviations of the principal components and the centering and scaling used
#' are stored as attributes to the tibble and can be accessed via
#' \code{attr(res, "standard_deviations")},
#' \code{attr(res, "center")} and \code{attr(res, "scale")}.
#'
#' name: Character. Names of rows and columns.
#'
#' type: Character. Type of entry ("row" or "col").
#'
#' PC1...PCX: Numeric. Resulting coordinates in all principal component dimensions.
#'
#' @examples
#' quantAAR::tidypca(datasets::USArrests)
#'
#' @rdname tidypca
#'
#' @export
tidypca <- function(...) {

  # call stats::prcomp() to perform PCA
  q <- stats::prcomp(...)

  # prepare tidy output
  row_res <- dplyr::bind_cols(
    tibble::tibble(
      name = rownames(q$x),
      type = "row"
    ),
    tibble::as_tibble(q$x)
  )

  col_res <- dplyr::bind_cols(
    tibble::tibble(
      name = rownames(q$rotation),
      type = "col"
    ),
    tibble::as_tibble(q$rotation)
  )

  res <- dplyr::bind_rows(
    row_res,
    col_res
  )

  # store dimension weights
  attr(res, "standard_deviations") <- q$sdev
  attr(res, "center") <- q$center
  attr(res, "scale") <- q$scale

  return(res)

}
