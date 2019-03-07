#' Principal Components Analysis
#'
#' Transforms the output coordinates of objects and variables of a principal components
#' analysis to a tidy data.frame.
#'
#' @param ... Input arguments of the relevant wrapped functions.
#'
#' @return A tibble with the PCA results for variables (columns) and objects (rows).
#' Additional values are stored in object attributes.
#'
#' name: Character. Names of rows and columns.
#'
#' type: Character. Type of entry ("row" or "col").
#'
#' ...: Additional variables as provided by the wrapped functions.
#'
#' x1...xX: Numeric. Resulting coordinates in all principal component dimensions.
#'
#' @examples
#' quantAAR::pca.stats_prcomp(datasets::USArrests)
#'
#' @rdname pca
#'
#' @export
pca.stats_prcomp <- function(...) {

  check_if_packages_are_available("stats")

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

  # rename dimensions
  colnames(res) <- gsub("PC", "x", colnames(res))

  # store dimension weights
  attr(res, "standard_deviations") <- q$sdev
  attr(res, "center") <- q$center
  attr(res, "scale") <- q$scale

  return(res)

}
