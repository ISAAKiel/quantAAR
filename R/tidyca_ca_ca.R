#' Mask function for ca:ca() to get the ca results in a tidy data.frame
#'
#' \code{tidyca()} delivers the coordinates of objects and variables of a correspondence
#' analyse as a tidy data.frame. The CA is calculated by \code{ca::ca()}.
#' See \code{?ca} for further information.
#'
#' @param ... Input arguments of \code{ca::ca}.
#'
#' @return A tibble with the ca results for variables (columns) and objects (rows).
#' The singular values and simplified dimension weights are stored as attributes to
#' the tibble and can be accessed via \code{attr(res, "singluar_values")} and
#' \code{attr(res, "simplified_dimension_weights")}.
#'
#' name: Character. Names of rows and columns.
#'
#' type: Character. Type of entry ("row" or "col").
#'
#' sup: Boolean. Was this entry treated as a supplementary point?
#'
#' mass: Mumeric. Row and column masses.
#'
#' dist: Mumeric. Row and column chi-square distances to centroid.
#'
#' mass: Mumeric. Row and column inertias.
#'
#' Dim1...DimX: Mumeric. Standard coordinates of this entry in all available dimensions.
#'
#' @examples
#' haireye <- margin.table(datasets::HairEyeColor, 1:2)
#' quantAAR::tidyca(haireye)
#'
#' @rdname tidyca
#'
#' @export
tidyca <- function(...) {

  # call ca::ca() to perform CA
  q <- ca::ca(...)

  # prepare tidy output
  row_res <- dplyr::bind_cols(
    tibble::tibble(
      name = q$rownames,
      type = "row",
      sup = 1:length(q$rownames) %in% q$rowsup,
      mass = q$rowmass,
      dist = q$rowdist,
      inertia = q$rowinertia
    ),
    tibble::as_tibble(q$rowcoord)
  )

  col_res <- dplyr::bind_cols(
    tibble::tibble(
      name = q$colnames,
      type = "col",
      sup = 1:length(q$colnames) %in% q$colsup,
      mass = q$colmass,
      dist = q$coldist,
      inertia = q$colinertia
    ),
    tibble::as_tibble(q$colcoord)
  )

  res <- dplyr::bind_rows(
    row_res,
    col_res
  )

  # store dimension weights
  attr(res, "singluar_values") <- q$sv
  attr(res, "simplified_dimension_weights") <- round(100 * (q$sv^2)/sum(q$sv^2), 2)

  return(res)
}

#' @param x Output of \code{tidyca}.
#' @param dim Index of dimension.
#'
#' @rdname tidyca
#'
#' @export
get_dimension_label <- function(x, dim) {
  return(
    paste0(
      "Dimension ", dim,
      " (",
      attr(x, "simplified_dimension_weights")[dim],
      "%)"
    )
  )
}
