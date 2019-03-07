#' Principal Components Analysis
#'
#' Principal Components Analysis function wrappers that give the result in a tidy data.frame.
#'
#' @param ... Input arguments of the relevant wrapped functions.
#' @param raw_output Logical. Should the raw output of the wrapped functions be stored as
#' an additional output attribute "raw"? Default: TRUE.
#'
#' @return A tibble with the PCA results for variables (columns) and objects (rows).
#' Additional values are stored in object attributes. See \code{attributes(result)$raw}.
#'
#' name: Character. Names of rows and columns.
#'
#' type: Character. Type of entry ("row" or "col").
#'
#' ...: Additional variables as provided by the wrapped functions.
#'
#' PC1...PCX: Numeric. Resulting coordinates.
#'
#' @examples
#' # establish an other distance measure in the data that allows application of PCA
#' matuskovo_distance <- vegan::decostand(matuskovo_material, "norm")
#'
#' pca.stats_prcomp(matuskovo_distance)
#' pca.vegan_rda(matuskovo_distance)
#'
#' @name pca
#' @rdname pca
NULL

#' @rdname pca
#'
#' @export
pca.stats_prcomp <- function(..., raw_output = TRUE) {

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

  # raw output
  if (raw_output) {
    attr(res, "raw") <- q
  }

  return(res)

}

#' @rdname pca
#'
#' @export
pca.vegan_rda <- function(..., raw_output = TRUE) {

  check_if_packages_are_available("vegan")

  # call ca::ca() to perform CA
  q <- vegan::rda(...)

  # CA
  if (is.null(q$CCA) & is.null(q$pCCA)) {
    eoi <- "CA"
  } else if (!is.null(q$CCA) & is.null(q$pCCA)) {
    eoi <- "CCA"
  } else {
    eoi <- "pCCA"
  }

  if (eoi == "CA") {

    # prepare tidy output
    row_res <- dplyr::bind_cols(
      tibble::tibble(
        name = rownames(q$CA$u),
        type = "row",
        sum = NA
      ),
      tibble::as_tibble(q$CA$u)
    )

    col_res <- dplyr::bind_cols(
      tibble::tibble(
        name = names(q$colsum),
        type = "col",
        sum = q$colsum
      ),
      tibble::as_tibble(q$CA$v)
    )

    res <- dplyr::bind_rows(
      row_res,
      col_res
    )

  } else {
    stop("CCA and pCCA are not implemented yet.")
  }

  # raw output
  if (raw_output) {
    attr(res, "raw") <- q
  }

  return(res)
}
