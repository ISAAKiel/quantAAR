#' Principal Curves
#'
#' Principal Curve fitting function wrappers that give the result in a tidy data.frame.
#'
#' @param ... Input arguments of the relevant wrapped functions.
#' @param raw_output Logical. Should the raw output of the wrapped functions be stored as
#' an additional output attribute "raw"? Default: TRUE.
#'
#' @return A tibble with the modified input point coordinates after the projection onto the curve.
#' Additional values are stored in object attributes. See \code{attributes(result)$raw}.
#'
#' ...: Additional variables as provided by the wrapped functions.
#'
#' X1...XX: Numeric. Resulting coordinates.
#'
#' @examples
#' principal_curve.princurve_principal_curve(
#'   as.matrix(ca.ca_ca(matuskovo_material)[, c("CA1", "CA2", "CA3")])
#' )
#'
#' @name principal_curve
#' @rdname principal_curve
NULL

#' @rdname principal_curve
#'
#' @export
principal_curve.princurve_principal_curve <- function(..., raw_output = TRUE) {

  check_if_packages_are_available("princurve")

  # call function
  q <- princurve::principal_curve(...)

  # prepare tidy output
  colnames(q$s) <- paste0("X", 1:ncol(q$s))
  res <- dplyr::bind_cols(
    tibble::tibble(
      ord = q$ord,
      lambda = q$lambda
    ),
    tibble::as_tibble(q$s)
  )

  # raw output
  if (raw_output) {
    attr(res, "raw") <- q
  }

  return(res)
}
