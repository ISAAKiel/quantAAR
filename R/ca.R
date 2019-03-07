#' Correspondence Analysis
#'
#' Correspondence Analysis function wrappers that give the result in a tidy data.frame.
#'
#' @param ... Input arguments of the relevant wrapped functions.
#'
#' @return A tibble with the ca results for variables (columns) and objects (rows).
#' Additional values are stored in object attributes. See \code{attributes(result)}.
#'
#' name: Character. Names of rows and columns.
#'
#' type: Character. Type of entry ("row" or "col").
#'
#' ...: Additional variables as provided by the wrapped functions.
#'
#' x1...xX: Numeric. Standard coordinates of this entry in all available dimensions.
#'
#' @examples
#' ca.ca_ca(matuskovo_material)
#' ca.vegan_cca(matuskovo_material)
#'
#' @rdname ca
#'
#' @export
ca.ca_ca <- function(...) {

  check_if_packages_are_available("ca")

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

  # rename dimensions
  colnames(res) <- gsub("Dim", "x", colnames(res))

  # store dimension weights
  attr(res, "sv") <- q$sv
  attr(res, "simplified_dimension_weights") <- round(100 * (q$sv^2)/sum(q$sv^2), 2)

  return(res)
}

#' @param x Output of \code{ca.ca_ca}.
#' @param dim Index of dimension.
#'
#' @rdname ca
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

#' @rdname ca
#'
#' @export
ca.vegan_cca <- function(...) {

  check_if_packages_are_available("vegan")

  # call ca::ca() to perform CA
  q <- vegan::cca(...)

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
        name = names(q$rowsum),
        type = "row",
        sum = q$rowsum
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

    # rename dimensions
    colnames(res) <- gsub("CA", "x", colnames(res))

    # attributes
    attr(res, "method") <- eoi
    attr(res, "eig") <- q$CA$eig
    attr(res, "poseig") <- q$CA$poseig
    attr(res, "rank") <- q$CA$rank
    attr(res, "Xbar") <- q$CA$Xbar

  } else {
    stop("CCA and pCCA are not implemented yet.")
  }

  # general attributes
  attr(res, "inertia") <- q$inertia
  attr(res, "grand.total") <- q$grand.total
  attr(res, "tot.chi") <- q$tot.chi
  attr(res, "Ybar") <- q$Ybar

  return(res)
}
