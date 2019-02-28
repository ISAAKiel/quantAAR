tidypca <- function(...) {

  # call stats::prcomp() to perform PCA
  q <- stats::prcomp(...)
  q <- stats::prcomp(USArrests)

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
