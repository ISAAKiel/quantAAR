# Begin CA Utility Functions  ---------------------------

#' Iteratively removes all rows and columns of a matrix or dataframe with less than a given
#' number of non zero elements.
#'
#' @details A matrix or data.frame with numeric values often contains rows and columns with an
#' insufficient amount of values > 0 for a certain task. For example correspondence analysis
#' or bivariate correlation analysis requires a minimum amount of usable values.
#' In an archaeological context this could apply for example, if certain find categories
#' are particularly rare in a burial site context.
#'
#' \code{delrc} allows to remove rows and columns, that don't fulfill the requirements.
#'
#' @param x matrix or data.frame. Table with only numeric values.
#' @param minnumber integer. A number of minimum non zero elements. How many values > 0 have to be
#' present to consider a row/column sufficiently linked to perform further analysis?
#' Every column with less values > 0 will be removed. Either \code{minnumber} or \code{cmin} and
#' \code{rmin} must be given.
#' @param cmin integer. Same as \code{minnumber}, but specifically for columns.
#' @param rmin integer. Same as \code{minnumber}, but specifically for rows.
#'
#' @return A matrix or dataframe with all rows and columns removed that had less than
#' the given number of non zero elements. If \code{minnumber} or \code{cmin} and
#' \code{rmin} are too restrictive and no content remains in the table, than the result is
#' \code{NA}.
#'
#' @examples
#' testmatrix <- data.frame(
#' c1 = c(0,3,8,2),
#' c2 = c(0,6,7,0),
#' c3 = c(0,0,0,0),
#' c4 = c(0,3,8,2),
#' c5 = c(0,6,7,0),
#' c6 = c(1,0,0,1)
#' )
#'
#' # The following code removes every column with less than 3 values > 0.
#' # That will remove the columns c2, c3, c5 and c6.
#' # Further, every row with less than 2 values gets removed.
#' # That will delete row 1.
#' itremove(testmatrix, cmin = 3, rmin = 2)
#'
#'@export
itremove <- function(x, minnumber = NA, cmin = NA, rmin = NA) {

  # check input parameters
  if (!any(c("matrix", "data.frame") %in% class(x))) {
    stop("x is neither a matrix nor a data.frame")
  }
  if (is.na(minnumber) & (is.na(cmin) | is.na(rmin))) {
    stop("minnumber is missing")
  } else if (!is.na(minnumber) & (!is.na(cmin) | !is.na(rmin))) {
    stop("use either min or cmin + rmin")
  } else if (!is.na(minnumber) & (is.na(cmin) & is.na(rmin))) {
    cmin <- rmin <- minnumber
  }

  return(itremove_algorithm(x, cmin, rmin))

}

itremove_algorithm <- function(x, cmin, rmin) {
  if (!any(c("matrix", "data.frame") %in% class(x))) {
    return(NA)
  }
  # search for zero values
  enough_non_zero_elements_x <- apply(
    x,
    MARGIN = 1,
    FUN = check_num_nonzero,
    minnumber = rmin
  )
  enough_non_zero_elements_y <- apply(
    x,
    MARGIN = 2,
    FUN = check_num_nonzero,
    minnumber = cmin
  )
  # check if the function ends or if it has to be called again (recursion)
  if (any(!c(enough_non_zero_elements_x, enough_non_zero_elements_y))) {
    return(
      itremove_algorithm(
        x[enough_non_zero_elements_x, enough_non_zero_elements_y],
        cmin, rmin
      )
    )
  } else {
    return(if (length(x) > 0) x else NA)
  }
}
check_num_nonzero <- function(x, minnumber) (sum(x == 0) + (minnumber - 1)) < length(x)

#' Mask function for ca:ca() to get the ca results in a tidy data.frame
#'
#' \code{camask()} delivers the coordinates of objects and variables of a correspondence
#' analyse as a tidy data.frame. Further it allows to directly call the function
#' \code{quantAAR::caplot()} to get three pages of plots for a first impression.
#' The CA is calculated by \code{ca::ca()}. See \code{?ca} for further information.
#'
#' @details
#' Structure of the resulting table:
#'
#' column 1 - X:    ca coordinates of objects/variables for different dimensions
#'
#' column X+1:      type (var, obj, passivevar, passiveobj)
#'
#' @param x data.frame with numeric values
#' @param supc numeric vector of indexes of variables (columns) that should be included as
#' passive entities into the ca
#'
#' default = NULL
#'
#' @param supr numeric vector of indexes of objects (rows) that should be included as
#' passive entities into the ca
#'
#' default = NULL
#'
#' @param caplot boolean switch, to decide, whether caplot() should be called to get
#' some early plots
#'
#' default = FALSE
#'
#' @param verbose boolean switch, to decide, whether the output should be a list with all
#' the diagnostic output of \code{ca::ca()} (TRUE) or just the result coordinate data.frame
#' (FALSE)
#'
#' default = FALSE
#'
#' @return data.frame with ca coordinates of variables and objects of the input data.frame
#'
#'@examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#' rownames(testmatrixrand) <- paste("row", seq(1:nrow(testmatrixrand)))
#'
#' camask(testmatrixrand, supc = c(1,2,3), supr = c(15,16))
#'
#' @export
camask <- function(x, supc = NULL, supr = NULL, caplot = FALSE, verbose = FALSE) {

  # call ca::ca() to perform CA
  # switch ist necessary to deal with presence or absence of passive variables:
  # ca() can't simply work with empty vectors
  if (!is.null(supr) && !is.null(supc)) {
    q <- ca::ca(x, suprow = supr, supcol = supc)
  } else if (!is.null(supr) && is.null(supc)) {
    q <- ca::ca(x, suprow = supr)
  } else if (is.null(supr) && !is.null(supc)) {
    q <- ca::ca(x, supcol = supc)
  } else {
    q <- ca::ca(x)
  }

  # call caplot()
  if (caplot) {
    quantAAR::caplot(q)
  }

  # create a dataframe with ca coordinates and a type column to distinguish
  # variables and objects
  res <- data.frame(
    rbind(q$colcoord, q$rowcoord),
    type = c(
      rep("var", nrow(q$colcoord)),
      rep("obj", nrow(q$rowcoord))
    ),
    row.names = c(
      colnames(x),
      rownames(x)
    ),
    stringsAsFactors = FALSE
  )

  # set type for passive variables/objects
  if (!is.null(supr) && !is.null(supc)) {
    res$type[supc] <- "passivevar"
    res$type[nrow(q$colcoord) + supr] <- "passiveobj"
  } else if (!is.null(supr) && is.null(supc)) {
    res$type[nrow(q$colcoord) + supr] <- "passiveobj"
  } else if (is.null(supr) && !is.null(supc)) {
    res$type[supc] <- "passivevar"
  }

  # prepare verbose output
  if (verbose) {
    verbose_out <- list(
      result = res,
      principal_inertias = data.frame(
        singular_value = q$sv,
        summary(q)[["scree"]] %>% `colnames<-`(paste0("summary_",
          c("dim_number", "eigenvalue", "percent", "cum_percent")
        ))
      ),
      row_diag = data.frame(
        rowname = if(is.null(q$rownames)) { NA } else { q$rownames },
        rowmass = q$rowmass,
        rowdist = q$rowdist,
        rowinertia = q$rowinertia,
        summary(q)[["rows"]] %>% `colnames<-`(paste0("summary_",
          c("name", "mass", "qlt", "inr", "k_1", "cor_1", "ctr_1", "k_2", "cor_2", "ctr_2")
        )),
        stringsAsFactors = FALSE
      ),
      col_diag = data.frame(
        colname = if(is.null(q$colnames)) { NA } else { q$colnames },
        colmass = q$colmass,
        coldist = q$coldist,
        colinertia = q$colinertia,
        summary(q)[["columns"]] %>% `colnames<-`(paste0("summary_",
          c("name", "mass", "qlt", "inr", "k_1", "cor_1", "ctr_1", "k_2", "cor_2", "ctr_2")
        )),
        stringsAsFactors = FALSE
      )
    )
  }

  # output
  if (verbose) {
    return(verbose_out)
  } else {
    return(res)
  }
}

# End CA Utility Functions  ---------------------------
