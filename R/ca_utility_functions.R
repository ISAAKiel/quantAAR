# Begin CA Utility Functions  ---------------------------

#' Checks if the number of non zero elements in a vector is at least equal to a given parameter.
#'
#' @param x The vector to be tested.
#' @param minnumber A number of minimum non zero elements.
#' @return The \code{true} if \code{x} has at least \code{minnumber}  non zero elements,
#' else \code{false}.
#'
#' @examples
#' check_num_nonzero(c(0,0,0,0,0), 1)
#' check_num_nonzero(c(0,0,0,0,1), 1)
#' check_num_nonzero(c(2:4,0,1), 1)
#'
#' @export
check_num_nonzero <- function(x, minnumber){
  if (sum(x == 0) + (minnumber - 1) < length(x)) T else F
}

#' Removes iterative all rows and columns of a matrix or dataframe with less than a given number of non zero elements.
#'
#' @param object A matrix or dataframe.
#' @param minnumber A number of minimum non zero elements.
#'
#' @return A matrix or dataframe with all rows and columns removed that had less than the given number of non zero elements.
#'
#' @examples
#' a <- matrix(c(0,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0),nrow=4)
#' a
#' itremove(a,1)
#'
#'@export
itremove <- function(object, minnumber) {

  enough_non_zero_elements_x <- apply(object,
                                      MARGIN = 1,
                                      FUN = check_num_nonzero,
                                      minnumber = minnumber)
  enough_non_zero_elements_y <- apply(object,
                                      MARGIN = 2,
                                      FUN = check_num_nonzero,
                                      minnumber = minnumber)

  if (any(c(enough_non_zero_elements_x, enough_non_zero_elements_y) == F)) {
    return(itremove(object[enough_non_zero_elements_x,
                           enough_non_zero_elements_y],
                    minnumber))
  } else {
    return(object)
  }
}

#' Delete "empty" rows and columns of a data.frame
#'
#' A row or a column of a data.frame with numeric values is considered empty, if all
#' its values are 0. \code{delempty} deletes all empty rows and columns of an input
#' data.frame.
#'
#' @param matrix data.frame with numeric values
#' @return data.frame without empty rows and columns (matrix[-z,-n])
#'
#' @examples
#' testmatrix <- data.frame(c1 = c(0,3,8,2), c2 = c(0,6,7,8), c3 = c(0,0,0,0))
#'
#' delempty(testmatrix)
#'
#' withoutempty <- delempty(matrix = testmatrix)
#'
#' @importFrom magrittr "%>%"
#'
#' @export
delempty <- function(matrix) {
  delrc(matrix, climit = 1, rlimit = 1) %>%
    return
}

#' Delete rows and columns of a data.frame by amount of values > 0
#'
#' A data.frame with numeric values often contains rows and columns with an insufficient
#' amount of values > 0 for a certain task. For example correspondence analysis or
#' bivariate correlation analysis requires a minimum amount of usable values.
#' In an archaeological context this could apply for example, if certain find categories
#' are particularly rare in a burial site context.
#' \code{delrc} allows to remove rows and columns, that don't fulfill the requirements.
#'
#' @param matrix data.frame with numeric values
#' @param climit numeric value: How many values > 0 have to be present to consider a
#' column sufficiently linked to perform further analysis? Every column with less values > 0
#' will be removed.
#'
#' default = 0 (no column will be deleted)
#'
#' @param rlimit numeric value: How many values > 0 have to be present to consider a
#' row sufficiently linked to perform further analysis? Every row with less values > 0
#' will be removed.
#'
#' default = 0 (no row will be deleted)
#'
#' @return data.frame without removed rows/colums.
#'
#' If no values or just one row are left in the data.frame after application of the selection
#' criteria, the output is FALSE.
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
#' delrc(testmatrix, climit = 3, rlimit = 2)
#'
#' testresult <- delrc(testmatrix, 3, 2)
#' rowSums(booleanize(testresult))
#' colSums(booleanize(testresult))
#'
#' @export
delrc <- function(matrix, climit = 0, rlimit = 0) {

  # reduce too big input to the extends of the matrix to cover the possible maximum
  if (climit > ncol(matrix)) {
    climit <- ncol(matrix) + 1
  }
  if (rlimit > nrow(matrix)) {
    rlimit <- nrow(matrix) + 1
  }

  # nested function: search empty columns and save index in a vector
  cdel <- function(matrix, climit) {
    n <- c()
    p <- 1
    if (!(ncol(matrix) == 0 | nrow(matrix) == 0) && !is.vector(matrix)) {
      for (s in 1:ncol(matrix)) {
        if (length(which(matrix[, s] > 0)) < climit) {
          n[p] <- s
          p <- p + 1
        }
      }
    }
    return(n)
  }

  # nested function: search empty rows and save index in a vector
  rdel <- function(matrix, rlimit) {
    z <- c()
    i <- 1
    u <- 1
    if (!(ncol(matrix) == 0 | nrow(matrix) == 0) &&
        !is.vector(matrix)) {
      for (i in 1:nrow(matrix)) {
        if (length(which(matrix[i, ] > 0)) < rlimit) {
          z[u] <- i
          u <- u + 1
        }
      }
    }
    return(z)
  }

  # loop to go through the matrix again and again until
  # every relevant row/column is removed. That's necessary,
  # because the removal of e.g. one column could cause the need
  # to remove an other row previously still relevant
  while (
    nrow(matrix) > 0 && ncol(matrix) > 0 &&
    (!is.null(cdel(matrix, climit)) || !is.null(rdel(matrix, rlimit)))
  ) {

    # call search function
    n <- cdel(matrix, climit)

    # delete columns
    if (!is.null(n) && ncol(matrix) >= max(n))  {
      matrix <- matrix[, -n]
    }

    # call search function
    z <- rdel(matrix, rlimit)

    # delete rows
    if (!is.null(z) && nrow(matrix) >= max(z)) {
      matrix <- matrix[-z, ]
    }
  }

  # nice and clean output: if no content is left, the function returns FALSE
  if (is.vector(matrix)) {
    return(FALSE)
  } else if (nrow(matrix) == 0 || ncol(matrix) == 0) {
    return(FALSE)
  } else {
    return(matrix)
  }

}

#' Mask function for ca:ca() to get the ca results in a tidy data.frame
#'
#' \code{camask()} delivers the coordinates of objects and variables of a correspondence
#' analyse as a tidy data.frame. Further it allows to directly call the function
#' \code{quantaar::caplot()} to get three pages of plots for a first impression.
#' The CA is calculated by \code{ca::ca()}. See \code{?ca} for further information.
#'
#' @details
#' Structure of the resulting table:
#'
#' column 1 - X:    ca coordinates of objects/variables for different dimensions
#'
#' column X+1:      type (var, obj, passivevar, passiveobj)
#'
#' @param matrix data.frame with numeric values
#' @param supc vector of indezes of variables (columns) that should be included as
#' passive entities into the ca
#'
#' default = c()
#'
#' @param supr vector of indezes of objects (rows) that should be included as
#' passive entities into the ca
#'
#' default = c()
#'
#' @param caplot boolean switch, to decide, whether caplot() should be called to get
#' some early plots
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
camask <- function(matrix, supc = c(), supr = c(), caplot = FALSE) {

  # call ca::ca() to perform CA
  # switch ist necessary to deal with presence or absence of passive variables:
  # ca() can't simply work with empty vectors
  if (!is.null(supr) && !is.null(supc)) {
    q <- ca::ca(matrix, suprow = supr, supcol = supc)
  } else if (!is.null(supr) && is.null(supc)) {
    q <- ca::ca(matrix, suprow = supr)
  } else if (is.null(supr) && !is.null(supc)) {
    q <- ca::ca(matrix, supcol = supc)
  } else {
    q <- ca::ca(matrix)
  }

  # create a dataframe with ca coordinates and a type column to distinguish
  # variables and objects
  df <- data.frame(
    rbind(q$colcoord, q$rowcoord),
    type = c(
      rep("var", nrow(q$colcoord)),
      rep("obj", nrow(q$rowcoord))
    ),
    row.names = c(
      colnames(matrix),
      rownames(matrix)
    ),
    stringsAsFactors = FALSE
  )

  # set type for passive variables/objects
  if (!is.null(supr) && !is.null(supc)) {
    df$type[supc] <- "passivevar"
    df$type[nrow(q$colcoord) + supr] <- "passiveobj"
  } else if (!is.null(supr) && is.null(supc)) {
    df$type[nrow(q$colcoord) + supr] <- "passiveobj"
  } else if (is.null(supr) && !is.null(supc)) {
    df$type[supc] <- "passivevar"
  }

  # call caplot()
  if (caplot) {
    quantaar::caplot(q)
  }

  return(df)
}

# End CA Utility Functions  ---------------------------
