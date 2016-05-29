# Begin General Data Munging Functions  ---------------------------

#' Count appearence of variables and objects in a data.frame with numeric values
#'
#' A row or a column of a data.frame with numeric values is considered empty, if all
#' its values are 0. If objects or variables are present, their rows and columns contain
#' values != 0. To count the appearence of objects or variables, \code{presencecount} counts
#' the amount of values != 0 within the rows or columns and writes them into a data.frame.
#' The result is sorted by the amount of appearences.
#'
#' @param matrix data.frame with numeric values
#' @param dim switch to define if the appearences in columns (variables) or rows (objects)
#' should be counted.
#'
#' 1: column (variables) appearences are counted
#'
#' 2: row (objects) appearences are counted
#'
#' @return sorted data.frame with amount of appearences for objacts or variables
#'
#' @examples
#' testmatrix <- data.frame(c1 = c(0,3,8,2), c2 = c(0,6,7,8), c3 = c(0,0,0,0))
#' rownames(testmatrix) <- c("r1","r2","r3","r4")
#'
#' countvar <- presencecount(testmatrix, 1)
#' countobj <- presencecount(testmatrix, 2)
#'
#' @export
#'

presencecount <- function(matrix, dim=1){

  # count variables
  if (dim == 1) {
    # prepare data.frame to store the results
    widthdataset <- length(matrix)
    presencecount <- data.frame(matrix[1, ], row.names = "count")
    # loop to determine the amount of appearences
    for (i in 1:widthdataset) {
      presencecount[1, i] <- length(which(matrix[, i] != "0"))
    }
    # sort results by amount of appearences
    presencecount <- presencecount[, order(presencecount[1, ])]
  }

  # count objects
  if (dim == 2) {
    # transpose matrix (then the code of dim=1 applies again)
    matrix <- data.frame(t(matrix))
    # prepare data.frame to store the results
    widthdataset <- length(matrix)
    presencecount <- data.frame(matrix[1, ], row.names = "count")
    # loop to determine the amount of appearences
    for (i in 1:widthdataset) {
      presencecount[1, i] <- length(which(matrix[, i] != "0"))
    }
    # sort results by amount of appearences
    presencecount <- presencecount[, order(presencecount[1, ])]
  }

  return(presencecount)
}

#' Reduce the numeric values of a data.frame to boolean values
#'
#' \code{booleanize} returns an other version of the input data.frame with
#' simple, definable present-absent information instead of numeric values.
#'
#' @param matrix data.frame with numeric values
#' @param present replacement values for cells with numeric value >0. default: TRUE
#' @param absent replacement values for cells with numeric value 0. default: FALSE
#' @return data.frame with present-absent values
#'
#' @examples
#' testmatrix <- data.frame(c1 = c(0,2,0,8), c2 = c(5,6,7,0), c3 = c(5,6,7,0))
#'
#' booleanize(testmatrix)
#' booleanize(testmatrix, 5, 10)
#'
#' cakedata <- booleanize(matrix = testmatrix, present = "cake", absent = "no cake")
#'
#' @export
#'

booleanize <- function(matrix, present=TRUE, absent=FALSE) {
  absent_elements <- which(matrix == 0, arr.ind = T)
  present_elements <- which(matrix > 0, arr.ind = T)
  matrix[absent_elements] <- absent
  matrix[present_elements] <- present

  return(matrix)

}

# End General Data Munging Functions  ---------------------------

# Begin Correlation Data Munging Functions  ---------------------------

#' Remove negative correlations from a correlation matrix
#'
#' \code{rmnegcorr} removes "negative correlations" from a correlation matrix.
#' A negative correlation is defined as a relation between to variables/objects,
#' that have a high correlation value due to NOT frequently appearing together.
#' This function can be applied for correlation matrizes created for variable or
#' object relations.
#'
#' @param corrmatrix correlation matrix (as produced by quantaar::corrmat())
#' @param matrix underlying data.frame
#' @param dim switch to define if the correlation matrix should be created
#' for columns or rows.
#'
#' 1: table is created for column (variables) relations.
#'
#' 2: table is created for row (objects) relations.
#'
#' @param niv decision niveau. The smaller niv the weaker the overlap of two
#' variables/objects can be, to still be recognised as the cause for a positiv relation.
#'
#' default: 0.1 -> 10\%
#'
#' @return correlation matrix without negative relations
#'
#' @examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#'
#' testmatrixrand[,1] <- c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)
#' testmatrixrand[,2] <- c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)
#'
#' testcorr <- corrmat(testmatrixrand, "chi2", chi2limit = 0.1, dim = 1)
#'
#' rmnegcorr(testcorr, testmatrixrand, dim = 1)
#'
#' @export
#'

rmnegcorr <- function (corrmatrix, matrix, dim, niv = 0.1) {

  # decision between application for or columns (variables) or rows (objects)
  if (dim == 2) {
    matrix <- t(matrix)
  }

  # loop to check every cell of the correlation matrix
  for (l in 1:length(corrmatrix[, 1])) {
    for (c in 1:length(corrmatrix[1, ])) {
      # calculation of a contingency table for the current corrmatrix cell
      corrtab <- table(matrix[, l], matrix[, c])
      # set ratio value to 100%
      prop1 <- 1
      prop2 <- 1
      # calculate ratio values for current cell, if there's a least one positiv
      # overlap of occurences of the underlying variables/objects
      if (corrtab[1, 2] != 0) {
        prop1 <- corrtab[2, 2] / sum(corrtab[, 2])
      }
      if (corrtab[2, 1] != 0) {
        prop2 <- corrtab[2, 2] / sum(corrtab[2, ])
      }
      # set correlation value to 0, if one or both ratio values is smaller than
      # a defined niveau.
      if (prop1 < niv) {
        corrmatrix[l, c] <- 0
      } else if (prop2 < niv) {
        corrmatrix[l, c] <- 0
      } else if (prop1 < niv && prop2 < niv) {
        corrmatrix[l, c] <- 0
      }
    }
  }

  return(corrmatrix)
}

#' Convert a correlation matrix to a table of relations
#'
#' \code{reltable} creates a sorted table of the relations of a correlation matrix.
#' Relations with a correlation value of zero and autocorrelations get removed.
#' \code{reltable} can consume a lot of time for big datasets!
#'
#' @details
#' Structure of the resulting table:
#'
#' column 1 + 2:    indezes of the variables/objects in the correlation matrix
#'
#' column 3:        correlation value
#'
#' column 4 + 5:    names of the variables/objects
#'
#' column 6:        optional second correlation value
#'
#' @param corrtable correlation matrix (as produced by quantaar::corrmat())
#' @param corrtable2 optional second corrmatrix correlation matrix (as produced by quantaar::corrmat())
#' @return table of relations and their correlation value
#'
#' @examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#'
#' testcorr <- corrmat(testmatrixrand, "lambda", dim = 1)
#' testcorr2 <- corrmat(testmatrixrand, "chi2", chi2limit = 0.1, dim = 1)
#'
#' reltable(testcorr)
#' reltable(testcorr, testcorr2)
#'
#' @export
#'

reltable <- function(corrtable, corrtable2 = data.frame()) {

  # copy matrix to apply an increasingly fast search algorithm
  # for the matrix maximum
  destroycorr <- corrtable

  # Setup an empty data.frame as basis for the relation table
  if (nrow(corrtable2) == 0) {
    a <- matrix(
      NA,
      nrow = length(corrtable[corrtable != 0]),
      ncol = 6
    )
    a <- data.frame(a)
    colnames(a) <- c(
      "indexvar1",
      "indexvar2",
      "corrvalue",
      "namevar1",
      "namevar2",
      "namehash"
    )
  } else {
    a <- matrix(
      NA,
      nrow = length(corrtable[corrtable != 0]),
      ncol = 7
    )
    a <- data.frame(a)
    colnames(a) <- c(
      "indexvar1",
      "indexvar2",
      "corrvalue",
      "namevar1",
      "namevar2",
      "namehash",
      "corrvalue2"
    )
  }

  # loop to fill relationship table (in order of decreasing correlation values)
  for (i in 1:length(a[, 1])) {
    if (max(destroycorr) != 0) {
      # search for current max value (highest correlation / best relation)
      a[i, 1:2] <- which(destroycorr == max(destroycorr), arr.ind = TRUE)[1, ]
      a[i, 3] <- destroycorr[a[i, 1], a[i, 2]]
      a[i, 4] <- colnames(destroycorr)[a[i, 1]]
      a[i, 5] <- colnames(destroycorr)[a[i, 2]]
      # create namehash to identify every relationship bijektivly
      suppressWarnings(
        namehashvector <- as.numeric(charToRaw(a[i, 4])) +
          as.numeric(charToRaw(a[i, 5]))
      )
      a[i, 6] <- paste(namehashvector, collapse = "")
      if (nrow(corrtable2) != 0) {
        a[i, 7] <- corrtable2[a[i, 1], a[i, 2]]
      }
      # set current relation to 0,
      # to find the next best relation in the next loop run
      destroycorr[a[i, 1], a[i, 2]] <- 0
    }
  }

  # remove autocorrelation
  b <- dplyr::filter(a, namevar1 != namevar2)

  # remove every relation, that is already present
  # inversely (var1 + var2 = var2 + var1)
  c <- b[which(duplicated(b[, 6])), ]

  row.names(c) <- 1:length(c[, 1])

  #remove namehash

  d <- c[, -6]

  return(d)
}

#' Create an empty correlation matrix of a given data.frame
#'
#' \code{newcorrtable} returns an empty correlation matrix of a data.frames columns
#' or rows. Empty means: filled with the numeric value "0".
#' This correlation matrix data.frame can be used to store the results of functions which
#' calculate correlation values for all bivariate relations.
#'
#' @param matrix data.frame
#' @param dim switch to define, whether the new correlation matrix should be created
#' for columns or rows.
#'
#' 1 (default): table is created for column (variables) relations.
#'
#' 2: table is created for row (objects) relations.
#'
#' @return empty correlation matrix data.frame
#'
#' @examples
#' testmatrix <- data.frame(c1 = c(5,2,3,8), c2 = c(5,6,7,0), c3 = c(5,6,7,9))
#'
#' # correlation table is created for the columns of the input data.frame testmatrix
#' newcorrtable(testmatrix)
#' newcorrtable(testmatrix, 1)
#' corrtabcolumns <- newcorrtable(matrix = testmatrix, dim = 1)
#'
#' # correlation table is created for the rows of the input data.frame testmatrix
#' newcorrtable(testmatrix, 2)
#' corrtabrows <- newcorrtable(matrix = testmatrix, dim = 2)
#'
#' @export
#'

newcorrtable <- function (matrix, dim = 1) {

  # table is created for column (variables) relations
  if (dim == 1) {
    matrixwidth <- length(matrix)
    newtable <- matrix(nrow = matrixwidth, ncol = matrixwidth, 0)
    colnames(newtable) <- colnames(matrix)
    rownames(newtable) <- colnames(matrix)
  }

  # table is created for row (objects) relations
  if (dim == 2) {
    matrixheight <- length(matrix[, 1])
    newtable <- matrix(nrow = matrixheight, ncol = matrixheight, 0)
    colnames(newtable) <- rownames(matrix)
    rownames(newtable) <- rownames(matrix)
  }

  newtable <- data.frame(newtable, check.names = FALSE)

  return(newtable)

}

#' Create a correlation matrix of an input data.frame
#'
#' \code{corrmat} returns a correlation matrix of a data.frame. Several different
#' correlation methods can be choosen and the matrix can be created for column or row
#' relations.
#'
#' The rmnegniv option allows to remove "negativ relations", by activating
#' the rmnegcorr function for values >0. The smaller rmnegniv the weaker the overlap
#' of two variables/objects can be, to still be recognised as the cause of a positiv
#' relation.
#'
#' See \code{?rmnegcorr} for further info. This function can also be applied later.
#'
#' @param matrix data.frame with numeric values
#' @param method switch to define which contingency value should be used:
#'
#' "chi2" (default): test decision of the chi-square test for a defined decision niveau.
#' A significant relation of two variables/objects is marked with a numeric "1", a
#' negativ test result with a numeric "0".
#'
#' "phi": Pearson's phi coefficient ("mean square contingency coefficient").
#'
#' "cc": Pearson's contingency coefficient.
#'
#' "lambda": Goodman and Kruskal's lambda value.
#'
#' @param dim switch to define if the correlation matrix should be created
#' for columns or rows.
#'
#' 1 (default): table is created for column (variables) relations.
#'
#' 2: table is created for row (objects) relations.
#'
#' @param chi2limit significance level for the test decision. Just relevant for method
#' "chi2". The higher chi2limit the less results will get removed.
#'
#' default: 0.05 -> 5\%
#'
#' @param rmnegniv option allows to remove "negativ relations". If >0 \code{rmnegcorr} gets
#' called and applied. A usual value for rmnegniv is 0.1.
#'
#' @return correlation matrix
#'
#'@examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#'
#' corrmat(testmatrixrand, "chi2", chi2limit = 0.03)
#'
#' corrmat(matrix = testmatrixrand, method = "lambda", dim = 2)
#'
#' phicorrtable <- corrmat(
#'  matrix = testmatrixrand,
#'  method = "phi",
#'  dim = 1
#'  )
#'
#' # Without negative relations:
#' phicorrtablewnr <- corrmat(
#'  matrix = testmatrixrand,
#'  method = "phi",
#'  dim = 1,
#'  rmnegniv = 0.1
#'  )
#'
#' @export
#'

corrmat <- function (matrix, method = "chi2",
                     dim = 1, chi2limit = 0.05, rmnegniv = 0) {

  # create empty correlation table for the input data.frame
  corrtab <- quantaar::newcorrtable(matrix, dim)
  my_dim <- c(2, 1)[dim]

  newcortab <- apply(matrix, my_dim, function(z){
    apply(matrix, my_dim, function(s){
      tbl <- table(z, s)
      x <- chisq.test(tbl)
      if (method == "chi2") {
        # comparing p-Value with defined decision niveau chi2limit
        # to make a test decision
        # DE: p-Value (Verwerfungsniveau) wird aus den Testergebnissen
        # extrahiert und mit der eingegebenen Irrtumswahrscheinlichkeit
        # chi2limit verglichen. Wenn die Wahrscheinlichkeit, dass man sich
        # bei einer Ablehnung der Nullhypothese (H0: kein Zusammenhang
        # der Variablen) irrt, kleiner als chi2limit ausfällt, dann kann
        # ein signifikanter Zusammenhang angenommen werden, der mit einer 1
        # in der Ergebnismatrix testtable festgehalten wird. Umgekehrt
        # weist eine 0 auf keinen signifikanten Zusammenhang hin.
        if (unlist(x[3]) < chi2limit) {
          result <- 1
        } else {
          result <- 0
        }
      } else if (method == "phi") {
        # calculation of phi = √(chi2/n)
        # -> chi2 is the chisquare value
        # -> n is the sum of the contingency table
        result <- sqrt(unlist(x[1]) / sum(tbl))
      } else if (method == "cc") {
        # calculation of CC = √(chi2/(chi2+n))
        # -> chi2 is the chisquare value
        # -> n is the sum of the contingency table
        result <- sqrt(unlist(x[1]) / (unlist(x[1]) + sum(tbl)))
      } else if (method == "lambda") {
        # calculation of lambda value
        result <- rapportools::lambda.test(tbl, direction = 2)
      } else {
        stop("Wrong method name!",
             call. = FALSE)
      }
      result
    })
  })

  newcortab <- as.data.frame(t(newcortab))

  rownames(newcortab) <- rownames(corrtab)
  colnames(newcortab) <- colnames(corrtab)
  corrtab <- newcortab

  # apply removal of negativ relations with rmnegcorr
  if (rmnegniv > 0) {
    corrtab <- quantaar::rmnegcorr(
      corrmatrix = corrtab,
      matrix = matrix,
      dim = dim,
      niv = rmnegniv
    )
  }

  return(corrtab)
}

# End Correlation Data Munging Functions  ---------------------------

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
#' @export
#'

delempty <- function(matrix) {

  # search empty columns and save index in a vector
  n <- c()
  u <- 1
  for (i in 1:length(matrix[1, ])) {
    if (sum(matrix[, i]) == 0) {
      n[u] <- i
      u <- u + 1
    }
  }

  # search empty rows and save index in a vector
  z <- c()
  i <- 1
  u <- 1
  for (i in 1:length(matrix[, 1])) {
    if (sum(matrix[i, ]) == 0) {
      z[u] <- i
      u <- u + 1
    }
  }

  # delete empty colums
  if (!is.null(n)) {
    matrix <- matrix[, -n]
  }

  # delete empty rows
  if (!is.null(z)) {
    matrix <- matrix[-z, ]
  }

  return(matrix)

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
#'

delrc <- function(matrix, climit = 0, rlimit = 0) {

  # reduce too big input to the extends of the matrix
  # to cover the possible maximum
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
    if (!(ncol(matrix) == 0 | nrow(matrix) == 0) &&
        !is.vector(matrix)) {
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
  while (nrow(matrix) > 0 &&
         ncol(matrix) > 0 &&
         (!is.null(cdel(matrix, climit)) ||
          !is.null(rdel(matrix, rlimit)))
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
#'

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
