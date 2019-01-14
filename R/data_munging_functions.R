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
booleanize <- function(matrix, present = TRUE, absent = FALSE) {

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
#' @param corrmatrix correlation matrix (as produced by quantAAR::corrmat())
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
  for (l in 1:nrow(corrmatrix)) {
    for (c in 1:ncol(corrmatrix)) {
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
#' @param corrtable correlation matrix (as produced by quantAAR::corrmat())
#' @param corrtable2 optional second corrmatrix correlation matrix (as produced by quantAAR::corrmat())
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

  # avoiding note (no visible binding for global variable)
  indexvar1 <- NULL;
  indexvar2 <- NULL;

  # copy matrix to apply an increasingly fast search algorithm
  # for the matrix maximum
  destroycorr <- corrtable

  # Setup an empty data.frame as basis for the relation table
  if (nrow(corrtable2) == 0) {
    a <- matrix(
      NA,
      nrow = length(corrtable[corrtable != 0]),
      ncol = 5
    )
    a <- data.frame(a)
    colnames(a) <- c(
      "indexvar1",
      "indexvar2",
      "corrvalue",
      "namevar1",
      "namevar2"
    )
  } else {
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
      "corrvalue2"
    )
  }

  # loop to fill relationship table (in order of decreasing correlation values)
  for (i in 1:nrow(a)) {
    if (max(destroycorr) != 0) {
      # search for current max value (highest correlation / best relation)
      a[i, 1:2] <- which(destroycorr == max(destroycorr), arr.ind = TRUE)[1, ]
      a[i, 3] <- destroycorr[a[i, 1], a[i, 2]]
      a[i, 4] <- colnames(destroycorr)[a[i, 1]]
      a[i, 5] <- colnames(destroycorr)[a[i, 2]]
      if (nrow(corrtable2) != 0) {
        a[i, 6] <- corrtable2[a[i, 1], a[i, 2]]
      }
      # set current relation to 0,
      # to find the next best relation in the next loop run
      destroycorr[a[i, 1], a[i, 2]] <- 0
    }
  }

  # remove autocorrelation
  b <- dplyr::filter(a, indexvar1 != indexvar2)

  # remove every relation, that is already present
  # inversely (var1 & var2 = var2 & var1)
  p1 <- 1
  while (p1 <= nrow(b)) {

    ind1 <- b[p1, 1]
    ind2 <- b[p1, 2]

    ind1in1 <- which(b$indexvar1 == ind1)
    ind1in2 <- which(b$indexvar2 == ind1)
    ind2in2 <- which(b$indexvar2 == ind2)
    ind2in1 <- which(b$indexvar1 == ind2)

    check1 <- ind1in1[ind1in1 %in% ind2in2]
    check2 <- ind1in2[ind1in2 %in% ind2in1]
    check <- c(check1, check2)


    if (length(check) > 1) {
      b <- b[-check[-1], ]
    }

    p1 <- p1 + 1
  }

  row.names(b) <- 1:nrow(b)

  return(b)
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
    matrixwidth <- ncol(matrix)
    newtable <- matrix(nrow = matrixwidth, ncol = matrixwidth, 0)
    colnames(newtable) <- colnames(matrix)
    rownames(newtable) <- colnames(matrix)
  }

  # table is created for row (objects) relations
  if (dim == 2) {
    matrixheight <- nrow(matrix)
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
#' "lambda": Goodman and Kruskal's lambda value. Mean of both values calculated
#' depending on what's the dependent and what's the independent variable
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

  # create empty correlation table that fits to the input data.frame
  corrtab <- quantAAR::newcorrtable(matrix, dim)

  # invert column/row selection due to unusal api (1 --> cols / 2 --> rows)
  my_dim <- c(2, 1)[dim]

  # loop to apply calculation of correlation values for every bivariate
  # variable relation
  newcortab <- apply(matrix, my_dim, function(z){
    apply(matrix, my_dim, function(s){

      # create data.frame and table of current relation
      tbl <- table(z, s)

      # perform chisq.test and store result values
      options(warn = -1)
      x <- stats::chisq.test(tbl)
      chi2 <- unlist(x[1])
      pval <- unlist(x[3])
      options(warn = 0)

      if (method == "chi2") {
        # comparing p-Value with defined decision niveau chi2limit
        # to make a test decision
        if (pval < chi2limit) {
          result <- 1
        } else {
          result <- 0
        }
      } else if (method == "phi") {
        # calculation of phi = sqrt(chi2/n)
        result <- sqrt((chi2) / sum(tbl))
      } else if (method == "cc") {
        # calculation of CC = sqrt(chi2/(chi2+n))
        result <- sqrt((chi2) / ((chi2) + sum(tbl)))
      } else if (method == "lambda") {
        # calculation of mean lambda value
        result <- mean(unlist(rapportools::lambda.test(tbl, direction = 0)))
      } else {
        stop("Wrong method name!",
             call. = FALSE)
      }
      result
    })
  })

  # transpose result correlation matrix to correct format
  newcortab <- as.data.frame(t(newcortab))

  # set colnames and rownames of result correlation matrix
  rownames(newcortab) <- rownames(corrtab)
  colnames(newcortab) <- colnames(corrtab)
  corrtab <- newcortab

  # apply removal of negativ relations with rmnegcorr
  if (rmnegniv > 0) {
    corrtab <- quantAAR::rmnegcorr(
      corrmatrix = corrtab,
      matrix = matrix,
      dim = dim,
      niv = rmnegniv
    )
  }

  return(corrtab)
}

# End Correlation Data Munging Functions  ---------------------------
