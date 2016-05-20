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
#'    matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
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

corrmat <- function (matrix, method = "chi2", dim = 1, chi2limit = 0.05, rmnegniv = 0) {

  # create empty correlation table for the input data.frame
  corrtab <- quantaar::newcorrtable(matrix, dim)

  if (dim == 1) {
    # loop to compare every column with every other column
    for (z in 1:length(matrix)) {
      for (s in 1:length(matrix)) {
        # calculation of a contingency table for the current column-column relation
        tbl = table(matrix[,z], matrix[,s])

        # method decision
        if (method == "chi2") {
          # perform chi-square test for the current relation
          x <- chisq.test(tbl)
          # comparing p-Value with defined decision niveau chi2limit to make a test decision
          # DE: p-Value (Verwerfungsniveau) wird aus den Testergebnissen extrahiert und mit der eingegebenen
          # Irrtumswahrscheinlichkeit chi2limit verglichen. Wenn die Wahrscheinlichkeit, dass man sich bei einer
          # Ablehnung der Nullhypothese (H0: kein Zusammenhang der Variablen) irrt, kleiner als chi2limit ausfällt,
          # dann kann ein signifikanter Zusammenhang angenommen werden, der mit einer 1 in der Ergebnismatrix
          # testtable festgehalten wird. Umgekehrt weist eine 0 auf keinen signifikanten Zusammenhang hin.
          if (unlist(x[3]) < chi2limit) {
            corrtab[z,s] <- 1
          } else {
            corrtab[z,s] <- 0
          }
        } else if (method == "phi") {
          # perform chi-square test for the current relation
          x <- chisq.test(tbl)
          # calculation of phi = √(chi2/n)
          # -> chi2 is the chisquare value
          # -> n is the sum of the contingency table
          corrtab[z,s] <- sqrt(unlist(x[1])/sum(tbl))
        } else if (method == "cc") {
          # perform chi-square test for the current relation
          x <- chisq.test(tbl)
          # calculation of CC = √(chi2/(chi2+n))
          # -> chi2 is the chisquare value
          # -> n is the sum of the contingency table
          corrtab[z,s] <- sqrt(unlist(x[1])/(unlist(x[1])+sum(tbl)))
        } else if (method == "lambda") {
          # calculation of lambda value
          corrtab[z,s] <- rapportools::lambda.test(tbl, direction = 2)
        } else {
          stop("Wrong method name!",
               call. = FALSE)
        }

      }
    }
  }

  if (dim == 2) {
    # loop to compare every row with every other row
    # other comments: see above
    for (z in 1:length(matrix[,1])) {
      for (s in 1:length(matrix[,1])) {
        tbl = table(unlist(matrix[z,]), unlist(matrix[s,]))

        if (method == "chi2") {
          x <- chisq.test(tbl)
          if (unlist(x[3]) < chi2limit) {
            corrtab[z,s] <- 1
          } else {
            corrtab[z,s] <- 0
          }
        } else if (method == "phi") {
          x <- chisq.test(tbl)
          corrtab[z,s] <- sqrt(unlist(x[1])/sum(tbl))
        } else if (method == "cc") {
          x <- chisq.test(tbl)
          corrtab[z,s] <- sqrt(unlist(x[1])/(unlist(x[1])+sum(tbl)))
        } else if (method == "lambda") {
          corrtab[z,s] <- rapportools::lambda.test(tbl, direction = 2)
        } else {
          stop("Wrong method name!",
               call. = FALSE)
        }

      }
    }
  }

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