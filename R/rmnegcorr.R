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
  for (l in 1:length(corrmatrix[,1])) {
    for (c in 1:length(corrmatrix[1,])) {
      # calculation of a contingency table for the current corrmatrix cell
      corrtab <- table(matrix[,l], matrix[,c])
      # set ratio value to 100%
      prop1 <- 1
      prop2 <- 1
      # calculate ratio values for current cell, if there's a least one positiv
      # overlap of occurences of the underlying variables/objects
      if (corrtab[1,2] != 0) {
        prop1 <- corrtab[2,2]/sum(corrtab[,2])
      }
      if (corrtab[2,1] != 0) {
        prop2 <- corrtab[2,2]/sum(corrtab[2,])
      }
      # set correlation value to 0, if one or both ratio values is smaller than
      # a defined niveau.
      if (prop1 < niv) {
        corrmatrix[l,c] <- 0
      } else if (prop2 < niv) {
        corrmatrix[l,c] <- 0
      } else if (prop1 < niv && prop2 < niv) {
        corrmatrix[l,c] <- 0
      }
    }
  }
  
  return(corrmatrix)
}
