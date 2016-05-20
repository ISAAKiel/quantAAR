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
    matrixheight <- length(matrix[,1])
    newtable <- matrix(nrow = matrixheight, ncol = matrixheight, 0)
    colnames(newtable) <- rownames(matrix) 
    rownames(newtable) <- rownames(matrix) 
  }
  
  newtable <- data.frame(newtable, check.names = FALSE)
  
  return(newtable)
  
}