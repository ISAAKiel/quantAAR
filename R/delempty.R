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
  for (i in 1:length(matrix[1,])) {
    if (sum(matrix[,i]) == 0) {
      n[u] <- i
      u <- u + 1
    }
  }
  
  # search empty rows and save index in a vector
  z <- c()
  i <- 1
  u <- 1
  for (i in 1:length(matrix[,1])) {
    if (sum(matrix[i,]) == 0) {
      z[u] <- i
      u <- u + 1
    }
  }
  
  # delete empty colums
  if (!is.null(n)) {
    matrix <- matrix[,-n]
  }
  
  # delete empty rows
  if (!is.null(z)) {
    matrix <- matrix[-z,]
  }
  
  return(matrix)
  
}