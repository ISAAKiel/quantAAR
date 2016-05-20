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
  
  # loop to reach every cell of the matrix 
  for(r in 1:length(matrix[,1])) {
    for(c in 1:length(matrix[1,])) {
      # if a cell contains 0, than the value of "absent" will be written into this cell  
      if (matrix[r,c] == 0) {
        matrix[r,c] <- absent
        # if a cell contains a numeric value >0, than the value of "present" will be written into this cell   
      } else if (matrix[r,c] > 0) {
        matrix[r,c] <- present
      }
    }
  }
  
  return(matrix)
  
}