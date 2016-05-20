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
  
  #matrix <- data.frame(matrix, names = rownames(matrix))
  
  # count variables
  if (dim == 1) {
    # prepare data.frame to store the results
    widthdataset <- length(matrix)  
    presencecount <- data.frame(matrix[1,], row.names = "count")
    # loop to determine the amount of appearences
    for (i in 1:widthdataset) {
      presencecount[1,i] <- length(which(matrix[,i] != "0"))
    }
    # sort results by amount of appearences
    presencecount <- presencecount[,order(presencecount[1,])]
  }
  
  # count objects
  if (dim == 2) {
    # transpose matrix (then the code of dim=1 applies again)
    matrix <- data.frame(t(matrix))
    # prepare data.frame to store the results
    widthdataset <- length(matrix)  
    presencecount <- data.frame(matrix[1,], row.names = "count")
    # loop to determine the amount of appearences
    for (i in 1:widthdataset) {
      presencecount[1,i] <- length(which(matrix[,i] != "0"))
    }
    # sort results by amount of appearences
    presencecount <- presencecount[,order(presencecount[1,])]
  }
  
  return(presencecount)
}