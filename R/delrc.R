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
    if (!(ncol(matrix) == 0 | nrow(matrix) == 0) &&
        !is.vector(matrix)) {
      for (s in 1:ncol(matrix)) {
        if (length(which(matrix[,s] > 0)) < climit) {
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
        if (length(which(matrix[i,] > 0)) < rlimit) {
          z[u] <- i
          u <- u + 1
        }
      }
    }
    return(z)
  }

  # loop to go through the matrix again and again until every relevant row/column is removed.
  # That's necessary, because the removal of e.g. one column  could cause the need to remove an other
  # row previously still relevant
  while (nrow(matrix) > 0 && 
         ncol(matrix) > 0 && 
         (!is.null(cdel(matrix, climit)) ||
         !is.null(rdel(matrix, rlimit)))
         ) {
    
    # call search function
    n <- cdel(matrix, climit)
    
    # delete columns
    if (!is.null(n) && ncol(matrix) >= max(n))  {
      matrix <- matrix[,-n]
    } 
    
    # call search function
    z <- rdel(matrix, rlimit)
    
    # delete rows
    if (!is.null(z) && nrow(matrix) >= max(z)) {
      matrix <- matrix[-z,]
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