#' Calculate linkage values of variables and objects of a numeric data.frame
#'
#' \code{linkage()} calculates a linkage value for every variable and every object
#' of an input data.frame and returns a table with this information. 
#' This linkage value allows predictions about whether a variable/object can be used
#' for multivariate analysis. If a variable/object is not well linked to the other entities, 
#' it will often appear as an outlier. 
#' 
#' @details 
#' Structure of the resulting table: 
#' 
#' column 1:  linkage value (see code to understand how it's calculated)
#'
#' column 2:  logarithm of linkage value (useful for interpretation)
#' 
#' column 3:  type (variable or object)
#' 
#' @param matrix data.frame with numeric values
#' @return table with linkage values
#' 
#' @examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
#' )
#'  
#' linkage(testmatrixrand)
#'  
#' link <- subset(linkage(testmatrixrand), type == "obj")  
#' barplot(link$linkage, names.arg = rownames(link))  
#'  
#' @export
#'

linkage <- function(matrix) {
  
  ## calculate linkage value for objects
  
  # create table to store linkage value for objects
  weigthtab.obj <- matrix[1:3]
  colnames(weigthtab.obj) <- c("linkage", "loglinkage", "type")
  
  ## loop to check every object
  for (i in 1:nrow(matrix)) {
    
    # calculate sum of the variables present in the current object  
    sumvar = 0
    for (n in 1:ncol(matrix)) {
      if (matrix[i,n] != 0) {
        sumvar = sumvar + length(which(matrix[,n] != "0"))
      }
      
    } 
    
    # calculate linkage value: sumvar * amount of different variables incorporated by the object
    weigthtab.obj[i,1] <- sumvar * length(which(matrix[i,] != "0"))
    
    # calculate logarithm of linkage value
    if (weigthtab.obj[i,1] != 0) {
      weigthtab.obj[i,2] <- log(weigthtab.obj[i,1]) 
    } else {
      weigthtab.obj[i,2] <- 0
    }
    
  }
  
  
  ## calculate linkage value for variables
  
  # transpose input matrix
  t.matrix <- t(matrix)
  
  # create table to store linkage value for variables
  weigthtab.var <- t.matrix[,1:3]
  colnames(weigthtab.var) <- c("linkage", "loglinkage", "type")
  
  ## loop to check every variable
  for (i in 1:nrow(t.matrix)) {
    
    # calculate sum of the objects that incorporate the current variable  
    sumobj = 0
    for (n in 1:ncol(t.matrix)) {
      if (t.matrix[i,n] != 0) {
        sumobj = sumobj + length(which(t.matrix[,n] != "0"))
      }
      
    } 
    
    # calculate linkage value: sumvar * amount of different objects that incorporate the variable
    weigthtab.var[i,1] <-  sumobj*length(which(t.matrix[i,] != "0"))
    
    # calculate logarithm of linkage value
    if (weigthtab.var[i,1] != 0) {
      weigthtab.var[i,2] <- log(weigthtab.var[i,1]) 
    } else {
      weigthtab.var[i,2] <- 0
    }
    
  }

  weigthtab.var <- data.frame(weigthtab.var)

    
  # fill column type to mark objects and variables
  weigthtab.obj$type <- rep("obj", 1, nrow(weigthtab.obj))
  weigthtab.var$type <- rep("var", 1, nrow(weigthtab.var))

  # combine object- and variable tables
  weigthtab <- rbind(weigthtab.obj, weigthtab.var)
  
  return(weigthtab)
  
}