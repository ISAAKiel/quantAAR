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

check_num_nonzero<-function(x,minnumber){
  if(sum(x==0)+(minnumber-1)<length(x)) {T} else {F}
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
itremove<-function(object,minnumber)
{
  enough_non_zero_elements_x<-apply(object,MARGIN=1,FUN=check_num_nonzero,minnumber=minnumber)
  enough_non_zero_elements_y<-apply(object,MARGIN=2,FUN=check_num_nonzero,minnumber=minnumber)
  if(any(c(enough_non_zero_elements_x,enough_non_zero_elements_y)==F))
  {
    return(itremove(object[enough_non_zero_elements_x,enough_non_zero_elements_y],minnumber))}
  else
  {return(object)}
}

# End CA Utility Functions  ---------------------------