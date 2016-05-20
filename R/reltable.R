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
#' @param corrmatrix correlation matrix (as produced by quantaar::corrmat())
#' @param optional second corrmatrix correlation matrix (as produced by quantaar::corrmat())
#' @return table of relations and their correlation value
#' 
#' @examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
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
  
  # copy matrix to apply an increasingly fast search algorithm for the matrix maximum
  destroycorr <- corrtable
  
  # Setup an empty data.frame as basis for the relation table
  if (nrow(corrtable2) == 0) {
    a <- matrix(
      NA, 
      nrow = length(corrtable[corrtable != 0]), 
      ncol=6
    )
    a <- data.frame(a)
    colnames(a) <- c(
      "indexvar1", 
      "indexvar2", 
      "corrvalue", 
      "namevar1", 
      "namevar2", 
      "namehash"
    ) 
  } else {
    a <- matrix(
      NA, 
      nrow = length(corrtable[corrtable != 0]), 
      ncol=7
    )
    a <- data.frame(a)
    colnames(a) <- c(
      "indexvar1", 
      "indexvar2", 
      "corrvalue", 
      "namevar1", 
      "namevar2", 
      "namehash",
      "corrvalue2"
    ) 
  }
  
  # loop to fill relationship table (in order of decreasing correlation values)
  for (i in 1:length(a[,1])) {
    if (max(destroycorr) != 0) {
      # search for current max value (highest correlation / best relation)
      a[i,1:2] <- which(destroycorr == max(destroycorr), arr.ind = TRUE)[1,]
      a[i,3] <- destroycorr[a[i,1],a[i,2]]
      a[i,4] <- colnames(destroycorr)[a[i,1]]
      a[i,5] <- colnames(destroycorr)[a[i,2]]
      # create namehash to identify every relationship bijektivly
      suppressWarnings(
      namehashvector <- as.numeric(charToRaw(a[i,4])) + as.numeric(charToRaw(a[i,5]))
      )
      a[i,6] <- paste(namehashvector,collapse="")
      if (nrow(corrtable2) != 0) {
        a[i,7] <- corrtable2[a[i,1],a[i,2]]
      }
      # set current relation to 0, to find the next best relation in the next loop run
      destroycorr[a[i,1],a[i,2]] <- 0
    }
  }
  
  # remove autocorrelation
  b <- dplyr::filter(a, namevar1 != namevar2)
  
  # remove every relation, that is already present inversely (var1 + var2 = var2 + var1)
  c <- b[which(duplicated(b[,6])),]
  
  row.names(c) <- 1:length(c[,1])
  
  #remove namehash
  
  d <- c[,-6]
  
  return(d)
}