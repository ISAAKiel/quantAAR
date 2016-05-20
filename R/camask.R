#' Mask function for ca:ca() to get the ca results in a tidy data.frame
#'
#' \code{camask()} delivers the coordinates of objects and variables of a correspondence
#' analyse as a tidy data.frame. Further it allows to directly call the function
#' \code{quantaar::caplot()} to get three pages of plots for a first impression.
#' The CA is calculated by \code{ca::ca()}. See \code{?ca} for further information.
#'
#' @details
#' Structure of the resulting table:
#'
#' column 1 - X:    ca coordinates of objects/variables for different dimensions
#'
#' column X+1:      type (var, obj, passivevar, passiveobj)
#'
#' @param matrix data.frame with numeric values
#' @param supc vector of indezes of variables (columns) that should be included as
#' passive entities into the ca
#'
#' default = c()
#'
#' @param supr vector of indezes of objects (rows) that should be included as
#' passive entities into the ca
#'
#' default = c()
#'
#' @param caplot boolean switch, to decide, whether caplot() should be called to get
#' some early plots
#'
#' default = FALSE
#'
#' @return data.frame with ca coordinates of variables and objects of the input data.frame
#'
#'@examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#' rownames(testmatrixrand) <- paste("row", seq(1:nrow(testmatrixrand)))
#'
#' camask(testmatrixrand, supc = c(1,2,3), supr = c(15,16))
#'
#' @export
#'

camask <- function(matrix, supc = c(), supr = c(), caplot = FALSE) {

  # call ca::ca() to perform CA
  # switch ist necessary to deal with presence or absence of passive variables:
  # ca() can't simply work with empty vectors
  if (!is.null(supr) && !is.null(supc)) {
    q <- ca::ca(matrix, suprow = supr, supcol = supc)
  } else if (!is.null(supr) && is.null(supc)) {
    q <- ca::ca(matrix, suprow = supr)
  } else if (is.null(supr) && !is.null(supc)) {
    q <- ca::ca(matrix, supcol = supc)
  } else {
    q <- ca::ca(matrix)
  }

  # create a dataframe with ca coordinates and a type column to distinguish
  # variables and objects
  df <- data.frame(
    rbind(q$colcoord, q$rowcoord),
    type = c(
      rep("var", nrow(q$colcoord)),
      rep("obj", nrow(q$rowcoord))
      ),
    row.names = c(
      colnames(matrix),
      rownames(matrix)
    ),
    stringsAsFactors = FALSE
  )

  # set type for passive variables/objects
  if (!is.null(supr) && !is.null(supc)) {
    df$type[supc] <- "passivevar"
    df$type[nrow(q$colcoord) + supr] <- "passiveobj"
  } else if (!is.null(supr) && is.null(supc)) {
    df$type[nrow(q$colcoord) + supr] <- "passiveobj"
  } else if (is.null(supr) && !is.null(supc)) {
    df$type[supc] <- "passivevar"
  }

  # call caplot()
  if (caplot) {
    quantaar::caplot(q)
  }

  return(df)

}