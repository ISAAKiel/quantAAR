#' Predict the relation of objects to a list of variables of interest
#'
#' By comparison of significant correlations within variables and the variables the objects
#' incorporate \code{predictvo()} makes an prediction about the relation to given variables
#' for every object.
#'
#' @param matrix data.frame with numeric values
#' @param reltable table of correlation values (e.g. produced by \code{reltable()})
#' @param mvars vector of variables of interest (full name)
#' @param level switch to define if the prediction should be based on level 1
#' or on level 1 + level 2 variables. A level 1 variable is directly linked to a variable
#' of interest, a level 2 variable is linked to the level 1 variables of said variable of
#' interest
#'
#' 1: prediction is based only on level 1 variables.
#'
#' 2 (default): prediction is based on level 1 + level 2 variables.
#'
#' @return table with predicted, normalized relation values of every object and given
#' variables of interest. If no variable of interest has correlations to other variables,
#' \code{predictvo()} returns FALSE.
#'
#' @examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#'
#' testcorr <- corrmat(testmatrixrand, "chi2", chi2limit = 0.1, dim = 1)
#'
#' rel <- reltable(testcorr)
#'
#' predictvo(testmatrixrand, rel, c("X2", "X3"))
#'
#' @export
#'

predictvo <- function (matrix, reltable, mvars, level = 2) {

  # loop: check relations of every variable of interest
  for (pointer in 1:length(mvars)){

    mvar <- mvars[pointer]

    # find variables, that are linked to the current variable of interest
    redtovar <- dplyr::filter(
      reltable,
      namevar1 == mvar | namevar2 == mvar
    )

    # if no variables are linked to the current variable of interest, the loop
    # continues with the next variable
    if (nrow(redtovar) == 0) {
      next()
    }

    # extract partner variables of the variable of interest (1. Level)
    withoutmvar <- c(
      redtovar[redtovar$namevar1 != mvar, ]$namevar1,
      redtovar[redtovar$namevar2 != mvar, ]$namevar2
    )

   # extract partner variables of partner variables of interest (2. Level)
    mvarnet <- dplyr::filter(
      reltable,
      namevar1 == withoutmvar[1] |
        namevar2 == withoutmvar[1]
    )
    for (i in 2:length(withoutmvar)) {
      mvarnet <- rbind(
        mvarnet,
        dplyr::filter (
          reltable,
          namevar1 == withoutmvar[i] |
            namevar2 == withoutmvar[i]
        )
      )
    }

    # create vector of partner variables
    # (2. Level or 1. Level + Variable of Interest)
    if (level == 1) {
      mvarvec <- c(withoutmvar, mvar)
    } else if (level == 2) {
      mvarvec <- c(mvarnet$namevar1, mvarnet$namevar2, withoutmvar, mvar)
    }

    # remove multiple values to get a simple list of partner variables
    mvarvec <- unique(mvarvec)

    mvarrel <- c()
    # loop: check relation of the variable of interest with every object
    for (i in 1:length(matrix[, 1])){
      # determine variables present in current object
      cur <- colnames(matrix)[as.logical(matrix[i, ])]
      # compare variables present in current object with the list of 2. Level
      # partner variables. Count overlap
      mvarrel[i] <- length(mvarvec[mvarvec %in% cur])
    }

    # normalize overlap vector
    for (i in 1:length(mvarrel)){
      mvarrel[i] <- mvarrel[i] / max(mvarrel)
    }

    # write overlap vector into a data.frame to collect the information for
    # every variable of information in one table
    if (!(exists("relvaluetable"))) {
      relvaluetable <- data.frame(mvarrel, matrix[, mvar])
    } else {
      relvaluetable <- data.frame(relvaluetable, mvarrel, matrix[, mvar])
    }

  }
  # If no variable of interest has correlations to other variables the function
  # returns FALSE.
  if (!(exists("relvaluetable"))) {
    return(FALSE)
  }

  # adjust colnames of resulting data.frame
  even <- seq(2, length(relvaluetable), 2)
  odd <- seq(1, length(relvaluetable), 2)
  suppressWarnings(
  colnames(relvaluetable)[odd] <- paste(mvars, "PREDICTION")
  )
  suppressWarnings(
  colnames(relvaluetable)[even] <- paste(mvars, "ACTUAL")
  )

  relvaluetable

}
