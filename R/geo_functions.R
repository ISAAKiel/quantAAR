# Begin geo modelling functions  ---------------------------

#' Apply kriging for a list of data.frames
#'
#' Test
#'
#' @param plist test
#' @param x test
#' @param y test
#' @param z test
#' @param lags test
#' @param model test
#'
#' @return list with data.frame maps
#'
#' @examples
#' 1 == 1
#'
#' @export
#'

kriglist <- function(plist, x = 1, y = 2, z = 3, lags = 6, model = "spherical") {
  # create output df
  maplist <- list()
  # loop to do kriging for all dfs in the input list
  for (i in 1:length(plist)) {
    maplist[[i]] <- kriging(
      x = plist[[i]][,x],
      y = plist[[i]][,y],
      response = plist[[i]][,z],
      model = model,
      lags = lags
    )$map
  }
  return(maplist)
}

# End geo modelling functions  ---------------------------

# Begin geo data layout transformation functions  ---------------------------

#' Transform a long surface describing df to a wide surface describing df
#'
#' Test
#'
#' @param x test
#' @param y test
#' @param z test
#'
#' @return widedf
#'
#' @examples
#' 1 == 1
#'
#' @export
#'

spatialwide <- function(x , y , z ) {

  longdf <- data.frame(x, y, z)

  xu <- unique(longdf$x)
  yu <- unique(longdf$y)
  widedf <- matrix(NA, length(yu), length(xu))

  for (p1 in 1:ncol(widedf)) {
    for (p2 in 1:nrow(widedf)) {
      zc <- filter(
        longdf,
        x == xu[p1],
        y == yu[p2]
      )$z
      widedf[p2, p1] <- zc
    }
  }

  widedf <- data.frame(widedf)
  colnames(widedf) <- round(xu, 3)
  rownames(widedf) <- round(yu, 3)

  return(widedf)

}

# End geo data layout transformation functions  ---------------------------
