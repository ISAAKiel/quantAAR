# Begin CA Plot Functions  ---------------------------

#' Plot a selection of plots to get a first impression of the results of a CA
#'
#' \code{caplot()} delivers three pages of plots based on \code{ca::plot.ca()}.
#' Useful to get a first impression of the results to decide, whether a more elaborated
#' plot should be created and whether other axis besides dim1 and dim2 contain interesting
#' information.
#'
#' @param cadoc object of class \code{ca}. Produced e.g. \code{by ca::ca()}
#'
#' @examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#' rownames(testmatrixrand) <- paste("row", seq(1:nrow(testmatrixrand)))
#'
#' library(ca)
#'
#' caplot(ca(testmatrixrand))
#'
#' @export
#'

caplot <- function (cadoc) {

  # save default graphics values
  .graphicdefault <- graphics::par(no.readonly = T)


  ## PAGE 1

  m <- graphics::layout (
    matrix(c(1, 2, 3, 4, 4, 4), 3, 2),
    widths = c(1.5, 3),
    heights = c(1, 1, 1)
  )

  # inertia barplot
  summary(cadoc)[[1]][, 2] -> inert
  inert <- inert[1:5]

  graphics::barplot(
    inert,
    names.arg = paste("", 1:length(inert)),
    col = 8,
    ylim = c(0, round(1.5 * max(inert), 1)),
    space = 0,
    las = 1
  )
  graphics::text(
    (1:length(inert)) - .5, inert, round(inert, 3),
    pos = 3
  )
  graphics::title(main = "Inertia value of dimensions 1-5", font = 2)

  # ca plots
  graphics::plot(
    cadoc,
    dim = c(1, 2),
    labels = c(0, 0),
    map = "rowprincipal",
    mass = c(TRUE, FALSE)
  )
  graphics::title(main = "Mass", font = 2)

  graphics::plot(
    cadoc,
    dim = c(1, 2),
    labels = c(0, 0),
    map = "rowprincipal",
    contrib = "relative"
  )
  graphics::title(main = "Quality", font = 2)

  graphics::plot(
    cadoc,
    dim = c(1, 2),
    labels = c(2, 0),
    map = "rowprincipal"
  )
  graphics::title(main = "CA - X: dim1, Y: dim2", font = 2)


  ## Page 2

  m <- graphics::layout (
    matrix(c(1, 1, 2, 3), 2, 2),
    widths = c(1, 1),
    heights = c(1, 1)
  )

  # ca plots
  graphics::plot(
    cadoc,
    dim = c(1, 2),
    labels = c(2, 0),
    map = "rowprincipal",
    xlim = c(-2, 2),
    ylim = c(-2, 2)
  )
  graphics::title(main = "X: dim1, Y: dim2 Zoom", font = 2)

  graphics::plot(
    cadoc,
    dim = c(1, 3),
    labels = c(2, 0),
    map = "rowprincipal"
  )
  graphics::title(main = "X: dim1, Y: dim3", font = 2)

  graphics::plot(
    cadoc,
    dim = c(2, 3),
    labels = c(2, 0),
    map = "rowprincipal")
  graphics::title(main = "X: dim2, Y: dim3", font = 2)


  ## PAGE 3

  m <- graphics::layout (
    matrix(c(1), 2, 2),
    widths = c(1, 1),
    heights = c(1, 1)
  )

  # ca plot
  graphics::plot(
    cadoc,
    dim = c(1, 2),
    labels = c(0, 2),
    map = "rowprincipal"
  )
  graphics::title(main = "X: dim1, Y: dim2", font = 2)

  # set graphic values back to default
  graphics::par(.graphicdefault)
}

# End CA Plot Functions  ---------------------------

# Begin corrplot Plot Functions  ---------------------------

#' Mask function for corrplot::corrplot()
#'
#' \code{corrplotmask} is an input mask for the \code{corrplot()} function of the
#' package corrplot. \code{corrplot()} visualizes correlation matrizes. See
#' \code{?corrplot()} for further info.
#' \code{corrplotmask} allows to get a nice, basic corrplot graphic as a png-file
#' that is directly saved to the file system. It doesn't allow to define specific
#' plot settings.
#'
#' @param corrmatrix correlation matrix (as produced by quantaar::corrmat())
#' @param xmatrix correlation matrix only with the values 0 and 1, where 1 serves as
#' a marker (as produced by quantaar::corrmat(method="chi2")). Relations with the
#' value 1 will be marked with an X in the corrplot.
#'
#' default = 0 (nothing is marked)
#'
#' @param mypath file.path where the plot file should be stored
#'
#' mypath <- file.path("~/path/to/my/directory/",paste("myfilename", ".png", sep = ""))
#'
#' @param voi vector of indezes of variables/objects that should be shown on the x-axis.
#'
#' default = 1:length(corrmatrix[1,]) (every variable/object is shown)
#'
#' @param w width of the resulting graphic file in px
#'
#' default = 3000
#'
#' @param h height of the resulting graphic file in px
#'
#' default = 3000
#'
#' @return graphic file of a correlation matrix plot
#'
#'@examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#'
#' testcorr <- corrmat(testmatrixrand, "lambda", chi2limit = 0.1, dim = 1)
#' xtestcorr <- corrmat(testmatrixrand, "chi2", chi2limit = 0.1, dim = 1)
#'
#' testpath <- file.path(".",paste("testfile", ".png", sep = ""))
#'
#' corrplotmask(
#'  corrmatrix = testcorr,
#'  xmatrix = xtestcorr,
#'  mypath = testpath,
#'  voi = 1:10,
#'  w = 500,
#'  h = 1000
#' )
#'
#' @export
#'

corrplotmask <- function(corrmatrix,
                         xmatrix = "notact",
                         mypath,
                         voi = 1:ncol(corrmatrix),
                         w = 3000,
                         h = 3000) {

  # define color palette (with a nice green)
  col2 <- grDevices::colorRampPalette(c("white", "white", "chartreuse4"))

  # plotting in file
  grDevices::png(file = mypath, width = w, height = h)

  # decision: with or without crossmarking of certain values.
  if (is.character(xmatrix)) {
    corrplot::corrplot(
      corrmatrix[, voi],
      method = "color",
      cl.lim = c(0, 1),
      tl.col = "black",
      tl.cex = 2,
      col = col2(50)
    )
  } else {
    corrplot::corrplot(
      as.matrix(corrmatrix[, voi]),
      p.mat = as.matrix(xmatrix[, voi]),
      method = "color",
      cl.lim = c(0, 1),
      tl.col = "black",
      tl.cex = 2,
      col = col2(50)
    )
  }

  grDevices::dev.off()

}


# End corrplot Plot Functions  ---------------------------

# Begin igraph Plot Functions  ---------------------------

#' Mask function for igraph::plot.igraph() to visualize a relation table as a graph
#'
#' \code{igraphmask} is an input mask for the \code{plot.igraph()} function of the
#' package igraph. \code{plot.igraph()} creates graph plots based on bivariate relations.
#' See \code{?igraph} for further info.
#' \code{igraphmask} allows to get a nice, basic graph plot as a png-file
#' that is directly saved to the file system. It doesn't allow to define specific
#' plot settings.
#'
#' @details For graph layout the fruchterman.reingold.grid algorithm is used.
#'
#' @param reltable list of bivariate relations (as produced by quantaar::reltable())
#' @param mypath file.path where the plot file should be stored
#'
#' mypath <- file.path("~/path/to/my/directory/",paste("myfilename", ".png", sep = ""))
#'
#' @param w width of the resulting graphic file in px
#'
#' default = 3000
#'
#' @param h width of the resulting graphic file in px
#'
#' default = 3000
#'
#' @param colorvector string vector with colour values to mark certain vertices
#'
#' default = c("red")
#'
#' @return graphic file of a graph plot
#'
#'@examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
#' )
#'
#' testcorr <- corrmat(testmatrixrand, "lambda", chi2limit = 0.1, dim = 1)
#' testrel <- reltable(testcorr)
#'
#' testpath <- file.path(".",paste("testfile", ".png", sep = ""))
#'
#' igraphmask(testrel, testpath, w = 1000, h = 1000)
#'
#' @export
#'

igraphmask <- function (reltable,
                        mypath,
                        w = 3000,
                        h = 3000,
                        colorvector = c("red")) {

  # reduce reltable to the necessary columns
  reltablesimple <- data.frame(from = reltable$namevar1,
                               to = reltable$namevar2,
                               weight = reltable$corrvalue)

  # create graph-list (class of igraph)
  graphbasis <- igraph::graph.data.frame(reltablesimple, directed = TRUE)

  # modify color vector to frame white vertices
  colorvector2 <- colorvector
  colorvector2[grep("white", colorvector2)] <- "black"

  # plotting in file
  grDevices::png(file = mypath, width = w, height = h)

  igraph::plot.igraph(
    graphbasis,
    layout = igraph::layout.fruchterman.reingold.grid(graphbasis),
    # design vertices
    vertex.shape = "circle",
    vertex.size = 7,
    vertex.color = colorvector,
    vertex.frame.color = colorvector2,
    vertex.label.family = "sans",
    vertex.label.cex = 1.5,
    vertex.label.color = "black",
    # design edges
    edge.color = "darkgrey",
    edge.width = 3,
    edge.label = NA,
    edge.lty = 3,
    edge.arrow.mode = 0
  )

  grDevices::dev.off()

}

# End igraph Plot Functions  ---------------------------