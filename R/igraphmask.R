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

igraphmask <- function (reltable, mypath, w = 3000, h = 3000, colorvector = c("red")) {

  # reduce reltable to the necessary columns
  reltablesimple <- data.frame(from = reltable$namevar1, to = reltable$namevar2, weight = reltable$corrvalue)

  # create graph-list (class of igraph)
  graphbasis <- igraph::graph.data.frame(reltablesimple, directed = TRUE)

  # modify color vector to frame white vertices
  colorvector2 <- colorvector
  colorvector2[grep("white", colorvector2)] <- "black"

  # plotting in file
  png(file = mypath, width = w, height = h)

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

  dev.off()

}
