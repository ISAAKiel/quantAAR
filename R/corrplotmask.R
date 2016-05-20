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
#'    matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
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

corrplotmask <- function(corrmatrix, xmatrix = "notact", mypath, voi=1:length(corrmatrix[1,]), w = 3000, h = 3000) {

  # define color palette (with a nice green)
  col2 <- colorRampPalette(c("white","white", "chartreuse4"))

  # plotting in file
  png(file = mypath, width=w, height=h)

    # decision: with or without crossmarking of certain values.
    if (is.character(xmatrix)) {
        corrplot::corrplot(
          corrmatrix[,voi],
          method = "color",
          cl.lim = c(0, 1),
          tl.col = "black",
          tl.cex = 2,
          col = col2(50)
          )
      } else {
        corrplot::corrplot(
          corrmatrix[,voi],
          p.mat=xmatrix[,voi],
          method = "color",
          cl.lim = c(0, 1),
          tl.col = "black",
          tl.cex = 2,
          col=col2(50)
          )
      }

  dev.off()

}