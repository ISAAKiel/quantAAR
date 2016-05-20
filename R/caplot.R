#' Plot a selection of plots to get a first impression of the results of a CA 
#' 
#' \code{caplot()} delivers three pages of plots based on \code{ca::plot.ca()}. 
#' Useful to get a first impression of the results to decide, whether a more elaborated
#' plot should be created and whether other axis besides dim1 and dim2 contain interesting 
#' information.
#'
#' @return cadoc object of class \code{ca}. Produced e.g. \code{by ca::ca()}
#' 
#'@examples
#' testmatrixrand <- data.frame(
#'    matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
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
  .graphicdefault <- par(no.readonly = T)
  
  
  ## PAGE 1
  
  m <- layout (
    matrix(c(1,2,3,4,4,4),3,2),
    widths=c(1.5,3),
    heights=c(1,1,1)
  )
  #layout.show(m)
  
  # inertia barplot
  summary(cadoc)[[1]][,2] -> inert
  inert <- inert[1:5]
  
  barplot(
    inert, 
    names.arg = paste('', 1:length(inert)), 
    col = 8, 
    ylim = c(0, round(1.5 * max(inert), 1)),
    space = 0, 
    las = 1
    )
  text(
    (1:length(inert))-.5, inert, round(inert,3), 
    pos=3
    )
  title(main="Inertia value of dimensions 1-5", font=2) 
  
  # ca plots
  plot(
    cadoc, 
    dim = c(1,2), 
    labels = c(0,0), 
    map = "rowprincipal", 
    mass = c(TRUE,FALSE)
    )
  title(main="Mass", font=2) 
  
  plot(
    cadoc, 
    dim = c(1,2), 
    labels = c(0,0), 
    map = "rowprincipal", 
    contrib = "relative"
    )
  title(main="Quality", font=2) 
  
  plot(
    cadoc, 
    dim = c(1,2), 
    labels = c(2,0), 
    map = "rowprincipal"
    )
  title(main="CA - X: dim1, Y: dim2", font=2) 
  
  
  ## Page 2

  m <- layout (
    matrix(c(1,1,2,3),2,2),
    widths=c(1,1),
    heights=c(1,1)
  )

  # ca plots
  plot(
    cadoc, 
    dim = c(1,2), 
    labels = c(2,0), 
    map = "rowprincipal", 
    xlim = c(-2,2), 
    ylim = c(-2,2)
    )
  title(main="X: dim1, Y: dim2 Zoom", font=2) 
  
  plot(
    cadoc, 
    dim = c(1,3), 
    labels = c(2,0), 
    map = "rowprincipal"
    )
  title(main="X: dim1, Y: dim3", font=2) 
  
  plot(
    cadoc, 
    dim = c(2,3), 
    labels = c(2,0), 
    map = "rowprincipal")
  title(main="X: dim2, Y: dim3", font=2) 
  
  
  ## PAGE 3 
  
  m <- layout (
    matrix(c(1),2,2),
    widths=c(1,1),
    heights=c(1,1)
  )

  # ca plot
  plot(
    cadoc, 
    dim = c(1,2), 
    labels = c(0,2), 
    map = "rowprincipal"
    )
  title(main="X: dim1, Y: dim2", font=2) 
  
  # set graphic values back to default
  par(.graphicdefault)
}