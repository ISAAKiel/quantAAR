## ---- echo=FALSE, message=FALSE------------------------------------------
devtools::load_all()
library(reshape2)
library(ggplot2)
library(corrplot)
library(dplyr)
library(igraph)
library(ca)

## ---- echo=FALSE---------------------------------------------------------
load("../data/bs1.rda")
bs1[1:10,9:16]

## ---- echo=FALSE, echo=TRUE----------------------------------------------
bs <- quantaar::booleanize(bs1)
bs <- quantaar::delempty(bs)
bs[1:10,9:16]

## ---- echo=TRUE, message=FALSE, fig.width=7.5, fig.height=5--------------
presencematerial <- quantaar::presencecount(bs[,7:18], dim = 1)

presence.m <- reshape2::melt(presencematerial)
ggplot(presence.m,
       aes(x = variable,
           y = value)) +
  geom_bar(stat = "identity")

## ---- echo=TRUE, message=FALSE, fig.width=7.5, fig.height=5--------------
bsred <- bs[,7:18]
bsred <- quantaar::delrc(bsred, climit = 10)

presencematerial <- quantaar::presencecount(bsred, dim = 1)
presence.m <- reshape2::melt(presencematerial)
ggplot(presence.m,
       aes(x = variable,
           y = value)) +
  geom_bar(stat = "identity")

## ---- echo=TRUE, message=FALSE, warning=FALSE, fig.width=7.5, fig.height=7.5----
bsprep <- data.frame(bs[,1:6], bsred)
corrtablechi2test <- quantaar::corrmat(bsprep, method = "chi2", dim = 1, chi2limit = 0.02)
corrtablephi <- quantaar::corrmat(bsprep, method = "phi", dim = 1)
mastercorr <- quantaar::rmnegcorr(corrtablephi, bsprep, niv = 0.1, dim = 1)

col2 <- colorRampPalette(c("white","white", "chartreuse4"))
corrplot::corrplot(
  t(mastercorr),
  method = c("color")   
  )

## ---- echo=TRUE, message=FALSE, fig.width=7.5, fig.height=5, warning=FALSE----
signicorr <- quantaar::reltable(mastercorr, corrtablechi2test)
signicorr <- filter(signicorr, corrvalue2 == TRUE)

signicorr

## ---- echo=TRUE, message=FALSE, fig.width=7.5, fig.height=5, warning=FALSE----
signicorrmod <- data.frame(
  from = signicorr$namevar1, 
  to = signicorr$namevar2, 
  weight = signicorr$corrvalue)

graphbasis <- igraph::graph.data.frame(signicorrmod, directed = TRUE)
plot.igraph(graphbasis)

## ---- echo=TRUE, message=FALSE, fig.width=7.5, fig.height=7.5, warning=FALSE----
mvar <- c("sex_male", "sex_female")

mvar1male <- dplyr::filter(
  signicorr, 
  namevar1 == mvar[1] | 
    namevar2 == mvar[1]
)

mvar1male

## ---- echo=TRUE, message=FALSE, fig.width=7.5, fig.height=7.5, warning=FALSE----
mvar1female <- dplyr::filter(
  signicorr, 
  namevar1 == mvar[2] | 
    namevar2 == mvar[2]
)


mvar1female

## ---- echo=TRUE, message=FALSE, fig.width=7.5, fig.height=5, warning=FALSE----
predictgen <- quantaar::predictvo(bsprep, signicorr, mvar, level = 1)

sexprediction <- data.frame(
  m = predictgen[,1], 
  w = predictgen[,3],
  statsex = NA,
  mtat = bsprep$sex_male, 
  wtat = bsprep$sex_female,
  names = make.names(rownames(bsprep))
  )

for (i in 1:length(sexprediction[,1])){
  if (sexprediction$m[i] >= 1.5*sexprediction$w[i]){
    sexprediction$statsex[i] <- "m"
  } else if (sexprediction$w[i] >= 1.5*sexprediction$m[i]) {
    sexprediction$statsex[i] <- "w"
  } else {
    sexprediction$statsex[i] <- "uncertain"
  }
}

sexprediction

