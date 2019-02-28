x <- quantAAR::itremove(quantAAR::matuskovo[,8:33], 3)

hu <- vegan::decostand(x, "norm")

spu <- stats::prcomp(hu)

biplot(spu)

library(ggplot2)
library(magrittr)
rownames(x) <- paste0("row_", rownames(x))
quantAAR::tidyca(x) %>% ggplot() + geom_text(aes(x = Dim1, y = Dim2, label = name))


ca::ca(schu) -> pu
pu$rownames
