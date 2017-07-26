bs1 <- read.csv("data-raw/bs1.csv",
                sep=";",
                header=TRUE,
                row.names=1,
                stringsAsFactors = FALSE,
                check.names = FALSE)

devtools::use_data(bs1, overwrite = TRUE)

devtools::load_all()

bs1
