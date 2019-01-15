context("Tests of function tidyca")

testmatrixrand <- data.frame(
  matrix(base::sample(0:1, 400, replace = T), nrow = 20, ncol = 20),
  check.names = FALSE
)
rownames(testmatrixrand) <- paste("row", seq(1:nrow(testmatrixrand)))

test_that(
  "the output of tidyca is a data.frame",  {
    expect_equal(
      is.data.frame(tidyca(testmatrixrand)),
      TRUE
    )
  }
)

test_that(
  "the output of tidyca is a data.frame with the correct heigth",  {
    expect_equal(
      nrow(tidyca(testmatrixrand)),
      ncol(testmatrixrand) +  nrow(testmatrixrand)
    )
  }
)

test_that(
  "the output of tidyca contains the correct markers
  for normal and passive ca elements",  {
    expect_equal(
      tidyca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[10],
      "row"
    )
    expect_true(
      tidyca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$sup[15]
    )
    expect_equal(
      tidyca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[30],
      "col"
    )
    expect_true(
      tidyca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$sup[22]
    )
  }
)
