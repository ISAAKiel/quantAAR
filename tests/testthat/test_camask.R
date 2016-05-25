context("Tests of function camask")

testmatrixrand <- data.frame(
  matrix(base::sample(0:1, 400, replace = T), nrow = 20, ncol = 20),
  check.names = FALSE
)
rownames(testmatrixrand) <- paste("row", seq(1:nrow(testmatrixrand)))

test_that(
  "the output of camask is a data.frame",  {
    expect_equal(
      is.data.frame(camask(testmatrixrand)),
      TRUE
    )
  }
)

test_that(
  "the output of camask is a data.frame with the correct heigth",  {
    expect_equal(
      nrow(camask(testmatrixrand)),
      ncol(testmatrixrand) +  nrow(testmatrixrand)
    )
  }
)

test_that(
  "the output of camask contains the correct markers
  for normal and passive ca elements",  {
    expect_equal(
      camask(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[15],
      "var"
    )
    expect_equal(
      camask(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[3],
      "passivevar"
    )
    expect_equal(
      camask(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[30],
      "obj"
    )
    expect_equal(
      camask(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[36],
      "passiveobj"
    )
  }
)