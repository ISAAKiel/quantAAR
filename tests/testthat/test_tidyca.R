context("Tests of function ca.ca_ca")

testmatrixrand <- data.frame(
  matrix(base::sample(0:1, 400, replace = T), nrow = 20, ncol = 20),
  check.names = FALSE
)
rownames(testmatrixrand) <- paste("row", seq(1:nrow(testmatrixrand)))

test_that(
  "the output of ca.ca_ca is a data.frame",  {
    expect_equal(
      is.data.frame(ca.ca_ca(testmatrixrand)),
      TRUE
    )
  }
)

test_that(
  "the output of ca.ca_ca is a data.frame with the correct heigth",  {
    expect_equal(
      nrow(ca.ca_ca(testmatrixrand)),
      ncol(testmatrixrand) +  nrow(testmatrixrand)
    )
  }
)

test_that(
  "the output of ca.ca_ca contains the correct markers
  for normal and passive ca elements",  {
    expect_equal(
      ca.ca_ca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[10],
      "row"
    )
    expect_true(
      ca.ca_ca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$sup[15]
    )
    expect_equal(
      ca.ca_ca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$type[30],
      "col"
    )
    expect_true(
      ca.ca_ca(testmatrixrand, supc = c(1, 2, 3), supr = c(15, 16))$sup[22]
    )
  }
)
