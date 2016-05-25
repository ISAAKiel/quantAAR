context("Tests of function presencecount")

testmatrix <- data.frame(c1 = c(0, 3, 8, 2),
                         c2 = c(0, 6, 7, 8),
                         c3 = c(0, 0, 0, 0))
rownames(testmatrix) <- c("r1", "r2", "r3", "r4")

test_that(
  "the output of presencecount is a data.frame",  {
    expect_equal(
      is.data.frame(presencecount(testmatrix)),
      TRUE
    )
  }
)

test_that(
  "the output of presencecount has the correct length",  {
    expect_equal(
      ncol(presencecount(testmatrix, dim = 1)),
      ncol(testmatrix)
    )
    expect_equal(
      ncol(presencecount(testmatrix, dim = 2)),
      nrow(testmatrix)
    )
  }
)

test_that(
  "presencecount counts correctly ",  {
    expect_equal(
      presencecount(testmatrix, dim = 1)[, 2],
      3
    )
    expect_equal(
      presencecount(testmatrix, dim = 2)[, 2],
      2
    )
  }
)