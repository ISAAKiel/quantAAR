context("Tests of function delempty")

testmatrix <- data.frame(c1 = c(0, 3, 8, 2),
                         c2 = c(0, 6, 7, 8),
                         c3 = c(0, 0, 0, 0))
rownames(testmatrix) <- c("r1", "r2", "r3", "r4")

test_that(
  "the output of delempty is a data.frame",  {
    expect_equal(
      is.data.frame(delempty(testmatrix)),
      TRUE
    )
  }
)

test_that(
  "the output of delempty is a data.frame without empty rows or columns",  {
    expect_equal(
      length(which(apply(delempty(testmatrix), 1, sum) == 0)) > 0,
      FALSE
    )
    expect_equal(
      length(which(apply(delempty(testmatrix), 2, sum) == 0)) > 0,
      FALSE
    )
  }
)