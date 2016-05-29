context("Tests of function booleanize")

testmatrix <- data.frame(c1 = c(0, 2, 0, 8),
                         c2 = c(5, 6, 7, 0),
                         c3 = c(5, 6, 7, 0))
rownames(testmatrix) <- c("r1", "r2", "r3", "r4")

test_that(
  "the output of booleanize is a data.frame",  {
    expect_equal(
      is.data.frame(booleanize(testmatrix)),
      TRUE
    )
  }
)

test_that(
  "the output of booleanize is a data.frame with
  the same size as the input data.frame",  {
  expect_equal(
    nrow(booleanize(testmatrix)),
    nrow(testmatrix)
    )
  expect_equal(
    length(booleanize(testmatrix)),
    length(testmatrix)
    )
  }
)

test_that(
  "the output of booleanize is a data.frame with
  the rownames and colnames as the input data.frame",  {
  expect_equal(
    colnames(booleanize(testmatrix)),
    colnames(testmatrix)
    )
  expect_equal(
    rownames(booleanize(testmatrix)),
    rownames(testmatrix)
    )
  }
)

test_that(
  "the output of booleanize contains the defined presence/absence values",  {
  expect_equal(
    booleanize(testmatrix, present = "cake", absent = "no cake")[2, 2],
    "cake"
    )
  expect_equal(
    booleanize(testmatrix, present = "cake", absent = "no cake")[1, 1],
    "no cake"
    )
  }
)

test_that(
  "the output of booleanize marks presence and absence correctly",  {
    expect_equal(
      which(testmatrix != 0),
      which(booleanize(testmatrix) != 0)
    )
    expect_equal(
      which(testmatrix == 0),
      which(booleanize(testmatrix) == 0)
    )
  }
)