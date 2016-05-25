context("Tests of function rmnegcorr")

testmatrix <- data.frame(
  c1 = c(5, 0, 0, 0, 1, 1),
  c2 = c(5, 0, 0, 0, 1, 1),
  c3 = c(5, 1, 7, 0, 0, 2),
  c4 = c(5, 6, 7, 0, 0, 0),
  c5 = c(5, 3, 2, 0, 0, 3),
  c6 = c(0, 6, 1, 0, 0, 0),
  c7 = c(0, 1, 1, 1, 0, 0)
)
testmatrix <- quantaar::booleanize(testmatrix)
testcorrmatrix <- corrmat(testmatrix, "chi2", chi2limit = 0.2, dim = 1)

test_that(
  "the output of rmnegcorr is a data.frame",  {
    expect_equal(
      is.data.frame(rmnegcorr(testmatrix, testmatrix, dim = 1, niv = 0.1)),
      TRUE
    )
  }
)

test_that(
  "the output of rmnegcorr is a data.frame
  with the correct width and heigth",  {
    expect_equal(
      ncol(rmnegcorr(testmatrix, testmatrix, dim = 1, niv = 0.1)),
      ncol(testmatrix)
    )
    expect_equal(
      nrow(newcorrtable(rmnegcorr(testmatrix, testmatrix, dim = 1, niv = 0.1))),
      ncol(testmatrix)
    )
  }
)

test_that(
  "the removal of negative relations in rmnegcorr works",  {
    expect_equal(
      testcorrmatrix[1, 7],
      1
    )
    expect_equal(
      rmnegcorr(testmatrix, testmatrix, dim = 1, niv = 0.1)[1, 7],
      0
    )
  }
)