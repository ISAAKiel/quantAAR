library(varnastats)
context("Tests of function predictvo")

testmatrix <- data.frame(
  c1 = c(5,0,0,0,1,1), 
  c2 = c(5,0,0,0,1,1), 
  c3 = c(5,1,7,0,0,2),
  c4 = c(5,6,7,0,0,0),
  c5 = c(5,3,2,0,0,3),
  c6 = c(0,6,1,0,0,0),
  c7 = c(0,1,1,1,0,0)
)
testmatrix <- varnastats::booleanize(testmatrix)
testcorr <- corrmat(testmatrix2, "chi2", chi2limit = 0.2, dim = 1)
rel <- reltable(testcorr)
testvars <- c("c1", "c3", "c7")

test_that(
  "the output of predictvo is a data.frame", 
  {
    expect_equal(
      is.data.frame(predictvo(testmatrix, rel, testvars)), 
      TRUE
    )
  }
)

test_that(
  "the output of predictvo has the correct amount of cols (prediction+actual for every
  variable of interest)", 
  {
    expect_equal(
      ncol(predictvo(testmatrix, rel, testvars)), 
      2*length(c("c1", "c3", "c7"))
    )
  }
)

test_that(
  "the output of predictvo has the correct amount of rows (one row for every object of the
  testmatrix)", 
  {
    expect_equal(
      nrow(predictvo(testmatrix, rel, testvars)), 
      nrow(testmatrix)
    )
  }
)

test_that(
  "the output of predictvo contains the correct actual presence values", 
  {
    expect_equal(
      predictvo(testmatrix, rel, testvars)[1,2], 
      1
    )
    expect_equal(
      predictvo(testmatrix, rel, testvars)[6,4], 
      1
    )
    expect_equal(
      predictvo(testmatrix, rel, testvars)[5,6], 
      0
    )
  }
)