library(varnastats)
context("Tests of function corrmat")

testmatrixrand1 <- data.frame(
  matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20),
  check.names = FALSE
)

test_that(
  "the output of corrmat is a data.frame", 
  {
    expect_equal(
      is.data.frame(corrmat(testmatrixrand1, dim = 1)), 
      TRUE
    )
    expect_equal(
      is.data.frame(corrmat(testmatrixrand1, dim = 2)), 
      TRUE
    )
  }
)
    
testmatrix2 <- data.frame(
  c1 = c(5,0,0,0,1,1), 
  c2 = c(5,0,0,0,1,1), 
  c3 = c(5,1,7,0,0,2),
  c4 = c(5,6,7,0,0,0),
  c5 = c(5,3,2,0,0,3),
  c6 = c(0,6,1,0,0,0),
  c7 = c(0,1,1,1,0,0)
  )
testmatrix2 <- varnastats::booleanize(testmatrix2)

test_that(
  "the output of corrmat is a data.frame with the correct width and heigth", 
  {
    expect_equal(
      length(newcorrtable(testmatrix2, dim = 1)), 
      length(testmatrix2)
    )
    expect_equal(
      length(newcorrtable(testmatrix2, dim = 2)), 
      nrow(testmatrix2)
    )
  }
)

test_that(
  "the different methods of corrmat are calculated correctly", 
  {
    expect_equal(
      corrmat(testmatrix2, "chi2", chi2limit = 0.2)[5,3], 
      1
    )
    expect_equal(
      round(corrmat(testmatrix2, "phi", chi2limit = 0.2)[5,3], 3), 
      0.625
    )
    expect_equal(
      round(corrmat(testmatrix2, "cc", chi2limit = 0.2)[5,3], 3), 
      0.53
    )
    expect_equal(
      round(corrmat(testmatrix2, "lambda", chi2limit = 0.2)[5,3], 3), 
      1
    )
  }
)

test_that(
  "the removal of negative relations in corrmat works",
  {
    expect_equal(
      corrmat(testmatrix2, "chi2", chi2limit = 0.2)[1,7], 
      1
    )
    expect_equal(
      corrmat(testmatrix2, "chi2", chi2limit = 0.2, rmnegniv = 0.1)[1,7], 
      0
    )
  }
)