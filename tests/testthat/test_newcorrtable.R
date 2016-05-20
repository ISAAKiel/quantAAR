library(varnastats)
context("Tests of function newcorrtable")

testmatrixrand <- data.frame(
  matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20),
  check.names = FALSE
)

test_that(
  "the output of newcorrtable is a data.frame", 
  {
    expect_equal(
      is.data.frame(newcorrtable(testmatrixrand, dim = 1)), 
      TRUE
    )
    expect_equal(
      is.data.frame(newcorrtable(testmatrixrand, dim = 2)), 
      TRUE
    )
  }
)

test_that(
  "the output of newcorrtable is a data.frame without any values besides 0 and the 
  correct amount of cells", 
  {
    expect_equal(
      length(which(newcorrtable(testmatrixrand, dim = 1) == 0)), 
      length(testmatrixrand)^2
    )
    expect_equal(
      length(which(newcorrtable(testmatrixrand, dim = 2) == 0)), 
      nrow(testmatrixrand)^2
    )
  }
)

testmatrixrand <- data.frame(c1 = c(5,2,3,8), c2 = c(5,6,7,0), c3 = c(5,6,7,9))

test_that(
  "the output of newcorrtable is a data.frame with the correct width and heigth", 
  {
    expect_equal(
      length(newcorrtable(testmatrixrand, dim = 1)), 
      length(testmatrixrand)
    )
    expect_equal(
      length(newcorrtable(testmatrixrand, dim = 2)), 
      nrow(testmatrixrand)
    )
  }
)