library(varnastats)
context("Tests of function linkage")

testmatrixrand <- data.frame(
  matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20)
)

test_that(
  "the output of linkage is a data.frame", 
  {
    expect_equal(
      is.data.frame(linkage(testmatrixrand)), 
      TRUE
    )
  }
)

test_that(
  "the output of linkage has the two main output-columns", 
  {
    expect_equal(
      c("linkage", "type") %in% colnames(linkage(testmatrixrand)), 
      c(TRUE, TRUE)
    )
  }
)

test_that(
  "the output of linkage has contains information for variables and objects", 
  {
    expect_equal(
      c("obj", "var") %in% linkage(testmatrixrand)$type, 
      c(TRUE, TRUE)
    )
  }
)

test_that(
  "the output of linkage has the correct length and therefore covers all variables and 
  objects", 
  {
    expect_equal(
      nrow(linkage(testmatrixrand)), 
      ncol(testmatrixrand) + nrow(testmatrixrand)
    )
  }
)
