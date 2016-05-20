context("Tests of function reltable")

testmatrixrand <- data.frame(
  matrix(base::sample(0:1,400,replace=T), nrow=20, ncol=20),
  check.names = FALSE
)
testcorr1 <- corrmat(testmatrixrand, "lambda", dim = 1)
testcorr2 <- corrmat(testmatrixrand, "chi2", chi2limit = 0.1, dim = 1)

test_that(
  "the output of reltable is a data.frame",
  {
    expect_equal(
      is.data.frame(reltable(testcorr1)),
      TRUE
    )
  }
)

test_that(
  "the output of reltable has the main output-columns",
  {
    expect_equal(
      c("indexvar1", "indexvar2", "corrvalue", "namevar1", "namevar2") %in%
        colnames(reltable(testcorr1)),
      c(TRUE, TRUE, TRUE, TRUE, TRUE)
    )
    expect_equal(
      c("indexvar1", "indexvar2", "corrvalue", "namevar1", "namevar2", "corrvalue2") %in%
        colnames(reltable(testcorr1, testcorr2)),
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
    )
  }
)

test_that(
  "the output of reltable doesn't contain autocorrelations",
  {
    expect_equal(
      nrow(subset(reltable(testcorr1, testcorr2), namevar1 == namevar2)) == 0,
      TRUE
    )
  }
)

test_that(
  "the output of reltable has a plausible amount of rows for relations",
  {
    expect_equal(
      nrow(reltable(testcorr1, testcorr2)) <= nrow(testcorr1)*ncol(testcorr1)-nrow(testcorr1),
      TRUE
    )
  }
)