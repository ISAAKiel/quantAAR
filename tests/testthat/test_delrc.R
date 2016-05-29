context("Tests of function delrc")

testmatrix1 <- data.frame(
  c1 = c(0, 3, 8, 2, 0),
  c2 = c(0, 6, 7, 0, 0),
  c3 = c(0, 0, 0, 0, 0),
  c4 = c(0, 3, 8, 2, 0),
  c5 = c(0, 6, 7, 0, 0),
  c6 = c(1, 0, 1, 1, 0)
)

testmatrix2 <- data.frame(
  c1 = c(0, 3, 8, 2),
  c2 = c(0, 6, 7, 0),
  c3 = c(0, 0, 0, 0)
)

test_that(
  "the output of delrc is a data.frame or FALSE",  {
    expect_equal(
      is.data.frame(delrc(testmatrix1, climit = 3, rlimit = 2)),
      TRUE
    )
    expect_equal(
      delrc(testmatrix2, climit = 3, rlimit = 2),
      FALSE
    )
  }
)

test_that(
  "the output of delrc with minimalistic settings is
  a data.frame without empty rows or columns ",  {
    expect_equal(
      length(
        which(
          apply(
            delrc(testmatrix1, climit = 0, rlimit = 1),
            1,
            sum) == 0)
        ) > 0,
      FALSE
    )
    expect_equal(
      length(
        which(
          apply(
            delrc(testmatrix1, climit = 1, rlimit = 0),
            2,
            sum) == 0)
        ) > 0,
      FALSE
    )
  }
)

test_that(
  "delrc removes the correct amount of columns and rows
  depending on the settings",  {
    expect_equal(
      length(
        which(
          apply(
            delrc(testmatrix1, climit = 3, rlimit = 2),
            1, sum) > 0)
        ),
      3
    )
    expect_equal(
      length(
        which(
          apply(
            delrc(testmatrix1, climit = 3, rlimit = 2),
            2,
            sum) > 0)
        ),
      2
    )
  }
)

test_that(
  "delrc keeps the correct rows and columns",  {
    expect_equal(
      colnames(delrc(testmatrix1, climit = 3, rlimit = 2)),
      c("c1", "c4")
    )
    expect_equal(
      rownames(delrc(testmatrix1, climit = 3, rlimit = 2)),
      c("2", "3", "4")
    )
  }
)