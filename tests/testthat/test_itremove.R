context("Tests of function itremove")

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
  "the output of itremove is a data.frame or FALSE",  {
    expect_equal(
      is.data.frame(itremove(testmatrix1, cmin = 3, rmin = 2)),
      TRUE
    )
    expect_equal(
      itremove(testmatrix2, cmin = 3, rmin = 2),
      NA
    )
  }
)

test_that(
  "the output of itremove with minimalistic settings is
  a data.frame without empty rows or columns ",  {
    expect_equal(
      length(
        which(
          apply(
            itremove(testmatrix1, cmin = 0, rmin = 1),
            1,
            sum) == 0)
        ) > 0,
      FALSE
    )
    expect_equal(
      length(
        which(
          apply(
            itremove(testmatrix1, cmin = 1, rmin = 0),
            2,
            sum) == 0)
        ) > 0,
      FALSE
    )
  }
)

test_that(
  "itremove removes the correct amount of columns and rows
  depending on the settings",  {
    expect_equal(
      length(
        which(
          apply(
            itremove(testmatrix1, cmin = 3, rmin = 2),
            1, sum) > 0)
        ),
      3
    )
    expect_equal(
      length(
        which(
          apply(
            itremove(testmatrix1, cmin = 3, rmin = 2),
            2,
            sum) > 0)
        ),
      2
    )
  }
)

test_that(
  "itremove keeps the correct rows and columns",  {
    expect_equal(
      colnames(itremove(testmatrix1, cmin = 3, rmin = 2)),
      c("c1", "c4")
    )
    expect_equal(
      rownames(itremove(testmatrix1, cmin = 3, rmin = 2)),
      c("2", "3", "4")
    )
  }
)

test_that("check_num_nonzero() checks correctly", {
  testvector <- c(0, 0, 1, 0)
  expect_true(check_num_nonzero(testvector, 1))
  expect_false(check_num_nonzero(testvector, 2))
})

test_that("itremove() ends up with correct number of rows and columns", {
  a <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0), nrow = 4)
  b <- itremove(a, 1)
  expect_equal(dim(b), c(3, 3))
})

test_that("itremove() returns the same class as input", {
  a <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0), nrow = 4)
  a.class <- class(a)
  b <- itremove(a, 1)
  b.class <- class(b)

  expect_equal(a.class, b.class)

  a <- as.data.frame(a)
  a.class <- class(a)
  b <- itremove(a, 1)
  b.class <- class(b)

  expect_equal(a.class, b.class)
})


