context("Utility Functions")

test_that("check_num_nonzero() checks correctly", {
  testvector <- c(0,0,1,0)
  expect_true(check_num_nonzero(testvector,1))
  expect_false(check_num_nonzero(testvector,2))
})

test_that("itremove() ends up with correct number of rows and columns", {
  a <- matrix(c(0,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0),nrow=4)
  b <- itremove(a,1)
  expect_equal(dim(b),c(3,3))
})

test_that("itremove() returns the same class as input", {
  a <- matrix(c(0,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0),nrow=4)
  a.class <- class(a)
  b <- itremove(a,1)
  b.class <- class(b)

  expect_equal(a.class,b.class)

  a <- as.data.frame(a)
  a.class <- class(a)
  b <- itremove(a,1)
  b.class <- class(b)

  expect_equal(a.class,b.class)
})
