test_that("Scalar output", {
  out <- OUanalytic(0.1,1,1)
  expect_equal(length(out), 1L)
  out <- OUanalytic(0.1,1.5,2)
  expect_equal(length(out), 1L)
})
test_that("Vector output", {
  N <- 5L
  out <- OUanalytic(10^seq(-1,2,len=N),1,1)
  expect_equal(length(out), N)
  N <- 10L
  out <- OUanalytic(10^seq(-1,2,len=N),1.5,2)
  expect_equal(length(out), N)
})
test_that("Scalar output, small w approx.", {
  out <- OUanalyticSmallW(0.1,1,1)
  expect_equal(length(out), 1L)
  out <- OUanalyticSmallW(0.1,1.5,2)
  expect_equal(length(out), 1L)
})
test_that("Vector output, small w approx.", {
  N <- 5L
  out <- OUanalyticSmallW(10^seq(-1,2,len=N),1,1)
  expect_equal(length(out), N)
  N <- 10L
  out <- OUanalyticSmallW(10^seq(-1,2,len=N),1.5,2)
  expect_equal(length(out), N)
})
