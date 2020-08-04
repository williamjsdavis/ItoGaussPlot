test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("Uniform time, scalar output", {
  time <- seq(0, 10, len = 1000)
  amplitude <- sin(time)
  out <- stdWindowSlopeCast(time, amplitude, 0.1)
  expect_equal(length(out), 1L)
})
test_that("Uniform time, vector output", {
  time <- seq(0, 10, len = 1000)
  amplitude <- sin(time)
  N <- 5L
  out <- stdWindowSlopeCast(time, amplitude, seq(0.1, 0.8, len = N))
  expect_equal(length(out), N)
})
test_that("Non-uniform time, scalar output", {
  time <- seq(0, 10, len = 1000)
  timeExp <- 0.001 * exp(time)
  amplitude <- sin(timeExp)
  out <- stdWindowSlopeCast(timeExp, amplitude, 10)
  expect_equal(length(out), 1L)
})
test_that("Non-uniform time, vector output", {
  time <- seq(0, 10, len = 1000)
  timeExp <- 0.001 * exp(time)
  N <- 5L
  out <- stdWindowSlopeCast(timeExp, amplitude, seq(0.1, 0.8, len = N))
  expect_equal(length(out), N)
})
