test_that("Uniform time, scalar output", {
  timeU <- seq(0, 10, len = 1000)
  amplitude <- sin(timeU)
  out <- stdWindowSlopeCast(timeU, amplitude, 0.1)
  expect_equal(length(out), 1L)

  timeU <- seq(1, 20, len = 2000)
  amplitude <- sin(timeU)
  out <- stdWindowSlopeCast(timeU, amplitude, 0.2)
  expect_equal(length(out), 1L)
})
test_that("Uniform time, vector output", {
  timeU <- seq(0, 10, len = 1000)
  amplitude <- sin(timeU)
  N <- 5L
  out <- stdWindowSlopeCast(timeU, amplitude, seq(0.1, 0.8, len = N))
  expect_equal(length(out), N)

  timeU <- seq(1, 20, len = 2000)
  amplitude <- sin(timeU)
  N <- 10L
  out <- stdWindowSlopeCast(timeU, amplitude, seq(0.2, 0.9, len = N))
  expect_equal(length(out), N)
})
test_that("Non-uniform time, scalar output", {
  timeU <- seq(0, 10, len = 1000)
  timeExp <- 0.001 * exp(timeU)
  amplitude <- sin(timeExp)
  out <- stdWindowSlopeCast(timeExp, amplitude, 10)
  expect_equal(length(out), 1L)

  timeU <- seq(1, 20, len = 2000)
  timeExp <- 0.001 * exp(timeU)
  amplitude <- sin(timeExp)
  out <- stdWindowSlopeCast(timeExp, amplitude, 20)
  expect_equal(length(out), 1L)
})
test_that("Non-uniform time, vector output", {
  timeU <- seq(0, 10, len = 1000)
  timeExp <- 0.001 * exp(timeU)
  amplitude <- sin(timeExp)
  N <- 5L
  out <- stdWindowSlopeCast(timeExp, amplitude, seq(0.1, 0.8, len = N))
  expect_equal(length(out), N)

  timeU <- seq(1, 20, len = 2000)
  timeExp <- 0.001 * exp(timeU)
  amplitude <- sin(timeExp)
  N <- 10L
  out <- stdWindowSlopeCast(timeExp, amplitude, seq(0.2, 0.9, len = N))
  expect_equal(length(out), N)
})
