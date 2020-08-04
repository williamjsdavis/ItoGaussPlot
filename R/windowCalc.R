stdWindowSlopeCast <- function(timeIn, ampIn, windows) {
  # Uniform or non-uniform sampling?
  Nt <- length(timeIn)
  equalTsteps <-
    diff(range(diff(timeIn))) < .Machine$double.eps ^ 0.5
  if (equalTsteps) {
    dt <- time[2] - time[1]
    stepHandle <-
      function(w)
        stdWindowSlopeUniform(timeIn, ampIn, w, dt, Nt)
  } else {
    stepHandle <-
      function(w)
        stdWindowSlopeNonUniform(timeIn, ampIn, w, Nt)
  }

  # Scalar or vector input?
  N <- length(windows)
  if (N == 1) {
    stepHandle(windows)
  } else {
    out <- windows
    for (i in 1:N) {
      out[i] <- stepHandle(windows[i])
    }
    out
  }
}
stdWindowSlopeUniform <-
  function(timeIn, ampIn, windowLength, dt, Nt) {
    windowSteps <- floor(windowLength / dt)
    Nwindows <- floor(Nt / windowSteps)
    iStart <- seq(1, Nt, windowSteps)

    n <- windowSteps
    N <- Nwindows
    sum <- 0
    sumSq <- 0

    for (i in 1:Nwindows) {
      ii <- iStart[i]
      sumX <- 0
      sumY <- 0
      sumXY <- 0
      sumXX <- 0
      for (j in 1:windowSteps) {
        sumX <- sumX + timeIn[ii]
        sumY <- sumY + ampIn[ii]
        sumXY <- sumXY + timeIn[ii] * ampIn[ii]
        sumXX <- sumXX + timeIn[ii] * timeIn[ii]
        ii <- ii + 1
      }
      # Slope of windowed section
      B <- (n * sumXY - sumX * sumY) / (n * sumXX - sumX * sumX)

      sum <- sum + B
      sumSq <- sumSq + B * B
    }
    # Variance of all slopes
    varB <- (sumSq - (sum * sum) / N) / (N - 1)
    sqrt(varB)
  }
stdWindowSlopeNonUniform <-
  function(timeIn, ampIn, windowLength, Nt) {
    Nwindows = floor(tail(timeIn, n = 1) / windowLength)

    tStart <- 0
    iStart <- 1L
    iEnd <- findInterval(windowLength, timeIn)

    N <- Nwindows
    sum <- 0
    sumSq <- 0
    for (i in 1:Nwindows) {
      n <- iEnd - iStart
      ii <- iStart
      sumX <- 0
      sumY <- 0
      sumXY <- 0
      sumXX <- 0
      for (j in 1:n) {
        sumX <- sumX + timeIn[ii]
        sumY <- sumY + ampIn[ii]
        sumXY <- sumXY + timeIn[ii] * ampIn[ii]
        sumXX <- sumXX + timeIn[ii] * timeIn[ii]
        ii <- ii + 1
      }
      # Slope of windowed section
      B <- (n * sumXY - sumX * sumY) / (n * sumXX - sumX * sumX)

      sum <- sum + B
      sumSq <- sumSq + B * B

      tStart <- tStart + windowLength
      iStart <- iEnd
      iEnd <- findInterval(tStart + windowLength, timeIn)
    }
    # Variance of all slopes
    varB <- (sumSq - (sum * sum) / N) / (N - 1)
    sqrt(varB)
  }
