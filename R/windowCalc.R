stdWindowSlopeCast <- function(timeIn, ampIn, windows) {
  # Uniform or non-uniform sampling?
  Nt <- length(timeIn)
  equalTsteps <-
    diff(range(diff(timeIn))) < .Machine$double.eps ^ 0.5
  if (equalTsteps) {
    dt <- timeIn[2] - timeIn[1]
    stepHandle <-
      function(w)
        stdWindowSlopeUniform(timeIn, ampIn, w, dt, Nt)
  } else {
    stepHandle <-
      function(w)
        stdWindowSlopeNonUniform2(timeIn, ampIn, w, Nt)
  }

  # Scalar or vector input?
  N <- length(windows)
  if (N == 1) {
    stepHandle(windows)
  } else {
    #out <- foreach(i = windows, .combine='c') %dopar% {
      #stepHandle(i)
    #}
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

    # Call C++ subroutines
    sqrt(varSlope(timeIn, ampIn, n, N))

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

      # Call C++ subroutine
      B <- slope(timeIn, ampIn, ii-1, n)

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
stdWindowSlopeNonUniform2 <-
  function(timeIn, ampIn, windowLength, Nt) {
    tEnd <- tail(timeIn, n = 1)
    Nwindows <- floor(tEnd / windowLength)
    N <- Nwindows

    # Call C++ subroutines
    sqrt(varSlopeNU(timeIn, ampIn, windowLength, N))

  }
