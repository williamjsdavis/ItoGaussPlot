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

    # Call C++ subroutines
    sqrt(varSlope(timeIn, ampIn, n, N))

  }
stdWindowSlopeNonUniform <-
  function(timeIn, ampIn, windowLength, Nt) {
    tEnd <- tail(timeIn, n = 1)
    Nwindows <- floor(tEnd / windowLength)
    N <- Nwindows

    # Call C++ subroutines
    sqrt(varSlopeNU(timeIn, ampIn, windowLength, N))

  }
