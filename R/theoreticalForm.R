OUanalytic <- function(windowLength, gamma, D, approx = "none") {
  if (approx == "none") {
    tau_gamma <- 1 / gamma
    wND <- windowLength / tau_gamma
    wND3 <- wND ^ -3
    wND4 <- wND ^ -4
    wND5 <- wND ^ -5
    wND6 <- wND ^ -6
    f_wND <- wND3 - 3 * wND4 + 12 * wND6 -
      exp(-wND) * (3 * wND4 + 12 * wND5 + 12 * wND6)
    sqrt(24 * gamma * D * f_wND)
  } else if (approx == "smallW") {
    sqrt(2.4 * D / windowLength)
  } else {
    print("Unrecognized approximation")
  }
}
OUanalyticSmallW <-
  function(windowLength, gamma, D)
    sqrt(2.4 * D / windowLength)
