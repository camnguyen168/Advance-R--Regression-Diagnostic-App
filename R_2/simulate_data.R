# simulate_data.R
# Small data generator for demo/testing.
# You can optionally inject heteroskedasticity or multicollinearity.

#' Simulate OLS Data with Optional Violations
#'
#' @param n Integer. Number of observations.
#' @param heteroskedastic Logical. If TRUE, error variance scales with X1.
#' @param multicollinear Logical. If TRUE, X2 is highly correlated with X1.
#' @param seed Optional integer. If given, sets the RNG seed so the data is
#'   reproducible; leave as NULL for a fresh random draw each call.
#' @return A data.frame containing Y, X1, and X2.
#' @export
simulate_ols_data <- function(n = 500, heteroskedastic = FALSE,
                              multicollinear = FALSE, seed = NULL) {
  
  # Use a fixed seed when reproducibility is needed.
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # First predictor.
  X1 <- rnorm(n, mean = 5, sd = 2)
  
  # If requested, make X2 highly correlated with X1.
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.5)
  } else {
    X2 <- rnorm(n, mean = 10, sd = 3)
  }
  
  # If requested, error spread grows with X1.
  if (heteroskedastic) {
    errors <- rnorm(n, mean = 0, sd = 1 + 1.5 * abs(X1))
  } else {
    errors <- rnorm(n, mean = 0, sd = 2)
  }
  
  # Data generating process.
  Y <- 10 + (2.5 * X1) - (1.5 * X2) + errors
  
  data.frame(Y = Y, X1 = X1, X2 = X2)
}


#' Simulate Time-Series Data with Optional Violations
#'
#' Generates a univariate time index and predictors with optional
#' multicollinearity. The disturbance follows an AR(1) process so residual
#' autocorrelation is present in a realistic way.
#'
#' @param n Integer. Number of time points.
#' @param heteroskedastic Logical. If TRUE, innovation variance scales with X1.
#' @param multicollinear Logical. If TRUE, X2 is highly correlated with X1.
#' @param seed Optional integer for reproducibility.
#' @param phi Numeric AR(1) coefficient for the error process.
#' @return A data.frame containing t, Y, X1, and X2.
#' @export
simulate_ts_data <- function(n = 500, heteroskedastic = FALSE,
                             multicollinear = FALSE, seed = NULL,
                             phi = 0.6) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  t <- seq_len(n)
  
  # Add mild persistence to predictors so the series looks time-like.
  X1 <- as.numeric(arima.sim(model = list(ar = 0.4), n = n, sd = 1.2)) + 5
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.35)
  } else {
    X2 <- as.numeric(arima.sim(model = list(ar = 0.25), n = n, sd = 1.5)) + 10
  }
  
  # Innovation process, optionally heteroskedastic.
  if (heteroskedastic) {
    innov <- rnorm(n, mean = 0, sd = 0.7 + 0.45 * abs(X1 - mean(X1)))
  } else {
    innov <- rnorm(n, mean = 0, sd = 1.2)
  }
  
  # AR(1) error process to induce serial correlation.
  e <- numeric(n)
  e[1] <- innov[1]
  for (i in 2:n) {
    e[i] <- phi * e[i - 1] + innov[i]
  }
  
  Y <- 10 + (2.5 * X1) - (1.5 * X2) + e
  
  data.frame(t = t, Y = Y, X1 = X1, X2 = X2)
}
