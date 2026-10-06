# simulate_data.R
# Data generators for demo/testing. Each simulator supports injecting
# specific assumption violations so the app's diagnostic tests have
# something real to detect -- this is what lets a user actually SEE a
# CHECK state in the Assumptions Check panel, not just a PASS every time.

#' Simulate OLS Data with Optional Violations
#'
#' @param n Integer. Number of observations.
#' @param heteroskedastic Logical. If TRUE, error variance scales with X1.
#' @param multicollinear Logical. If TRUE, X2 is highly correlated with X1.
#' @param non_normal Logical. If TRUE, errors are drawn from a heavy-tailed
#'   t-distribution (df = 3) instead of Normal -- this is what lets the
#'   Shapiro-Wilk / Jarque-Bera box actually show CHECK. With the default
#'   Gaussian errors, residual normality tests will essentially always PASS,
#'   so there was previously no way to demonstrate that diagnostic failing.
#' @param seed Optional integer. If given, sets the RNG seed so the data is
#'   reproducible; leave as NULL for a fresh random draw each call.
#' @return A data.frame containing Y, X1, and X2.
#' @export
simulate_ols_data <- function(n = 500, heteroskedastic = FALSE,
                              multicollinear = FALSE, non_normal = FALSE,
                              seed = NULL) {
  
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
  
  # Error distribution: heteroskedastic (variance scales with X1),
  # non-normal (heavy-tailed t), both, or plain Gaussian.
  if (heteroskedastic && non_normal) {
    errors <- rt(n, df = 3) * (1 + 1.5 * abs(X1)) / sqrt(3)
  } else if (heteroskedastic) {
    errors <- rnorm(n, mean = 0, sd = 1 + 1.5 * abs(X1))
  } else if (non_normal) {
    # t(3) scaled to have a comparable spread to the default sd = 2 errors.
    errors <- rt(n, df = 3) * 2 / sqrt(3)
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
#' @param unit_root Logical. If TRUE, Y (and X1) are generated as genuine
#'   random walks (AR coefficient = 1) instead of a stationary process --
#'   this is what lets the ADF box actually show CHECK (non-stationary).
#'   With the default stationary AR(0.6) disturbance, the ADF test will
#'   essentially always reject the unit root and PASS, so there was
#'   previously no way to demonstrate that diagnostic failing.
#' @param seed Optional integer for reproducibility.
#' @param phi Numeric AR(1) coefficient for the error process. Ignored
#'   (fixed at 1) when unit_root = TRUE.
#' @return A data.frame containing t, Y, X1, and X2.
#' @export
simulate_ts_data <- function(n = 500, heteroskedastic = FALSE,
                             multicollinear = FALSE, unit_root = FALSE,
                             seed = NULL, phi = 0.6) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  t <- seq_len(n)
  
  if (unit_root) {
    # Genuine random walk: X1_t = X1_{t-1} + innovation. No stationary mean
    # to revert to, so the ADF test should fail to reject the unit-root
    # null -- exactly the CHECK state the diagnostic exists to catch.
    X1 <- cumsum(rnorm(n, mean = 0, sd = 1.2)) + 5
  } else {
    # Add mild persistence to predictors so the series looks time-like.
    X1 <- as.numeric(arima.sim(model = list(ar = 0.4), n = n, sd = 1.2)) + 5
  }
  
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.35)
  } else if (unit_root) {
    X2 <- cumsum(rnorm(n, mean = 0, sd = 1.5)) + 10
  } else {
    X2 <- as.numeric(arima.sim(model = list(ar = 0.25), n = n, sd = 1.5)) + 10
  }
  
  # Innovation process, optionally heteroskedastic.
  if (heteroskedastic) {
    innov <- rnorm(n, mean = 0, sd = 0.7 + 0.45 * abs(X1 - mean(X1)))
  } else {
    innov <- rnorm(n, mean = 0, sd = 1.2)
  }
  
  # Error process: random walk (unit root) or stationary AR(1).
 e <- numeric(n)
e[1] <- innov[1]
effective_phi <- if (unit_root) 1 else phi
if (n > 1) {
  for (i in 2:n) {
    e[i] <- effective_phi * e[i - 1] + innov[i]
  }
}
  
  Y <- 10 + (2.5 * X1) - (1.5 * X2) + e
  
  data.frame(t = t, Y = Y, X1 = X1, X2 = X2)
}


#' Simulate Binary (Logistic) Data with Optional Violations
#'
#' Generates a binary outcome from a known logistic data-generating process,
#' Y ~ Bernoulli(plogis(b0 + b1*X1 + b2*X2)), with optional violations that
#' the diag_lm logistic diagnostics are designed to detect.
#'
#' @param n Integer. Number of observations.
#' @param multicollinear Logical. If TRUE, X2 is highly correlated with X1.
#' @param nonlinear Logical. If TRUE, an omitted quadratic term in X1 is added
#'   to the true linear predictor, so the fitted (linear) logit model is
#'   mis-specified. Detectable via the Hosmer-Lemeshow test / binned residual
#'   plot.
#' @param imbalance Logical. If TRUE, shifts the intercept so the positive
#'   class is rare (~10 percent), producing a class-imbalanced outcome.
#' @param seed Optional integer. If given, sets the RNG seed so the data is
#'   reproducible; leave as NULL for a fresh random draw each call.
#' @return A data.frame containing Y (0/1), X1, and X2.
#' @export
simulate_logit_data <- function(n = 500, multicollinear = FALSE,
                                nonlinear = FALSE, imbalance = FALSE,
                                seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  X1 <- rnorm(n, mean = 0, sd = 1.5)
  
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.3)
  } else {
    X2 <- rnorm(n, mean = 0, sd = 1.5)
  }
  
  intercept <- if (imbalance) -3 else 0
  
  linear_predictor <- intercept + (1.4 * X1) - (1.1 * X2)
  
  # An omitted non-linear term breaks the linearity-in-the-logit assumption
  # while the fitted model (Y ~ X1 + X2) stays linear, so the mis-specification
  # shows up in the Hosmer-Lemeshow test and the binned residual plot.
  if (nonlinear) {
    linear_predictor <- linear_predictor + 0.6 * X1^2
  }
  
  prob <- plogis(linear_predictor)
  Y <- rbinom(n, size = 1, prob = prob)
  
  data.frame(Y = Y, X1 = X1, X2 = X2)
}


#' Simulate Panel Data with Optional Violations
#'
#' Generates a balanced panel (entity x time) dataset with a true
#' entity-level effect built in. This is the piece the app's panel
#' diagnostics (Hausman test, LSDV/Within/Random Effects, poolability,
#' panel Breusch-Godfrey) previously had no simulated-data path to
#' demonstrate against -- every panel feature required an uploaded CSV.
#'
#' Column names are chosen ("entity", "year") to be auto-detected by the
#' app's Cross-Section Index / Time Index guessing logic without the user
#' needing to pick them manually.
#'
#' @param n_entities Integer. Number of cross-sectional units (e.g. firms).
#' @param n_periods Integer. Number of time periods per entity.
#' @param correlated_effects Logical. This is the single most important
#'   parameter for demonstrating the Hausman test. If TRUE, each entity's
#'   fixed effect is constructed to be correlated with that entity's average
#'   X -- exactly the condition under which Random Effects becomes
#'   inconsistent (biased). The Hausman test should then favor Fixed
#'   Effects. If FALSE, the entity effect is drawn independently of X, so
#'   Random Effects remains consistent (and more efficient) -- the Hausman
#'   test should not reject it.
#' @param serial_correlation Logical. If TRUE, errors follow an AR(1)
#'   process within each entity (but are independent across entities),
#'   inducing serial correlation detectable via the panel Breusch-Godfrey
#'   test. If FALSE, errors are independent within each entity too.
#' @param heteroskedastic Logical. If TRUE, error variance scales with X.
#' @param seed Optional integer for reproducibility.
#' @return A data.frame containing entity, year, Y, X.
#' @export
simulate_panel_data <- function(n_entities = 30, n_periods = 8,
                                correlated_effects = TRUE,
                                serial_correlation = FALSE,
                                heteroskedastic = FALSE,
                                seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  entity <- rep(seq_len(n_entities), each = n_periods)
  year   <- rep(seq_len(n_periods), times = n_entities) + 2010
  n <- n_entities * n_periods
  
  # Entity-level average of X -- used both to build X itself (so entities
  # differ systematically, not just noise) and, when correlated_effects is
  # TRUE, to correlate the fixed effect with X.
  entity_x_mean <- rnorm(n_entities, mean = 5, sd = 2)
  X <- entity_x_mean[entity] + rnorm(n, mean = 0, sd = 1.5)
  
  # True entity fixed effect.
  if (correlated_effects) {
    alpha_i <- 0.8 * (entity_x_mean - mean(entity_x_mean)) + rnorm(n_entities, sd = 1)
  } else {
    alpha_i <- rnorm(n_entities, mean = 0, sd = 3)
  }
  alpha <- alpha_i[entity]
  
  # Errors: optionally AR(1) within each entity (never across entities),
  # optionally heteroskedastic in X.
  if (serial_correlation) {
    e <- numeric(n)
    for (i in seq_len(n_entities)) {
      idx <- which(entity == i)
      innov <- if (heteroskedastic) {
        rnorm(n_periods, sd = 1 + 0.5 * abs(X[idx] - mean(X[idx])))
      } else {
        rnorm(n_periods, sd = 1.5)
      }
      ei <- numeric(n_periods)
      ei[1] <- innov[1]
      if (n_periods > 1) {
      for (tt in 2:n_periods) {
      ei[tt] <- 0.6 * ei[tt - 1] + innov[tt]
  }
}
e[idx] <- ei
    }
  } else if (heteroskedastic) {
    e <- rnorm(n, mean = 0, sd = 1 + 0.5 * abs(X - mean(X)))
  } else {
    e <- rnorm(n, mean = 0, sd = 1.8)
  }
  
  Y <- 5 + alpha + (2 * X) + e
  
  data.frame(entity = entity, year = year, Y = Y, X = X)
}