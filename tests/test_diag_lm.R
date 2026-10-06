# Quick smoke checks for diag_lm.
# Not a full test framework, just a script to catch obvious regressions.
# Run from the project root with: source("tests/test_diag_lm.R")

source("R/diag_lm_class.R")

# Baseline model with multiple predictors.
cat("== Test 1: Multi-predictor model ==\n")
my_model <- lm(mpg ~ wt + hp + disp, data = mtcars)
my_diag_obj <- new_diag_lm(my_model)

stopifnot(inherits(my_diag_obj, "diag_lm"))
cat("Class check passed:", class(my_diag_obj), "\n\n")

print(my_diag_obj)

cat("\n== Test: summary() returns a gt table ==\n")
tbl <- summary(my_diag_obj)
stopifnot(inherits(tbl, "gt_tbl"))
cat("summary() returned a gt_tbl object.\n")

cat("\n== Test: plot() returns a ggplot object ==\n")
for (t in c("residuals", "qq", "scale_location", "histogram")) {
  p <- plot(my_diag_obj, type = t)
  stopifnot(inherits(p, "ggplot"))
  cat("plot(type =", t, ") returned a ggplot object.\n")
}

cat("\n== Test: unknown plot type errors ==\n")
bad_type <- tryCatch(
  plot(my_diag_obj, type = "qqplot"),
  error = function(e) conditionMessage(e)
)
cat("Caught expected error:", bad_type, "\n")

cat("\n== Test 2: Single-predictor model (VIF should be NA) ==\n")
simple_model <- lm(mpg ~ wt, data = mtcars)
simple_diag <- new_diag_lm(simple_model)
stopifnot(is.na(simple_diag$diagnostics$vif_scores))
cat("VIF correctly set to NA for single predictor.\n\n")

cat("== Test 3: Input validation (should error) ==\n")
bad_input <- tryCatch(
  new_diag_lm(mtcars),
  error = function(e) conditionMessage(e)
)
cat("Caught expected error:", bad_input, "\n")

cat("\n== Test 4: Logistic regression (glm binomial) ==\n")
source("R/simulate_data.R")
logit_df <- simulate_logit_data(n = 400, seed = 42)
logit_model <- glm(Y ~ X1 + X2, data = logit_df, family = binomial())
logit_diag <- new_diag_lm(logit_model, data_type = "logistic")

stopifnot(inherits(logit_diag, "diag_lm"))
stopifnot(identical(logit_diag$data_type, "binary"))
cat("Class / data_type checks passed.\n")

stopifnot(!is.na(logit_diag$diagnostics$logistic$hl_pvalue))
stopifnot(!is.na(logit_diag$diagnostics$logistic$auc))
stopifnot(!is.na(logit_diag$diagnostics$logistic$mcfadden_r2))
cat("Logistic diagnostics (Hosmer-Lemeshow / AUC / McFadden R2) computed.\n")

logit_tbl <- summary(logit_diag)
stopifnot(inherits(logit_tbl, "gt_tbl"))
cat("summary() returned a gt_tbl object for the logistic model.\n")

for (t in c("binned_residuals", "roc", "calibration")) {
  p <- plot(logit_diag, type = t)
  stopifnot(inherits(p, "ggplot"))
  cat("plot(type =", t, ") returned a ggplot object.\n")
}

cat("\n== Test 5: Logistic plot types reject non-logistic models ==\n")
cs_model <- lm(mpg ~ wt + hp, data = mtcars)
cs_diag <- new_diag_lm(cs_model)
bad_plot <- tryCatch(
  plot(cs_diag, type = "roc"),
  error = function(e) conditionMessage(e)
)
cat("Caught expected error:", bad_plot, "\n")

cat("\n== Test 6: Time-series-only plot types (residuals_time / pacf) ==\n")
ts_model <- lm(mpg ~ wt + hp, data = mtcars)
ts_diag <- new_diag_lm(ts_model, data_type = "ts")
for (t in c("residuals_time", "pacf")) {
  p <- plot(ts_diag, type = t)
  stopifnot(inherits(p, "ggplot"))
  cat("plot(type =", t, ") returned a ggplot object.\n")
}

cat("\nAll tests passed.\n")
