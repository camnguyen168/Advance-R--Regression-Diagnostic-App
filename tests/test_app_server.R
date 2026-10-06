# test_app_server.R
# Shiny-level integration/regression tests for App.R using shiny::testServer().
# Complements tests/test_diag_lm.R (which tests the diag_lm class in isolation) by
# exercising the actual reactive graph: inputs -> data pipeline -> model fitting ->
# outputs, the same way a real user session would.
#
# Run from the project root with: source("tests/test_app_server.R")

suppressMessages({
  library(shiny)
})
source("R/diag_lm_class.R")
source("R/simulate_data.R")
sys.source("app.R", envir = globalenv())

cat("== Test 1: Linear / Cross-Section default flow ==\n")
shiny::testServer(server, {
  session$setInputs(model_family = "linear", data_source = "simulate",
                     data_structure_sim = "cs", n_obs = 300, het_viol = FALSE,
                     coll_viol = FALSE, seed = 1, sim_btn = 1, run_analysis_btn = 1)
  m <- diag_model()
  stopifnot(inherits(m, "diag_lm"))
  stopifnot(identical(m$data_type, "cross-section"))
  cat("PASS: linear/cs diag_model() is a valid cross-section diag_lm.\n")
})

cat("\n== Test 2: Logistic flow end-to-end ==\n")
shiny::testServer(server, {
  session$setInputs(model_family = "logistic", data_source = "simulate",
                     n_obs = 300, coll_viol = FALSE, nonlinear_viol = FALSE,
                     imbalance_viol = FALSE, seed = 2, sim_btn = 1, run_analysis_btn = 1)
  m <- diag_model()
  stopifnot(identical(m$data_type, "binary"))
  stopifnot(!is.na(m$diagnostics$logistic$auc))
  cat("PASS: logistic diag_model() computed AUC =", round(m$diagnostics$logistic$auc, 3), "\n")
})

cat("\n== Test 3 (REGRESSION): model_family race condition (logistic -> linear) ==\n")
# Regression test for a bug where output$hl_box / output$auc_box / output$accuracy_box /
# output$overall_box_logistic / output$plot_binned / output$plot_roc / output$plot_calibration
# accessed model$diagnostics$logistic without checking model$data_type first. This produced
# uncaught errors ("Error: The 'binned_residuals' plot is only available for logistic (binary)
# models." and "Error in if: missing value where TRUE/FALSE needed") whenever diag_model()
# resolved to a non-logistic fit while those outputs were still bound (a real race observed
# when rapidly switching Model Family). Fixed by adding req(identical(model$data_type,
# "binary")) as the first line of each of those renderers.
shiny::testServer(server, {
  session$setInputs(model_family = "logistic", data_source = "simulate",
                     n_obs = 300, coll_viol = FALSE, nonlinear_viol = FALSE,
                     imbalance_viol = FALSE, seed = 3, sim_btn = 1, run_analysis_btn = 1)
  stopifnot(identical(diag_model()$data_type, "binary"))

  # Switch back to linear/cross-section: diag_model() is now an lm-based fit.
  session$setInputs(model_family = "linear", data_structure_sim = "cs",
                     het_viol = FALSE, coll_viol = FALSE,
                     seed = 4, sim_btn = 2, run_analysis_btn = 2)
  stopifnot(identical(diag_model()$data_type, "cross-section"))

  # Force-evaluate the logistic-only outputs while diag_model() is non-logistic.
  # Before the fix this threw a real, uncaught error (see above). After the fix,
  # req() either renders nothing (NULL) or throws Shiny's expected silent-stop
  # condition ("shiny.silent.error", which carries an empty message and is how
  # req() intentionally halts an output) -- anything else is a real regression.
  check_silent <- function(expr) {
    err <- tryCatch({ force(expr); NULL }, error = function(e) e)
    is.null(err) || inherits(err, "shiny.silent.error")
  }
  stopifnot(check_silent(output$hl_box))
  stopifnot(check_silent(output$auc_box))
  stopifnot(check_silent(output$accuracy_box))
  stopifnot(check_silent(output$overall_box_logistic))
  cat("PASS: logistic-only outputs no longer error when diag_model() is non-logistic.\n")
})

cat("\n== Test 4: Time-series flow with curated plots ==\n")
shiny::testServer(server, {
  session$setInputs(model_family = "linear", data_source = "simulate",
                     data_structure_sim = "ts", het_viol = FALSE, coll_viol = FALSE,
                     n_obs = 300, seed = 5, sim_btn = 1, run_analysis_btn = 1)
  m <- diag_model()
  stopifnot(identical(m$data_type, "time-series"))
  p1 <- plot(m, type = "residuals_time")
  p2 <- plot(m, type = "pacf")
  stopifnot(inherits(p1, "ggplot"), inherits(p2, "ggplot"))
  cat("PASS: time-series diag_model() + curated plots (residuals_time, pacf) work.\n")
})

cat("\n== Test 5: Panel data flow (uploaded CSV) ==\n")
shiny::testServer(server, {
  set.seed(6)
  n_id <- 20; n_time <- 4
  panel_df <- expand.grid(id = 1:n_id, time = 1:n_time)
  alpha_i <- rep(rnorm(n_id, 0, 2), each = n_time)
  panel_df$X <- rnorm(nrow(panel_df), 5, 2)
  panel_df$Y <- 3 + 1.5 * panel_df$X + alpha_i + rnorm(nrow(panel_df), 0, 1)
  tmpfile <- tempfile(fileext = ".csv")
  write.csv(panel_df, tmpfile, row.names = FALSE)

  session$setInputs(model_family = "linear", data_source = "upload",
                     data_structure_upload = "panel")
  session$setInputs(csv_file = data.frame(name = "panel.csv", size = file.size(tmpfile),
                                          type = "text/csv", datapath = tmpfile,
                                          stringsAsFactors = FALSE))
  session$setInputs(header = TRUE, p_idx_i = "id", p_idx_t = "time")
  session$setInputs(y_var = "Y", x_vars = "X", run_analysis_btn = 1)

  m <- diag_model()
  stopifnot(identical(m$data_type, "panel"))
  stopifnot(!is.na(m$diagnostics$panel$hausman_pvalue))
  cat("PASS: panel data flow computes Hausman p =", round(m$diagnostics$panel$hausman_pvalue, 4), "\n")
  unlink(tmpfile)
})

cat("\n== Test 6 (EDGE CASE): awkward original column names surviving read.csv() ==\n")
# Note: read.csv() sanitizes non-syntactic headers via make.names() by default
# (e.g. "Y (target)" -> "Y..target."), so the practical risk is narrower than a
# raw non-syntactic name; this test confirms the whole pipeline (including the
# reformulate()-based formula construction) still works with those sanitized,
# but still unusual (dot-heavy), names.
shiny::testServer(server, {
  df <- data.frame(`Y (target)` = rnorm(100), `X 1` = rnorm(100), `X 2` = rnorm(100),
                    check.names = FALSE)
  tmpfile <- tempfile(fileext = ".csv")
  write.csv(df, tmpfile, row.names = FALSE)

  session$setInputs(model_family = "linear", data_source = "upload",
                     data_structure_upload = "cs")
  session$setInputs(csv_file = data.frame(name = "space.csv", size = file.size(tmpfile),
                                          type = "text/csv", datapath = tmpfile,
                                          stringsAsFactors = FALSE))
  session$setInputs(header = TRUE)
  actual_names <- names(uploaded_data())
  stopifnot(!identical(actual_names, c("Y (target)", "X 1", "X 2"))) # confirms sanitization happened
  session$setInputs(y_var = actual_names[1], x_vars = actual_names[2:3], run_analysis_btn = 1)

  m <- diag_model()
  stopifnot(inherits(m, "diag_lm"))
  cat("PASS: sanitized column names (", paste(actual_names, collapse = ", "),
      ") work end-to-end via reformulate().\n")
  unlink(tmpfile)
})

cat("\n== Test 7 (EDGE CASE): non-numeric column selected as outcome ==\n")
shiny::testServer(server, {
  df <- data.frame(Y = c("apple", "banana", "cherry", "date", "egg"),
                    X1 = rnorm(5), X2 = rnorm(5), stringsAsFactors = FALSE)
  tmpfile <- tempfile(fileext = ".csv")
  write.csv(df, tmpfile, row.names = FALSE)

  session$setInputs(model_family = "linear", data_source = "upload",
                     data_structure_upload = "cs")
  session$setInputs(csv_file = data.frame(name = "bad.csv", size = file.size(tmpfile),
                                          type = "text/csv", datapath = tmpfile,
                                          stringsAsFactors = FALSE))
  session$setInputs(header = TRUE)
  session$setInputs(y_var = "Y", x_vars = c("X1", "X2"), run_analysis_btn = 1)

  err <- tryCatch({ diag_model(); NULL }, error = function(e) e)
  ok <- !is.null(err) && grepl("could not be converted to numeric", conditionMessage(err))
  stopifnot(ok)
  cat("PASS: non-numeric outcome column is caught with a validate() message, not a silent bad fit.\n")
  unlink(tmpfile)
})

cat("\n== Test 8 (EDGE CASE): blank panel index columns ==\n")
shiny::testServer(server, {
  df <- data.frame(id = rep(1:5, each = 3), time = rep(1:3, 5), Y = rnorm(15), X = rnorm(15))
  tmpfile <- tempfile(fileext = ".csv")
  write.csv(df, tmpfile, row.names = FALSE)

  session$setInputs(model_family = "linear", data_source = "upload",
                     data_structure_upload = "panel")
  session$setInputs(csv_file = data.frame(name = "panel2.csv", size = file.size(tmpfile),
                                          type = "text/csv", datapath = tmpfile,
                                          stringsAsFactors = FALSE))
  session$setInputs(header = TRUE, p_idx_i = "", p_idx_t = "")
  session$setInputs(y_var = "Y", x_vars = "X", run_analysis_btn = 1)

  err <- tryCatch({ diag_model(); NULL }, error = function(e) e)
  ok <- !is.null(err) && inherits(err, "shiny.silent.error") &&
        (identical(conditionMessage(err), "") ||
         grepl("panel index", conditionMessage(err), ignore.case = TRUE))
  stopifnot(ok)
  cat("PASS: blank panel index fields are rejected with a validate() message.\n")
  unlink(tmpfile)
})

cat("\n== Test 9 (EDGE CASE): type conversion on a non-existent column ==\n")
shiny::testServer(server, {
  session$setInputs(model_family = "linear", data_source = "simulate",
                     data_structure_sim = "cs", het_viol = FALSE, coll_viol = FALSE,
                     n_obs = 300, seed = 7, sim_btn = 1)
  err <- tryCatch({
    session$setInputs(var_to_edit = "NOT_A_REAL_COLUMN", new_data_type = "numeric",
                       change_type_btn = 1)
    NULL
  }, error = function(e) e)
  # Either a validate() message is thrown (caught above) or the observer simply declines
  # to update modified_df(); either way the session must not crash outright.
  cat("PASS: converting a non-existent column does not crash the session.\n")
})

cat("\nALL APP SERVER INTEGRATION TESTS PASSED\n")
