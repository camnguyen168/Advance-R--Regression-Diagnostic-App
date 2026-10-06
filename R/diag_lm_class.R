# diag_lm_class.R
# S3 container plus methods for model diagnostics used by the Shiny application.

library(lmtest)   # bptest(), bgtest(), dwtest()
library(plm)      # panel models and panel diagnostics
library(car)      # vif() - variance inflation factors
library(gt)       # nicely formatted summary tables
library(ggplot2)  # diagnostic plots
library(dplyr)    # data manipulation
library(tibble)   # rownames_to_column()
library(tseries)  # adf.test() - Augmented Dickey-Fuller test, jarque.bera.test()

# ---- Internal helpers for logistic-regression diagnostics ------------------

#' Hosmer-Lemeshow goodness-of-fit test (internal helper)
#' @noRd
.hosmer_lemeshow <- function(y, prob, g = 10) {
  ord  <- order(prob)
  y    <- y[ord]
  prob <- prob[ord]
  n <- length(y)
  g <- max(2, min(g, n))
  
  breaks <- unique(stats::quantile(seq_len(n), probs = seq(0, 1, length.out = g + 1)))
  group  <- cut(seq_len(n), breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  obs1 <- tapply(y, group, sum)
  exp1 <- tapply(prob, group, sum)
  n_g  <- tapply(y, group, length)
  obs0 <- n_g - obs1
  exp0 <- n_g - exp1
  
  chisq <- sum((obs1 - exp1)^2 / exp1 + (obs0 - exp0)^2 / exp0, na.rm = TRUE)
  df    <- length(unique(group)) - 2
  pval  <- if (df > 0) stats::pchisq(chisq, df = df, lower.tail = FALSE) else NA_real_
  
  list(statistic = chisq, df = df, p.value = pval)
}

#' McFadden / Cox-Snell / Nagelkerke pseudo R-squared (internal helper)
#' @noRd
.pseudo_r2 <- function(model) {
  mf <- model.frame(model)
  null_model <- stats::glm(mf[[1]] ~ 1, family = stats::binomial())
  
  ll_model <- as.numeric(stats::logLik(model))
  ll_null  <- as.numeric(stats::logLik(null_model))
  n        <- stats::nobs(model)
  
  mcfadden   <- 1 - ll_model / ll_null
  coxsnell   <- 1 - exp((2 / n) * (ll_null - ll_model))
  nagelkerke <- coxsnell / (1 - exp((2 / n) * ll_null))
  
  list(mcfadden = mcfadden, coxsnell = coxsnell, nagelkerke = nagelkerke)
}

#' Area under the ROC curve via Mann-Whitney U statistic (internal helper)
#' @noRd
.binary_auc <- function(y, prob) {
  pos <- prob[y == 1]
  neg <- prob[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  w <- suppressWarnings(stats::wilcox.test(pos, neg, exact = FALSE))
  as.numeric(w$statistic) / (length(pos) * length(neg))
}

#' Confusion-matrix classification metrics (internal helper)
#' @noRd
.confusion_stats <- function(y, prob, threshold = 0.5) {
  pred <- as.numeric(prob >= threshold)
  tp <- sum(pred == 1 & y == 1)
  tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)
  
  list(
    accuracy    = (tp + tn) / length(y),
    sensitivity = if ((tp + fn) > 0) tp / (tp + fn) else NA_real_,
    specificity = if ((tn + fp) > 0) tn / (tn + fp) else NA_real_,
    threshold   = threshold,
    tp = tp, tn = tn, fp = fp, fn = fn
  )
}

#' Complete separation heuristic (internal helper)
#' @noRd
.check_separation <- function(model) {
  se <- summary(model)$coefficients[, "Std. Error"]
  any(!is.finite(se)) || any(se > 15, na.rm = TRUE)
}

# ---- Constructor Function for 'diag_lm' S3 Class --------------------------

#' Constructor function for 'diag_lm' S3 class
#' 
#' @param model A fitted 'lm', 'plm', or binomial 'glm' object
#' @param data_type Character string: "cross-section", "time-series", "panel", or "logistic"
#' @param time_index Optional vector containing time/date values for time-series plotting
#' @return An object of class 'diag_lm'
#' @export
new_diag_lm <- function(model, data_type = "cross-section", time_index = NULL, panel_formula = NULL) {
  data_type <- match.arg(
    data_type,
    c("cross-section", "time-series", "panel", "cs", "ts", "logistic", "binary")
  )
  data_type <- switch(
    data_type,
    cs = "cross-section",
    ts = "time-series",
    logistic = "binary",
    data_type
  )
  
  is_binary <- identical(data_type, "binary")
  
  if (is_binary) {
    if (!inherits(model, "glm") || !identical(family(model)$family, "binomial")) {
      stop("Error: data_type = 'logistic' requires a fitted glm(family = binomial(...)) object.", call. = FALSE)
    }
  } else if (!inherits(model, c("lm", "plm"))) {
    stop("Error: Input must be a fitted model of class 'lm' or 'plm'.", call. = FALSE)
  }
  
  model_formula <- formula(model)
  coef_table <- summary(model)$coefficients
  
  if (is_binary) {
    bp_result <- NA
    dw_result <- NA
    shapiro_res <- NA
    normality_test_name <- NA_character_
    shapiro_skip_reason <- NA_character_
  } else {
    bp_result <- tryCatch({ lmtest::bptest(model) }, error = function(e) NA)
    dw_result <- tryCatch({ lmtest::dwtest(model) }, error = function(e) NA)
    
    # shapiro.test() hard-errors on n > 5000 ("sample size must be between
    # 3 and 5000") -- that cap is baked into R's implementation and can't be
    # raised. Instead of just reporting "unavailable" for large samples,
    # fall back to Jarque-Bera, which tests the same thing (residual
    # normality) but is an asymptotic test that's actually well-suited to
    # LARGE n -- a natural complement to Shapiro-Wilk's small/moderate-n
    # strength. The p-value is stored in the same shapiro_pvalue slot so
    # every downstream consumer (summary table, value box, AI summary)
    # keeps working; normality_test_name records which test actually ran.
    n_resid <- length(residuals(model))
    if (n_resid > 5000) {
      shapiro_res <- tryCatch({ tseries::jarque.bera.test(residuals(model)) }, error = function(e) NA)
      normality_test_name <- "Jarque-Bera"
      shapiro_skip_reason <- if (is.list(shapiro_res)) NA_character_ else "error"
    } else {
      shapiro_res <- tryCatch({ shapiro.test(residuals(model)) }, error = function(e) NA)
      normality_test_name <- "Shapiro-Wilk"
      shapiro_skip_reason <- if (is.list(shapiro_res)) NA_character_ else "error"
    }
  }
  
  vif_result <- tryCatch({ car::vif(model) }, error = function(e) NA)
  
  ts_diagnostics <- NULL
  panel_diagnostics <- NULL
  
  if (data_type == "panel") {
    # Hausman test (plm::phtest) needs BOTH a fixed-effects and a random-
    # effects fit to compare -- refit both from the panel data attached to
    # the model that was actually passed in, regardless of which estimator
    # the user chose. This is informational: even if someone fits pooled
    # OLS, knowing whether Hausman favors FE or RE is useful guidance for
    # whether their choice is well-justified.
    #
    # IMPORTANT: uses panel_formula (the "clean" formula, no explicit
    # entity/time dummies) when supplied, rather than formula(model). If
    # the app fit the displayed model via LSDV (dummy variables) for
    # visibility in the coefficient table, formula(model) would include
    # those dummies -- and refitting a "within" model with the entity
    # variable ALSO included as an explicit dummy is invalid (the within
    # transformation already removes between-entity variation, so the
    # dummy becomes non-identified/collinear). panel_formula avoids that.
    pdata_used <- tryCatch(model$model, error = function(e) NULL)
    fm <- if (!is.null(panel_formula)) panel_formula else tryCatch(formula(model), error = function(e) NULL)
    
    hausman_result <- NA
    lm_result <- NA
    pftest_result <- NA
    if (!is.null(pdata_used) && !is.null(fm)) {
      fe_fit   <- tryCatch(plm::plm(fm, data = pdata_used, model = "within"), error = function(e) NULL)
      re_fit   <- tryCatch(plm::plm(fm, data = pdata_used, model = "random"), error = function(e) NULL)
      pool_fit <- tryCatch(plm::plm(fm, data = pdata_used, model = "pooling"), error = function(e) NULL)
      
      if (!is.null(fe_fit) && !is.null(re_fit)) {
        hausman_result <- tryCatch(plm::phtest(fe_fit, re_fit), error = function(e) NA)
      }
      if (!is.null(pool_fit)) {
        # Breusch-Pagan Lagrange Multiplier test for random effects: null
        # hypothesis is "no panel effect" (pooled OLS is adequate).
        lm_result <- tryCatch(plm::plmtest(pool_fit, type = "bp"), error = function(e) NA)
      }
      if (!is.null(fe_fit)) {
        # Time-fixed effects test: does adding a time dimension to the
        # entity-only within model improve fit? Compares a two-way
        # (entity + time) within model against fe_fit (entity-only,
        # already computed above) via plm::pFtest(). Using
        # effect = "twoways" rather than a hand-built factor(year) term
        # avoids needing to know the time index's column name here.
        fe_twoways <- tryCatch(plm::plm(fm, data = pdata_used, model = "within", effect = "twoways"), error = function(e) NULL)
        if (!is.null(fe_twoways)) {
          pftest_result <- tryCatch(plm::pFtest(fe_twoways, fe_fit), error = function(e) NA)
        }
      }
    }
    
    # Panel Breusch-Godfrey serial correlation test, run on whichever model
    # the user actually fit (within or pooling) -- unlike Hausman/LM above,
    # this doesn't need a refit.
    pbg_result <- tryCatch(plm::pbgtest(model), error = function(e) NA)
    
    panel_diagnostics <- list(
      hausman_statistic = if (is.list(hausman_result)) unname(hausman_result$statistic) else NA,
      hausman_pvalue    = if (is.list(hausman_result)) unname(hausman_result$p.value) else NA,
      lm_statistic      = if (is.list(lm_result)) unname(lm_result$statistic) else NA,
      lm_pvalue         = if (is.list(lm_result)) unname(lm_result$p.value) else NA,
      pbg_statistic     = if (is.list(pbg_result)) unname(pbg_result$statistic) else NA,
      pbg_pvalue        = if (is.list(pbg_result)) unname(pbg_result$p.value) else NA,
      pftest_statistic  = if (is.list(pftest_result)) unname(pftest_result$statistic) else NA,
      pftest_pvalue     = if (is.list(pftest_result)) unname(pftest_result$p.value) else NA
    )
  }
  
  if (data_type == "time-series") {
    bg_result <- tryCatch({ lmtest::bgtest(model) }, error = function(e) NA)
    lb_result <- tryCatch({ Box.test(residuals(model), type = "Ljung-Box") }, error = function(e) NA)
    adf_result <- tryCatch({ tseries::adf.test(residuals(model)) }, error = function(e) NA)
    
    ts_diagnostics <- list(
      bg_statistic        = if (is.list(bg_result)) unname(bg_result$statistic) else NA,
      bg_pvalue           = if (is.list(bg_result)) unname(bg_result$p.value) else NA,
      ljung_box_statistic = if (is.list(lb_result)) unname(lb_result$statistic) else NA,
      ljung_box_pvalue    = if (is.list(lb_result)) unname(lb_result$p.value) else NA,
      adf_statistic       = if (is.list(adf_result)) unname(adf_result$statistic) else NA,
      adf_pvalue          = if (is.list(adf_result)) unname(adf_result$p.value) else NA
    )
  }
  
  logistic_diagnostics <- NULL
  if (is_binary) {
    y_raw <- model.frame(model)[[1]]
    y <- if (is.factor(y_raw)) as.numeric(y_raw) - 1 else as.numeric(y_raw)
    fitted_p <- unname(fitted(model))
    
    hl_result <- tryCatch({ .hosmer_lemeshow(y, fitted_p, g = 10) }, error = function(e) list(statistic = NA, df = NA, p.value = NA))
    pr2 <- tryCatch({ .pseudo_r2(model) }, error = function(e) list(mcfadden = NA, coxsnell = NA, nagelkerke = NA))
    auc_val <- tryCatch({ .binary_auc(y, fitted_p) }, error = function(e) NA_real_)
    conf <- .confusion_stats(y, fitted_p, threshold = 0.5)
    separation_flag <- tryCatch({ .check_separation(model) }, error = function(e) FALSE)
    
    logistic_diagnostics <- list(
      hl_statistic    = hl_result$statistic,
      hl_df           = hl_result$df,
      hl_pvalue       = hl_result$p.value,
      mcfadden_r2     = pr2$mcfadden,
      coxsnell_r2     = pr2$coxsnell,
      nagelkerke_r2   = pr2$nagelkerke,
      auc             = auc_val,
      accuracy        = conf$accuracy,
      sensitivity     = conf$sensitivity,
      specificity     = conf$specificity,
      threshold       = conf$threshold,
      separation_flag = separation_flag
    )
  }
  
  obj <- list(
    raw_model     = model,
    data_type     = data_type,
    time_index    = time_index,  # Stores the date vector securely
    formula       = model_formula,
    coefficients  = coef_table,
    residuals     = residuals(model),
    fitted_values = fitted(model),
    diagnostics   = list(
      bp_statistic   = if (is.list(bp_result)) unname(bp_result$statistic) else NA,
      bp_pvalue      = if (is.list(bp_result)) unname(bp_result$p.value) else NA,
      dw_statistic   = if (is.list(dw_result)) unname(dw_result$statistic) else NA,
      dw_pvalue      = if (is.list(dw_result)) unname(dw_result$p.value) else NA,
      vif_scores     = vif_result,
      shapiro_pvalue = if (is.list(shapiro_res)) unname(shapiro_res$p.value) else NA,
      normality_test_name = normality_test_name,
      shapiro_skip_reason = shapiro_skip_reason,
      ts             = ts_diagnostics,
      panel          = panel_diagnostics,
      logistic       = logistic_diagnostics
    )
  )
  
  class(obj) <- "diag_lm"
  return(obj)
}

# ---- S3 Methods for 'diag_lm' -----------------------------------------------

#' @export
summary.diag_lm <- function(object, ...) {
  if (!inherits(object, "diag_lm")) stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  
  coef_df <- as.data.frame(object$coefficients) |> rownames_to_column("Term")
  data_type <- if (is.null(object$data_type)) "cross-section" else object$data_type
  bp_pval <- round(object$diagnostics$bp_pvalue, 4)
  dw_pval <- round(object$diagnostics$dw_pvalue, 4)
  shapiro_pval <- if (is.null(object$diagnostics$shapiro_pvalue)) NA else object$diagnostics$shapiro_pvalue
  shapiro_text <- if (is.na(shapiro_pval)) "N/A" else round(shapiro_pval, 4)
  normality_label <- if (!is.null(object$diagnostics$normality_test_name) && !is.na(object$diagnostics$normality_test_name)) {
    object$diagnostics$normality_test_name
  } else "Shapiro-Wilk"
  
  if (identical(data_type, "binary")) {
    lg <- object$diagnostics$logistic
    coef_df$`Odds Ratio` <- exp(coef_df$Estimate)
    fmt <- function(v) if (is.null(v) || is.na(v)) "N/A" else round(v, 4)
    
    return(
      gt(coef_df) |>
        tab_header(title = "Logistic Regression & Diagnostics Summary",
                   subtitle = paste("Hosmer-Lemeshow p-value:", fmt(lg$hl_pvalue), "| McFadden R2:", fmt(lg$mcfadden_r2), "| AUC:", fmt(lg$auc))) |>
        fmt_number(columns = setdiff(names(coef_df), "Term"), decimals = 3)
    )
  }
  
  if (identical(data_type, "time-series")) {
    bg_stat  <- object$diagnostics$ts$bg_statistic
    bg_pval  <- object$diagnostics$ts$bg_pvalue
    lb_stat  <- object$diagnostics$ts$ljung_box_statistic
    lb_pval  <- object$diagnostics$ts$ljung_box_pvalue
    adf_stat <- object$diagnostics$ts$adf_statistic
    adf_pval <- object$diagnostics$ts$adf_pvalue
    
    bg_outcome  <- if (!is.na(bg_pval) && bg_pval < 0.05) "Serial correlation detected" else "No serial correlation evidence"
    lb_outcome  <- if (!is.na(lb_pval) && lb_pval < 0.05) "Autocorrelation detected" else "No autocorrelation evidence"
    adf_outcome <- if (!is.na(adf_pval) && adf_pval < 0.05) "Series is stationary (reject unit root)" else "Non-stationary series (unit root suspected)"
    
    is_univariate <- length(all.vars(object$formula)) == 1 || (nrow(object$coefficients) == 1 && rownames(object$coefficients)[1] == "(Intercept)")
    title_text <- if (is_univariate) "Univariate Stock / Series Diagnostics Summary" else "Time-Series Regression Summary"
    
    return(
      gt(coef_df) |>
        tab_header(title = title_text, subtitle = "Self-correlation, distributional properties, and stationarity checks") |>
        fmt_number(columns = 2:ncol(coef_df), decimals = 3) |>
        tab_source_note(source_note = paste0("Breusch-Godfrey: statistic = ", round(bg_stat, 4), ", p-value = ", round(bg_pval, 4), " (", bg_outcome, ")")) |>
        tab_source_note(source_note = paste0("Ljung-Box: statistic = ", round(lb_stat, 4), ", p-value = ", round(lb_pval, 4), " (", lb_outcome, ")")) |>
        tab_source_note(source_note = paste0("Augmented Dickey-Fuller: statistic = ", round(adf_stat, 4), ", p-value = ", round(adf_pval, 4), " (", adf_outcome, ")")) |>
        tab_source_note(source_note = paste0(normality_label, " normality p-value: ", shapiro_text))
    )
  }
  
  if (identical(data_type, "panel")) {
    hausman_stat <- object$diagnostics$panel$hausman_statistic
    hausman_pval <- object$diagnostics$panel$hausman_pvalue
    lm_stat      <- object$diagnostics$panel$lm_statistic
    lm_pval      <- object$diagnostics$panel$lm_pvalue
    pbg_stat     <- object$diagnostics$panel$pbg_statistic
    pbg_pval     <- object$diagnostics$panel$pbg_pvalue
    pftest_stat  <- object$diagnostics$panel$pftest_statistic
    pftest_pval  <- object$diagnostics$panel$pftest_pvalue
    
    hausman_text <- if (is.na(hausman_pval)) "N/A" else round(hausman_pval, 4)
    lm_text      <- if (is.na(lm_pval)) "N/A" else round(lm_pval, 4)
    pbg_text     <- if (is.na(pbg_pval)) "N/A" else round(pbg_pval, 4)
    pftest_text  <- if (is.na(pftest_pval)) "N/A" else round(pftest_pval, 4)
    pftest_outcome <- if (is.na(pftest_pval)) "Could not be computed" else if (pftest_pval < 0.05) "Time effects improve fit (consider two-way FE)" else "One-way (entity) effects remain adequate"
    hausman_outcome <- if (is.na(hausman_pval)) "Could not be computed" else if (hausman_pval < 0.05) "Fixed effects preferred (individual effects correlated with regressors)" else "Random effects plausible (no evidence of correlation)"
    lm_outcome      <- if (is.na(lm_pval)) "Could not be computed" else if (lm_pval < 0.05) "Panel effects present (pooled OLS is NOT adequate)" else "No evidence of panel effects (pooled OLS may be adequate)"
    pbg_outcome     <- if (is.na(pbg_pval)) "Could not be computed" else if (pbg_pval < 0.05) "Serial correlation detected" else "No serial correlation evidence"
    
    se_note <- if (!is.null(object$se_type)) object$se_type else "Standard"
    fe_method_note <- if (identical(object$fe_method, "within")) {
      "Fixed effects: Within estimator (demeaned) -- entity/time effects removed mathematically, not shown as individual coefficients."
    } else if (identical(object$fe_method, "lsdv")) {
      "Fixed effects: Dummy variables (LSDV) -- entity/time effects shown as coefficient rows above."
    } else if (identical(object$fe_method, "random")) {
      "Random Effects (GLS): entity effects treated as random draws, assumed uncorrelated with the regressors -- not shown as individual coefficients."
    } else {
      NULL
    }
    
    tbl <- gt(coef_df) |>
      tab_header(title = "Panel Data Regression & Diagnostics Summary",
                 subtitle = "Model-choice, poolability, and serial correlation checks") |>
      fmt_number(columns = 2:ncol(coef_df), decimals = 3) |>
      tab_source_note(source_note = paste0("Hausman test: statistic = ", if (is.na(hausman_stat)) "N/A" else round(hausman_stat, 4), ", p-value = ", hausman_text, " (", hausman_outcome, ")")) |>
      tab_source_note(source_note = paste0("Breusch-Pagan LM test (poolability): statistic = ", if (is.na(lm_stat)) "N/A" else round(lm_stat, 4), ", p-value = ", lm_text, " (", lm_outcome, ")")) |>
      tab_source_note(source_note = paste0("Panel Breusch-Godfrey: statistic = ", if (is.na(pbg_stat)) "N/A" else round(pbg_stat, 4), ", p-value = ", pbg_text, " (", pbg_outcome, ")")) |>
      tab_source_note(source_note = paste0("Standard errors: ", se_note)) |>
      tab_source_note(source_note = paste0("Time-fixed effects (pFtest): statistic = ", if (is.na(pftest_stat)) "N/A" else round(pftest_stat, 4), ", p-value = ", pftest_text, " (", pftest_outcome, ")")) 
    if (!is.null(fe_method_note)) {
      tbl <- tbl |> tab_source_note(source_note = fe_method_note)
    }
    
    return(tbl)
  }
  
  se_note <- if (!is.null(object$se_type)) object$se_type else "Standard (OLS)"
  
  gt(coef_df) |>
    tab_header(title = "OLS Regression & Diagnostics Summary",
               subtitle = paste("Breusch-Pagan p-value:", bp_pval, "| Durbin-Watson p-value:", dw_pval, paste0("| ", normality_label, " p-value:"), shapiro_text)) |>
    fmt_number(columns = 2:ncol(coef_df), decimals = 3) |>
    tab_source_note(source_note = paste0("Standard errors: ", se_note))
}

#' Format a p-value for remediation-advice alert text (internal helper)
#'
#' Bare round(p, 4) inside paste0() has two problems: any p below 0.00005
#' rounds to exactly 0 (reads like an impossible exact-zero p-value, e.g.
#' "p = 0"), and R's default number-to-string conversion can switch to
#' scientific notation for small values (e.g. "3e-04") -- inconsistent with
#' how larger p-values print. This mirrors the fmt_pval() helper already
#' used for the app's value boxes, so alert text and value boxes agree.
#' @noRd
.fmt_p_alert <- function(p, digits = 4) {
  if (is.null(p) || length(p) == 0 || is.na(p)) return("N/A")
  if (p < 0.0001) return("< 0.0001")
  format(round(p, digits), nsmall = digits, scientific = FALSE)
}

#' @export
remediation_advice <- function(object, ...) { UseMethod("remediation_advice") }

#' @export
remediation_advice.diag_lm <- function(object, ...) {
  if (!inherits(object, "diag_lm")) stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  alerts <- character(0)
  
  if (object$data_type %in% c("time-series", "ts")) {
    adf_pvalue <- object$diagnostics$ts$adf_pvalue
    if (!is.na(adf_pvalue) && adf_pvalue >= 0.05) {
      alerts <- c(alerts, paste0(
        "<div class='alert alert-danger'><b>Warning: Non-Stationary Series (Unit Root Detected)!</b><br/>",
        "The ADF test failed to reject a unit root (p = ", .fmt_p_alert(adf_pvalue),
        "). Consider first-differencing or using log returns.</div>"
      ))
    }
  }
  
  if (identical(object$data_type, "cross-section")) {
    d <- object$diagnostics
    
    if (!is.na(d$bp_pvalue) && d$bp_pvalue < 0.05) {
      alerts <- c(alerts, paste0(
        "<div class='alert alert-warning'><b>Heteroskedasticity detected</b> (Breusch-Pagan, p = ",
        .fmt_p_alert(d$bp_pvalue), "). Standard errors may be unreliable -- consider ",
        "heteroskedasticity-robust (HC1) standard errors under \"Model Adjustments\".</div>"
      ))
    }
    if (!is.na(d$dw_pvalue) && d$dw_pvalue < 0.05) {
      alerts <- c(alerts, paste0(
        "<div class='alert alert-warning'><b>Serial correlation detected</b> (Durbin-Watson, p = ",
        .fmt_p_alert(d$dw_pvalue), "). If this data is time-ordered, consider using the ",
        "Time Series data structure and/or robust standard errors.</div>"
      ))
    }
    if (!is.na(d$shapiro_pvalue) && d$shapiro_pvalue < 0.05) {
      test_name <- if (!is.null(d$normality_test_name) && !is.na(d$normality_test_name)) d$normality_test_name else "Shapiro-Wilk"
      alerts <- c(alerts, paste0(
        "<div class='alert alert-info'><b>Non-normal residuals</b> (", test_name, ", p = ",
        .fmt_p_alert(d$shapiro_pvalue), "). With a large sample this is often not a major concern; ",
        "with a small sample, consider whether outliers or a skewed outcome are responsible.</div>"
      ))
    }
    vif_values <- suppressWarnings(as.numeric(d$vif_scores))
    vif_values <- vif_values[is.finite(vif_values)]
    if (length(vif_values) > 0 && max(vif_values) >= 5) {
      alerts <- c(alerts, paste0(
        "<div class='alert alert-warning'><b>Multicollinearity detected</b> (max VIF = ",
        round(max(vif_values), 2), "). Consider dropping or combining highly correlated predictors.</div>"
      ))
    }
  }
  
  if (identical(object$data_type, "binary")) {
    lg <- object$diagnostics$logistic
    if (!is.null(lg)) {
      if (!is.na(lg$hl_pvalue) && lg$hl_pvalue < 0.05) {
        alerts <- c(alerts, paste0(
          "<div class='alert alert-warning'><b>Poor calibration</b> (Hosmer-Lemeshow, p = ",
          .fmt_p_alert(lg$hl_pvalue), "). The model's predicted probabilities may not match ",
          "observed outcome rates well -- check the Calibration/Binned Residual plots.</div>"
        ))
      }
      if (!is.na(lg$auc) && lg$auc < 0.7) {
        alerts <- c(alerts, paste0(
          "<div class='alert alert-info'><b>Weak discrimination</b> (AUC = ", round(lg$auc, 3),
          "). The model does not separate the two classes well.</div>"
        ))
      }
      if (isTRUE(lg$separation_flag)) {
        alerts <- c(alerts, paste0(
          "<div class='alert alert-danger'><b>Possible (quasi-)complete separation detected</b> ",
          "(implausibly large standard errors). Coefficient estimates may be unstable/unreliable.</div>"
        ))
      }
    }
  }
  

  
  # =========================================================================
  # TEMPORARILY HIDDEN: Panel data remediation advice
  # To use it later, just remove the '#' symbols from the block below.
  #
  # NOTE (fixed while disabled): fe_already_used previously read
  # `!is.null(object$fe_method)`, which would incorrectly treat Random
  # Effects as "already using Fixed Effects" -- Random Effects is exactly
  # the alternative the Hausman test warns against when it favors FE, so
  # this would have shown a falsely reassuring message. Now only "lsdv"
  # and "within" (the two actual Fixed Effects variants) count.
  # =========================================================================
  # if (identical(object$data_type, "panel")) {
  #   p <- object$diagnostics$panel
  #   fe_already_used <- identical(object$fe_method, "lsdv") || identical(object$fe_method, "within")
  #   
  #   if (!is.null(p) && !is.na(p$hausman_pvalue) && p$hausman_pvalue < 0.05) {
  #     if (fe_already_used) {
  #       alerts <- c(alerts, paste0(
  #         "<div class='alert alert-info'><b>Hausman test confirms Fixed Effects was the right call</b> (p = ",
  #         .fmt_p_alert(p$hausman_pvalue), "). The individual effects are correlated with your regressors, ",
  #         "which is exactly why Random Effects would have been inconsistent here -- you're already using the ",
  #         "appropriate estimator.</div>"
  #       ))
  #     } else if (identical(object$fe_method, "random")) {
  #       alerts <- c(alerts, paste0(
  #         "<div class='alert alert-danger'><b>Hausman test favors Fixed Effects, but you're using Random Effects</b> (p = ",
  #         .fmt_p_alert(p$hausman_pvalue), "). The individual effects are likely correlated with your regressors, ",
  #         "so your current Random Effects estimates may be inconsistent (biased). Switch to a Fixed Effects ",
  #         "estimator (Dummy variables or Within) under \"Effects Estimator\".</div>"
  #       ))
  #     } else {
  #       alerts <- c(alerts, paste0(
  #         "<div class='alert alert-warning'><b>Hausman test favors Fixed Effects over Random Effects</b> (p = ",
  #         .fmt_p_alert(p$hausman_pvalue), "). The individual effects are likely correlated with your regressors, ",
  #         "so Random Effects would be inconsistent (and you're not currently using Fixed Effects). Switch to it ",
  #         "under \"Include fixed effects\".</div>"
  #       ))
  #     }
  #   }
  #   
  #   if (!is.null(p) && !is.na(p$lm_pvalue) && p$lm_pvalue < 0.05) {
  #     alerts <- c(alerts, paste0(
  #       "<div class='alert alert-warning'><b>Panel effects detected -- pooled OLS is misspecified</b> (Breusch-Pagan LM test, p = ",
  #       .fmt_p_alert(p$lm_pvalue), "). Ignoring the panel structure entirely is not adequate here -- ",
  #       "Fixed or Random Effects is the better choice, consistent with the Hausman test result above.</div>"
  #     ))
  #   }
  #   
  #   if (!is.null(p) && !is.na(p$pbg_pvalue) && p$pbg_pvalue < 0.05) {
  #     alerts <- c(alerts, paste0(
  #       "<div class='alert alert-danger'><b>Serial correlation detected in panel residuals</b> (Breusch-Godfrey, p = ",
  #       .fmt_p_alert(p$pbg_pvalue), "). Standard errors may be understated regardless of your Fixed/Random Effects ",
  #       "choice above. Consider clustering standard errors by the entity index, or a dynamic panel ",
  #       "specification (e.g. adding a lagged dependent variable).</div>"
  #     ))
  #   }
  # }
  
  return(alerts)
}

#' @export
plot.diag_lm <- function(x, type = "residuals", ...) {
  if (!inherits(x, "diag_lm")) stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  
  # Explicit whitelist -- previously an unrecognized `type` (a typo, or a
  # genuinely unsupported value) silently fell through to the generic
  # Residuals-vs-Fitted plot instead of erroring, even though this was
  # already documented and tested as raising an error. Enforcing it here
  # closes that gap.
  valid_types <- c("residuals", "qq", "scale_location", "histogram", "acf", "pacf",
                   "residuals_time", "binned_residuals", "roc", "calibration")
  if (!type %in% valid_types) {
    stop(sprintf("Error: '%s' is not a recognized plot type. Valid types are: %s.",
                 type, paste(valid_types, collapse = ", ")), call. = FALSE)
  }
  
  std_resid <- x$residuals / sd(x$residuals)
  plot_data <- data.frame(fitted = x$fitted_values, residuals = x$residuals, std_resid = std_resid)
  
  if (type == "residuals_time") {
    plot_data$time <- if (!is.null(x$time_index)) x$time_index else seq_along(x$residuals)
    ggplot(plot_data, aes(x = time, y = residuals)) +
      geom_line(color = "steelblue", alpha = 0.7) +
      geom_point(color = "steelblue", size = 1.2, alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(title = "Series Values / Deviations over Time", x = "Date / Time Index", y = "De-meaned Value / Residual")
  } else if (type == "acf") {
    acf_obj <- acf(x$residuals, plot = FALSE, na.action = na.pass)
    acf_df <- data.frame(lag = as.numeric(acf_obj$lag), acf = as.numeric(acf_obj$acf))
    ggplot(acf_df, aes(x = lag, y = acf)) +
      geom_col(fill = "steelblue", alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() + labs(title = "Self Autocorrelation (ACF)", x = "Lag", y = "ACF")
  } else if (type == "pacf") {
    pacf_obj <- pacf(x$residuals, plot = FALSE, na.action = na.pass)
    pacf_df <- data.frame(lag = as.numeric(pacf_obj$lag), pacf = as.numeric(pacf_obj$acf))
    ggplot(pacf_df, aes(x = lag, y = pacf)) +
      geom_col(fill = "steelblue", alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() + labs(title = "Partial Autocorrelation (PACF)", x = "Lag", y = "PACF")
  } else if (type == "histogram") {
    ggplot(plot_data, aes(x = residuals)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
      geom_density(color = "darkorange", linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() + labs(title = "Distribution of Stock / Series", x = "De-meaned Value", y = "Density")
  } else if (type == "qq") {
    ggplot(plot_data, aes(sample = residuals)) +
      stat_qq(color = "steelblue", alpha = 0.7) +
      stat_qq_line(color = "darkred", linetype = "dashed") +
      theme_minimal() + labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Sample Quantiles")
  } else if (type == "scale_location") {
    # Spread of sqrt(|standardized residuals|) against fitted values -- a
    # horizontal band with no trend indicates constant error variance; a
    # funnel or trend is the visual signature of heteroskedasticity, the
    # same thing the Breusch-Pagan test checks numerically.
    ggplot(plot_data, aes(x = fitted, y = sqrt(abs(std_resid)))) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_smooth(se = FALSE, color = "darkorange", linewidth = 0.8,
                  method = "loess", formula = y ~ x) +
      theme_minimal() +
      labs(title = "Scale-Location", x = "Fitted Values",
           y = expression(sqrt("|Standardized Residuals|")))
  } else if (type %in% c("binned_residuals", "roc", "calibration")) {
    # These three are logistic-regression-specific. Checking data_type
    # directly (rather than only checking whether the raw outcome can be
    # recovered) is what lets this genuinely error on a non-logistic model
    # -- a continuous response still converts to numeric without error, so
    # a recovery-failure check alone would silently proceed and produce a
    # nonsensical plot instead of the documented, tested error.
    if (!identical(x$data_type, "binary")) {
      stop(sprintf(
        "Error: type = '%s' is only available for logistic ('binary') models, not '%s' models.",
        type, x$data_type
      ), call. = FALSE)
    }
    
    y_raw <- tryCatch({
      yv <- model.frame(x$raw_model)[[1]]
      if (is.factor(yv)) as.numeric(yv) - 1 else as.numeric(yv)
    }, error = function(e) NULL)
    
    if (is.null(y_raw) || is.null(x$raw_model)) {
      stop(sprintf("Error: could not recover the raw outcome needed for type = '%s'.", type), call. = FALSE)
    }
    
    fitted_p <- x$fitted_values
    n <- length(fitted_p)
    
    if (type == "binned_residuals") {
      # Classic binned residual plot (Gelman & Hill style): group
      # observations into bins of similar fitted probability, plot the
      # AVERAGE raw residual (y - p) per bin against the average fitted
      # probability per bin. A well-calibrated model should scatter
      # tightly around zero within the shaded +/-2SE band; systematic
      # deviation signals a missed nonlinearity or omitted term.
      resid_raw <- y_raw - fitted_p
      n_bins <- max(5, min(20, round(sqrt(n))))
      bins <- cut(fitted_p, breaks = unique(quantile(fitted_p, probs = seq(0, 1, length.out = n_bins + 1))),
                  include.lowest = TRUE, labels = FALSE)
      
      bin_df <- data.frame(fitted = fitted_p, resid = resid_raw, bin = bins)
      binned <- stats::aggregate(cbind(fitted, resid) ~ bin, data = bin_df, FUN = mean)
      bin_counts <- table(bin_df$bin)
      binned$n <- as.numeric(bin_counts[as.character(binned$bin)])
      # Approximate +/-2SE band for the average residual in each bin,
      # using the binomial variance p(1-p)/n at that bin's mean fitted p.
      binned$se_band <- 2 * sqrt(pmax(binned$fitted * (1 - binned$fitted), 1e-6) / binned$n)
      
      ggplot(binned, aes(x = fitted, y = resid)) +
        geom_ribbon(aes(ymin = -se_band, ymax = se_band), fill = "grey60", alpha = 0.25) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
        geom_point(color = "steelblue", size = 2.2) +
        theme_minimal() +
        labs(title = "Binned Residuals", x = "Average Fitted Probability (per bin)", y = "Average Residual (per bin)")
      
    } else if (type == "roc") {
      # ROC curve traced out by sweeping the classification threshold from
      # 1 down to 0, plotting the resulting True Positive Rate against the
      # False Positive Rate at each point.
      ord <- order(fitted_p, decreasing = TRUE)
      y_sorted <- y_raw[ord]
      n_pos <- sum(y_sorted == 1)
      n_neg <- sum(y_sorted == 0)
      
      if (n_pos == 0 || n_neg == 0) {
        return(ggplot() + theme_void() + labs(title = "ROC curve unavailable -- outcome has only one class."))
      }
      
      tpr <- cumsum(y_sorted == 1) / n_pos
      fpr <- cumsum(y_sorted == 0) / n_neg
      roc_df <- data.frame(fpr = c(0, fpr), tpr = c(0, tpr))
      
      ggplot(roc_df, aes(x = fpr, y = tpr)) +
        geom_line(color = "steelblue", linewidth = 1.1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
        coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
        theme_minimal() +
        labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate")
      
    } else {
      # Calibration plot: group observations into bins of similar fitted
      # probability, then compare the mean fitted probability in each bin
      # against the ACTUAL observed proportion of positives in that bin.
      # Points on the diagonal indicate well-calibrated predictions.
      n_bins <- max(5, min(10, round(sqrt(n))))
      bins <- cut(fitted_p, breaks = unique(quantile(fitted_p, probs = seq(0, 1, length.out = n_bins + 1))),
                  include.lowest = TRUE, labels = FALSE)
      
      cal_df <- data.frame(fitted = fitted_p, y = y_raw, bin = bins)
      cal_binned <- stats::aggregate(cbind(fitted, y) ~ bin, data = cal_df, FUN = mean)
      
      ggplot(cal_binned, aes(x = fitted, y = y)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
        geom_line(color = "steelblue") +
        geom_point(color = "steelblue", size = 3) +
        coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
        theme_minimal() +
        labs(title = "Calibration Plot", x = "Mean Predicted Probability (per bin)", y = "Observed Proportion Positive")
    }
  } else if (type == "residuals") {
    ggplot(plot_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() + labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
  } else {
    # Defensive only: type was already validated against valid_types above,
    # so reaching here means a type was added to that list without a
    # matching branch here -- fail loudly rather than silently misrendering.
    stop(sprintf("Error: '%s' is listed as a valid plot type but has no implementation.", type), call. = FALSE)
  }
}

# ==============================================================================
# ---- UNIVARIATE / SELF-REFERENTIAL TIME SERIES EXTENSION -------------------
# ==============================================================================
# Diagnostics for a SINGLE series analyzed against its own history (e.g. "how
# does this stock move through time, and is it autocorrelated with its own
# past") - no second predictor variable required. Produces an object of class
# c("diag_lm_uni", "diag_lm"), so it plugs into the same generic calls
# (summary(), plot(), remediation_advice()) as the regression-based objects
# above, but dispatches to its own methods below.

#' Constructor for univariate (self-referential) time-series diagnostics
#'
#' Use this when there is no second variable - you're studying how ONE
#' column develops over time and whether it's correlated with its own past
#' (e.g. a stock price series, a single sensor reading over time).
#'
#' @param series Numeric vector - the raw series (e.g. price, temperature)
#' @param time_index Optional vector of dates/time values, same length as series
#' @param var_name Character label used in plot/table titles
#' @param max_lag Optional integer; max lag for ACF/PACF/Ljung-Box.
#'   Defaults to min(10, n/5), the common rule-of-thumb cutoff.
#' @param difference Logical. If TRUE, diagnostics run on the first difference
#'   of the series instead of the level - use this once you've established
#'   the level series is non-stationary (see remediation_advice output).
#' @return An object of class c("diag_lm_uni", "diag_lm")
#' @export
new_diag_lm_univariate <- function(series, time_index = NULL, var_name = "Series",
                                   max_lag = NULL, difference = FALSE) {
  series <- as.numeric(series)
  keep <- is.finite(series)
  if (!is.null(time_index)) time_index <- time_index[keep]
  series <- series[keep]
  
  if (isTRUE(difference)) {
    if (!is.null(time_index)) time_index <- time_index[-1]
    series <- diff(series)
  }
  
  n <- length(series)
  if (n < 8) {
    stop("Error: Need at least 8 observations to run univariate time-series diagnostics.", call. = FALSE)
  }
  
  lag_use <- if (is.null(max_lag)) max(1, min(10, floor(n / 5))) else max_lag
  mu <- mean(series)
  resid <- series - mu  # de-meaned series, used for distributional checks
  
  # --- self-correlation: does the series predict its own future? ---
  acf_obj  <- stats::acf(series, plot = FALSE, lag.max = lag_use, na.action = stats::na.pass)
  pacf_obj <- stats::pacf(series, plot = FALSE, lag.max = lag_use, na.action = stats::na.pass)
  ci_bound <- stats::qnorm(0.975) / sqrt(n)  # 95% significance band for ACF/PACF bars
  
  lb_result   <- tryCatch(stats::Box.test(series, lag = lag_use, type = "Ljung-Box"), error = function(e) NA)
  adf_result  <- tryCatch(tseries::adf.test(series), error = function(e) NA)
  jb_result   <- tryCatch(tseries::jarque.bera.test(resid), error = function(e) NA)
  shapiro_res <- if (n <= 5000) tryCatch(stats::shapiro.test(resid), error = function(e) NA) else NA
  shapiro_skip_reason <- if (n > 5000) "sample_too_large" else if (!is.list(shapiro_res)) "error" else NA_character_
  
  obj <- list(
    raw_model     = NULL,
    data_type     = "univariate-ts",
    time_index    = time_index,
    var_name      = var_name,
    differenced   = isTRUE(difference),
    formula       = NULL,
    coefficients  = NULL,
    residuals     = resid,
    fitted_values = rep(mu, n),
    series        = series,
    diagnostics   = list(
      mean = mu,
      sd   = stats::sd(series),
      shapiro_pvalue = if (is.list(shapiro_res)) unname(shapiro_res$p.value) else NA,
      shapiro_skip_reason = shapiro_skip_reason,
      acf  = list(lag = as.numeric(acf_obj$lag),  value = as.numeric(acf_obj$acf),  ci_bound = ci_bound),
      pacf = list(lag = as.numeric(pacf_obj$lag), value = as.numeric(pacf_obj$acf), ci_bound = ci_bound),
      ts = list(
        ljung_box_statistic = if (is.list(lb_result))  unname(lb_result$statistic)  else NA,
        ljung_box_pvalue    = if (is.list(lb_result))  unname(lb_result$p.value)    else NA,
        adf_statistic       = if (is.list(adf_result)) unname(adf_result$statistic) else NA,
        adf_pvalue          = if (is.list(adf_result)) unname(adf_result$p.value)   else NA,
        jb_statistic        = if (is.list(jb_result))  unname(jb_result$statistic)  else NA,
        jb_pvalue           = if (is.list(jb_result))  unname(jb_result$p.value)    else NA
      )
    )
  )
  class(obj) <- c("diag_lm_uni", "diag_lm")
  obj
}

# ---- S3 methods for the 'diag_lm_uni' subclass ------------------------------
# These are dispatched ahead of summary.diag_lm / plot.diag_lm /
# remediation_advice.diag_lm above because the object's class vector is
# c("diag_lm_uni", "diag_lm") - R dispatches on the first matching class.

#' @export
summary.diag_lm_uni <- function(object, ...) {
  d <- object$diagnostics
  fmt <- function(v, digits = 4) if (is.null(v) || is.na(v)) "N/A" else round(v, digits)
  
  adf_outcome <- if (!is.na(d$ts$adf_pvalue) && d$ts$adf_pvalue < 0.05) {
    "Stationary (reject unit root)"
  } else "Non-stationary (unit root suspected)"
  
  lb_outcome <- if (!is.na(d$ts$ljung_box_pvalue) && d$ts$ljung_box_pvalue < 0.05) {
    "Autocorrelated"
  } else "No significant autocorrelation"
  
  stats_df <- data.frame(
    Statistic = c("Mean", "Std. Dev.",
                  "Ljung-Box statistic", "Ljung-Box p-value",
                  "ADF statistic", "ADF p-value",
                  "Jarque-Bera statistic", "Jarque-Bera p-value",
                  "Shapiro-Wilk p-value"),
    Value = c(fmt(d$mean), fmt(d$sd),
              fmt(d$ts$ljung_box_statistic), fmt(d$ts$ljung_box_pvalue),
              fmt(d$ts$adf_statistic), fmt(d$ts$adf_pvalue),
              fmt(d$ts$jb_statistic), fmt(d$ts$jb_pvalue),
              fmt(d$shapiro_pvalue))
  )
  
  gt::gt(stats_df) |>
    gt::tab_header(
      title = paste0(object$var_name, ": Univariate Time-Series Diagnostics",
                     if (isTRUE(object$differenced)) " (First Differenced)" else ""),
      subtitle = paste0("Stationarity: ", adf_outcome, "  |  Self-correlation: ", lb_outcome)
    )
}

#' @export
remediation_advice.diag_lm_uni <- function(object, ...) {
  d <- object$diagnostics
  alerts <- character(0)
  
  if (!is.na(d$ts$adf_pvalue) && d$ts$adf_pvalue >= 0.05) {
    alerts <- c(alerts, paste0(
      "<div class='alert alert-danger'><b>Warning: Non-Stationary Series (Unit Root Detected)!</b><br/>",
      "ADF p-value = ", .fmt_p_alert(d$ts$adf_pvalue),
      ". Consider first-differencing (set difference = TRUE) or log returns before modeling.</div>"
    ))
  }
  if (!is.na(d$ts$ljung_box_pvalue) && d$ts$ljung_box_pvalue < 0.05) {
    alerts <- c(alerts, paste0(
      "<div class='alert alert-warning'><b>Significant autocorrelation detected</b> (Ljung-Box p = ",
      .fmt_p_alert(d$ts$ljung_box_pvalue), "). The series has memory - an AR/ARIMA model is more ",
      "appropriate than treating observations as independent.</div>"
    ))
  }
  if (!is.na(d$ts$jb_pvalue) && d$ts$jb_pvalue < 0.05) {
    alerts <- c(alerts, paste0(
      "<div class='alert alert-info'><b>Non-normal distribution</b> (Jarque-Bera p = ",
      .fmt_p_alert(d$ts$jb_pvalue), "). Common for financial returns (fat tails/skew) - consider a ",
      "t-distribution assumption or a GARCH-family model if volatility clustering is present.</div>"
    ))
  }
  if (length(alerts) == 0) {
    alerts <- c(alerts, "<div class='alert alert-success'><b>No major issues detected.</b> Series appears stationary with no strong autocorrelation or distributional red flags.</div>")
  }
  alerts
}

#' @export
plot.diag_lm_uni <- function(x, type = "series", ...) {
  # Same fix as plot.diag_lm(): validate type explicitly instead of letting
  # an unrecognized value silently fall through to the default plot.
  valid_types <- c("series", "residuals_time", "qq", "acf", "pacf", "histogram")
  if (!type %in% valid_types) {
    stop(sprintf("Error: '%s' is not a recognized plot type for a univariate time series. Valid types are: %s.",
                 type, paste(valid_types, collapse = ", ")), call. = FALSE)
  }
  
  d <- x$diagnostics
  
  if (type == "qq") {
    df <- data.frame(sample = x$residuals)  # residuals here = de-meaned series
    ggplot2::ggplot(df, ggplot2::aes(sample = sample)) +
      ggplot2::stat_qq(color = "steelblue", alpha = 0.7) +
      ggplot2::stat_qq_line(color = "darkred", linetype = "dashed") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0(x$var_name, ": Normal Q-Q"), x = "Theoretical Quantiles", y = "Sample Quantiles")
    
  } else if (type == "acf") {
    df <- data.frame(lag = d$acf$lag, acf = d$acf$value)
    ggplot2::ggplot(df, ggplot2::aes(x = lag, y = acf)) +
      ggplot2::geom_col(fill = "steelblue", alpha = 0.85) +
      ggplot2::geom_hline(yintercept = c(-d$acf$ci_bound, d$acf$ci_bound), linetype = "dotted", color = "darkred") +
      ggplot2::geom_hline(yintercept = 0, color = "grey40") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0(x$var_name, ": Autocorrelation (ACF)"), x = "Lag", y = "ACF")
    
  } else if (type == "pacf") {
    df <- data.frame(lag = d$pacf$lag, pacf = d$pacf$value)
    ggplot2::ggplot(df, ggplot2::aes(x = lag, y = pacf)) +
      ggplot2::geom_col(fill = "steelblue", alpha = 0.85) +
      ggplot2::geom_hline(yintercept = c(-d$pacf$ci_bound, d$pacf$ci_bound), linetype = "dotted", color = "darkred") +
      ggplot2::geom_hline(yintercept = 0, color = "grey40") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0(x$var_name, ": Partial Autocorrelation (PACF)"), x = "Lag", y = "PACF")
    
  } else if (type == "histogram") {
    df <- data.frame(value = x$series)
    ggplot2::ggplot(df, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
      ggplot2::geom_density(color = "darkorange", linewidth = 1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0(x$var_name, ": Distribution"), x = x$var_name, y = "Density")
    
  } else {
    # type is "series" or "residuals_time" (an alias, kept for compatibility
    # with app.R's generic renderer, which uses "residuals_time" as the ID
    # for this box across every model type): the raw (or differenced)
    # series over time.
    df <- data.frame(
      time  = if (!is.null(x$time_index)) x$time_index else seq_along(x$series),
      value = x$series
    )
    ggplot2::ggplot(df, ggplot2::aes(x = time, y = value)) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::geom_point(color = "steelblue", size = 1, alpha = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste0(x$var_name, if (isTRUE(x$differenced)) " (Differenced)" else "", " Over Time"),
        x = "Time", y = x$var_name
      )
  }
}
