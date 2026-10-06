# diag_lm_class.R
# S3 container plus methods for model diagnostics used by the app.

library(lmtest)  # bptest() - Breusch-Pagan test
library(plm)     # panel models and panel diagnostics
library(car)     # vif() - variance inflation factors
library(gt)      # nicely formatted summary tables
library(ggplot2) # diagnostic plots
library(dplyr)   # used in the summary pipeline
library(tibble)  # rownames_to_column()

#' Constructor function for 'diag_lm' S3 class
#' 
#' @param model A fitted 'lm' or 'plm' object
#' @param data_type Character string: "cross-section", "time-series", or "panel"
#' @return An object of class 'diag_lm'
new_diag_lm <- function(model, data_type = "cross-section") {
  data_type <- match.arg(
    data_type,
    c("cross-section", "time-series", "panel", "cs", "ts")
  )
  data_type <- switch(
    data_type,
    cs = "cross-section",
    ts = "time-series",
    data_type
  )
  
  # Validate early so failures are easy to understand.
  if (!inherits(model, c("lm", "plm"))) {
    stop("Error: Input must be a fitted model of class 'lm' or 'plm'.", call. = FALSE)
  }
  
  # Keep core model pieces for downstream methods.
  model_formula <- formula(model)
  coef_table <- summary(model)$coefficients
  
  # Breusch-Pagan for heteroskedasticity.
  bp_result <- tryCatch({
    lmtest::bptest(model)
  }, error = function(e) {
    message("Notice: Breusch-Pagan test could not be computed.")
    return(NA)
  })
  
  # Durbin-Watson for serial correlation.
  dw_result <- tryCatch({
    lmtest::dwtest(model)
  }, error = function(e) {
    message("Notice: Durbin-Watson test could not be computed.")
    return(NA)
  })
  
  # VIF can fail for one-predictor models; keep NA in that case.
  vif_result <- tryCatch({
    car::vif(model)
  }, error = function(e) {
    message("Notice: VIF could not be computed (likely a single-predictor model).")
    return(NA) 
  })
  
  # Shapiro-Wilk can fail for constant residuals or very large samples.
  shapiro_res <- tryCatch({
    shapiro.test(residuals(model))
  }, error = function(e) {
    return(NA)
  })
  
  # Optional diagnostics by data structure.
  ts_diagnostics <- NULL
  panel_diagnostics <- NULL
  
  if (data_type == "time-series") {
    bg_result <- tryCatch({
      lmtest::bgtest(model)
    }, error = function(e) {
      message("Notice: Breusch-Godfrey test could not be computed.")
      return(NA)
    })
    
    lb_result <- tryCatch({
      Box.test(residuals(model), type = "Ljung-Box")
    }, error = function(e) {
      message("Notice: Ljung-Box test could not be computed.")
      return(NA)
    })
    
    ts_diagnostics <- list(
      bg_statistic = if (is.list(bg_result)) unname(bg_result$statistic) else NA,
      bg_pvalue = if (is.list(bg_result)) unname(bg_result$p.value) else NA,
      ljung_box_statistic = if (is.list(lb_result)) unname(lb_result$statistic) else NA,
      ljung_box_pvalue = if (is.list(lb_result)) unname(lb_result$p.value) else NA
    )
  }
  
  if (data_type == "panel") {
    panel_diagnostics <- tryCatch({
      if (!inherits(model, "plm")) {
        stop("Panel diagnostics require a 'plm' pooling model.")
      }
      if (!identical(model$args$model, "pooling")) {
        stop("For data_type = 'panel', pass a pooling plm model.")
      }
      
      panel_formula <- formula(model)
      panel_data <- model.frame(model)
      panel_index <- names(plm::index(model))
      
      fe_model <- plm::plm(
        formula = panel_formula,
        data = panel_data,
        model = "within",
        index = panel_index
      )
      re_model <- plm::plm(
        formula = panel_formula,
        data = panel_data,
        model = "random",
        index = panel_index
      )
      
      panel_bp <- plm::plmtest(model, type = "bp")
      hausman <- plm::phtest(fe_model, re_model)
      wooldridge <- plm::pbgtest(fe_model)
      
      list(
        bp_statistic = unname(panel_bp$statistic),
        bp_pvalue = unname(panel_bp$p.value),
        hausman_statistic = unname(hausman$statistic),
        hausman_pvalue = unname(hausman$p.value),
        wooldridge_statistic = unname(wooldridge$statistic),
        wooldridge_pvalue = unname(wooldridge$p.value)
      )
    }, error = function(e) {
      message("Notice: Panel diagnostics could not be computed.")
      list(
        bp_statistic = NA,
        bp_pvalue = NA,
        hausman_statistic = NA,
        hausman_pvalue = NA,
        wooldridge_statistic = NA,
        wooldridge_pvalue = NA
      )
    })
  }
  
  # Store everything in one object.
  obj <- list(
    raw_model     = model,
    data_type     = data_type,
    formula       = model_formula,
    coefficients  = coef_table,
    residuals     = residuals(model),
    fitted_values = fitted(model),
    diagnostics   = list(
      bp_statistic = if (is.list(bp_result)) unname(bp_result$statistic) else NA,
      bp_pvalue    = if (is.list(bp_result)) unname(bp_result$p.value) else NA,
      dw_statistic = if (is.list(dw_result)) unname(dw_result$statistic) else NA,
      dw_pvalue    = if (is.list(dw_result)) unname(dw_result$p.value) else NA,
      vif_scores   = vif_result,
      shapiro_pvalue = if (is.list(shapiro_res)) unname(shapiro_res$p.value) else NA,
      ts           = ts_diagnostics,
      panel        = panel_diagnostics
    )
  )
  
  # Set class for S3 dispatch.
  class(obj) <- "diag_lm"
  
  return(obj)
}


#' Print method for 'diag_lm' objects
#'
#' @param x An object of class 'diag_lm'
#' @param ... Further arguments passed to or from other methods
#' @return Invisibly returns the object
print.diag_lm <- function(x, ...) {
  
  if (!inherits(x, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # Compact console overview.
  cat("Diagnostic Linear Model (diag_lm)\n")
  cat("---------------------------------\n")
  cat("Formula: ")
  print(x$formula)
  cat("\nCoefficients:\n")
  print(round(x$coefficients, 4))
  
  cat("\nDiagnostics:\n")
  cat(sprintf("  Breusch-Pagan statistic : %.4f\n", x$diagnostics$bp_statistic))
  cat(sprintf("  Breusch-Pagan p-value   : %.4f", x$diagnostics$bp_pvalue))
  
  # Small p-value suggests heteroskedasticity.
  if (x$diagnostics$bp_pvalue < 0.05) {
    cat("  (evidence of heteroskedasticity)\n")
  } else {
    cat("  (no evidence of heteroskedasticity)\n")
  }
  
  cat("  VIF scores              :\n")
  # VIF is NA for one-predictor models.
  if (length(x$diagnostics$vif_scores) == 1 && is.na(x$diagnostics$vif_scores)) {
    cat("    Not available (single-predictor model).\n")
  } else {
    print(round(x$diagnostics$vif_scores, 4))
  }
  
  invisible(x)
}


#' Custom summary method for 'diag_lm'
#'
#' Returns a formatted gt summary table that adapts to the selected
#' data structure.
#'
#' @param object An object of class 'diag_lm'
#' @param ... Additional arguments
#' @return A 'gt_tbl' object
#' @export
summary.diag_lm <- function(object, ...) {
  
  if (!inherits(object, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # Keep term names as a regular column for gt.
  coef_df <- as.data.frame(object$coefficients) |>
    rownames_to_column("Term")
  
  data_type <- if (is.null(object$data_type)) "cross-section" else object$data_type
  bp_pval <- round(object$diagnostics$bp_pvalue, 4)
  dw_pval <- round(object$diagnostics$dw_pvalue, 4)
  shapiro_pval <- if (is.null(object$diagnostics$shapiro_pvalue)) {
    NA
  } else {
    object$diagnostics$shapiro_pvalue
  }
  shapiro_text <- if (is.na(shapiro_pval)) "N/A" else round(shapiro_pval, 4)
  
  # Cross-section layout.
  if (identical(data_type, "cross-section")) {
    summary_table <- gt(coef_df) |>
      tab_header(
        title = "OLS Regression Coefficient Summary",
        subtitle = "Estimated coefficients and statistical significance"
      ) |>
      fmt_number(
        columns = 2:ncol(coef_df),
        decimals = 3
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      )
    
    return(summary_table)
  }
  
  # Time-series layout with BG and Ljung-Box notes.
  if (identical(data_type, "time-series")) {
    bg_stat <- object$diagnostics$ts$bg_statistic
    bg_pval <- object$diagnostics$ts$bg_pvalue
    lb_stat <- object$diagnostics$ts$ljung_box_statistic
    lb_pval <- object$diagnostics$ts$ljung_box_pvalue
    
    bg_outcome <- if (!is.na(bg_pval) && bg_pval < 0.05) {
      "Serial correlation detected"
    } else {
      "No serial correlation evidence"
    }
    
    lb_outcome <- if (!is.na(lb_pval) && lb_pval < 0.05) {
      "Autocorrelation detected"
    } else {
      "No autocorrelation evidence"
    }
    
    summary_table <- gt(coef_df) |>
      tab_header(
        title = "Time-Series OLS Summary",
        subtitle = "Coefficient estimates with serial-correlation checks"
      ) |>
      fmt_number(
        columns = 2:ncol(coef_df),
        decimals = 3
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_source_note(
        source_note = paste0(
          "Breusch-Godfrey: statistic = ", round(bg_stat, 4),
          ", p-value = ", round(bg_pval, 4),
          " (", bg_outcome, ")"
        )
      ) |>
      tab_source_note(
        source_note = paste0(
          "Ljung-Box: statistic = ", round(lb_stat, 4),
          ", p-value = ", round(lb_pval, 4),
          " (", lb_outcome, ")"
        )
      ) |>
      tab_source_note(
        source_note = paste0(
          "Shapiro-Wilk normality p-value: ", shapiro_text
        )
      )
    
    return(summary_table)
  }
  
  # Panel layout focused on model-specification tests.
  if (identical(data_type, "panel")) {
    panel_df <- data.frame(
      Test = c("Panel Breusch-Pagan (LM)", "Hausman"),
      Statistic = c(
        object$diagnostics$panel$bp_statistic,
        object$diagnostics$panel$hausman_statistic
      ),
      `p-value` = c(
        object$diagnostics$panel$bp_pvalue,
        object$diagnostics$panel$hausman_pvalue
      ),
      Outcome = c(
        if (!is.na(object$diagnostics$panel$bp_pvalue) && object$diagnostics$panel$bp_pvalue < 0.05) {
          "Panel effects detected"
        } else {
          "No panel effects evidence"
        },
        if (!is.na(object$diagnostics$panel$hausman_pvalue) && object$diagnostics$panel$hausman_pvalue < 0.05) {
          "Prefer Fixed Effects"
        } else {
          "Random Effects remains admissible"
        }
      ),
      check.names = FALSE
    )
    
    summary_table <- gt(panel_df) |>
      tab_header(
        title = "Panel Model Specification Summary",
        subtitle = "Hausman and Panel Breusch-Pagan diagnostic checks"
      ) |>
      fmt_number(
        columns = 2:3,
        decimals = 4
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_source_note(
        source_note = paste0(
          "Shapiro-Wilk normality p-value: ", shapiro_text
        )
      )
    
    return(summary_table)
  }
  
  # Fallback layout.
  summary_table <- gt(coef_df) |>
    tab_header(
      title = "OLS Regression & Diagnostics Summary",
      subtitle = paste(
        "Breusch-Pagan Test p-value:", bp_pval,
        "| Durbin-Watson Test p-value:", dw_pval,
        "| Shapiro-Wilk Test p-value:", shapiro_text
      )
    ) |>
    fmt_number(
      columns = 2:ncol(coef_df),
      decimals = 3
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
  
  return(summary_table)
}


#' S3 generic for remediation advice
#'
#' @param object Model diagnostics object
#' @param ... Additional arguments
#' @return Character vector of HTML-formatted messages
#' @export
remediation_advice <- function(object, ...) {
  UseMethod("remediation_advice")
}


#' Remediation advice for diag_lm objects
#'
#' Returns HTML alert messages based on stored diagnostic p-values.
#'
#' @param object An object of class 'diag_lm'
#' @param ... Additional arguments
#' @return Character vector of HTML-formatted messages
#' @export
remediation_advice.diag_lm <- function(object, ...) {
  if (!inherits(object, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  alerts <- character(0)
  
  if (object$data_type %in% c("cross-section", "cs")) {
    bp_pvalue <- object$diagnostics$bp_pvalue
    if (!is.na(bp_pvalue) && bp_pvalue < 0.05) {
      alerts <- c(
        alerts,
        "<div class='alert alert-warning'>Warning: Heteroskedasticity detected. Consider using robust standard errors.</div>"
      )
    }
  }
  
  if (object$data_type %in% c("time-series", "ts")) {
    bg_pvalue <- object$diagnostics$ts$bg_pvalue
    ljung_box_pvalue <- object$diagnostics$ts$ljung_box_pvalue
    
    if ((!is.na(bg_pvalue) && bg_pvalue < 0.05) ||
        (!is.na(ljung_box_pvalue) && ljung_box_pvalue < 0.05)) {
      alerts <- c(
        alerts,
        "<div class='alert alert-warning'>Warning: Residual autocorrelation detected. Consider adjusting lagging variables.</div>"
      )
    }
  }
  
  if (identical(object$data_type, "panel")) {
    hausman_pvalue <- object$diagnostics$panel$hausman_pvalue
    if (!is.na(hausman_pvalue) && hausman_pvalue < 0.05) {
      alerts <- c(
        alerts,
        "<div class='alert alert-info'>Advice: Hausman test rejects Random Effects. Use a Fixed Effects (Within) model specification.</div>"
      )
    }
  }
  
  shapiro_pval <- object$diagnostics$shapiro_pvalue
  if (!is.null(shapiro_pval) && !is.na(shapiro_pval) && shapiro_pval < 0.05) {
    alerts <- c(
      alerts,
      paste0(
        "<div class='alert alert-warning'><b>Warning: Normality Assumption Violated!</b><br/>",
        "The Shapiro-Wilk test rejects residual normality (p = ",
        round(shapiro_pval, 4),
        "). Coefficients can remain unbiased, but t/F tests may be unreliable in small samples. ",
        "Check the Q-Q plot for heavy tails or outliers, and consider a log or other transformation.</div>"
      )
    )
  }
  
  return(alerts)
}


#' Custom plot method for 'diag_lm'
#'
#' Draws a diagnostic plot from the stored residuals/fitted values.
#' The 'type' argument picks one of:
#'   "residuals"      - residuals vs fitted (heteroskedasticity / non-linearity)
#'   "qq"             - normal Q-Q plot (normality of residuals)
#'   "scale_location" - scale-location plot (spread of residuals)
#'   "histogram"      - histogram of the residuals (normality / skew)
#'   "acf"            - residual ACF bars (serial correlation)
#'
#' @param x An object of class 'diag_lm'
#' @param type String indicating which diagnostic plot to show
#' @param ... Additional arguments
#' @return A 'ggplot' object
#' @export
plot.diag_lm <- function(x, type = "residuals", ...) {
  
  if (!inherits(x, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # Build a plotting frame once and reuse it.
  std_resid <- x$residuals / sd(x$residuals)
  plot_data <- data.frame(
    fitted     = x$fitted_values,
    residuals  = x$residuals,
    std_resid  = std_resid
  )
  
  # Residuals vs fitted.
  if (type == "residuals") {
    
    p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      geom_smooth(method = "loess", se = FALSE, color = "darkorange") +
      theme_minimal() +
      labs(
        title = "Residuals vs Fitted",
        subtitle = "Checks for non-linear patterns and heteroskedasticity",
        x = "Fitted Values",
        y = "Residuals"
      )
    
    return(p)
    
  } else if (type == "qq") {
    
    # Normal Q-Q.
    p <- ggplot(plot_data, aes(sample = std_resid)) +
      stat_qq(alpha = 0.6, color = "steelblue", size = 2) +
      stat_qq_line(color = "darkred", linetype = "dashed") +
      theme_minimal() +
      labs(
        title = "Normal Q-Q",
        subtitle = "Checks whether the residuals are normally distributed",
        x = "Theoretical Quantiles",
        y = "Standardised Residuals"
      )
    
    return(p)
    
  } else if (type == "scale_location") {
    
    # Scale-location view.
    plot_data$root_abs_resid <- sqrt(abs(plot_data$std_resid))
    
    p <- ggplot(plot_data, aes_string(x = "fitted", y = "root_abs_resid")) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "darkorange") +
      theme_minimal() +
      labs(
        title = "Scale-Location",
        subtitle = "Checks whether residual spread is constant",
        x = "Fitted Values",
        y = expression(sqrt("|Standardised Residuals|"))
      )
    
    return(p)
    
  } else if (type == "histogram") {
    
    # Residual histogram.
    p <- ggplot(plot_data, aes(x = residuals)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
      geom_density(color = "darkorange", linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(
        title = "Distribution of Residuals",
        subtitle = "Checks for skew and departures from normality",
        x = "Residuals",
        y = "Density"
      )
    
    return(p)
    
  } else if (type == "acf") {
    
    # Residual autocorrelation bars.
    acf_obj <- acf(x$residuals, plot = FALSE, na.action = na.pass)
    acf_df <- data.frame(
      lag = as.numeric(acf_obj$lag),
      acf = as.numeric(acf_obj$acf)
    )
    
    p <- ggplot(acf_df, aes(x = lag, y = acf)) +
      geom_col(fill = "steelblue", alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(
        title = "Residual Autocorrelation (ACF)",
        subtitle = "Visual check for serial correlation across lags",
        x = "Lag",
        y = "ACF"
      )
    
    return(p)
    
  } else {
    # Unknown plot type.
    stop("Error: Plot type '", type, "' is not implemented yet.", call. = FALSE)
  }
}
