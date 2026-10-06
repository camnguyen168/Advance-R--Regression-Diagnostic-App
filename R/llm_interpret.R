# llm_interpret.R
# Plain-language interpretation of diag_lm diagnostics via the Groq API.
# Never called automatically -- app.R only invokes get_llm_interpretation()
# when the user clicks "Generate AI Interpretation", and it never sends raw
# data, only the already-computed aggregate diagnostics.

library(ellmer)
library(commonmark)
library(htmltools)

# ---- Shared helpers ---------------------------------------------------------

#' Format a p-value compactly for LLM input (internal helper)
#' @noRd
.fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("NA")
  if (p < 0.0001) return("< 0.0001")
  paste0("= ", format(round(p, 4), nsmall = 4))
}

#' Format a numeric value (or vector) compactly for LLM input (internal helper)
#' @noRd
.fmt_num <- function(v, digits = 4) {
  if (is.null(v) || length(v) == 0 || all(is.na(v))) return("NA")
  paste(round(v, digits), collapse = ", ")
}

#' Build the "Coefficient estimates: ..." line plus Fixed effects / Standard
#' error lines for the diagnostic summary (internal helper).
#'
#' Coefficient list is capped, because fixed effects (factor() dummies) can
#' turn a single fixed-effect variable with many categories into hundreds of
#' extra coefficients. Left uncapped, that bloats the prompt enough to trip
#' Groq's request-size limit (HTTP 413) -- summarize instead of listing every
#' dummy once the table gets large. Each entry includes its p-value so the
#' LLM can speak to statistical significance, not just direction/magnitude.
#'
#' @param object An object of class 'diag_lm' (non-univariate)
#' @return A character vector of summary lines (coefficients, fixed effects, SE type)
#' @noRd
.format_coefficient_lines <- function(object) {
  coef_mat <- object$coefficients
  n_coef <- if (is.null(coef_mat)) 0 else nrow(coef_mat)
  MAX_COEF_LISTED <- 15
  pval_col <- if (n_coef > 0) intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(coef_mat)) else character(0)
  
  fmt_coef_row <- function(idx) {
    line <- sprintf("%s = %s", rownames(coef_mat)[idx], .fmt_num(coef_mat[idx, 1]))
    if (length(pval_col) > 0) {
      line <- paste0(line, " (p ", .fmt_p(coef_mat[idx, pval_col[1]]), ")")
    }
    line
  }
  
  if (n_coef == 0) {
    coef_summary <- "None"
  } else if (n_coef <= MAX_COEF_LISTED) {
    coef_summary <- paste(sapply(seq_len(n_coef), fmt_coef_row), collapse = "; ")
  } else {
    # Rank by |t value| (or |z value| for glm) when available, else by |estimate|,
    # so the summary highlights the coefficients that actually matter.
    rank_col <- intersect(c("t value", "z value"), colnames(coef_mat))
    rank_vals <- if (length(rank_col) > 0) abs(coef_mat[, rank_col[1]]) else abs(coef_mat[, 1])
    top_idx <- order(rank_vals, decreasing = TRUE)[seq_len(min(MAX_COEF_LISTED, n_coef))]
    top_idx <- top_idx[order(top_idx)]  # keep original row order for readability
    
    top_lines <- paste(sapply(top_idx, fmt_coef_row), collapse = "; ")
    coef_summary <- paste0(
      top_lines,
      " (showing ", length(top_idx), " of ", n_coef, " coefficients, ranked by significance; ",
      "remainder omitted -- likely fixed-effect dummy variables)"
    )
  }
  
  lines <- c(
    paste0("Model type: ", object$data_type),
    paste0("Formula: ", deparse(object$formula)),
    paste0("Coefficient estimates: ", coef_summary)
  )
  
  if (!is.null(object$fe_vars) && length(object$fe_vars) > 0) {
    lines <- c(lines, paste0("Fixed effects included for: ", paste(object$fe_vars, collapse = ", ")))
  }
  if (!is.null(object$se_type)) {
    lines <- c(lines, paste0("Standard error type: ", object$se_type))
  }
  
  lines
}

#' Truncate a summary to a hard character cap before sending to the API
#' (internal helper). Final safety net regardless of what caused the bloat.
#' @noRd
.cap_summary <- function(summary_text, max_chars = 6000) {
  if (nchar(summary_text) > max_chars) {
    summary_text <- paste0(substr(summary_text, 1, max_chars), "\n... [truncated -- summary exceeded size limit]")
  }
  summary_text
}

#' Call Groq via ellmer::chat_groq() and return rendered HTML, or a friendly
#' error message on failure (internal helper).
#'
#' Uses ellmer's chat interface instead of hand-rolled httr2 requests --
#' chat_groq() handles the request/response plumbing, and chat$chat()
#' returns the model's reply as a plain character string directly (no
#' manual JSON parsing of body$choices[[1]]$message$content needed).
#' @noRd
.call_groq_chat <- function(system_prompt, user_content, api_key, model, max_tokens = 1024) {
  if (!nzchar(api_key)) {
    return(shiny::HTML(paste0(
      "<div class='alert alert-info'>AI interpretation is not configured. ",
      "Set the <code>GROQ_API_KEY</code> environment variable (e.g. in a local ",
      "<code>.Renviron</code> file) to enable this feature.</div>"
    )))
  }
  
  # ellmer's chat_groq() reads GROQ_API_KEY from the environment by default,
  # the same variable this function's api_key parameter defaults to -- so
  # the normal call path (no explicit override) already picks up the key
  # checked above. If a caller passes a *different* api_key, honor it by
  # setting it for the duration of this call only, then restoring whatever
  # was there before.
  old_key <- Sys.getenv("GROQ_API_KEY", unset = NA)
  if (!identical(api_key, Sys.getenv("GROQ_API_KEY"))) {
    Sys.setenv(GROQ_API_KEY = api_key)
    on.exit({
      if (is.na(old_key)) Sys.unsetenv("GROQ_API_KEY") else Sys.setenv(GROQ_API_KEY = old_key)
    }, add = TRUE)
  }
  
  result <- tryCatch({
    # Deliberately minimal, matching the confirmed-working pattern from the
    # course exercises (chat_groq(model=..., system_prompt=...)) as closely
    # as possible. temperature/max_tokens are useful but not essential --
    # if a 404 turns out to be caused by something in params(), removing
    # them here isolates that.
    chat <- ellmer::chat_groq(
      model = model,
      system_prompt = system_prompt,
      base_url = "https://api.groq.com/openai/v1",
      params = ellmer::params(temperature = 0.7),
      echo = "none"
    )
    text <- chat$chat(user_content)
    list(ok = TRUE, text = text)
  }, error = function(e) {
    msg <- conditionMessage(e)
    # Pull out whatever extra detail is available on the condition object --
    # httr2 errors often carry the parsed response body, which usually
    # explains WHY (e.g. "model_decommissioned", "invalid model ID") far
    # better than the bare "HTTP 404 Not Found" status line does.
    extra_detail <- tryCatch({
      body <- NULL
      if (!is.null(e$body)) body <- e$body
      else if (!is.null(e$resp_body)) body <- e$resp_body
      if (!is.null(body)) paste0(" Detail: ", paste(utils::capture.output(str(body)), collapse = " ")) else ""
    }, error = function(e2) "")
    
    # Translate common failure modes into something actionable rather than
    # surfacing a raw HTTP status code.
    friendly <- if (grepl("413", msg, fixed = TRUE)) {
      paste(
        "the diagnostics summary was too large to send. This usually happens",
        "with a very high-cardinality fixed-effect variable (many dummy",
        "coefficients) -- try removing it or grouping it into fewer categories."
      )
    } else if (grepl("404", msg, fixed = TRUE)) {
      paste0(
        "the model '", model, "' was not found at Groq's endpoint (HTTP 404). ",
        "This usually means the model ID has been deprecated/renamed -- check ",
        "https://console.groq.com/docs/models for the current list of available ",
        "model IDs and update the 'model' argument.", extra_detail
      )
    } else if (grepl("timeout|timed out", msg, ignore.case = TRUE)) {
      "the request to Groq timed out. Please try again."
    } else if (grepl("401|403|unauthorized|invalid api key", msg, ignore.case = TRUE)) {
      "the Groq API key was rejected. Check the GROQ_API_KEY environment variable."
    } else {
      paste0(msg, extra_detail)
    }
    list(ok = FALSE, text = friendly)
  })
  
  if (!result$ok) {
    return(shiny::HTML(paste0(
      "<div class='alert alert-warning'>AI interpretation unavailable: ",
      htmltools::htmlEscape(result$text), "</div>"
    )))
  }
  
  body_html <- commonmark::markdown_html(
    result$text,
    extensions = c("table", "strikethrough")
  )
  
  shiny::HTML(paste0("<div class='ai-interpretation'>", body_html, "</div>"))
}

# ==============================================================================
# ---- 1. General diagnostic interpretation (bottom of Diagnostics tab) ------
# ==============================================================================

#' Build a compact, privacy-safe text summary of a diag_lm object's diagnostics
#'
#' Only aggregated statistics are included -- never raw data rows -- to keep
#' the prompt small and avoid sending potentially sensitive data to a
#' third-party API.
#'
#' @param object An object of class 'diag_lm' (or its 'diag_lm_uni' subclass)
#' @return A single character string describing the model and its diagnostics
#' @export
build_diag_summary <- function(object) {
  if (!inherits(object, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # ---- Univariate / self-referential time series: no formula, no
  # coefficients, no VIF/BP/DW -- build a separate summary entirely. ----
  if (inherits(object, "diag_lm_uni")) {
    d <- object$diagnostics
    
    lines <- c(
      paste0("Model type: univariate time series (series analyzed against its own history)"),
      paste0("Series name: ", object$var_name),
      paste0("Observations: ", length(object$series)),
      paste0("First-differenced: ", isTRUE(object$differenced)),
      paste0("Mean: ", .fmt_num(d$mean)),
      paste0("Std. Dev.: ", .fmt_num(d$sd)),
      paste0("Ljung-Box p-value (autocorrelation with own past): ", .fmt_num(d$ts$ljung_box_pvalue)),
      paste0("Augmented Dickey-Fuller (ADF) p-value (stationarity): ", .fmt_num(d$ts$adf_pvalue)),
      paste0("Jarque-Bera p-value (distribution normality): ", .fmt_num(d$ts$jb_pvalue)),
      paste0("Shapiro-Wilk p-value (distribution normality): ", .fmt_num(d$shapiro_pvalue))
    )
    
    return(paste(lines, collapse = "\n"))
  }
  
  lines <- .format_coefficient_lines(object)
  d <- object$diagnostics
  
  if (identical(object$data_type, "binary")) {
    lg <- d$logistic
    lines <- c(
      lines,
      paste0("Hosmer-Lemeshow p-value: ", .fmt_num(lg$hl_pvalue)),
      paste0("McFadden pseudo-R2: ", .fmt_num(lg$mcfadden_r2)),
      paste0("AUC: ", .fmt_num(lg$auc)),
      paste0("Accuracy (0.5 cutoff): ", .fmt_num(lg$accuracy)),
      paste0("Separation flag: ", isTRUE(lg$separation_flag))
    )
  } else {
    normality_label <- if (!is.null(d$normality_test_name) && !is.na(d$normality_test_name)) d$normality_test_name else "Shapiro-Wilk"
    lines <- c(
      lines,
      paste0("Breusch-Pagan p-value (heteroskedasticity): ", .fmt_num(d$bp_pvalue)),
      paste0("Durbin-Watson p-value (serial correlation): ", .fmt_num(d$dw_pvalue)),
      paste0(normality_label, " p-value (normality): ", .fmt_num(d$shapiro_pvalue))
    )
  }
  
  if (!is.null(d$vif_scores) && !(length(d$vif_scores) == 1 && is.na(d$vif_scores))) {
    lines <- c(lines, paste0("VIF scores: ", .fmt_num(d$vif_scores)))
  }
  
  if (!is.null(d$ts)) {
    lines <- c(
      lines,
      paste0("Breusch-Godfrey p-value: ", .fmt_num(d$ts$bg_pvalue)),
      paste0("Ljung-Box p-value: ", .fmt_num(d$ts$ljung_box_pvalue)),
      paste0("Augmented Dickey-Fuller (ADF) p-value (stationarity): ", .fmt_num(d$ts$adf_pvalue))
    )
  }
  
if (!is.null(d$panel)) {
  lines <- c(lines,
    paste0("Hausman test p-value: ", .fmt_num(d$panel$hausman_pvalue)),
    paste0("Breusch-Pagan LM (poolability) p-value: ", .fmt_num(d$panel$lm_pvalue)),
    paste0("Panel Breusch-Godfrey (serial correlation) p-value: ", .fmt_num(d$panel$pbg_pvalue)),
    paste0("Time-fixed-effects (pFtest) p-value: ", .fmt_num(d$panel$pftest_pvalue))
  )
}
  
  .cap_summary(paste(lines, collapse = "\n"))
}

#' Get an AI-generated plain-language interpretation of diagnostics via Groq
#'
#' General diagnostic health check: which assumption tests pass/fail and
#' what to do about it.
#'
#' @param object An object of class 'diag_lm' (or its 'diag_lm_uni' subclass)
#' @param api_key Groq API key. Defaults to GROQ_API_KEY environment variable.
#' @param model Groq model identifier. Defaults to openai/gpt-oss-120b --
#'   Groq deprecated llama-3.3-70b-versatile (June 17, 2026) and recommends
#'   this as the direct replacement. See https://console.groq.com/docs/deprecations
#'   if this needs updating again in the future.
#' @return An HTML string (shiny::HTML) suitable for uiOutput/renderUI
#' @export
get_llm_interpretation <- function(object,
                                   api_key = Sys.getenv("GROQ_API_KEY"),
                                   model = "openai/gpt-oss-120b") {
  
  summary_text <- build_diag_summary(object)
  is_univariate <- inherits(object, "diag_lm_uni")
  
  system_prompt <- if (is_univariate) {
    paste(
      "You are an econometrics teaching assistant. Explain time-series diagnostic",
      "output in plain, non-technical language for a student, using well-structured",
      "Markdown so it renders cleanly as HTML: a short '## Summary' paragraph",
      "(1-2 sentences on whether this series behaves like a stable, independent",
      "process or shows memory/trend), then a '## Diagnostic Findings' bulleted",
      "list with one bullet per test (use the exact test name as given in the",
      "input, e.g. 'Ljung-Box', 'Augmented Dickey-Fuller', 'Jarque-Bera',",
      "'Shapiro-Wilk' -- never invent or rename a test; state pass/fail in bold,",
      "then a one-line plain-language meaning), then a '## Recommended Actions'",
      "bulleted list with one concrete remedy per violated assumption (e.g.",
      "first-differencing for non-stationarity, an ARIMA model for significant",
      "autocorrelation; omit this section if nothing is violated). Note this is",
      "a single series analyzed against its own past, not a regression -- do",
      "not refer to 'predictors', 'coefficients', or 'the model fit'. Keep the",
      "whole answer under 250 words. Do not repeat raw p-values verbatim beyond",
      "what is needed to justify the pass/fail call."
    )
  } else {
    paste(
      "You are an econometrics teaching assistant. Explain regression",
      "diagnostic output in plain, non-technical language for a student,",
      "using well-structured Markdown so it renders cleanly as HTML:",
      "a short '## Summary' paragraph (1-2 sentences on overall model health),",
      "then a '## Diagnostic Findings' bulleted list with one bullet per test",
      "(use the exact test name as given in the input, e.g. 'Breusch-Pagan',",
      "'Durbin-Watson', 'Shapiro-Wilk', 'Jarque-Bera', 'Augmented Dickey-Fuller',",
      "'Hosmer-Lemeshow' -- never invent or rename a test; state pass/fail in",
      "bold, then a one-line plain-language",
      "meaning), then a '## Recommended Actions' bulleted list with one concrete",
      "remedy per violated assumption (omit this section if nothing is violated).",
      "This is about model ASSUMPTIONS, not what individual coefficients mean --",
      "do not interpret specific coefficient estimates here. Keep the whole",
      "answer under 250 words. Do not repeat raw p-values verbatim beyond what",
      "is needed to justify the pass/fail call."
    )
  }
  
  .call_groq_chat(system_prompt, summary_text, api_key, model, max_tokens = 1024)
}

# ==============================================================================
# ---- 2. Per-plot interpretation (lightbulb button above each plot) ---------
# ==============================================================================

#' Get a short AI-generated interpretation of a single diagnostic plot
#'
#' Unlike get_llm_interpretation() (a full diagnostic health check), this
#' gives a focused 2-3 sentence read on ONE specific plot -- meant to be
#' triggered by clicking a small lightbulb icon above that plot, not the
#' main "Generate AI Interpretation" panel.
#'
#' @param object An object of class 'diag_lm' (or its 'diag_lm_uni' subclass)
#' @param plot_type One of the type strings accepted by plot.diag_lm() /
#'   plot.diag_lm_uni(): "residuals", "qq", "scale_location", "histogram",
#'   "acf", "pacf", "residuals_time", "binned_residuals", "roc",
#'   "calibration", "series"
#' @param api_key Groq API key. Defaults to GROQ_API_KEY environment variable.
#' @param model Groq model identifier.
#' @return An HTML string (shiny::HTML) suitable for uiOutput/renderUI
#' @export
get_plot_interpretation <- function(object, plot_type,
                                    api_key = Sys.getenv("GROQ_API_KEY"),
                                    model = "openai/gpt-oss-120b") {
  
  plot_label <- switch(plot_type,
                       "residuals"        = "Residuals vs Fitted",
                       "qq"               = "Normal Q-Q",
                       "scale_location"   = "Scale-Location",
                       "histogram"        = "Residual/Series Histogram",
                       "acf"              = "Autocorrelation (ACF)",
                       "pacf"             = "Partial Autocorrelation (PACF)",
                       "residuals_time"   = "Residuals/Series over Time",
                       "binned_residuals" = "Binned Residuals",
                       "roc"              = "ROC Curve",
                       "calibration"      = "Calibration Plot",
                       "series"           = "Series over Time",
                       plot_type
  )
  
  # Reuses the same compact, privacy-safe diagnostics summary as the main
  # interpretation panel -- no need for a separate summary builder, just a
  # narrower, plot-specific instruction on top of the same input.
  summary_text <- build_diag_summary(object)
  
  system_prompt <- paste(
    "You are an econometrics teaching assistant. You are given a compact",
    "summary of a regression model's diagnostics. The user is looking",
    "specifically at the", shQuote(plot_label), "plot right now, not the",
    "full diagnostic report. Write EXACTLY 2-3 short sentences: (1) what",
    "this specific plot is checking for, in plain language, and (2) based",
    "ONLY on whichever statistic(s) in the summary are actually relevant to",
    "THIS plot, what it most likely shows here and whether that's a",
    "concern. For a Q-Q or histogram plot, reference the normality test",
    "(Shapiro-Wilk/Jarque-Bera). For Residuals vs Fitted or Scale-Location,",
    "reference the heteroskedasticity test (Breusch-Pagan). For ACF/PACF,",
    "residuals-over-time, or a stationarity-related plot, reference serial",
    "correlation or the ADF test. For ROC/Calibration/Binned Residuals,",
    "reference AUC, Hosmer-Lemeshow, or accuracy. Do not mention or",
    "describe any statistic that isn't relevant to this specific plot.",
    "No headers, no bullet points -- plain prose only, under 60 words total."
  )
  
  .call_groq_chat(system_prompt, summary_text, api_key, model, max_tokens = 220)
}