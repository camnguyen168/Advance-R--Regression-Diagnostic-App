# Quick smoke checks for llm_interpret.R.
# Not a full test framework, just a script to catch obvious regressions.
# No real network calls are made here (avoids flaky/costly tests).
# Run from the project root with: source("tests/test_llm_interpret.R")

source("R/diag_lm_class.R")
source("R/simulate_data.R")
source("R/llm_interpret.R")

cat("== Test 1: build_diag_summary() on a linear model ==\n")
lin_model <- lm(mpg ~ wt + hp, data = mtcars)
lin_diag <- new_diag_lm(lin_model, data_type = "cross-section")
lin_summary <- build_diag_summary(lin_diag)
stopifnot(is.character(lin_summary), nchar(lin_summary) > 0)
stopifnot(grepl("Breusch-Pagan", lin_summary))
cat("build_diag_summary() returned a non-empty summary for a linear model.\n\n")

cat("== Test 2: build_diag_summary() on a logistic model ==\n")
set.seed(1)
logit_data <- simulate_logit_data(n = 200)
logit_model <- glm(Y ~ X1 + X2, data = logit_data, family = binomial())
logit_diag <- new_diag_lm(logit_model, data_type = "logistic")
logit_summary <- build_diag_summary(logit_diag)
stopifnot(is.character(logit_summary), nchar(logit_summary) > 0)
stopifnot(grepl("Hosmer-Lemeshow", logit_summary))
cat("build_diag_summary() returned a non-empty summary for a logistic model.\n\n")

cat("== Test 3: get_llm_interpretation() falls back gracefully with no API key ==\n")
old_key <- Sys.getenv("GROQ_API_KEY")
Sys.setenv(GROQ_API_KEY = "")
fallback <- get_llm_interpretation(lin_diag)
stopifnot(inherits(fallback, "shiny.tag") || inherits(fallback, "html") || is.character(fallback))
stopifnot(grepl("not configured", as.character(fallback)))
Sys.setenv(GROQ_API_KEY = old_key)
cat("get_llm_interpretation() returned the expected fallback message with no API key set.\n")
