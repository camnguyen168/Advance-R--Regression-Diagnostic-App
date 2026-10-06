# Interactive Regression Diagnostics with R Shiny

An interactive **R Shiny application for regression diagnostics** across linear, logistic, time-series, and panel-data models.

The project combines econometric diagnostics, simulation, visualization, model remediation, and optional AI-assisted interpretation in a unified framework built around a custom R **S3 class (`diag_lm`)**.

Link to the app: https://camnguyen16.shinyapps.io/Project/
<img width="1492" height="860" alt="image" src="https://github.com/user-attachments/assets/5fcc8f32-a2c8-47e2-831d-46d787a0dd25" />
<img width="1508" height="756" alt="image" src="https://github.com/user-attachments/assets/967ee01d-b041-4ab7-9336-443b5c8847f3" />
<img width="1271" height="591" alt="image" src="https://github.com/user-attachments/assets/3f90d772-77a9-49e8-8566-964408214dac" />
<img width="1268" height="517" alt="image" src="https://github.com/user-attachments/assets/9adb22a9-14b9-4ed4-946b-2fb57a5005ee" />


## Overview

Regression models rely on assumptions concerning error structure, functional form, dependence, stationarity, and model specification. Violations of these assumptions can lead to unreliable inference or misleading conclusions.

This project provides an interactive framework that allows users to:

- simulate or upload datasets,
- estimate different regression models,
- run model-specific diagnostic tests,
- visualize diagnostic patterns,
- identify potential assumption violations,
- apply transformations and robust inference,
- and optionally generate AI-assisted explanations of diagnostic results.

The framework supports:

- Linear regression
- Logistic regression
- Time-series regression
- Panel-data regression

---

## Application Workflow

The general workflow of the application is:

```text
Data / Simulation
       ↓
Model Specification
       ↓
lm() / glm() / plm()
       ↓
diag_lm S3 Object
       ↓
Statistical Diagnostics
       ↓
Diagnostic Plots
       ↓
Remediation / Robust Inference
       ↓
Optional AI Interpretation
```

The diagnostic tests do not directly use the simulation settings. Instead, simulation settings modify the underlying **data-generating process (DGP)**, and the diagnostic framework must identify the resulting statistical properties from the fitted model.

---

## The `diag_lm` S3 Framework

The core of the project is the custom **`diag_lm` S3 class**.

Rather than implementing separate workflows for every model, `diag_lm` provides a common interface around models estimated using:

```r
lm()
glm(..., family = binomial)
plm()
```

A simplified workflow is:

```r
model <- lm(Y ~ X1 + X2, data = df)

diagnostic <- new_diag_lm(
  model,
  data_type = "cross-section"
)

summary(diagnostic)
plot(diagnostic)
```

S3 method dispatch allows functions such as `summary()` and `plot()` to behave according to the diagnostic object.

The constructor validates model inputs and uses defensive error handling so that unsupported or unavailable diagnostics return gracefully rather than crashing the application.

---

## Diagnostic Framework

Different diagnostics are applied depending on the model and data structure.

### Linear Regression

Diagnostics include:

- Breusch-Pagan test for heteroskedasticity
- Durbin-Watson test
- Variance Inflation Factor (VIF)
- Shapiro-Wilk / Jarque-Bera normality diagnostics
- Residual-vs-fitted plots
- Q-Q plots
- Scale-location plots
- Residual distribution plots

### Time-Series Regression

Time-series diagnostics include:

- Breusch-Godfrey test
- Ljung-Box test
- Augmented Dickey-Fuller (ADF) test
- ACF
- PACF
- Residuals over time

These diagnostics help identify serial correlation and potential stationarity problems.

### Logistic Regression

Logistic-regression diagnostics include:

- Hosmer-Lemeshow goodness-of-fit test
- ROC curve
- AUC
- Classification metrics
- Pseudo-R²
- VIF
- Calibration diagnostics
- Binned residual plots
- Separation checks

This allows both **discrimination** and **calibration** to be evaluated.

### Panel Regression

Panel-data diagnostics include:

- Panel Breusch-Pagan LM test
- Hausman test
- Panel serial-correlation diagnostics
- Fixed-effects vs. random-effects comparison
- Time-effect testing

The application also supports robust inference and optional one-way clustered standard errors.

---

## Simulation Study

A dedicated simulation module is used to validate whether the diagnostic framework responds correctly when specific statistical problems are deliberately introduced.

Because the **true data-generating process is known**, simulations provide a controlled environment for evaluating the diagnostic procedures.

### Heteroskedasticity

Heteroskedastic errors can be generated using an X-dependent error variance:

```r
errors <- rnorm(
  n,
  mean = 0,
  sd = 1 + 1.5 * abs(X1)
)
```

Therefore:

```text
Var(error | X) is not constant
```

The Breusch-Pagan test and residual plots should respond to this violation.

### Multicollinearity

Strong correlation between predictors can be generated using:

```r
X2 <- X1 + rnorm(n, 0, 0.5)
```

This creates high correlation between `X1` and `X2`, which should be reflected in the VIF.

### Non-Normal Errors

Heavy-tailed errors can be generated using a Student-t distribution:

```r
errors <- rt(n, df = 3) * 2 / sqrt(3)
```

The scaling maintains a comparable variance while changing the shape of the error distribution.

### Serial Correlation

AR(1) serial correlation is deliberately introduced into regression errors:

```r
e[t] <- 0.6 * e[t - 1] + eps[t]
```

The resulting residual structure should be detected by ACF/PACF and formal tests such as Breusch-Godfrey and Ljung-Box.

### Logistic Misspecification

A nonlinear relationship can be included in the true DGP:

```r
eta <- 1.4 * X1 - 1.1 * X2 + 0.6 * X1^2
```

while estimating a model that omits `X1^2`.

This provides a controlled example of functional-form misspecification.

### Class Imbalance

Rare positive outcomes can be generated by shifting the logistic intercept:

```r
intercept <- -3
```

This demonstrates why accuracy alone may be misleading for imbalanced classification problems.

### Correlated Panel Effects

Panel data can deliberately violate the random-effects orthogonality assumption:

```r
alpha_i <-
  0.8 * (entity_x_mean - mean(entity_x_mean)) +
  rnorm(n_entities)
```

This creates:

```text
Cov(X_it, alpha_i) ≠ 0
```

and provides a controlled setting for comparing fixed and random effects using the Hausman test.



## AI-Assisted Interpretation

The application includes an optional AI-assisted interpretation component.

Instead of replacing statistical diagnostics, the AI layer receives a structured summary of results such as:

- coefficient estimates,
- p-values,
- diagnostic statistics,
- model information,
- and diagnostic outcomes.

The LLM then converts these results into a more accessible interpretation.

```text
Statistical Model
       ↓
Deterministic Diagnostics
       ↓
Structured Diagnostic Summary
       ↓
LLM Interpretation
```

The statistical calculations remain deterministic and independent of the language model.

AI is therefore used as an **interpretation layer**, not as a statistical testing engine.

---

## Software Architecture

The project separates statistical computation, simulation, AI interpretation, and the Shiny interface.

```text
app.R
│
├── R/
│   ├── diag_lm_class.R
│   ├── simulate_data.R
│   └── llm_interpret.R
│
└── tests/
    ├── test_diag_lm.R
    ├── test_llm_interpret.R
    └── test_app_server.R
```

### `app.R`

Controls the Shiny user interface, reactive workflow, data selection, model estimation, and output rendering.

### `diag_lm_class.R`

Contains the S3 diagnostic framework, statistical tests, plotting methods, and remediation logic.

### `simulate_data.R`

Generates controlled datasets for linear, logistic, time-series, and panel-data experiments.

### `llm_interpret.R`

Creates structured diagnostic summaries and handles optional LLM-based interpretation.

---

## Testing and Validation

The project uses multiple levels of validation:

1. **Unit testing** of diagnostic functions and S3 methods
2. **Shiny server testing** of reactive application behavior
3. **Simulation-based validation** using known DGPs
4. **Applied-data validation** using empirical datasets

Tests cover model construction, diagnostic output, plotting behavior, logistic diagnostics, time-series functionality, panel models, invalid inputs, and Shiny reactive workflows.

---

## Key Design Principle

The central idea of the project is:

```text
Known DGP
   ↓
Deliberately inject a statistical violation
   ↓
Estimate the model
   ↓
Run diag_lm
   ↓
Evaluate whether the diagnostic detects the violation
```

This separates **how the problem is generated** from **how the problem is detected**, providing a transparent way to validate the diagnostic framework.

---

## Technologies

- R
- Shiny
- S3 Object-Oriented Programming
- `lm`
- `glm`
- `plm`
- `lmtest`
- `car`
- `sandwich`
- `ggplot2`
- `testthat`
- LLM integration for optional AI-assisted interpretation

---

## Limitations

The application is designed as an educational and diagnostic framework rather than a replacement for complete econometric model validation.

Some diagnostic tests rely on asymptotic approximations, and their finite-sample size and power may vary.

The framework currently focuses on linear, binary logistic, time-series, and standard panel-regression settings. More advanced extensions could include cross-sectional dependence diagnostics, two-way clustered covariance estimators, Driscoll-Kraay standard errors, formal cointegration procedures, and additional time-series models such as ARIMA or GARCH.

---

## Purpose

This project demonstrates how **econometrics, statistical programming, simulation, software engineering, interactive visualization, and generative AI** can be integrated into a practical regression-diagnostics application.

The goal is not only to report diagnostic statistics, but to help users understand:

**what assumption may be violated, how the violation can be detected, why it matters, and which remediation strategies may be appropriate.**
