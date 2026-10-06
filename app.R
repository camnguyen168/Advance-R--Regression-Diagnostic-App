# app.R
# Shiny front-end for the diagnostics workflow with custom color palette,
# Time-Series support (with ADF stationarity check), multiple pre-loaded datasets,
# and dynamic date profiling.

# Load Packages
library(shiny)
library(shinydashboard)
library(gt)
library(ggplot2)
library(htmltools)
library(sandwich)

# Load helper scripts.
source("R/diag_lm_class.R")
source("R/simulate_data.R")
source("R/llm_interpret.R")

# Let users upload larger CSV files.
options(shiny.maxRequestSize = 30 * 1024^2)

# ---- Pre-defined Path & Datasets for System Data ----------------------------
DATA_DIR <- here::here("data")  # portable: resolves via the project root marker, not a hardcoded path

AVAILABLE_DATASETS <- c(
  "Wage Data (wage.csv)"                          = "wage.csv",
  "Titanic Survival Data (titanic.csv)"            = "titanic.csv",
  "Stock Indices Time Series (stock_indices.csv)" = "stock_indices.csv"
)

# Helper function to resolve dataset paths (checks local working dir first, then DATA_DIR)
get_dataset_path <- function(filename) {
  if (file.exists(filename)) {
    return(filename)
  }
  alt_path <- file.path(DATA_DIR, filename)
  if (file.exists(alt_path)) {
    return(alt_path)
  }
  return(filename) # Fallback
}

# Crash-proof dynamic date parsing helper
parse_date_dynamically <- function(x) {
  x_clean <- sub("\\.0+$", "", trimws(as.character(x)))
  
  if (!any(is.na(suppressWarnings(as.numeric(x_clean))))) {
    num_vals <- as.numeric(x_clean)
    try_date <- tryCatch(as.Date(num_vals, origin = "1970-01-01"), error = function(e) NULL)
    if (!is.null(try_date)) return(try_date)
  }
  
  parsed <- as.Date(x_clean, format = "%Y-%m-%d")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%Y/%m/%d")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d-%m-%Y")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d.%m.%Y")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d%m%y")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d%m%Y")
  
  if (all(is.na(parsed))) {
    return(x)
  }
  return(parsed)
}

# ---- User interface --------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "RegressionDiagnostics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Setup & Specifications", tabName = "setup", icon = icon("sliders")),
      menuItem("2. Diagnostics", tabName = "diagnostics", icon = icon("chart-line")),
      menuItem("3. Transformation", tabName = "transformation", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* --- CUSTOM PALETTE SKINNING OVERRIDES --- */
        .main-header .navbar, .main-header .logo {
          background-color: #3d5a80 !important;
          color: #ffffff !important;
        }
        .main-header .navbar .sidebar-toggle:hover {
          background-color: #293241 !important;
        }
        
        .main-sidebar {
          background-color: #293241 !important;
        }
        .sidebar-menu > li.active > a, .sidebar-menu > li:hover > a {
          background-color: #3d5a80 !important;
          border-left-color: #98c1d9 !important;
          color: #ffffff !important;
        }
        .sidebar-menu > li > a {
          color: #98c1d9 !important;
        }
        
        .box.box-solid.box-primary > .box-header {
          background-color: #3d5a80 !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-info > .box-header {
          background-color: #293241 !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-warning > .box-header {
          background-color: #ee6c4d !important;
          color: #ffffff !important;
        }
        .box {
          border-top-color: #98c1d9 !important;
          box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        }
        
        .btn-custom-primary {
          background-color: #3d5a80 !important;
          color: white !important;
          border: none;
        }
        .btn-custom-primary:hover { background-color: #293241 !important; }
        
        .btn-custom-success {
          background-color: #3d5a80 !important;
          color: white !important;
          border: none;
          font-weight: bold;
        }
        .btn-custom-success:hover { background-color: #293241 !important; }
        
        .btn-custom-warning {
          background-color: #ee6c4d !important;
          color: white !important;
          border: none;
        }
        .btn-custom-warning:hover { background-color: #ee6c4d !important; }
        
        .btn-custom-danger {
          background-color: #293241 !important;
          color: #98c1d9 !important;
          border: 1px solid #98c1d9;
        }
        .btn-custom-danger:hover { background-color: #ee6c4d !important; color: white !important; }
        
        .irs-bar, .irs-bar-edge, .irs-single {
          background: #3d5a80 !important;
        }
        
        .scrollable-table {
          max-height: 400px;
          overflow-y: auto;
          overflow-x: auto;
          border: 1px solid #ddd;
          background-color: white;
        }
        .scrollable-table table {
          width: 100%;
          border-collapse: collapse;
        }
        .scrollable-table th {
          position: sticky;
          top: 0;
          background-color: #f5f5f5;
          z-index: 10;
          box-shadow: inset 0 -2px 0 #ccc;
        }
        .codebook-text {
          white-space: pre-wrap;
          font-family: monospace;
          font-size: 12px;
          line-height: 1.4;
        }
        .formula-preview-box {
          background-color: #f0f4f7;
          border: 1px solid #d3dfe4;
          border-radius: 4px;
          padding: 15px;
          margin-bottom: 20px;
          font-family: 'Courier New', Courier, monospace;
          color: #555;
          font-size: 14px;
          line-height: 1.6;
        }
        .orange-heading {
          color: #ee6c4d;
          font-weight: bold;
          margin-bottom: 8px;
        }
        .ai-interpretation {
          background-color: #f8fafb;
          border: 1px solid #d3dfe4;
          border-left: 4px solid #3d5a80;
          border-radius: 4px;
          padding: 15px 18px;
          line-height: 1.5;
        }
        
        /* --- Assumptions Check value box polish --- */
        .small-box {
          position: relative;
          overflow: hidden !important;
          border-radius: 8px !important;
          box-shadow: 0 3px 8px rgba(0,0,0,0.10) !important;
          transition: box-shadow 0.15s ease;
        }
        .small-box:hover {
          box-shadow: 0 6px 16px rgba(0,0,0,0.16) !important;
        }
        .small-box h3 {
          font-size: 25px !important;
          margin-bottom: 6px !important;
        }
        .small-box p {
          font-size: 13px !important;
          line-height: 1.45 !important;
        }
        /* Keep the large watermark icon fully inside the box regardless of
           box height (which now varies with multi-line text) -- vertically
           centered instead of the default fixed top offset that bleeds out. */
        .small-box .icon-large,
        .small-box .icon-large i,
        .small-box > .icon {
          top: 50% !important;
          right: 16px !important;
          transform: translateY(-50%) !important;
          font-size: 42px !important;
        }
        .assumptions-legend {
          background-color: #f0f4f7;
          border: 1px solid #d3dfe4;
          border-left: 4px solid #3d5a80;
          border-radius: 6px;
          padding: 10px 16px;
          margin-bottom: 16px;
          font-size: 13px;
          color: #444;
          line-height: 1.5;
        }
        .assumptions-legend i {
          margin-right: 6px;
          color: #3d5a80;
        }
        .assumptions-legend b {
          color: #293241;
        }
      "))
    ),
    tabItems(
      # --- TAB 1: DATA SETUP ---
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            title = "Data Setup",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            selectInput("model_family", "Model Family:",
                        choices = c("OLS (Linear Regression)" = "linear",
                                    "Logistic (Binomial)" = "logistic",
                                    "Univariate Time Series" = "ts"),
                        selected = "linear"),
            radioButtons("data_source", NULL,
                         choices = c("Simulate data" = "simulate",
                                     "Choose available data" = "available",
                                     "Upload a CSV" = "upload"),
                         selected = "available"),
            uiOutput("data_config_controls"),
            uiOutput("panel_index_selectors"),
            
            hr(),
            
            h4(tags$strong("Modify Variable Types")),
            uiOutput("var_to_edit_ui"),
            selectInput("new_data_type", "New data type:",
                        choices = c("Factor" = "factor",
                                    "Numeric" = "numeric",
                                    "Integer" = "integer",
                                    "Character" = "character",
                                    "Smart Date (Auto-Detect)" = "date")),
            actionButton("change_type_btn", "Convert Data Type", class = "btn-custom-warning btn-sm w-100")
          ),
          box(
            title = "Data Preview & Statistical Properties", 
            width = 8, 
            status = "info", 
            solidHeader = TRUE,
            h4("System Metadata & Structural Profile"),
            uiOutput("metadata_display_ui"),
            hr(),
            h4("Raw Data Grid"),
            div(class = "scrollable-table", tableOutput("data_preview")),
            hr(),
            h4("Data Column Profiles"),
            div(class = "scrollable-table", gt_output("data_summary_table")),
            br(),
            h4("Pearson Correlation Matrix (Numeric Fields)"),
            gt_output("correlation_matrix_table")
          )
        )
      ),
      # --- TAB 2: DIAGNOSTICS & MODEL FIT ---
      tabItem(
        tabName = "diagnostics",
        fluidRow(
          box(
            title = "Define Statistical Model Layout",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            
            # --- Time Series always studies the series against its own
            # history (autocorrelation). "Regression on another variable"
            # was removed as a separate mode -- if you want a regression on
            # time-ordered data, use Linear (OLS) with a Time Series data
            # structure instead, which still runs the same residual-based
            # autocorrelation/stationarity diagnostics.
            conditionalPanel(
              condition = "input.model_family == 'ts'",
              checkboxInput("ts_difference",
                            "Use first difference (recommended if non-stationary)",
                            value = FALSE),
              hr()
            ),
            
            uiOutput("diagnostics_var_selectors"),
            br(),
            fluidRow(
              column(6, actionButton("run_analysis_btn", "Run analysis", class = "btn-custom-success w-100")),
              column(6, actionButton("reset_model_btn", "Reset model", class = "btn-custom-danger w-100"))
            )
          ),
          box(
            title = "Statistical Analysis Output Profile",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            
            h3(tags$strong("Statistical Analysis")),
            div(class = "orange-heading", "Selected variables for analysis"),
            uiOutput("formula_summary_preview_ui"),
            
            div(class = "orange-heading", "The table shows the results from the fitted regression model"),
            gt_output("summary_table"),
            br(),
            textOutput("simulator_used_note"),
            br(),
            textOutput("time_series_note"),
            br(),
            htmlOutput("remediation")
          )
        ),
        
        # --- ASSUMPTIONS CHECK ---
        fluidRow(
          box(
            title = "Assumptions Check",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            uiOutput("assumptions_valueboxes_ui")
          )
        ),
        
        # --- DIAGNOSTIC PLOTS ---
        uiOutput("diagnostic_plots_ui"),
        
        # --- AI INTERPRETATION ---
        fluidRow(
          box(
            title = "AI Interpretation (Groq)",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            actionButton("ai_interpret_btn", "Generate AI Interpretation", class = "btn-custom-success w-100"),
            br(), br(),
            uiOutput("ai_interpretation_ui")
          )
        )
      ),
      # --- TAB 3: TRANSFORMATION ---
      tabItem(
        tabName = "transformation",
        fluidRow(
          box(
            title = "Configure Transformation",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            uiOutput("transform_var_selector_ui"),
            selectInput("transform_type", "Select Operator:",
                        choices = c("Logarithmic (log)" = "log",
                                    "Log (x + 1) -- handles zero values" = "log1p",
                                    "Exponential (exp)" = "exp",
                                    "Square (x^2)" = "square",
                                    "Standardize (z-score)" = "standardize",
                                    "Log Return" = "log_return",
                                    "Lag (-1)" = "lag")),
            textInput("new_var_name", "Custom Column Name:", value = ""),
            helpText("Provide a unique identifier or leave blank for auto-generation."),
            br(),
            actionButton("apply_transform_btn", "Execute Transformation", class = "btn-custom-success w-100")
          ),
          box(
            title = "Transformed Structural Workspace Summary",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            h4("Transformed Grid View (Top 25 Records Displayed)"),
            div(class = "scrollable-table", tableOutput("transformed_data_preview"))
          )
        )
      )
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Format a p-value for display. Rounding to 4 decimals silently turns any
  # p < 0.00005 into "0.0000", which reads like an exact zero (impossible for
  # a p-value) rather than "very small". Show "< 0.0001" instead, and prefix
  # with "=" for anything else so boxes read as "p = 0.0500" / "p < 0.0001".
  fmt_pval <- function(p, digits = 4) {
    if (is.null(p) || length(p) == 0 || is.na(p)) return("N/A")
    if (p < 0.0001) return("< 0.0001")
    paste0("= ", format(round(p, digits), nsmall = digits))
  }
  
  # Builds a dashboard box containing: a small lightbulb button (click to
  # reveal a short AI interpretation of this specific plot), the
  # interpretation output area (empty until clicked), then the plot itself.
  # `key` must be unique per distinct plot -- it's reused to build the
  # button ID, output ID, and (via .plot_interp_types below) the plot type
  # string passed to get_plot_interpretation().
  plot_box <- function(title, key, plot_id, width = 6) {
    box(title = title, width = width, status = "primary", solidHeader = TRUE,
        div(style = "margin-bottom: 8px;",
            actionButton(paste0("interpret_btn_", key), label = NULL, icon = icon("lightbulb"),
                         class = "btn-xs", title = "AI interpretation of this plot")
        ),
        uiOutput(paste0("interpret_ui_", key)),
        plotOutput(plot_id, height = "350px")
    )
  }
  
  # Determines which panel estimator was actually requested. Unifies two
  # different UI shapes: simulated panel data shows an ungated 4-way radio
  # (Dummy variables / Within / Random Effects / Pooled OLS, "pooling" as
  # an explicit choice), while uploaded panel data gates a 3-way radio
  # (no "pooling" option) behind the "Include entity/time effects"
  # checkbox, defaulting to pooling when that checkbox is unchecked.
  panel_estimator_choice <- function() {
    if (identical(input$data_source, "simulate")) {
      if (!is.null(input$fe_method)) input$fe_method else "lsdv"
    } else {
      if (isTRUE(input$use_fixed_effects) && !is.null(input$fe_method)) input$fe_method else "pooling"
    }
  }
  
  modified_df <- reactiveVal(NULL)
  active_interactions <- reactiveVal(character(0))
  reset_trigger <- reactiveVal(0)
  
  observeEvent(list(input$csv_file, input$sim_btn, input$data_source, input$available_file), {
    modified_df(NULL)
    active_interactions(character(0))
  })
  
  observeEvent(input$reset_model_btn, {
    active_interactions(character(0))
    reset_trigger(reset_trigger() + 1)
  })
  
  output$data_config_controls <- renderUI({
    req(input$data_source, input$model_family)
    is_logistic <- identical(input$model_family, "logistic")
    is_ts       <- identical(input$model_family, "ts")
    
    if (identical(input$data_source, "simulate")) {
      if (is_logistic) {
        tagList(
          helpText("Simulate a binary outcome to test logistic-regression assumptions."),
          sliderInput("n_obs", "Sample Size (n):", min = 50, max = 1000, value = 500),
          checkboxInput("coll_viol", "Inject Multicollinearity", value = FALSE),
          checkboxInput("nonlinear_viol", "Inject Omitted Non-linearity", value = FALSE),
          checkboxInput("imbalance_viol", "Inject Class Imbalance", value = FALSE),
          numericInput("seed", "Random seed (leave blank for a new draw):", value = NULL),
          actionButton("sim_btn", "Simulate & Load Framework", class = "btn-custom-primary", width = "100%")
        )
      } else {
        is_panel_sim <- !is_ts && identical(input$data_structure_sim, "panel")
        is_ts_sim    <- is_ts || identical(input$data_structure_sim, "ts")
        tagList(
          if (!is_ts) {
            selectInput(
              "data_structure_sim",
              "Select Econometric Data Structure:",
              choices = c("Cross Sectional Data" = "cs","Time Series" = "ts", "Panel Data" = "panel"),
              selected = if (!is.null(input$data_structure_sim)) input$data_structure_sim else "cs"
            )
          },
          if (is_panel_sim) {
            tagList(
              helpText("Simulates a balanced entity x year panel with a true entity-level effect built in."),
              sliderInput("n_entities_sim", "Number of entities (e.g. firms):", min = 5, max = 100, value = 30),
              sliderInput("n_periods_sim", "Number of time periods:", min = 3, max = 20, value = 8),
              checkboxInput("panel_correlated_effects", "Correlate entity effects with X (favors Fixed Effects)", value = TRUE),
              helpText("Checked: entity effects correlate with X, so Random Effects becomes inconsistent -- the Hausman test should favor Fixed Effects. Unchecked: entity effects are independent of X, so Random Effects stays valid -- the Hausman test shouldn't reject it."),
              checkboxInput("panel_serial_viol", "Inject Serial Correlation (within entity)", value = FALSE),
              checkboxInput("het_viol", "Inject Heteroskedasticity", value = FALSE),
              numericInput("seed", "Random seed (leave blank for a new draw):", value = NULL),
              actionButton("sim_btn", "Simulate & Load Framework", class = "btn-custom-primary", width = "100%")
            )
          } else {
            tagList(
              helpText("Simulate a dataset to test OLS / Time Series assumptions."),
              sliderInput("n_obs", "Sample Size (n):", min = 50, max = 1000, value = 500),
              checkboxInput("het_viol", "Inject Heteroskedasticity", value = FALSE),
              checkboxInput("coll_viol", "Inject Multicollinearity", value = FALSE),
              checkboxInput("nonnormal_viol", "Inject Non-Normal Errors (heavy tails)", value = FALSE),
              if (is_ts_sim) {
                checkboxInput("unitroot_viol", "Inject Non-Stationarity (unit root / random walk)", value = FALSE)
              },
              numericInput("seed", "Random seed (leave blank for a new draw):", value = NULL),
              actionButton("sim_btn", "Simulate & Load Framework", class = "btn-custom-primary", width = "100%")
            )
          }
        )
      }
    } else if (identical(input$data_source, "available")) {
      tagList(
        selectInput("available_file", "Choose Available Dataset:", choices = AVAILABLE_DATASETS, selected = "wage.csv"),
        uiOutput("available_file_status")
      )
    } else if (is_logistic) {
      tagList(
        helpText("Upload a CSV file with a binary 0/1 outcome for logistic regression."),
        fileInput("csv_file", "CSV file:", accept = c(".csv", "text/csv")),
        checkboxInput("header", "File has a header row", value = TRUE)
      )
    } else {
      tagList(
        if (!is_ts) {
          selectInput(
            "data_structure_upload",
            "Select Econometric Data Structure:",
            choices = c("Cross Sectional Data" = "cs", "Time Series" = "ts", "Panel Data" = "panel"),
            selected = if (!is.null(input$data_structure_upload)) input$data_structure_upload else "cs"
          )
        },
        helpText("Upload a CSV file to inspect assumptions."),
        fileInput("csv_file", "CSV file:", accept = c(".csv", "text/csv")),
        checkboxInput("header", "File has a header row", value = TRUE)
      )
    }
  })
  
  # Entity/time index pickers for Panel Data -- deliberately a SEPARATE
  # uiOutput from data_config_controls above. That block also renders the
  # fileInput itself, and current_data() blocks (via req()) until a file is
  # actually uploaded -- so reading current_data() inside the same renderUI
  # would prevent the fileInput from ever appearing in the first place.
  # This output only needs the data once it exists, so it's safe for it to
  # simply render nothing until then.
  output$fe_method_helptext <- renderUI({
    req(input$fe_method)
    if (identical(input$fe_method, "within")) {
      helpText("Demeans the data within each entity (and time period, if two-way) before fitting -- mathematically removes the fixed effects rather than estimating them individually, so they will NOT appear as rows in the results table. More efficient with many entities; matches the classical \"fixed effects estimator\" from econometrics textbooks.")
    } else if (identical(input$fe_method, "random")) {
      helpText("Treats entity effects as random draws, assumed uncorrelated with your regressors (GLS estimation). More efficient than Fixed Effects when that assumption holds -- check the Hausman test box below to see if it's plausible for your data. No dummy rows, and the Two-way toggle does not apply (Random Effects here is one-way, entity-level).")
    } else {
      helpText("Adds entity and/or time as dummy variables (factor()) using the Cross-Section Index and Time Index set above -- so the fixed effects appear as regular coefficient rows in the results table, same as any other model. Coefficients on your actual predictors are the same as the Within method; the table is just longer with many entities.")
    }
  })
  
  output$panel_index_selectors <- renderUI({
    is_upload_panel   <- identical(input$data_source, "upload") && identical(input$data_structure_upload, "panel")
    is_simulate_panel <- identical(input$data_source, "simulate") && identical(input$data_structure_sim, "panel")
    req(is_upload_panel || is_simulate_panel)
    df <- current_data()
    cols <- names(df)
    req(length(cols) > 0)
    
    # Best-effort default guesses so the dropdowns aren't just alphabetical --
    # look for common naming patterns for entity vs. time columns.
    #
    # FIX: the previous patterns used bare substrings like "id" / "time" /
    # "date", which match false positives inside unrelated column names
    # (e.g. "width" contains "id", "runtime" contains "time"). These three
    # short/ambiguous tokens are now required to sit at a word/separator
    # boundary (start/end of string, or next to "_", ".", or whitespace) --
    # this still matches common naming conventions like "firm_id",
    # "entity_id", "id_number", "last_date", "obs_time", but no longer
    # matches those tokens as an incidental substring of an unrelated word.
    # The other tokens (firm/state/entity/company/country/panel/year/
    # period/wave) are specific enough that a plain substring match is
    # low-risk and is left as-is.
    guess_entity <- cols[grepl("(^|[_. ])id($|[_. ])|firm|state|entity|company|country|panel",
                               cols, ignore.case = TRUE, perl = TRUE)]
    guess_time   <- cols[grepl("year|(^|[_. ])time($|[_. ])|(^|[_. ])date($|[_. ])|period|wave",
                               cols, ignore.case = TRUE, perl = TRUE)]
    
    entity_default <- if (length(guess_entity) > 0) guess_entity[1] else cols[1]
    time_default <- if (length(guess_time) > 0) {
      guess_time[1]
    } else if (length(cols) > 1) {
      cols[2]
    } else {
      cols[1]
    }
    
    tagList(
      hr(),
      h5(tags$strong("Panel Structure")),
      selectInput("p_idx_i", "Cross-Section Index (entity ID, e.g. firm/state):",
                  choices = cols, selected = entity_default),
      selectInput("p_idx_t", "Time Index (e.g. year):",
                  choices = cols, selected = time_default),
      helpText("Chosen from your uploaded data's actual columns -- pick whichever identifies the entity (cross-section) and the time period.")
    )
  })
  
  output$available_file_status <- renderUI({
    req(input$available_file)
    filepath <- get_dataset_path(input$available_file)
    if (file.exists(filepath)) {
      p(tags$span(style="color:green; font-weight:bold;", paste("✔ System file ready:", input$available_file)))
    } else {
      p(tags$span(style="color:red; font-weight:bold;", paste("Missing File Error: Could not find '", input$available_file, "' in current folder or in DATA_DIR.")))
    }
  })
  
  data_structure <- reactive({
    req(input$data_source, input$model_family)
    if (identical(input$model_family, "logistic")) {
      "cs"
    } else if (identical(input$model_family, "ts")) {
      "ts"
    } else if (identical(input$data_source, "simulate")) {
      req(input$data_structure_sim)
      input$data_structure_sim
    } else if (identical(input$data_source, "available")) {
      req(input$available_file)
      if (identical(input$available_file, "stock_indices.csv")) "ts" else "cs"
    } else {
      req(input$data_structure_upload)
      input$data_structure_upload
    }
  })
  
  sim_data <- eventReactive(input$sim_btn, {
    seed_val <- if (is.null(input$seed) || is.na(input$seed)) NULL else input$seed
    if (identical(input$model_family, "logistic")) {
      get("simulate_logit_data", mode = "function")(n = input$n_obs, multicollinear = input$coll_viol, nonlinear = input$nonlinear_viol, imbalance = input$imbalance_viol, seed = seed_val)
    } else if (identical(data_structure(), "panel")) {
      get("simulate_panel_data", mode = "function")(
        n_entities = input$n_entities_sim, n_periods = input$n_periods_sim,
        correlated_effects = isTRUE(input$panel_correlated_effects),
        serial_correlation = isTRUE(input$panel_serial_viol),
        heteroskedastic = isTRUE(input$het_viol),
        seed = seed_val
      )
    } else if (identical(input$model_family, "ts") || identical(data_structure(), "ts")) {
      get("simulate_ts_data", mode = "function")(
        n = input$n_obs, heteroskedastic = input$het_viol, multicollinear = input$coll_viol,
        unit_root = isTRUE(input$unitroot_viol), seed = seed_val
      )
    } else {
      get("simulate_ols_data", mode = "function")(
        n = input$n_obs, heteroskedastic = input$het_viol, multicollinear = input$coll_viol,
        non_normal = isTRUE(input$nonnormal_viol), seed = seed_val
      )
    }
  }, ignoreNULL = FALSE)
  
  uploaded_data <- reactive({
    req(input$csv_file)
    tryCatch(
      read.csv(input$csv_file$datapath, header = input$header, stringsAsFactors = FALSE),
      error = function(e) {
        validate(need(FALSE, paste("Could not read the file:", conditionMessage(e))))
      }
    )
  })
  
  system_available_data <- reactive({
    req(input$available_file)
    filepath <- get_dataset_path(input$available_file)
    validate(need(file.exists(filepath), paste("Missing File Error: Unable to locate", input$available_file)))
    tryCatch(
      read.csv(filepath, stringsAsFactors = FALSE),
      error = function(e) {
        validate(need(FALSE, paste("Could not read system file:", conditionMessage(e))))
      }
    )
  })
  
  current_data <- reactive({
    if (!is.null(modified_df())) {
      modified_df()
    } else if (identical(input$data_source, "simulate")) {
      req(sim_data())
      sim_data()
    } else if (identical(input$data_source, "available")) {
      system_available_data()
    } else {
      req(uploaded_data())
      uploaded_data()
    }
  })
  
  output$var_to_edit_ui <- renderUI({
    req(current_data())
    selectInput("var_to_edit", "Select variable to edit:", choices = names(current_data()))
  })
  
  observeEvent(input$change_type_btn, {
    req(current_data(), input$var_to_edit, input$new_data_type)
    df <- current_data()
    target_col <- input$var_to_edit
    validate(need(target_col %in% names(df), paste0("Column '", target_col, "' not found.")))
    
    if (identical(input$new_data_type, "date")) {
      # FIX: the interactive "Smart Date (Auto-Detect)" conversion had no
      # safeguard against low-cardinality columns (binary 0/1 indicators,
      # small category codes) that can look like valid epoch day-counts to
      # as.Date() -- e.g. 0 parses as "1970-01-01" -- even though they are
      # categorical, not dates. The read-only "Data Column Profiles" table
      # already guards against exactly this; this mirrors the same
      # cardinality guard here, plus a parse-success-rate check, so the
      # button can no longer silently turn a categorical column into
      # nonsense dates. Any failure surfaces as a clear validate() message
      # instead of a silent (wrong) conversion.
      raw_col <- df[[target_col]]
      char_col <- as.character(raw_col)
      non_empty_vals <- char_col[!is.na(char_col) & char_col != "" & char_col != "NA"]
      non_empty_cnt <- length(non_empty_vals)
      n_unique <- length(unique(non_empty_vals))
      min_unique_for_date <- max(10, ceiling(0.05 * non_empty_cnt))
      
      validate(need(
        non_empty_cnt > 0 && n_unique >= min_unique_for_date,
        paste0(
          "Column '", target_col, "' has only ", n_unique, " distinct non-missing value(s), ",
          "too few to plausibly be a date column (needs at least ", min_unique_for_date, "). ",
          "This is usually a categorical/indicator column, not a real date -- conversion aborted."
        )
      ))
      
      parsed <- parse_date_dynamically(raw_col)
      valid_mask <- !is.na(char_col) & char_col != "" & char_col != "NA"
      success_rate <- if (non_empty_cnt > 0) sum(!is.na(parsed[valid_mask])) / non_empty_cnt else 0
      
      validate(need(
        inherits(parsed, "Date") && success_rate >= 0.8,
        paste0(
          "Column '", target_col, "' could not be reliably parsed as a date (only ",
          round(success_rate * 100, 1), "% of non-missing values parsed successfully). ",
          "Conversion aborted -- no change was made to this column."
        )
      ))
      
      df[[target_col]] <- parsed
    } else {
      df[[target_col]] <- switch(input$new_data_type,
                                 "factor"    = as.factor(df[[target_col]]),
                                 "numeric"   = suppressWarnings(as.numeric(as.character(df[[target_col]]))),
                                 "integer"   = suppressWarnings(as.integer(as.character(df[[target_col]]))),
                                 "character" = as.character(df[[target_col]])
      )
    }
    modified_df(df)
  })
  
  output$transform_var_selector_ui <- renderUI({
    req(current_data())
    selectInput("transform_target_col", "Select target column:", choices = names(current_data()))
  })
  
  observeEvent(input$apply_transform_btn, {
    req(current_data(), input$transform_target_col, input$transform_type)
    df <- current_data()
    
    raw_vector <- suppressWarnings(as.numeric(as.character(df[[input$transform_target_col]])))
    validate(need(!all(is.na(raw_vector)), "Selected column must be numeric."))
    
    transformed_values <- switch(input$transform_type,
                                 "log" = {
                                   validate(need(all(raw_vector > 0, na.rm = TRUE), "Log requires strictly positive values. If this column includes zeros (e.g. years of experience for new entrants), use \"Log (x + 1)\" instead."))
                                   log(raw_vector)
                                 },
                                 "log1p" = {
                                   validate(need(all(raw_vector > -1, na.rm = TRUE), "Log (x + 1) requires values greater than -1."))
                                   log1p(raw_vector)
                                 },
                                 "exp" = {
                                   res <- exp(raw_vector)
                                   validate(need(all(is.finite(res)), "Exponential overflow detected."))
                                   res
                                 },
                                 "square" = raw_vector ^ 2,
                                 "standardize" = {
                                   validate(need(sd(raw_vector, na.rm = TRUE) > 0, "Non-zero SD required."))
                                   (raw_vector - mean(raw_vector, na.rm = TRUE)) / sd(raw_vector, na.rm = TRUE)
                                 },
                                 "log_return" = {
                                   validate(need(length(raw_vector) > 1, "Insufficient observations."))
                                   validate(need(all(raw_vector > 0, na.rm = TRUE), "Log Return requires strictly positive values."))
                                   c(NA, diff(log(raw_vector)))
                                 },
                                 "lag" = {
                                   validate(need(length(raw_vector) > 1, "Insufficient observations for lagging."))
                                   c(NA, head(raw_vector, -1))
                                 }
    )
    
    final_col_name <- trimws(input$new_var_name)
    if (identical(final_col_name, "")) {
      final_col_name <- paste0(input$transform_target_col, "_", input$transform_type)
    }
    
    df[[final_col_name]] <- transformed_values
    modified_df(df)
    updateTextInput(session, "new_var_name", value = "")
  })
  
  output$transformed_data_preview <- renderTable({
    req(current_data())
    head(current_data(), 25)
  })
  
  output$diagnostics_var_selectors <- renderUI({
    reset_trigger()
    df <- current_data()
    all_cols <- names(df)
    
    is_uni_ts <- identical(input$model_family, "ts")  # Time Series is always univariate now
    
    if (is_uni_ts) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      date_cols <- names(df)[sapply(df, function(x) inherits(x, c("Date", "POSIXct")))]
      
      tagList(
        selectInput("ts_series_col", "Series to analyze:", choices = numeric_cols),
        selectInput("ts_date_col", "Time/Date column (optional):",
                    choices = c("None (use row order)" = "", date_cols)),
        helpText("Diagnostics will check how this series correlates with its own past ",
                 "(ACF/PACF), whether it's stationary (ADF), and its distribution.")
      )
    } else if (identical(input$data_source, "simulate")) {
      if (identical(data_structure(), "panel")) {
        tagList(
          p(tags$em("Simulated panel dataset locks standard defaults: Y ~ X")),
          hr(),
          h5(tags$strong("Model Adjustments (OLS)")),
          radioButtons("fe_method", "Effects Estimator:",
                       choices = c("Dummy variables (LSDV) -- shows FE coefficients" = "lsdv",
                                   "Within estimator (demeaning) -- no dummy rows" = "within",
                                   "Random Effects (GLS)" = "random",
                                   "Pooled OLS (no entity/time effects)" = "pooling"),
                       selected = "lsdv"),
          conditionalPanel(
            condition = "input.fe_method == 'lsdv' || input.fe_method == 'within'",
            checkboxInput("fe_twoway", "Two-way (entity + time) instead of one-way (entity only)", value = FALSE)
          ),
          uiOutput("fe_method_helptext"),
          checkboxInput("use_robust_se", "Adjust standard errors for heteroskedasticity", value = FALSE),
          conditionalPanel(
            condition = "input.use_robust_se == true",
            selectInput("cluster_var", "Cluster standard errors by (optional):",
                        choices = c("None - use HC1 robust SE" = "", all_cols)),
            helpText("Leave as \"None\" for heteroskedasticity-robust (HC1) SEs. Choosing a column clusters standard errors by that group instead -- for panel data, clustering by the entity index is standard practice.")
          )
        )
      } else {
        tagList(p(tags$em("Simulated dataset locks standard defaults: Y ~ X1 + X2")))
      }
    } else {
      y_sel <- ""
      x_sel <- NULL
      
      if (identical(input$data_source, "available")) {
        req(input$available_file)
        if (input$available_file == "wage.csv" && "wage" %in% all_cols) {
          y_sel <- "wage"; x_sel <- intersect(c("experience", "age", "female"), all_cols)
        } else if (input$available_file == "titanic.csv" && "survived" %in% all_cols) {
          y_sel <- "survived"; x_sel <- intersect(c("pclass", "age", "sex", "fare"), all_cols)
        } else if (input$available_file == "stock_indices.csv" && length(all_cols) >= 2) {
          y_sel <- all_cols[2]; x_sel <- all_cols[c(3, 4, 5)] # SPX ~ DJI + NDX + RUT
        }
      }
      
      tagList(
        selectInput("y_var", "Select outcome variable", choices = c("", all_cols), selected = y_sel),
        selectInput("x_vars", "Select predictor variable(s)", choices = all_cols, selected = x_sel, multiple = TRUE),
        selectInput("interaction_vars", "Select interaction term(s)", choices = all_cols, selected = NULL, multiple = TRUE),
        actionButton("add_interaction_btn", "Create and add term(s)", class = "btn-custom-primary btn-sm w-100"),
        br(), br(),
        uiOutput("interaction_checkboxes_ui"),
        
        # --- Model adjustments: Linear (OLS) only ---
        if (identical(input$model_family, "linear")) {
          is_panel_now <- identical(data_structure(), "panel")
          tagList(
            hr(),
            h5(tags$strong("Model Adjustments (OLS)")),
            checkboxInput("use_fixed_effects",
                          if (is_panel_now) "Include entity/time effects (Fixed or Random)" else "Include fixed effects (categorical control variable(s))",
                          value = FALSE),
            conditionalPanel(
              condition = "input.use_fixed_effects == true",
              if (is_panel_now) {
                tagList(
                  radioButtons("fe_method", "Effects Estimator:",
                               choices = c("Dummy variables (LSDV) -- shows FE coefficients" = "lsdv",
                                           "Within estimator (demeaning) -- no dummy rows" = "within",
                                           "Random Effects (GLS)" = "random"),
                               selected = "lsdv"),
                  conditionalPanel(
                    condition = "input.fe_method == 'lsdv' || input.fe_method == 'within'",
                    checkboxInput("fe_twoway", "Two-way (entity + time) instead of one-way (entity only)", value = FALSE)
                  ),
                  uiOutput("fe_method_helptext")
                )
              } else {
                tagList(
                  selectInput("fe_vars", "Fixed effect variable(s):", choices = all_cols, multiple = TRUE),
                  helpText("Absorbed as dummy variables (factor()). Select more than one to combine them (e.g. a two-way-style specification).")
                )
              }
            ),
            checkboxInput("use_robust_se", "Adjust standard errors for heteroskedasticity", value = FALSE),
            conditionalPanel(
              condition = "input.use_robust_se == true",
              selectInput("cluster_var", "Cluster standard errors by (optional):",
                          choices = c("None - use HC1 robust SE" = "", all_cols)),
              helpText("Leave as \"None\" for heteroskedasticity-robust (HC1) SEs. Choosing a column clusters standard errors by that group instead (e.g. firm, state, individual ID). For panel data, clustering by the entity index is standard practice.")
            )
          )
        }
      )
    }
  })
  
  observeEvent(input$add_interaction_btn, {
    req(input$interaction_vars)
    if (length(input$interaction_vars) >= 2) {
      new_term <- paste(input$interaction_vars, collapse = "*")
      current_list <- active_interactions()
      if (!(new_term %in% current_list)) {
        active_interactions(c(current_list, new_term))
      }
    }
  })
  
  output$interaction_checkboxes_ui <- renderUI({
    req(active_interactions())
    if (length(active_interactions()) > 0) {
      checkboxGroupInput("selected_interactions", "Active Model Interactions Toggle:",
                         choices = active_interactions(),
                         selected = active_interactions())
    }
  })
  
  output$formula_summary_preview_ui <- renderUI({
    is_uni_ts <- identical(input$model_family, "ts")  # Time Series is always univariate now
    
    if (is_uni_ts) {
      series_txt <- if (is.null(input$ts_series_col) || identical(input$ts_series_col, "")) "[Not Chosen]" else input$ts_series_col
      date_txt   <- if (is.null(input$ts_date_col) || identical(input$ts_date_col, "")) "Row order (no date column selected)" else input$ts_date_col
      diff_txt   <- if (isTRUE(input$ts_difference)) "Yes (first difference)" else "No (level series)"
      
      return(
        div(class = "formula-preview-box",
            div(paste("Mode: Autocorrelation with itself (univariate)")),
            div(paste("Series analyzed:", series_txt)),
            div(paste("Time index:", date_txt)),
            div(paste("Differenced:", diff_txt))
        )
      )
    }
    
    if (identical(input$data_source, "simulate")) {
      outcome_txt <- "Y"
      predictors_txt <- "X1 + X2"
    } else {
      outcome_txt <- if (is.null(input$y_var) || input$y_var == "") "[Not Chosen]" else input$y_var
      
      preds <- setdiff(input$x_vars, input$y_var)
      if (length(preds) == 0) {
        predictors_txt <- "[Not Chosen]"
      } else {
        predictors_txt <- paste(preds, collapse = " + ")
      }
      
      if (!is.null(input$selected_interactions) && length(input$selected_interactions) > 0) {
        predictors_txt <- paste(predictors_txt, "+", paste(input$selected_interactions, collapse = " + "))
      }
    }
    
    div(class = "formula-preview-box",
        div(paste("Outcome variable:", outcome_txt)),
        div(paste("Predictor variable(s):", predictors_txt)),
        if (identical(input$model_family, "linear")) {
          is_panel_preview <- identical(data_structure(), "panel")
          fe_txt <- if (is_panel_preview) {
            switch(panel_estimator_choice(),
                   "lsdv"    = paste0(if (isTRUE(input$fe_twoway)) "Two-way" else "One-way", " Fixed Effects (dummy variables / LSDV)"),
                   "within"  = paste0(if (isTRUE(input$fe_twoway)) "Two-way" else "One-way", " Fixed Effects (within/demeaned)"),
                   "random"  = "Random Effects (GLS)",
                   "pooling" = "Pooled OLS (no entity/time effects)",
                   "None"
            )
          } else if (!isTRUE(input$use_fixed_effects)) {
            "None"
          } else if (!is.null(input$fe_vars) && length(input$fe_vars) > 0) {
            paste(input$fe_vars, collapse = ", ")
          } else "None"
          
          se_txt <- if (isTRUE(input$use_robust_se)) {
            if (!is.null(input$cluster_var) && nzchar(input$cluster_var)) {
              paste0("Cluster-robust (clustered by '", input$cluster_var, "')")
            } else {
              "Heteroskedasticity-robust (HC1)"
            }
          } else "Standard (OLS)"
          
          tagList(
            div(paste("Fixed/Random effects:", fe_txt)),
            div(paste("Standard errors:", se_txt))
          )
        }
    )
  })
  
  diag_model <- eventReactive(input$run_analysis_btn, {
    req(data_structure())
    df <- current_data()
    
    is_uni_ts <- identical(input$model_family, "ts")  # Time Series is always univariate now
    
    if (is_uni_ts) {
      req(input$ts_series_col)
      validate(need(input$ts_series_col %in% names(df), "Selected series column not found in the data."))
      
      series_vec <- suppressWarnings(as.numeric(df[[input$ts_series_col]]))
      validate(need(!all(is.na(series_vec)), paste0("Series '", input$ts_series_col, "' could not be converted to numeric.")))
      
      time_idx <- NULL
      if (!is.null(input$ts_date_col) && nzchar(input$ts_date_col) && input$ts_date_col %in% names(df)) {
        time_idx <- df[[input$ts_date_col]]
      }
      
      return(get("new_diag_lm_univariate", mode = "function")(
        series     = series_vec,
        time_index = time_idx,
        var_name   = input$ts_series_col,
        difference = isTRUE(input$ts_difference)
      ))
    }
    
    panel_lsdv_formula <- NULL
    is_panel <- identical(data_structure(), "panel")
    
    if (input$data_source == "simulate") {
      term_labels <- if (is_panel) "X" else c("X1", "X2")
      model_formula <- reformulate(termlabels = term_labels, response = "Y")
    } else {
      req(input$y_var, input$x_vars)
      validate(need(input$y_var != "", "Please select a dependent outcome variable (Y)."))
      preds <- setdiff(input$x_vars, input$y_var)
      validate(need(length(preds) >= 1, "Select distinct valid predictor terms."))
      
      df[[input$y_var]] <- suppressWarnings(as.numeric(df[[input$y_var]]))
      validate(need(!all(is.na(df[[input$y_var]])), paste0("Outcome variable '", input$y_var, "' conversion failed.")))
      
      for (p in preds) {
        df[[p]] <- suppressWarnings(as.numeric(df[[p]]))
        validate(need(!all(is.na(df[[p]])), paste0("Predictor '", p, "' conversion failed.")))
      }
      
      term_labels <- preds
      if (!is.null(input$selected_interactions) && length(input$selected_interactions) > 0) {
        term_labels <- c(term_labels, input$selected_interactions)
      }
      
      is_linear <- identical(input$model_family, "linear")
      want_fe   <- is_linear && !is_panel && isTRUE(input$use_fixed_effects) &&
        !is.null(input$fe_vars) && length(input$fe_vars) > 0
      
      # Fixed effects as dummy variables via factor() -- only for the lm()
      # path (non-panel). For Panel Data, the estimator (Pooling / FE-LSDV /
      # FE-Within / Random Effects) is chosen via panel_estimator_choice().
      if (want_fe) {
        fe_cols <- intersect(input$fe_vars, names(df))
        validate(need(length(fe_cols) > 0, "Selected fixed-effect variable(s) not found in the data."))
        term_labels <- c(term_labels, paste0("factor(", fe_cols, ")"))
      }
      
      model_formula <- reformulate(termlabels = term_labels, response = input$y_var)
    }
    
    # For Panel Data + LSDV fixed effects specifically, build a SEPARATE
    # formula that adds entity/time dummies explicitly. model_formula stays
    # "clean" (no dummies) -- that's what the internal Hausman/LM
    # poolability refits need, since demeaning-based "within" estimation
    # and explicit dummy columns for the same variable can't be mixed.
    # Works identically for simulated and uploaded panel data now, via the
    # unified panel_estimator_choice() helper. Only relevant when the
    # requested estimator is "lsdv" -- "within", "random", and "pooling"
    # don't use dummy columns at all.
    if (is_panel && identical(input$model_family, "linear") && identical(panel_estimator_choice(), "lsdv")) {
      fe_dummy_terms <- paste0("factor(", input$p_idx_i, ")")
      if (isTRUE(input$fe_twoway)) {
        fe_dummy_terms <- c(fe_dummy_terms, paste0("factor(", input$p_idx_t, ")"))
      }
      panel_response <- if (input$data_source == "simulate") "Y" else input$y_var
      panel_lsdv_formula <- reformulate(termlabels = c(term_labels, fe_dummy_terms), response = panel_response)
    }
    
    if (identical(input$model_family, "logistic")) {
      fit <- glm(model_formula, data = df, family = binomial())
      return(get("new_diag_lm", mode = "function")(fit, data_type = "logistic"))
    }
    
    is_linear <- identical(input$model_family, "linear")
    # is_panel already computed above -- data_structure() hasn't changed.
    
    if (is_panel) {
      validate(need(input$data_source %in% c("upload", "simulate"), "Panel Data requires simulated or uploaded data."))
      req(input$p_idx_i, input$p_idx_t)
      validate(need(input$p_idx_i %in% names(df), paste0("Cross-Section Index '", input$p_idx_i, "' not found in the data.")))
      validate(need(input$p_idx_t %in% names(df), paste0("Time Index '", input$p_idx_t, "' not found in the data.")))
      # FIX: previously nothing stopped a user from picking the SAME column
      # for both the entity index and the time index -- plm() would then
      # fail with an opaque internal error (or silently misinterpret the
      # panel structure) instead of a clear, actionable message.
      validate(need(input$p_idx_i != input$p_idx_t,
                    "Cross-Section Index and Time Index must be two different columns."))
      
      estimator <- panel_estimator_choice()
      fe_method_used <- NULL
      
      if (identical(estimator, "within")) {
        # Within (demeaning) estimator: fixed effects are removed
        # mathematically, not estimated individually -- the coefficient
        # table will only contain the actual predictors, no dummy rows.
        fe_effect <- if (isTRUE(input$fe_twoway)) "twoways" else "individual"
        fit <- plm::plm(formula = model_formula, data = df, model = "within", effect = fe_effect,
                        index = c(input$p_idx_i, input$p_idx_t))
        fe_method_used <- "within"
      } else if (identical(estimator, "random")) {
        # Random Effects: entity effects treated as a random draw,
        # assumed uncorrelated with the regressors (GLS estimation). No
        # dummy columns, no demeaning -- a genuinely different estimator
        # from both Fixed Effects variants. One-way only in this app (the
        # Two-way toggle is hidden for this choice in the UI).
        fit <- plm::plm(formula = model_formula, data = df, model = "random",
                        index = c(input$p_idx_i, input$p_idx_t))
        fe_method_used <- "random"
      } else if (identical(estimator, "lsdv")) {
        # LSDV (dummy variables): entity/time factor() dummies added as
        # regular formula terms, fit via pooling -- fixed effects show up
        # as visible coefficient rows, same table format as any other model.
        fit_formula <- if (!is.null(panel_lsdv_formula)) panel_lsdv_formula else model_formula
        fit <- plm::plm(formula = fit_formula, data = df, model = "pooling",
                        index = c(input$p_idx_i, input$p_idx_t))
        fe_method_used <- "lsdv"
      } else {
        fit <- plm::plm(formula = model_formula, data = df, model = "pooling",
                        index = c(input$p_idx_i, input$p_idx_t))
      }
    } else {
      fit <- lm(model_formula, data = df)
    }
    
    result <- get("new_diag_lm", mode = "function")(
      fit, data_type = data_structure(),
      panel_formula = if (is_panel) model_formula else NULL
    )
    
    if (is_panel && !is.null(fe_method_used)) {
      result$fe_method <- fe_method_used
    }
    
    # Heteroskedasticity-robust / cluster-robust standard errors: Linear
    # (OLS) only. Recomputes the coefficient table with a sandwich vcov and
    # swaps it in -- coeftest()'s output has the same column structure as
    # summary(lm)$coefficients, so every downstream table/summary works
    # unchanged. Failures here silently fall back to the model's default SEs
    # rather than breaking the whole analysis.
    if (is_linear && isTRUE(input$use_robust_se)) {
      cluster_col <- input$cluster_var
      has_cluster <- !is.null(cluster_col) && nzchar(cluster_col) && cluster_col %in% names(df)
      
      vc <- tryCatch({
        if (has_cluster) {
          sandwich::vcovCL(fit, cluster = df[[cluster_col]])
        } else {
          sandwich::vcovHC(fit, type = "HC1")
        }
      }, error = function(e) NULL)
      
      if (!is.null(vc)) {
        # NOTE: lmtest::coeftest()'s covariance argument is named "vcov."
        # (with a trailing dot) -- not "vcov". Using the wrong name relies
        # on partial argument matching, which is fragile. Being explicit
        # here avoids that.
        robust_coef <- tryCatch(lmtest::coeftest(fit, vcov. = vc), error = function(e) NULL)
        if (!is.null(robust_coef)) {
          # coeftest() returns an object with class "coeftest", not
          # "matrix" -- structurally it IS a matrix (Estimate/Std. Error/
          # t value/Pr(>|t|) columns, one row per term), but the custom
          # class can make as.data.frame() downstream (in summary.diag_lm)
          # treat it unpredictably instead of preserving row/column names
          # cleanly. Strip the class down to a plain matrix here, which
          # keeps all dimnames intact and makes the downstream conversion
          # behave exactly like it does for a normal lm/plm coefficient
          # table (which itself has no class attribute at all -- just a
          # bare matrix).
          robust_coef <- unclass(robust_coef)
          
          result$coefficients <- robust_coef
          result$se_type <- if (has_cluster) {
            paste0("Cluster-robust (clustered by '", cluster_col, "')")
          } else {
            "Heteroskedasticity-robust (HC1)"
          }
        }
      }
    }
    
    result
  }, ignoreNULL = FALSE)
  
  # ---- Data Preview Logic Renderers -----------------------------------------
  output$metadata_display_ui <- renderUI({
    df <- current_data()
    type_label <- switch(data_structure(), "cs" = "Cross Sectional Data", "ts" = "Time Series", "panel" = "Panel Data")
    tags$p(HTML(paste0(
      "<strong>Data Structure Class:</strong> ", type_label, "<br>",
      "<strong>Observations Count (N):</strong> ", nrow(df), "<br>",
      "<strong>Total Variables Shown:</strong> ", ncol(df)
    )))
  })
  
  output$data_preview <- renderTable({ head(current_data(), 25) })
  
  # --- DATA COLUMN PROFILES SUMMARY TABLE WITH DATE RANGE PROFILING ---
  output$data_summary_table <- render_gt({
    df <- current_data()
    total_rows <- nrow(df)
    codebook_list <- list()
    
    for (i in seq_along(names(df))) {
      var_name <- names(df)[i]
      col_data <- df[[var_name]]
      data_type <- class(col_data)[1]
      
      n_missing <- sum(is.na(col_data) | col_data == "" | col_data == "NA")
      pct_missing <- (n_missing / total_rows) * 100
      missing_str <- sprintf("%d\n(%.1f%%)", n_missing, pct_missing)
      
      # --- 1. Smart Date Detection Logic ---
      is_col_date <- inherits(col_data, "Date") || inherits(col_data, "POSIXt")
      if (!is_col_date && data_type %in% c("character", "factor")) {
        non_empty_vals <- col_data[!is.na(col_data) & col_data != "" & col_data != "NA"]
        non_empty_cnt <- length(non_empty_vals)
        n_unique <- length(unique(non_empty_vals))
        
        # Guard against false positives on low-cardinality columns (binary
        # indicators like 0/1, small category codes like 0/1/2). These can
        # look like valid epoch day-counts to as.Date() -- e.g. 0 parses as
        # "1970-01-01" -- even though they're categorical, not dates. Real
        # date columns almost always have many distinct values, so require
        # a minimum before even attempting to parse as a date.
        min_unique_for_date <- max(10, ceiling(0.05 * non_empty_cnt))
        
        if (non_empty_cnt > 0 && n_unique >= min_unique_for_date) {
          test_parsed <- suppressWarnings(parse_date_dynamically(col_data))
          if (inherits(test_parsed, "Date") && !all(is.na(test_parsed))) {
            if ((sum(!is.na(test_parsed)) / non_empty_cnt) >= 0.8) {
              is_col_date <- TRUE
              col_data <- test_parsed
              data_type <- "Date"
            }
          }
        }
      }
      
      # --- 2. Numeric Detection Logic ---
      suppressWarnings(numeric_test <- as.numeric(as.character(col_data)))
      is_col_numeric <- !is_col_date && !all(is.na(numeric_test)) && (data_type %in% c("numeric", "integer", "double"))
      
      # --- 3. Format Output Profiles ---
      if (is_col_date) {
        v_dates <- col_data[!is.na(col_data)]
        if (length(v_dates) > 0) {
          earliest  <- min(v_dates)
          latest    <- max(v_dates)
          span_days <- as.numeric(difftime(latest, earliest, units = "days"))
          
          stats_str <- sprintf("Earliest Date : %s\nLatest Date   : %s", 
                               format(earliest, "%Y-%m-%d"), 
                               format(latest, "%Y-%m-%d"))
          freqs_str <- sprintf("%d valid date records\nSpan: %s days", length(v_dates), format(span_days, big.mark = ","))
        } else {
          stats_str <- "All date values are missing"; freqs_str <- "-"
        }
      } else if (is_col_numeric) {
        v <- numeric_test[!is.na(numeric_test)]
        if (length(v) > 0) {
          avg <- mean(v); std <- sd(v); mn <- min(v); md <- median(v); mx <- max(v)
          stats_str <- sprintf("Mean (sd) : %.1f (%.1f)\nmin <= med <= max:\n%.1f <= %.1f <= %.1f\nIQR (CV) : %.1f (%.1f)", 
                               avg, std, mn, md, mx, IQR(v), if(avg != 0) std / abs(avg) else NA)
          freqs_str <- sprintf("%d distinct values", length(unique(v)))
        } else {
          stats_str <- "All values are missing"; freqs_str <- "-"
        }
      } else {
        v_chars <- as.character(col_data)
        v_chars[v_chars == "" | v_chars == "NA"] <- NA
        v_valid <- v_chars[!is.na(v_chars)]
        
        if (length(v_valid) > 0) {
          freq_table <- sort(table(v_valid), decreasing = TRUE)
          top_n <- min(10, length(freq_table))
          stats_lines <- c(); freqs_lines <- c()
          
          for (j in 1:top_n) {
            val_name <- names(freq_table)[j]
            val_count <- freq_table[j]
            if (nchar(val_name) > 25) val_name <- paste0(substr(val_name, 1, 22), "...")
            stats_lines <- c(stats_lines, sprintf("%d. %s", j, val_name))
            freqs_lines <- c(freqs_lines, sprintf("%d ( %.1f%%)", val_count, (val_count / length(v_valid)) * 100))
          }
          if (length(freq_table) > 10) {
            others_count <- sum(freq_table[(top_n + 1):length(freq_table)])
            stats_lines <- c(stats_lines, sprintf("[ %d others ]", length(freq_table) - 10))
            freqs_lines <- c(freqs_lines, sprintf("%d (%.1f%%)", others_count, (others_count / length(v_valid)) * 100))
          }
          stats_str <- paste(stats_lines, collapse = "\n")
          freqs_str <- paste(freqs_lines, collapse = "\n")
        } else {
          stats_str <- "Empty column"; freqs_str <- "-"
        }
      }
      
      codebook_list[[i]] <- data.frame(No = i, Variable = sprintf("%s\n[%s]", var_name, data_type),
                                       `Stats / Values` = stats_str, `Freqs (% of Valid)` = freqs_str,
                                       Missing = missing_str, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    gt(do.call(rbind, codebook_list)) %>%
      tab_options(table.width = pct(100), table.font.size = 13) %>%
      cols_align(align = "left", columns = c(Variable, `Stats / Values`, `Freqs (% of Valid)`)) %>%
      cols_align(align = "center", columns = c(No, Missing)) %>%
      text_transform(locations = cells_body(), fn = function(x) { paste0("<div class='codebook-text'>", gsub("\n", "<br>", x), "</div>") })
  })
  
   output$correlation_matrix_table <- render_gt({
    df <- current_data()
    # FIX: Date/POSIXt columns previously slipped into the correlation
    # matrix, because as.numeric(a_Date) returns its epoch day-count
    # without ever producing NA -- so the old numeric-detection test
    # (!all(is.na(as.numeric(x)))) treated dates as "numeric". Correlating
    # a raw epoch day-count against other variables is economically
    # meaningless and misleading if shown next to genuine numeric fields.
    # Date/POSIXt columns are now excluded explicitly before the numeric
    # test is applied, regardless of what as.numeric() would return for them.
    is_date_col <- vapply(df, function(x) inherits(x, c("Date", "POSIXt")), logical(1))
    num_cols <- names(df)[!is_date_col & vapply(df, function(x) !all(is.na(suppressWarnings(as.numeric(x)))), logical(1))]
    validate(need(length(num_cols) >= 2, "Need at least two numeric fields to build a correlation matrix."))
    
    numeric_df <- as.data.frame(lapply(df[, num_cols, drop = FALSE], function(x) suppressWarnings(as.numeric(x))))
    cor_mat <- as.data.frame(cor(numeric_df, use = "pairwise.complete.obs", method = "pearson"))
    cor_mat <- cbind(Variable = rownames(cor_mat), cor_mat)
    
    gt(cor_mat) %>% 
      fmt_number(columns = -Variable, decimals = 3) %>% 
      tab_options(table.width = pct(100)) %>% 
      data_color(columns = -Variable, palette = c("#d73027", "#f7f7f7", "#4575b4"), domain = c(-1, 1))
  })
  
  # ---- Diagnostics ValueBoxes & Summary Output Renderers --------------------
  output$summary_table <- render_gt({ summary(diag_model()) })
  
  # Breusch-Pagan diagnostic box
  output$bp_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$bp_pvalue
    if (is.na(p_value)) {
      valueBox("N/A", "Breusch-Pagan test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0("Breusch-Pagan | Constant error variance | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("Breusch-Pagan | Unequal error variance | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Durbin-Watson diagnostic box
  output$dw_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$dw_pvalue
    statistic <- model$diagnostics$dw_statistic
    if (is.na(p_value)) {
      valueBox("N/A", "Durbin-Watson test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0("Durbin-Watson | Independent errors | DW = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("Durbin-Watson | Serial correlation concern | DW = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Shapiro-Wilk diagnostic box
  output$sw_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$shapiro_pvalue
    test_name <- model$diagnostics$normality_test_name
    if (is.null(test_name) || is.na(test_name)) test_name <- "Shapiro-Wilk"
    
    if (is.na(p_value)) {
      valueBox("N/A", paste0(test_name, " test unavailable"), icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0(test_name, " | Approximately normal residuals | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0(test_name, " | Residual normality concern | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # VIF diagnostic box
  output$vif_box <- renderValueBox({
    model <- diag_model()
    vif_values <- suppressWarnings(as.numeric(model$diagnostics$vif_scores))
    vif_values <- vif_values[is.finite(vif_values)]
    
    if (length(vif_values) == 0) {
      valueBox("N/A", "VIF unavailable", icon = icon("question-circle"), color = "yellow")
    } else {
      max_vif <- max(vif_values)
      if (max_vif < 5) {
        valueBox("PASS", paste0("VIF | Low multicollinearity | Max VIF = ", round(max_vif, 2)), icon = icon("check-circle"), color = "green")
      } else if (max_vif < 10) {
        valueBox("CAUTION", paste0("VIF | Moderate multicollinearity | Max VIF = ", format(round(max_vif, 2), nsmall = 2)), icon = icon("exclamation-circle"), color = "yellow")
      } else {
        valueBox("CHECK", paste0("VIF | High multicollinearity | Max VIF = ", format(round(max_vif, 2), nsmall = 2)), icon = icon("exclamation-triangle"), color = "red")
      }
    }
  })
  
  # --- TIME SERIES SPECIFIC VALUEBOXES ---
  # Augmented Dickey-Fuller (ADF) Stationarity diagnostic box
  output$adf_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$ts$adf_pvalue
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "ADF stationarity test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value < 0.05) {
      valueBox("PASS", paste0("ADF | Residuals stationary | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("ADF | Non-stationary / Unit root | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Breusch-Godfrey Autocorrelation diagnostic box
  output$bg_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$ts$bg_pvalue
    statistic <- model$diagnostics$ts$bg_statistic
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Breusch-Godfrey test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0("Breusch-Godfrey | No serial correlation | BG = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("Breusch-Godfrey | Serial correlation concern | BG = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Ljung-Box Autocorrelation diagnostic box (univariate self-correlation)
  output$ljungbox_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$ts$ljung_box_pvalue
    statistic <- model$diagnostics$ts$ljung_box_statistic
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Ljung-Box test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0("Ljung-Box | No self-correlation | LB = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("Ljung-Box | Autocorrelated with own past | LB = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Jarque-Bera distribution diagnostic box (univariate)
  output$jb_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$ts$jb_pvalue
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Jarque-Bera test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0("Jarque-Bera | Approximately normal distribution | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("Jarque-Bera | Non-normal (fat tails/skew) | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Overall Time Series diagnostic status box (regression-on-another-variable mode)
  output$overall_box_ts <- renderValueBox({
    model <- diag_model()
    
    adf_p <- model$diagnostics$ts$adf_pvalue
    bg_p  <- model$diagnostics$ts$bg_pvalue
    sw_p  <- model$diagnostics$shapiro_pvalue
    
    vif_values <- suppressWarnings(as.numeric(model$diagnostics$vif_scores))
    vif_values <- vif_values[is.finite(vif_values)]
    
    adf_pass <- !is.null(adf_p) && !is.na(adf_p) && adf_p < 0.05
    bg_pass  <- !is.null(bg_p)  && !is.na(bg_p)  && bg_p >= 0.05
    sw_pass  <- !is.null(sw_p)  && !is.na(sw_p)  && sw_p >= 0.05
    vif_pass <- length(vif_values) == 0 || max(vif_values) < 5
    
    results <- c(adf_pass, bg_pass, sw_pass, vif_pass)
    passed <- sum(results)
    total <- length(results)
    
    if (passed == total) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: No major diagnostic concerns", icon = icon("check-double"), color = "green", width = 12)
    } else if (passed >= 3) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Review highlighted diagnostic", icon = icon("exclamation-circle"), color = "yellow", width = 12)
    } else {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Multiple diagnostic concerns require attention", icon = icon("exclamation-triangle"), color = "red", width = 12)
    }
  })
  
  # Hausman test: MODEL-SELECTION GUIDANCE, not a pass/fail assumption
  # check. A "significant" result isn't a data problem -- it just tells you
  # which estimator is theoretically valid. Red/CHECK would misleadingly
  # read as "something is wrong," so this uses a neutral color and a direct
  # recommendation label for both outcomes instead.
  output$hausman_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$panel$hausman_pvalue
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Hausman test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("RANDOM EFFECTS", paste0("Hausman | Random Effects is consistent | p ", fmt_pval(p_value)), icon = icon("scale-balanced"), color = "light-blue")
    } else {
      valueBox("FIXED EFFECTS", paste0("Hausman | Fixed Effects recommended | p ", fmt_pval(p_value)), icon = icon("scale-balanced"), color = "light-blue")
    }
  })
  
  # Breusch-Pagan LM: also model-selection guidance (pooled OLS vs. a panel
  # model), not an assumption check.
  output$panel_lm_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$panel$lm_pvalue
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Breusch-Pagan LM test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("POOLED OLS OK", paste0("Breusch-Pagan LM | No evidence of panel effects | p ", fmt_pval(p_value)), icon = icon("layer-group"), color = "light-blue")
    } else {
      valueBox("USE PANEL MODEL", paste0("Breusch-Pagan LM | Panel effects present | p ", fmt_pval(p_value)), icon = icon("layer-group"), color = "light-blue")
    }
  })
  
  # Time-fixed effects (pFtest): same category -- one-way vs. two-way is a
  # specification choice, not a violation.
  output$panel_pftest_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$panel$pftest_pvalue
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Time-fixed effects test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("ONE-WAY FE OK", paste0("Time effects | One-way FE is adequate | p ", fmt_pval(p_value)), icon = icon("clock"), color = "light-blue")
    } else {
      valueBox("USE TWO-WAY FE", paste0("Time effects | Two-way FE recommended | p ", fmt_pval(p_value)), icon = icon("clock"), color = "light-blue")
    }
  })
  # Panel Breusch-Godfrey serial correlation test.
  output$panel_bg_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$panel$pbg_pvalue
    statistic <- model$diagnostics$panel$pbg_statistic
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Panel Breusch-Godfrey test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0("Panel Breusch-Godfrey | No serial correlation | BG = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("Panel Breusch-Godfrey | Serial correlation concern | BG = ", round(statistic, 2), " | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Overall Panel Data diagnostic status box
  output$overall_box_panel <- renderValueBox({
    model <- diag_model()
    hausman_p <- model$diagnostics$panel$hausman_pvalue
    lm_p      <- model$diagnostics$panel$lm_pvalue
    pbg_p     <- model$diagnostics$panel$pbg_pvalue
    sw_p      <- model$diagnostics$shapiro_pvalue
    
    hausman_pass <- !is.null(hausman_p) && !is.na(hausman_p) && hausman_p >= 0.05
    lm_pass      <- !is.null(lm_p)      && !is.na(lm_p)      && lm_p >= 0.05
    pbg_pass     <- !is.null(pbg_p)     && !is.na(pbg_p)     && pbg_p >= 0.05
    sw_pass      <- !is.null(sw_p)      && !is.na(sw_p)      && sw_p >= 0.05
    
    results <- c(hausman_pass, lm_pass, pbg_pass, sw_pass)
    passed <- sum(results)
    total <- length(results)
    
    if (passed == total) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: No major diagnostic concerns", icon = icon("check-double"), color = "green", width = 12)
    } else if (passed >= 3) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Review highlighted diagnostic", icon = icon("exclamation-circle"), color = "yellow", width = 12)
    } else {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Multiple diagnostic concerns require attention", icon = icon("exclamation-triangle"), color = "red", width = 12)
    }
  })
  
  # Overall OLS diagnostic status box
  output$overall_box <- renderValueBox({
    model <- diag_model()
    bp_p <- model$diagnostics$bp_pvalue
    dw_p <- model$diagnostics$dw_pvalue
    sw_p <- model$diagnostics$shapiro_pvalue
    
    vif_values <- suppressWarnings(as.numeric(model$diagnostics$vif_scores))
    vif_values <- vif_values[is.finite(vif_values)]
    
    bp_pass <- !is.na(bp_p) && bp_p >= 0.05
    dw_pass <- !is.na(dw_p) && dw_p >= 0.05
    sw_pass <- !is.na(sw_p) && sw_p >= 0.05
    vif_pass <- length(vif_values) > 0 && max(vif_values) < 5
    
    results <- c(bp_pass, dw_pass, sw_pass, vif_pass)
    passed <- sum(results)
    total <- length(results)
    
    if (passed == total) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: No major diagnostic concerns", icon = icon("check-double"), color = "green", width = 12)
    } else if (passed >= 3) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Review highlighted diagnostic", icon = icon("exclamation-circle"), color = "yellow", width = 12)
    } else {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Multiple diagnostic concerns require attention", icon = icon("exclamation-triangle"), color = "red", width = 12)
    }
  })
  
  # Logistic Diagnostic ValueBoxes
  output$hl_box <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    p_value <- model$diagnostics$logistic$hl_pvalue
    if (is.null(p_value) || is.na(p_value)) {
      valueBox("N/A", "Hosmer-Lemeshow test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox("PASS", paste0("Hosmer-Lemeshow | Adequate fit | p ", fmt_pval(p_value)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("CHECK", paste0("Hosmer-Lemeshow | Poor fit | p ", fmt_pval(p_value)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  output$auc_box <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    auc <- model$diagnostics$logistic$auc
    if (is.null(auc) || is.na(auc)) {
      valueBox("N/A", "AUC unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (auc >= 0.8) {
      valueBox("PASS", paste0("AUC | Good discrimination | AUC = ", round(auc, 3)), icon = icon("check-circle"), color = "green")
    } else if (auc >= 0.7) {
      valueBox("CAUTION", paste0("AUC | Acceptable discrimination | AUC = ", round(auc, 3)), icon = icon("exclamation-circle"), color = "yellow")
    } else {
      valueBox("CHECK", paste0("AUC | Weak discrimination | AUC = ", round(auc, 3)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  output$accuracy_box <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    acc <- model$diagnostics$logistic$accuracy
    if (is.null(acc) || is.na(acc)) {
      valueBox("N/A", "Accuracy unavailable", icon = icon("question-circle"), color = "yellow")
    } else {
      valueBox(paste0(round(acc * 100, 1), "%"), "Accuracy | Classification accuracy at 0.5 cutoff", icon = icon("bullseye"), color = if (acc >= 0.75) "green" else if (acc >= 0.6) "yellow" else "red")
    }
  })
  
  output$overall_box_logistic <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    hl_p <- model$diagnostics$logistic$hl_pvalue
    auc  <- model$diagnostics$logistic$auc
    vif_values <- suppressWarnings(as.numeric(model$diagnostics$vif_scores))
    vif_values <- vif_values[is.finite(vif_values)]
    
    hl_pass  <- !is.na(hl_p) && hl_p >= 0.05
    auc_pass <- !is.na(auc) && auc >= 0.7
    vif_pass <- length(vif_values) == 0 || max(vif_values) < 5
    sep_pass <- !isTRUE(model$diagnostics$logistic$separation_flag)
    
    results <- c(hl_pass, auc_pass, vif_pass, sep_pass)
    passed <- sum(results)
    total <- length(results)
    
    if (passed == total) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: No major diagnostic concerns", icon = icon("check-double"), color = "green", width = 12)
    } else if (passed >= 3) {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Review highlighted diagnostic", icon = icon("exclamation-circle"), color = "yellow", width = 12)
    } else {
      valueBox(paste0(passed, "/", total, " assumptions passed"), "Overall status: Multiple diagnostic concerns require attention", icon = icon("exclamation-triangle"), color = "red", width = 12)
    }
  })
  
  # Dynamic Assumptions Check ValueBoxes UI render logic
  output$assumptions_valueboxes_ui <- renderUI({
    req(input$model_family)
    
    legend <- function(note) {
      div(class = "assumptions-legend",
          icon("circle-info"),
          HTML(paste0(
            "<b>Green = PASS, Red = CHECK.</b> ", note
          ))
      )
    }
    
    if (identical(input$model_family, "logistic")) {
      tagList(
        legend("For every test shown here, a HIGH p-value (\u2265 0.05) is the good outcome."),
        fluidRow(
          valueBoxOutput("hl_box", width = 3),
          valueBoxOutput("auc_box", width = 3),
          valueBoxOutput("accuracy_box", width = 3),
          valueBoxOutput("vif_box", width = 3)
        ),
        fluidRow(valueBoxOutput("overall_box_logistic", width = 12))
      )
    } else if (identical(input$model_family, "ts") || identical(data_structure(), "ts")) {
      is_uni_ts <- identical(input$model_family, "ts")  # Time Series is always univariate now
      
      if (is_uni_ts) {
        tagList(
          legend("ADF is the exception here: a LOW p is good (rejects a unit root). For Ljung-Box, Jarque-Bera, and Shapiro-Wilk, a HIGH p is good."),
          fluidRow(
            valueBoxOutput("adf_box", width = 3),
            valueBoxOutput("ljungbox_box", width = 3),
            valueBoxOutput("jb_box", width = 3),
            valueBoxOutput("sw_box", width = 3)
          )
        )
      } else {
        tagList(
          legend("ADF is the exception here: a LOW p is good (rejects a unit root). For Breusch-Godfrey, Shapiro-Wilk, and VIF, a HIGH p (or a low VIF) is good."),
          fluidRow(
            valueBoxOutput("adf_box", width = 3),
            valueBoxOutput("bg_box", width = 3),
            valueBoxOutput("sw_box", width = 3),
            valueBoxOutput("vif_box", width = 3)
          ),
          fluidRow(valueBoxOutput("overall_box_ts", width = 12))
        )
      }
    } else if (identical(data_structure(), "panel")) {
      tagList(
        div(class = "assumptions-legend",
            icon("circle-info"),
            HTML("<b>Blue boxes = model-selection guidance</b> (Hausman, Breusch-Pagan LM, Time-fixed effects) -- they tell you which specification to use; neither outcome is 'bad'. <b>Green = PASS, Red = CHECK</b> apply only to the two genuine assumption checks below (serial correlation, normality).")
        ),
        fluidRow(
          valueBoxOutput("hausman_box", width = 4),
          valueBoxOutput("panel_lm_box", width = 4),
          valueBoxOutput("panel_pftest_box", width = 4)
        ),
        fluidRow(
          valueBoxOutput("panel_bg_box", width = 6),
          valueBoxOutput("sw_box", width = 6)
        ),
        fluidRow(valueBoxOutput("overall_box_panel", width = 12))
      )
    } else {
      tagList(
        legend("For every p-value test shown here, a HIGH p-value (\u2265 0.05) is the good outcome; for VIF, a LOW score is good."),
        fluidRow(
          valueBoxOutput("bp_box", width = 3),
          valueBoxOutput("dw_box", width = 3),
          valueBoxOutput("sw_box", width = 3),
          valueBoxOutput("vif_box", width = 3)
        ),
        fluidRow(valueBoxOutput("overall_box", width = 12))
      )
    }
  })
  
  # ---- Plot Renderers -------------------------------------------------------
  output$plot_residuals      <- renderPlot({ plot(diag_model(), type = "residuals") })
  output$plot_qq             <- renderPlot({ plot(diag_model(), type = "qq") })
  output$plot_scale          <- renderPlot({ plot(diag_model(), type = "scale_location") })
  output$plot_hist           <- renderPlot({ plot(diag_model(), type = "histogram") })
  output$plot_acf            <- renderPlot({ plot(diag_model(), type = "acf") })
  output$plot_pacf           <- renderPlot({ plot(diag_model(), type = "pacf") })
  output$plot_residuals_time <- renderPlot({ plot(diag_model(), type = "residuals_time") })
  output$plot_binned         <- renderPlot({ req(identical(diag_model()$data_type, "binary")); plot(diag_model(), type = "binned_residuals") })
  output$plot_roc            <- renderPlot({ req(identical(diag_model()$data_type, "binary")); plot(diag_model(), type = "roc") })
  output$plot_calibration    <- renderPlot({ req(identical(diag_model()$data_type, "binary")); plot(diag_model(), type = "calibration") })
  
  output$diagnostic_plots_ui <- renderUI({
    req(data_structure(), input$model_family)
    
    if (identical(input$model_family, "logistic")) {
      tagList(
        fluidRow(
          plot_box("Binned Residuals", "binned", "plot_binned", width = 6),
          plot_box("ROC Curve", "roc", "plot_roc", width = 6)
        ),
        fluidRow(
          plot_box("Calibration Plot", "calibration", "plot_calibration", width = 6),
          plot_box("Normal Q-Q", "qq", "plot_qq", width = 6)
        )
      )
    } else if (identical(data_structure(), "ts")) {
      is_uni_ts <- identical(input$model_family, "ts")  # Time Series is always univariate now
      series_time_title <- if (is_uni_ts) "Series Over Time" else "Residuals over Time"
      hist_title <- if (is_uni_ts) "Distribution" else "Residual Histogram"
      
      tagList(
        fluidRow(
          plot_box(series_time_title, "residuals_time", "plot_residuals_time", width = 6),
          plot_box("Autocorrelation (ACF)", "acf", "plot_acf", width = 6)
        ),
        fluidRow(
          plot_box("Partial Autocorrelation (PACF)", "pacf", "plot_pacf", width = 6),
          plot_box("Normal Q-Q", "qq", "plot_qq", width = 6)
        ),
        fluidRow(
          plot_box(hist_title, "hist", "plot_hist", width = 12)
        )
      )
    } else {
      tagList(
        fluidRow(
          plot_box("Residuals vs Fitted", "residuals", "plot_residuals", width = 6),
          plot_box("Normal Q-Q", "qq", "plot_qq", width = 6)
        ),
        fluidRow(
          plot_box("Scale-Location", "scale", "plot_scale", width = 6),
          plot_box("Residual Histogram", "hist", "plot_hist", width = 6)
        )
      )
    }
  })
  
  # ---- Per-plot AI interpretation (lightbulb buttons) -----------------------
  # Maps each plot's `key` (used above in plot_box()) to the `type` string
  # plot.diag_lm() / get_plot_interpretation() expect. Generates a
  # click observer + output for every plot in one loop, instead of writing
  # ~10 nearly-identical observeEvent/renderUI blocks by hand.
  .plot_interp_types <- c(
    residuals      = "residuals",
    qq             = "qq",
    scale          = "scale_location",
    hist           = "histogram",
    acf            = "acf",
    pacf           = "pacf",
    residuals_time = "residuals_time",
    binned         = "binned_residuals",
    roc            = "roc",
    calibration    = "calibration"
  )
  
  lapply(names(.plot_interp_types), function(key) {
    local({
      this_key  <- key
      this_type <- .plot_interp_types[[this_key]]
      btn_id    <- paste0("interpret_btn_", this_key)
      out_id    <- paste0("interpret_ui_", this_key)
      
      observeEvent(input[[btn_id]], {
        req(diag_model())
        output[[out_id]] <- renderUI({
          get_plot_interpretation(diag_model(), this_type)
        })
      })
    })
  })
  
  output$simulator_used_note <- renderText({
    req(diag_model())
    if (identical(input$data_source, "simulate")) {
      if (identical(input$model_family, "logistic")) "Simulator used: Logistic simulator (simulate_logit_data)."
      else if (identical(data_structure(), "ts")) "Simulator used: Time-Series simulator." else "Simulator used: Cross-Section simulator."
    } else if (identical(input$data_source, "available")) {
      paste("Data source used: Local system dataset (", input$available_file, ").")
    } else "Data source used: Uploaded CSV file."
  })
  
  output$time_series_note <- renderText({
    if (identical(data_structure(), "ts")) "Time Series Selected: Check serial-correlation & ADF stationarity diagnostics in the output."
  })
  
  output$remediation <- renderUI({ HTML(get("remediation_advice", mode = "function")(diag_model())) })
  
  ai_interpretation <- eventReactive(input$ai_interpret_btn, {
    req(diag_model())
    get_llm_interpretation(diag_model())
  })
  output$ai_interpretation_ui <- renderUI({ ai_interpretation() })
}

shinyApp(ui = ui, server = server)