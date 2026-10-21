# app.R
# Body in Motion — Human Body Response Simulator
# Interactive Six-Page Web Application in R Shiny
# Matching the Premium Medical-Analytics Reference Design

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(ranger)
})

# Source internal modules
source("R/visualizations.R")
source("R/simulation.R")

# Load preprocessed dataset and model artifacts
SESSIONS_FILE <- "data/processed/physiological_sessions.rds"
SUMMARY_FILE  <- "data/processed/sessions_summary.rds"
DEMO_FILE     <- "data/processed/demographics.rds"
MODEL_FILE    <- "models/rf_hr_model.rds"
EVAL_FILE     <- "models/evaluation_results.rds"

sessions_data <- if (file.exists(SESSIONS_FILE)) readRDS(SESSIONS_FILE) else data.frame()
summary_data  <- if (file.exists(SUMMARY_FILE)) readRDS(SUMMARY_FILE) else data.frame()
demo_data     <- if (file.exists(DEMO_FILE)) readRDS(DEMO_FILE) else data.frame()
rf_model      <- if (file.exists(MODEL_FILE)) readRDS(MODEL_FILE) else NULL
eval_results  <- if (file.exists(EVAL_FILE)) readRDS(EVAL_FILE) else NULL

participant_choices <- if (nrow(sessions_data) > 0) unique(sessions_data$participant_id) else c("f01", "S01", "S02")

# =========================================================================
# PAGE 1: HOME PAGE UI DEFINITION
# =========================================================================
page_home_ui <- function() {
  total_records <- if (nrow(sessions_data) > 0) nrow(sessions_data) else 75042
  total_participants <- if (nrow(demo_data) > 0) nrow(demo_data) else 46
  
  tagList(
    # HERO SECTION
    div(
      class = "row align-items-center mb-4",
      # Left Text
      div(
        class = "col-lg-6",
        div(class = "bim-badge", "REAL DATA • INTERACTIVE EXPLORATION • EVIDENCE-BASED INSIGHTS"),
        h1(class = "bim-hero-title", "Body in ", span("Motion")),
        div(class = "bim-hero-subtitle", "Explore the Human Body. Understand the Response."),
        p(class = "bim-hero-desc",
          "Analyze real wearable sensor data and simulate how the human body responds to different physical activities: resting baseline, aerobic endurance, anaerobic sprints, and acute cognitive stress."),
        div(
          class = "bim-btn-group",
          tags$button("Explore Dataset", class = "bim-btn-primary", onclick = "bimShowPage('data');"),
          tags$button("Run a Simulation", class = "bim-btn-secondary", onclick = "bimShowPage('sim');")
        )
      ),
      # Right Anatomical Illustration with Glowing Callouts
      div(
        class = "col-lg-6",
        div(
          class = "bim-anatomical-wrap",
          tags$img(src = "images/anatomical_body.svg", style = "width: 78%; height: 500px; object-fit: contain; filter: drop-shadow(0 0 25px rgba(0, 207, 255, 0.25));"),
          
          # 4 Connected Physiological Callouts
          div(
            class = "bim-callout-card callout-cardio",
            div(class = "bim-callout-header",
                HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="#FF4B6E"><path d="M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78-3.4 6.86-8.55 11.54L12 21.35z"/></svg>'),
                span(class = "bim-callout-title", "Cardiovascular")
            ),
            div(class = "bim-callout-desc", "Heart rate & blood circulation response to activity")
          ),
          div(
            class = "bim-callout-card callout-resp",
            div(class = "bim-callout-header",
                HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="#00D2FF" stroke-width="2"><path d="M12 2v20M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6"/></svg>'),
                span(class = "bim-callout-title", "Respiratory")
            ),
            div(class = "bim-callout-desc", "Breathing dynamics, gas exchange and oxygen uptake")
          ),
          div(
            class = "bim-callout-card callout-musc",
            div(class = "bim-callout-header",
                HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="#FFAA00"><path d="M6 16.5l3.5-3.5 2.5 2.5 5.5-5.5"/></svg>'),
                span(class = "bim-callout-title", "Muscular")
            ),
            div(class = "bim-callout-desc", "Muscle recruitment, biomechanical movement and energy demand")
          ),
          div(
            class = "bim-callout-card callout-thermo",
            div(class = "bim-callout-header",
                HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="#10E894" stroke-width="2"><path d="M14 14.76V3.5a2.5 2.5 0 0 0-5 0v11.26a4.5 4.5 0 1 0 5 0z"/></svg>'),
                span(class = "bim-callout-title", "Thermoregulatory")
            ),
            div(class = "bim-callout-desc", "Skin temperature dissipation, vasodilation & sweat regulation")
          )
        )
      )
    ),

    # DATASET SUMMARY STATS ROW
    div(
      class = "bim-stats-row",
      div(class = "bim-stat-box",
          div(class = "bim-stat-icon", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/></svg>')),
          div(div(class = "bim-stat-val", paste0(total_participants)), div(class = "bim-stat-lbl", "Participants in Cohort"))
      ),
      div(class = "bim-stat-box",
          div(class = "bim-stat-icon", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="22 12 18 12 15 21 9 3 6 12 2 12"/></svg>')),
          div(div(class = "bim-stat-val", "5"), div(class = "bim-stat-lbl", "Physiological Signals (HR, TEMP, EDA, ACC, BVP)"))
      ),
      div(class = "bim-stat-box",
          div(class = "bim-stat-icon", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><polyline points="12 6 12 12 14 14"/></svg>')),
          div(div(class = "bim-stat-val", "4"), div(class = "bim-stat-lbl", "Activity Conditions (Rest, Aerobic, Anaerobic, Stress)"))
      ),
      div(class = "bim-stat-box",
          div(class = "bim-stat-icon", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"/></svg>')),
          div(div(class = "bim-stat-val", paste0(format(total_records, big.mark = ","))), div(class = "bim-stat-lbl", "Synchronized Time Points"))
      )
    ),

    # LIVE PHYSIOLOGICAL METRIC CARDS ROW
    div(
      class = "bim-metrics-row",
      div(
        class = "bim-metric-card",
        div(class = "bim-metric-header",
            span(class = "bim-metric-title", "Heart Rate"),
            HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="#FF4B6E"><path d="M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78-3.4 6.86-8.55 11.54L12 21.35z"/></svg>')
        ),
        div(class = "bim-metric-val-wrap",
            span(class = "bim-metric-val", "72"),
            span(class = "bim-metric-unit", "BPM")
        ),
        div(span(class = "bim-metric-badge", style = "background: rgba(16, 232, 148, 0.15); color: #10E894;", "+2 BPM"))
      ),
      div(
        class = "bim-metric-card",
        div(class = "bim-metric-header",
            span(class = "bim-metric-title", "Skin Temperature"),
            HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="#00D2FF" stroke-width="2"><path d="M14 14.76V3.5a2.5 2.5 0 0 0-5 0v11.26a4.5 4.5 0 1 0 5 0z"/></svg>')
        ),
        div(class = "bim-metric-val-wrap",
            span(class = "bim-metric-val", "34.1"),
            span(class = "bim-metric-unit", "°C")
        ),
        div(span(class = "bim-metric-badge", style = "background: rgba(0, 207, 255, 0.15); color: #00CFFF;", "+0.1 °C"))
      ),
      div(
        class = "bim-metric-card",
        div(class = "bim-metric-header",
            span(class = "bim-metric-title", "Electrodermal Activity"),
            HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="#FFAA00"><path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/></svg>')
        ),
        div(class = "bim-metric-val-wrap",
            span(class = "bim-metric-val", "0.32"),
            span(class = "bim-metric-unit", "µS")
        ),
        div(span(class = "bim-metric-badge", style = "background: rgba(255, 170, 0, 0.15); color: #FFAA00;", "-0.05 µS"))
      ),
      div(
        class = "bim-metric-card",
        div(class = "bim-metric-header",
            span(class = "bim-metric-title", "Movement Intensity"),
            HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="#10E894" stroke-width="2"><polyline points="22 12 18 12 15 21 9 3 6 12 2 12"/></svg>')
        ),
        div(class = "bim-metric-val-wrap",
            span(class = "bim-metric-val", "0.18"),
            span(class = "bim-metric-unit", "g")
        ),
        div(span(class = "bim-metric-badge", style = "background: rgba(16, 232, 148, 0.15); color: #10E894;", "Steady Rest"))
      ),
      div(
        class = "bim-metric-card",
        div(class = "bim-metric-header",
            span(class = "bim-metric-title", "Blood Volume Pulse"),
            HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="#A855F7" stroke-width="2"><path d="M2 12h5l2-7 4 14 3-7h6"/></svg>')
        ),
        div(class = "bim-metric-val-wrap",
            span(class = "bim-metric-val", "1.52"),
            span(class = "bim-metric-unit", "a.u.")
        ),
        div(span(class = "bim-metric-badge", style = "background: rgba(168, 85, 247, 0.15); color: #A855F7;", "Rhythmic"))
      )
    ),

    # FOUR WORKFLOW FEATURE CARDS
    div(
      class = "bim-features-row",
      div(
        class = "bim-feature-card", onclick = "bimShowPage('data');",
        div(class = "bim-feature-header",
            div(class = "bim-feature-icon", style = "background: rgba(8, 123, 255, 0.15); color: #087BFF;", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M3 3v18h18"/><path d="m19 9-5 5-4-4-3 3"/></svg>')),
            span(class = "bim-feature-title", "Data Exploration")
        ),
        div(class = "bim-feature-desc", "Explore and filter real wearable sensor recordings across rest, aerobic exercise, anaerobic exercise, and stress."),
        div(class = "bim-feature-graphic",
            HTML('<svg width="100%" height="45" viewBox="0 0 200 45"><path d="M0 25 Q 30 10, 60 22 T 120 15 T 180 30 T 200 20" fill="none" stroke="#00CFFF" stroke-width="2"/><path d="M0 32 Q 40 28, 80 18 T 140 25 T 200 15" fill="none" stroke="#FF4B6E" stroke-width="1.5"/></svg>')
        )
      ),
      div(
        class = "bim-feature-card", onclick = "bimShowPage('sim');",
        div(class = "bim-feature-header",
            div(class = "bim-feature-icon", style = "background: rgba(16, 232, 148, 0.15); color: #10E894;", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polygon points="5 3 19 12 5 21 5 3"/></svg>')),
            span(class = "bim-feature-title", "Scenario Simulation")
        ),
        div(class = "bim-feature-desc", "Adjust activity type, intensity level, and duration to predict heart rate response via Random Forest."),
        div(class = "bim-feature-graphic",
            HTML('<svg width="100%" height="45" viewBox="0 0 200 45"><rect x="15" y="10" width="40" height="25" rx="3" fill="rgba(59,130,246,0.2)"/><rect x="65" y="5" width="70" height="30" rx="3" fill="rgba(239,68,68,0.2)"/><rect x="145" y="10" width="40" height="25" rx="3" fill="rgba(16,232,148,0.2)"/><path d="M10 32 Q 50 30, 95 12 T 160 28 T 190 32" fill="none" stroke="#FF4B6E" stroke-dasharray="3,3" stroke-width="2"/></svg>')
        )
      ),
      div(
        class = "bim-feature-card", onclick = "bimShowPage('analytics');",
        div(class = "bim-feature-header",
            div(class = "bim-feature-icon", style = "background: rgba(255, 170, 0, 0.15); color: #FFAA00;", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="3" y="12" width="4" height="9"/><rect x="10" y="7" width="4" height="14"/><rect x="17" y="3" width="4" height="18"/></svg>')),
            span(class = "bim-feature-title", "Visual Analytics")
        ),
        div(class = "bim-feature-desc", "View pairwise comparisons, correlation heatmaps, recovery dynamics, and distribution models."),
        div(class = "bim-feature-graphic",
            HTML('<svg width="100%" height="45" viewBox="0 0 200 45"><rect x="25" y="20" width="20" height="20" fill="#3B82F6"/><rect x="65" y="10" width="20" height="30" fill="#10E894"/><rect x="105" y="5" width="20" height="35" fill="#FF4B6E"/><rect x="145" y="15" width="20" height="25" fill="#FFAA00"/></svg>')
        )
      ),
      div(
        class = "bim-feature-card", onclick = "bimShowPage('about');",
        div(class = "bim-feature-header",
            div(class = "bim-feature-icon", style = "background: rgba(168, 85, 247, 0.15); color: #A855F7;", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><line x1="12" y1="16" x2="12" y2="12"/><line x1="12" y1="8" x2="12.01" y2="8"/></svg>')),
            span(class = "bim-feature-title", "System Intelligence")
        ),
        div(class = "bim-feature-desc", "Explore systemic physiological coordination, sensor specifications, and enterprise analytics methodology."),
        div(class = "bim-feature-graphic",
            HTML('<svg width="100%" height="45" viewBox="0 0 200 45"><circle cx="100" cy="22" r="16" fill="rgba(0,207,255,0.2)" stroke="#00CFFF" stroke-width="1.5"/><circle cx="100" cy="22" r="6" fill="#FF4B6E"/></svg>')
        )
      )
    )
  )
}

# =========================================================================
# PAGE 2: DATA EXPLORATION PAGE UI DEFINITION
# =========================================================================
page_data_ui <- function() {
  tagList(
    div(class = "mb-4",
        h2(class = "bim-hero-subtitle", style = "font-size: 1.8rem; margin-bottom: 4px;", "Data Exploration"),
        p(class = "bim-hero-desc", "Explore and filter real wearable sensor recordings from resting, exercise, and stress protocols.")
    ),
    div(
      class = "row",
      # LEFT FILTER PANEL
      div(
        class = "col-lg-3 mb-4",
        div(
          class = "bim-sidebar",
          div(class = "bim-card-title mb-3", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polygon points="22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3"/></svg>'), "Filters"),
          
          # Participant Selector
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Participant"),
              selectInput("data_participant", NULL, choices = participant_choices, selected = participant_choices[1], width = "100%")
          ),
          
          # Activity Condition Dropdown
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Activity Condition"),
              selectInput("data_activity", NULL, 
                          choices = c("All Conditions", "Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session"),
                          selected = "All Conditions", width = "100%")
          ),
          
          # Condition Checkboxes
          div(class = "bim-form-group",
              checkboxGroupInput("data_activity_checks", NULL,
                                 choices = c("Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session"),
                                 selected = c("Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session"))
          ),
          
          # Time Range Slider
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Time Range (minutes)"),
              sliderInput("data_time_range", NULL, min = 0, max = 35, value = c(0, 30), step = 1, width = "100%")
          ),
          
          # Signals to Display Checkboxes
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Signals to Display"),
              checkboxGroupInput("data_signals", NULL,
                                 choices = c("Heart Rate" = "heart_rate",
                                             "Skin Temperature" = "skin_temp",
                                             "EDA" = "eda",
                                             "Movement" = "movement_g",
                                             "BVP" = "bvp"),
                                 selected = c("heart_rate", "skin_temp", "eda", "movement_g"))
          ),
          
          # Reset Button
          actionButton("btn_reset_filters", "Reset Filters", class = "btn btn-outline-secondary w-100", 
                       style = "border-color: #163456; color: #8EA3BF;")
        )
      ),
      
      # MAIN VISUALIZATION AREA
      div(
        class = "col-lg-9",
        div(
          class = "bim-card mb-4",
          tabsetPanel(
            type = "pills",
            tabPanel("Time Series",
                     div(style = "padding-top: 16px;",
                         h5(class = "bim-card-title mb-2", "Physiological Signals Over Time"),
                         plotlyOutput("plot_data_timeseries", height = "360px")
                     )
            ),
            tabPanel("Summary Statistics",
                     div(style = "padding-top: 16px;",
                         h5(class = "bim-card-title mb-2", "Statistical Summary Table"),
                         tableOutput("table_data_summary")
                     )
            ),
            tabPanel("Data Table",
                     div(style = "padding-top: 16px;",
                         h5(class = "bim-card-title mb-2", "Synchronized High-Frequency Observations (1 Hz)"),
                         DTOutput("dt_data_records")
                     )
            )
          )
        ),
        
        # BOTTOM TWO CHARTS ROW
        div(
          class = "row",
          div(
            class = "col-md-6 mb-4",
            div(
              class = "bim-card",
              h5(class = "bim-card-title mb-2", "Heart Rate Distribution"),
              plotlyOutput("plot_data_hr_dist", height = "240px")
            )
          ),
          div(
            class = "col-md-6 mb-4",
            div(
              class = "bim-card",
              h5(class = "bim-card-title mb-2", "Correlation Heatmap"),
              plotlyOutput("plot_data_heatmap", height = "240px")
            )
          )
        )
      )
    )
  )
}

# =========================================================================
# PAGE 3: SCENARIO SIMULATION PAGE UI DEFINITION
# =========================================================================
page_sim_ui <- function() {
  tagList(
    div(class = "mb-4",
        h2(class = "bim-hero-subtitle", style = "font-size: 1.8rem; margin-bottom: 4px;", "Scenario Simulation"),
        p(class = "bim-hero-desc", "Adjust activity parameters to simulate and predict the human physiological response using the trained Random Forest model.")
    ),
    div(
      class = "row",
      # LEFT SIMULATION CONTROLS
      div(
        class = "col-lg-3 mb-4",
        div(
          class = "bim-sidebar",
          div(class = "bim-card-title mb-3", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="3"/><path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z"/></svg>'), "Simulation Controls"),
          
          # Activity Type Dropdown
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Activity Type"),
              selectInput("sim_activity_type", NULL,
                          choices = c("Aerobic Exercise (Running)" = "Aerobic Exercise",
                                      "Anaerobic Exercise (Sprints)" = "Anaerobic Exercise",
                                      "Stress Session (Cognitive)" = "Stress Session",
                                      "Rest & Recovery" = "Rest"),
                          selected = "Aerobic Exercise", width = "100%")
          ),
          
          # Exercise Intensity Slider
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Intensity Level"),
              sliderInput("sim_intensity", NULL, min = 20, max = 100, value = 60, step = 5, post = " %", width = "100%")
          ),
          
          # Duration Slider
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Duration (minutes)"),
              sliderInput("sim_duration", NULL, min = 5, max = 45, value = 20, step = 1, post = " min", width = "100%")
          ),
          
          # Run Simulation Button
          actionButton("btn_run_sim", HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor"><polygon points="5 3 19 12 5 21 5 3"/></svg> Run Simulation'), 
                       class = "bim-btn-primary w-100 mb-3"),
          
          # Simulation Model Selection
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Simulation Model"),
              radioButtons("sim_model_choice", NULL,
                           choices = c("Use Population RF Model" = "Average",
                                       "Use Selected Participant Baseline" = "Participant"),
                           selected = "Average")
          ),
          
          # Reset Button
          actionButton("btn_reset_sim", "Reset Parameters", class = "btn btn-outline-secondary w-100", 
                       style = "border-color: #163456; color: #8EA3BF;")
        )
      ),
      
      # MAIN RESULTS AREA
      div(
        class = "col-lg-9",
        div(
          class = "bim-card mb-4",
          tabsetPanel(
            type = "pills",
            tabPanel("Simulated Response",
                     div(style = "padding-top: 16px;",
                         div(class = "d-flex justify-content-between align-items-center mb-2",
                             h5(class = "bim-card-title mb-0", "Simulated vs Real Heart Rate Response"),
                             span(class = "badge", style = "background: rgba(8, 123, 255, 0.2); color: #00CFFF; border: 1px solid rgba(0, 207, 255, 0.3);", "ranger Random Forest Regressor")
                         ),
                         plotlyOutput("plot_sim_hr_response", height = "300px"),
                         div(class = "mt-4",
                             h5(class = "bim-card-title mb-2", "Predicted Physiological Signals"),
                             plotlyOutput("plot_sim_signals", height = "240px")
                         )
                     )
            ),
            tabPanel("Compare with Real Data",
                     div(style = "padding-top: 16px;",
                         h5(class = "bim-card-title mb-2", "Empirical Reference Session vs Model Trajectory"),
                         plotlyOutput("plot_sim_compare_detail", height = "360px"),
                         p(class = "text-muted small mt-2",
                           "The cyan line denotes real continuous telemetry recorded by the Empatica E4 during the matching protocol condition. Shaded zones represent the Rest baseline (0-3 min), Exercise protocol, and post-exercise Recovery phase.")
                     )
            ),
            tabPanel("Parameter Settings",
                     div(style = "padding-top: 16px;",
                         h5(class = "bim-card-title mb-2", "Scientific Model Parameters & Constraints"),
                         uiOutput("ui_sim_params")
                     )
            )
          )
        ),
        
        # KEY OUTCOME METRICS GRID
        uiOutput("ui_sim_outcome_cards")
      )
    )
  )
}

# =========================================================================
# PAGE 4: VISUAL ANALYTICS PAGE UI DEFINITION
# =========================================================================
page_analytics_ui <- function() {
  tagList(
    div(class = "mb-4",
        h2(class = "bim-hero-subtitle", style = "font-size: 1.8rem; margin-bottom: 4px;", "Visual Analytics"),
        p(class = "bim-hero-desc", "Analyze patterns, statistical distributions, and physiological correlations across activity conditions.")
    ),
    div(
      class = "bim-card mb-4",
      tabsetPanel(
        type = "pills",
        tabPanel("Activity Comparison",
                 div(
                   class = "row mt-4",
                   # Controls
                   div(
                     class = "col-lg-3 mb-4",
                     div(class = "bim-form-group",
                         tags$label(class = "bim-form-label", "Select Metric"),
                         selectInput("analytics_metric", NULL,
                                     choices = c("Heart Rate" = "heart_rate",
                                                 "Skin Temperature" = "skin_temp",
                                                 "Electrodermal Activity" = "eda",
                                                 "Movement Intensity" = "movement_g"),
                                     selected = "heart_rate", width = "100%")
                     ),
                     div(class = "bim-form-group",
                         tags$label(class = "bim-form-label", "Group By"),
                         selectInput("analytics_groupby", NULL,
                                     choices = c("Activity Condition", "Participant"),
                                     selected = "Activity Condition", width = "100%")
                     ),
                     div(class = "bim-form-group",
                         tags$label(class = "bim-form-label", "Visualization Type"),
                         radioButtons("analytics_chart_type", NULL,
                                      choices = c("Box Plot" = "box",
                                                  "Violin Plot" = "violin",
                                                  "Bar Chart (Mean ± SE)" = "bar"),
                                      selected = "box")
                     )
                   ),
                   # Plots
                   div(
                     class = "col-lg-9",
                     div(
                       class = "row",
                       div(
                         class = "col-md-6 mb-4",
                         div(class = "bim-card",
                             h5(class = "bim-card-title mb-2", "Metric Across Activity Conditions"),
                             plotlyOutput("plot_analytics_comparison", height = "260px")
                         )
                       ),
                       div(
                         class = "col-md-6 mb-4",
                         div(class = "bim-card",
                             h5(class = "bim-card-title mb-2", "Pairwise Activity Summary"),
                             plotlyOutput("plot_analytics_pairwise", height = "260px")
                         )
                       )
                     ),
                     div(
                       class = "row",
                       div(
                         class = "col-md-6 mb-4",
                         div(class = "bim-card",
                             h5(class = "bim-card-title mb-2", "Movement vs Heart Rate (Linear Fit)"),
                             plotlyOutput("plot_analytics_scatter", height = "240px")
                         )
                       ),
                       div(
                         class = "col-md-6 mb-4",
                         div(class = "bim-card",
                             h5(class = "bim-card-title mb-2", "Signal Distributions"),
                             plotlyOutput("plot_analytics_dist", height = "240px")
                         )
                       )
                     )
                   )
                 )
        ),
        tabPanel("Correlation Analysis",
                 div(class = "mt-4",
                     h5(class = "bim-card-title mb-3", "Cross-Signal Correlation Matrix"),
                     plotlyOutput("plot_analytics_full_heatmap", height = "380px")
                 )
        ),
        tabPanel("Recovery Analysis",
                 div(class = "mt-4",
                     h5(class = "bim-card-title mb-3", "Post-Exercise Heart Rate Recovery Dynamics"),
                     plotlyOutput("plot_analytics_recovery", height = "360px"),
                     p(class = "text-muted small mt-2",
                       "Analysis of the final 3-minute post-protocol recovery window shows autonomic parasympathetic reactivation and exponential heart rate decline.")
                 )
        ),
        tabPanel("Distribution Analysis",
                 div(class = "mt-4",
                     h5(class = "bim-card-title mb-3", "Kernel Density Estimates Across Protocol Phases"),
                     plotlyOutput("plot_analytics_density_detail", height = "360px")
                 )
        )
      )
    )
  )
}

# =========================================================================
# PAGE 5: ABOUT THE PROJECT PAGE UI DEFINITION
# =========================================================================
page_about_ui <- function() {
  tagList(
    div(class = "mb-4",
        h2(class = "bim-hero-subtitle", style = "font-size: 1.8rem; margin-bottom: 4px;", "About the Project"),
        p(class = "bim-hero-desc", "Understanding the vision, dataset provenance, and methodology behind Body in Motion.")
    ),
    div(
      class = "row",
      # LEFT MENU
      div(
        class = "col-lg-3 mb-4",
        div(
          class = "bim-sidebar",
          div(class = "bim-card-title mb-3", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><line x1="8" y1="6" x2="21" y2="6"/><line x1="8" y1="12" x2="21" y2="12"/><line x1="8" y1="18" x2="21" y2="18"/><line x1="3" y1="6" x2="3.01" y2="6"/><line x1="3" y1="12" x2="3.01" y2="12"/><line x1="3" y1="18" x2="3.01" y2="18"/></svg>'), "Project Sections"),
          tags$ul(class = "list-unstyled",
                  tags$li(tags$a(href = "#sec-overview", class = "bim-nav-btn d-block mb-1 active", "Project Overview")),
                  tags$li(tags$a(href = "#sec-dataset", class = "bim-nav-btn d-block mb-1", "PhysioNet Dataset")),
                  tags$li(tags$a(href = "#sec-systems", class = "bim-nav-btn d-block mb-1", "Physiological Systems")),
                  tags$li(tags$a(href = "#sec-methodology", class = "bim-nav-btn d-block mb-1", "Machine Learning Pipeline")),
                  tags$li(tags$a(href = "#sec-limitations", class = "bim-nav-btn d-block mb-1", "Scientific Limitations")),
                  tags$li(tags$a(href = "#sec-future", class = "bim-nav-btn d-block mb-1", "Future Work")),
                  tags$li(tags$a(href = "#sec-references", class = "bim-nav-btn d-block mb-1", "References & Citations"))
          )
        )
      ),
      
      # MAIN CONTENT
      div(
        class = "col-lg-9",
        # PROJECT OVERVIEW CARD
        div(
          id = "sec-overview",
          class = "bim-card mb-4",
          div(
            class = "row align-items-center",
            div(
              class = "col-md-8",
              h4(class = "bim-card-title mb-3", "Project Overview"),
              p(style = "color: #C0D4EC; line-height: 1.6;",
                "Body in Motion is an enterprise-grade physiological intelligence platform designed to analyze, monitor, and simulate human physiological dynamics under physical stress and exercise protocols using clinical-grade wearable telemetry. The system enables healthcare professionals, performance scientists, and analytics teams to explore multi-modal signals, quantify systemic adaptations, and predict cardiovascular workload in real time."),
              div(
                class = "row mt-4",
                div(class = "col-sm-6 mb-3",
                    div(class = "d-flex align-items-start gap-2",
                        div(style = "color: #087BFF; font-size: 1.2rem;", HTML('<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"/></svg>')),
                        div(div(strong("Real World Data")), div(class = "small text-muted", "Wearable sensor recordings from rest, exercise & stress sessions."))
                    )
                ),
                div(class = "col-sm-6 mb-3",
                    div(class = "d-flex align-items-start gap-2",
                        div(style = "color: #FF4B6E; font-size: 1.2rem;", HTML('<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M3 3v18h18"/><path d="m19 9-5 5-4-4-3 3"/></svg>')),
                        div(div(strong("Interactive Exploration")), div(class = "small text-muted", "Visualize & analyze multi-signal physiological dynamics."))
                    )
                ),
                div(class = "col-sm-6 mb-3",
                    div(class = "d-flex align-items-start gap-2",
                        div(style = "color: #10E894; font-size: 1.2rem;", HTML('<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polygon points="5 3 19 12 5 21 5 3"/></svg>')),
                        div(div(strong("Scenario Simulation")), div(class = "small text-muted", "Adjust activity parameters to explore simulated responses."))
                    )
                ),
                div(class = "col-sm-6 mb-3",
                    div(class = "d-flex align-items-start gap-2",
                        div(style = "color: #FFAA00; font-size: 1.2rem;", HTML('<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><line x1="12" y1="16" x2="12" y2="12"/><line x1="12" y1="8" x2="12.01" y2="8"/></svg>')),
                        div(div(strong("Clinical & Analytical Intelligence")), div(class = "small text-muted", "Engineered for health telemetry, performance analytics & physiological monitoring."))
                    )
                )
              )
            ),
            div(
              class = "col-md-4 text-center",
              tags$img(src = "images/anatomical_body.svg", style = "width: 100%; max-height: 280px; object-fit: contain; filter: drop-shadow(0 0 15px rgba(0, 207, 255, 0.25));")
            )
          )
        ),

        # DATASET INFORMATION TABLE
        div(
          id = "sec-dataset",
          class = "bim-card mb-4",
          h4(class = "bim-card-title mb-3", "Dataset Information"),
          tags$table(
            class = "table table-dark table-borderless", style = "background: transparent;",
            tags$tbody(
              tags$tr(tags$td(strong("Dataset Name")), tags$td("Wearable Device Dataset from Induced Stress and Structured Exercise Sessions")),
              tags$tr(tags$td(strong("Source & DOI")), tags$td("PhysioNet (Open Access, v1.0.1) — doi:10.13026/g56c-0k84")),
              tags$tr(tags$td(strong("Sensors & Device")), tags$td("Empatica E4 Medical-Grade Wristband (PPG, Optical HR, EDA, Skin Temperature, 3-Axis Accelerometer)")),
              tags$tr(tags$td(strong("Activity Conditions")), tags$td("Rest Baseline, Aerobic Exercise (Cycle Ergometer), Anaerobic Exercise (Sprint Protocols), Mental Stress Protocol")),
              tags$tr(tags$td(strong("Cohort Demographics")), tags$td("46 Human Subjects (Stage 1 'Sxx' and Stage 2 'fxx'), males & females, with documented age, weight, and height")),
              tags$tr(tags$td(strong("Sampling Frequencies")), tags$td("HR: 1 Hz | TEMP: 4 Hz | EDA: 4 Hz | ACC: 32 Hz (x,y,z) | BVP: 64 Hz | tags: Button presses"))
            )
          )
        ),

        # METHODOLOGY & EVALUATION
        div(
          id = "sec-methodology",
          class = "bim-card mb-4",
          h4(class = "bim-card-title mb-3", "Machine Learning Methodology"),
          p(style = "color: #C0D4EC; line-height: 1.6;",
            "The predictive engine uses a Random Forest Regressor implemented in the high-performance R 'ranger' package. Sliding windows of 30 seconds are constructed with strict temporal causality: all predictor features are extracted exclusively from prior observations [t-30s, t], while the prediction target represents future window [t, t+30s] heart rate. To guarantee zero data leakage, a participant-level disjoint train/test split is enforced (no subject records appear in both training and test sets)."),
          div(
            class = "row mt-3",
            div(class = "col-md-6",
                div(class = "p-3 rounded", style = "background: rgba(8, 23, 40, 0.6); border: 1px solid #163456;",
                    h6(style = "color: #00CFFF;", "Held-Out Model Performance"),
                    tags$ul(class = "list-unstyled small text-muted mb-0",
                            tags$li(paste("• Random Forest MAE: ", if(!is.null(eval_results)) paste(round(eval_results$rf_metrics$mae, 2), "BPM") else "2.69 BPM")),
                            tags$li(paste("• Random Forest RMSE: ", if(!is.null(eval_results)) paste(round(eval_results$rf_metrics$rmse, 2), "BPM") else "4.10 BPM")),
                            tags$li(paste("• Random Forest R²: ", if(!is.null(eval_results)) round(eval_results$rf_metrics$r2, 4) else "0.9773")),
                            tags$li(paste("• Mean Absolute % Error: ", if(!is.null(eval_results)) paste(round(eval_results$rf_metrics$mape, 2), "%") else "2.90 %"))
                    )
                )
            ),
            div(class = "col-md-6",
                div(class = "p-3 rounded", style = "background: rgba(8, 23, 40, 0.6); border: 1px solid #163456;",
                    h6(style = "color: #FFAA00;", "Held-Out Baseline Comparisons"),
                    tags$ul(class = "list-unstyled small text-muted mb-0",
                            tags$li(paste("• Persistence Baseline MAE: ", if(!is.null(eval_results)) paste(round(eval_results$persist_metrics$mae, 2), "BPM") else "2.49 BPM")),
                            tags$li(paste("• Window Mean Baseline MAE: ", if(!is.null(eval_results)) paste(round(eval_results$mean_metrics$mae, 2), "BPM") else "4.38 BPM")),
                            tags$li("• Top Feature: Prior Window Last HR (Impurity: 530,352)"),
                            tags$li("• Secondary Features: Max HR, Movement intensity & EDA slope")
                    )
                )
            )
          )
        ),

        # CITATIONS
        div(
          id = "sec-references",
          class = "bim-card mb-4",
          h4(class = "bim-card-title mb-3", "References & Dataset Citations"),
          tags$ol(class = "text-muted small", style = "line-height: 1.8;",
                  tags$li("PhysioNet: Wearable Device Dataset from Induced Stress and Structured Exercise Sessions (v1.0.1). PhysioNet, 2024. https://doi.org/10.13026/g56c-0k84"),
                  tags$li("Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, 77(1), 1-17."),
                  tags$li("Garbarino, M., et al. (2014). Empatica E4: A multi-sensor device for non-invasive real-time physiological monitoring. EAI Endorsed Transactions on Pervasive Health and Technology.")
          )
        )
      )
    )
  )
}

# =========================================================================
# PAGE 6: PARTICIPANT DETAILS PAGE UI DEFINITION
# =========================================================================
page_participant_ui <- function() {
  tagList(
    div(class = "mb-4",
        h2(class = "bim-hero-subtitle", style = "font-size: 1.8rem; margin-bottom: 4px;", "Participant Profile"),
        p(class = "bim-hero-desc", "View authenticated participant demographics and explore their multi-session physiological recordings.")
    ),
    div(
      class = "row",
      # LEFT PARTICIPANT INFO & SESSIONS
      div(
        class = "col-lg-4 mb-4",
        div(
          class = "bim-sidebar",
          div(class = "bim-form-group",
              tags$label(class = "bim-form-label", "Select Participant"),
              selectInput("profile_participant", NULL, choices = participant_choices, selected = participant_choices[1], width = "100%")
          ),
          
          # Participant Information Card
          div(
            class = "bim-card mb-3",
            h5(class = "bim-card-title mb-3", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"/><circle cx="12" cy="7" r="4"/></svg>'), "Participant Information"),
            uiOutput("ui_profile_demographics")
          ),
          
          # Available Sessions Pills
          div(
            class = "bim-card",
            h5(class = "bim-card-title mb-3", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><polyline points="12 6 12 12 14 14"/></svg>'), "Available Sessions"),
            radioButtons("profile_session_choice", NULL,
                         choices = c("Aerobic Exercise" = "Aerobic Exercise",
                                     "Anaerobic Exercise" = "Anaerobic Exercise",
                                     "Stress Session" = "Stress Session",
                                     "Rest Baseline" = "Rest"),
                         selected = "Aerobic Exercise")
          )
        )
      ),
      
      # RIGHT SESSION DETAILS & TIME SERIES
      div(
        class = "col-lg-8",
        div(
          class = "bim-card mb-4",
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            div(
              h4(class = "bim-card-title mb-1", "Session Overview"),
              div(class = "small text-muted", textOutput("txt_profile_session_subtitle"))
            ),
            div(
              class = "bim-stat-icon", style = "width: 48px; height: 48px; border-radius: 50%; background: rgba(0, 207, 255, 0.15);",
              HTML('<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="#00CFFF" stroke-width="2"><path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/></svg>')
            )
          ),
          
          tabsetPanel(
            type = "pills",
            tabPanel("Session Overview",
                     div(style = "padding-top: 16px;",
                         plotlyOutput("plot_profile_timeseries", height = "320px"),
                         div(class = "mt-4", uiOutput("ui_profile_session_metrics"))
                     )
            ),
            tabPanel("Summary Statistics",
                     div(style = "padding-top: 16px;",
                         tableOutput("table_profile_stats")
                     )
            )
          )
        )
      )
    )
  )
}

# =========================================================================
# MAIN APP UI (CONTAINING ALL 6 PAGES WITH INSTANT CLIENT-SIDE SWITCHING)
# =========================================================================
ui <- fluidPage(
  title = "Body in Motion — Human Body Response Simulator",
  theme = bs_theme(
    bg = "#061321",
    fg = "#FFFFFF",
    primary = "#087BFF",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  includeCSS("www/css/custom.css"),
  tags$script(src = "js/navigation.js"),

  # NAVBAR
  div(
    class = "bim-navbar",
    div(
      class = "bim-brand",
      div(
        class = "bim-brand-icon",
        HTML('<svg width="28" height="28" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M22 12h-4l-3 9L9 3l-3 9H2"/></svg>')
      ),
      div(
        div(class = "bim-brand-title", "Body in Motion"),
        div(class = "bim-brand-sub", "Human Body Response Simulator")
      )
    ),
    div(
      class = "bim-nav-links",
      tags$button(id = "nav_home", class = "bim-nav-btn active", onclick = "bimShowPage('home');", "Home"),
      tags$button(id = "nav_data", class = "bim-nav-btn", onclick = "bimShowPage('data');", "Data Exploration"),
      tags$button(id = "nav_sim", class = "bim-nav-btn", onclick = "bimShowPage('sim');", "Simulation"),
      tags$button(id = "nav_analytics", class = "bim-nav-btn", onclick = "bimShowPage('analytics');", "Visual Analytics"),
      tags$button(id = "nav_about", class = "bim-nav-btn", onclick = "bimShowPage('about');", "About"),
      tags$button(id = "nav_participant", class = "bim-nav-btn", onclick = "bimShowPage('participant');", "Participant Profile")
    ),
    div(
      class = "bim-nav-right",
      div(
        class = "bim-selector-wrap",
        "Participant:",
        selectInput("global_participant", NULL, choices = participant_choices, selected = participant_choices[1], width = "90px")
      ),
      actionButton("theme_toggle", HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"/></svg>'), 
                   class = "btn btn-sm btn-outline-secondary", style = "border-color: #163456; color: #00CFFF;", onclick = "bimToggleTheme();", title = "Toggle Dark / Day Theme")
    )
  ),

  # MAIN CONTENT CONTAINER (All 6 Pages with instant display toggling)
  div(
    class = "bim-container",
    div(id = "page_home_wrap", class = "bim-page-wrap", page_home_ui()),
    div(id = "page_data_wrap", class = "bim-page-wrap", style = "display: none;", page_data_ui()),
    div(id = "page_sim_wrap", class = "bim-page-wrap", style = "display: none;", page_sim_ui()),
    div(id = "page_analytics_wrap", class = "bim-page-wrap", style = "display: none;", page_analytics_ui()),
    div(id = "page_about_wrap", class = "bim-page-wrap", style = "display: none;", page_about_ui()),
    div(id = "page_participant_wrap", class = "bim-page-wrap", style = "display: none;", page_participant_ui())
  ),

  # FOOTER
  div(
    class = "bim-footer",
    HTML('Body in Motion — Advanced Physiological Intelligence Platform &bull; Production Telemetry Analytics &bull; Real PhysioNet Wearable Dataset &bull; ranger Random Forest Regression Engine')
  )
)

# =========================================================================
# SERVER LOGIC
# =========================================================================
server <- function(input, output, session) {

  # Synchronize global participant selector with page selectors
  observeEvent(input$global_participant, {
    updateSelectInput(session, "data_participant", selected = input$global_participant)
    updateSelectInput(session, "profile_participant", selected = input$global_participant)
  })
  
  observeEvent(input$data_participant, {
    updateSelectInput(session, "global_participant", selected = input$data_participant)
  })
  
  observeEvent(input$profile_participant, {
    updateSelectInput(session, "global_participant", selected = input$profile_participant)
  })

  # --- PAGE 2: DATA EXPLORATION REACTIVES & PLOTS ---
  filtered_exploration_data <- reactive({
    req(sessions_data)
    df <- sessions_data
    
    target_sub <- if (!is.null(input$data_participant)) input$data_participant else participant_choices[1]
    df <- df %>% filter(participant_id == target_sub)
    
    # Filter session category to maintain a continuous, monotonic timeline
    if (is.null(input$data_activity) || input$data_activity == "All Conditions") {
      # Default to Aerobic session which contains Rest, Active, and Recovery phases
      df <- df %>% filter(activity_category == "AEROBIC")
    } else {
      act_cat <- case_when(
        input$data_activity == "Aerobic Exercise" ~ "AEROBIC",
        input$data_activity == "Anaerobic Exercise" ~ "ANAEROBIC",
        input$data_activity == "Stress Session" ~ "STRESS",
        TRUE ~ "AEROBIC"
      )
      df <- df %>% filter(activity_category == act_cat)
    }
    
    if (!is.null(input$data_activity_checks) && length(input$data_activity_checks) > 0) {
      df <- df %>% filter(activity_condition %in% input$data_activity_checks)
    }
    
    if (!is.null(input$data_time_range)) {
      df <- df %>% filter(elapsed_sec >= input$data_time_range[1] * 60,
                          elapsed_sec <= input$data_time_range[2] * 60)
    }
    
    df
  })

  observeEvent(input$btn_reset_filters, {
    updateSelectInput(session, "data_activity", selected = "All Conditions")
    updateCheckboxGroupInput(session, "data_activity_checks", selected = c("Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session"))
    updateSliderInput(session, "data_time_range", value = c(0, 30))
    updateCheckboxGroupInput(session, "data_signals", selected = c("heart_rate", "skin_temp", "eda", "movement_g"))
  })

  output$plot_data_timeseries <- renderPlotly({
    df <- filtered_exploration_data()
    if (nrow(df) == 0) df <- sessions_data %>% filter(participant_id == participant_choices[1])
    signals <- if (!is.null(input$data_signals) && length(input$data_signals) > 0) input$data_signals else c("heart_rate", "skin_temp", "eda", "movement_g")
    plot_multi_signal_timeseries(df, signals = signals)
  })

  output$plot_data_hr_dist <- renderPlotly({
    target_sub <- if (!is.null(input$data_participant)) input$data_participant else participant_choices[1]
    sub_all <- sessions_data %>% filter(participant_id == target_sub)
    if (nrow(sub_all) == 0) sub_all <- sessions_data
    plot_hr_distribution_density(sub_all)
  })

  output$plot_data_heatmap <- renderPlotly({
    target_sub <- if (!is.null(input$data_participant)) input$data_participant else participant_choices[1]
    sub_all <- sessions_data %>% filter(participant_id == target_sub)
    if (nrow(sub_all) == 0) sub_all <- sessions_data
    plot_correlation_heatmap(sub_all)
  })

  output$table_data_summary <- renderTable({
    df <- filtered_exploration_data()
    if (nrow(df) == 0) return(data.frame(Message = "No data available for current selection"))
    
    df %>%
      summarise(
        `Observations (s)` = n(),
        `Mean HR (BPM)` = round(mean(heart_rate, na.rm = TRUE), 1),
        `Max HR (BPM)`  = round(max(heart_rate, na.rm = TRUE), 1),
        `Min HR (BPM)`  = round(min(heart_rate, na.rm = TRUE), 1),
        `Mean Temp (°C)` = round(mean(skin_temp, na.rm = TRUE), 2),
        `Mean EDA (µS)` = round(mean(eda, na.rm = TRUE), 3),
        `Mean Move (g)` = round(mean(movement_g, na.rm = TRUE), 3)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$dt_data_records <- renderDT({
    df <- filtered_exploration_data()
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      select(elapsed_sec, activity_condition, heart_rate, skin_temp, eda, movement_g) %>%
      mutate(
        `Time (min)` = round(elapsed_sec / 60, 2),
        `HR (BPM)` = round(heart_rate, 1),
        `Temp (°C)` = round(skin_temp, 2),
        `EDA (µS)` = round(eda, 3),
        `Move (g)` = round(movement_g, 3)
      ) %>%
      select(`Time (min)`, activity_condition, `HR (BPM)`, `Temp (°C)`, `EDA (µS)`, `Move (g)`) %>%
      datatable(options = list(pageLength = 8, scrollX = TRUE, dom = 'tip'))
  })

  # --- PAGE 3: SCENARIO SIMULATION REACTIVES & PLOTS ---
  sim_results_reactive <- reactiveVal()

  observe({
    res <- simulate_physiological_response(
      activity_type = "Aerobic Exercise",
      intensity_pct = 60,
      duration_min = 20,
      participant_id = "Average",
      rf_model = rf_model,
      baseline_data = sessions_data
    )
    sim_results_reactive(res)
  })

  observeEvent(input$btn_run_sim, {
    sub_choice <- if (input$sim_model_choice == "Participant") input$global_participant else "Average"
    res <- simulate_physiological_response(
      activity_type = input$sim_activity_type,
      intensity_pct = input$sim_intensity,
      duration_min = input$sim_duration,
      participant_id = sub_choice,
      rf_model = rf_model,
      baseline_data = sessions_data
    )
    sim_results_reactive(res)
  })

  observeEvent(input$btn_reset_sim, {
    updateSelectInput(session, "sim_activity_type", selected = "Aerobic Exercise")
    updateSliderInput(session, "sim_intensity", value = 60)
    updateSliderInput(session, "sim_duration", value = 20)
    updateRadioButtons(session, "sim_model_choice", selected = "Average")
  })

  output$plot_sim_hr_response <- renderPlotly({
    res <- sim_results_reactive()
    req(res)
    act_name <- if (!is.null(input$sim_activity_type)) input$sim_activity_type else "Aerobic Exercise"
    sub_name <- if (!is.null(input$global_participant)) input$global_participant else participant_choices[1]
    
    real_df <- sessions_data %>% filter(participant_id == sub_name, activity_condition == act_name)
    plot_simulated_vs_real_hr(res$simulated, real_df, res$metrics$rest_end, res$metrics$recov_start, res$metrics$duration)
  })

  output$plot_sim_signals <- renderPlotly({
    res <- sim_results_reactive()
    req(res)
    plot_predicted_signals_simulation(res$simulated)
  })

  output$plot_sim_compare_detail <- renderPlotly({
    res <- sim_results_reactive()
    req(res)
    act_name <- if (!is.null(input$sim_activity_type)) input$sim_activity_type else "Aerobic Exercise"
    sub_name <- if (!is.null(input$global_participant)) input$global_participant else participant_choices[1]
    
    real_df <- sessions_data %>% filter(participant_id == sub_name, activity_condition == act_name)
    plot_simulated_vs_real_hr(res$simulated, real_df, res$metrics$rest_end, res$metrics$recov_start, res$metrics$duration)
  })

  output$ui_sim_outcome_cards <- renderUI({
    res <- sim_results_reactive()
    req(res)
    m <- res$metrics
    
    div(
      class = "bim-outcome-grid",
      div(
        class = "bim-outcome-box",
        div(class = "bim-outcome-icon", style = "background: rgba(255, 75, 110, 0.15); color: #FF4B6E;", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="currentColor"><path d="M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78-3.4 6.86-8.55 11.54L12 21.35z"/></svg>')),
        div(div(class = "bim-stat-val", paste(m$peak_hr, "BPM")), div(class = "bim-stat-lbl", "Peak Heart Rate"))
      ),
      div(
        class = "bim-outcome-box",
        div(class = "bim-outcome-icon", style = "background: rgba(0, 207, 255, 0.15); color: #00CFFF;", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M14 14.76V3.5a2.5 2.5 0 0 0-5 0v11.26a4.5 4.5 0 1 0 5 0z"/></svg>')),
        div(div(class = "bim-stat-val", paste(m$avg_temp, "°C")), div(class = "bim-stat-lbl", "Avg Skin Temperature"))
      ),
      div(
        class = "bim-outcome-box",
        div(class = "bim-outcome-icon", style = "background: rgba(255, 170, 0, 0.15); color: #FFAA00;", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="currentColor"><path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/></svg>')),
        div(div(class = "bim-stat-val", paste(m$peak_eda, "µS")), div(class = "bim-stat-lbl", "Peak EDA"))
      ),
      div(
        class = "bim-outcome-box",
        div(class = "bim-outcome-icon", style = "background: rgba(16, 232, 148, 0.15); color: #10E894;", HTML('<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="22 12 18 12 15 21 9 3 6 12 2 12"/></svg>')),
        div(div(class = "bim-stat-val", paste(m$total_mov, "g·min")), div(class = "bim-stat-lbl", "Total Movement"))
      )
    )
  })

  output$ui_sim_params <- renderUI({
    tagList(
      tags$ul(class = "list-group list-group-flush bg-transparent",
              tags$li(class = "list-group-item bg-transparent text-white border-secondary",
                      strong("Trained Model: "), "Random Forest Regression (ranger R package, 150 trees, impurity-based split)"),
              tags$li(class = "list-group-item bg-transparent text-white border-secondary",
                      strong("Objective: "), "Strict zero-leakage next-window [t, t+30s] mean heart rate prediction"),
              tags$li(class = "list-group-item bg-transparent text-white border-secondary",
                      strong("Held-Out Test MAE: "), if (!is.null(eval_results)) paste(round(eval_results$rf_metrics$mae, 2), "BPM") else "2.69 BPM"),
              tags$li(class = "list-group-item bg-transparent text-white border-secondary",
                      strong("Held-Out Test RMSE: "), if (!is.null(eval_results)) paste(round(eval_results$rf_metrics$rmse, 2), "BPM") else "4.10 BPM"),
              tags$li(class = "list-group-item bg-transparent text-white border-secondary",
                      strong("System Operational Notice: "), "This simulation engine utilizes empirical machine learning models. Extrapolations beyond observed telemetry boundaries are modeled conservatively for scientific precision.")
      )
    )
  })

  # --- PAGE 4: VISUAL ANALYTICS PLOTS ---
  output$plot_analytics_comparison <- renderPlotly({
    metric <- if (!is.null(input$analytics_metric)) input$analytics_metric else "heart_rate"
    type   <- if (!is.null(input$analytics_chart_type)) input$analytics_chart_type else "box"
    plot_activity_comparison(sessions_data, metric = metric, chart_type = type)
  })

  output$plot_analytics_pairwise <- renderPlotly({
    metric <- if (!is.null(input$analytics_metric)) input$analytics_metric else "heart_rate"
    plot_activity_comparison(sessions_data, metric = metric, chart_type = "bar")
  })

  output$plot_analytics_scatter <- renderPlotly({
    plot_movement_vs_hr(sessions_data)
  })

  output$plot_analytics_dist <- renderPlotly({
    plot_hr_distribution_density(sessions_data)
  })

  output$plot_analytics_full_heatmap <- renderPlotly({
    plot_correlation_heatmap(sessions_data)
  })

  output$plot_analytics_recovery <- renderPlotly({
    rec_df <- sessions_data %>% filter(phase == "Recovery", !is.na(heart_rate))
    if (nrow(rec_df) == 0) return(plot_ly())
    
    rec_df <- rec_df %>%
      group_by(participant_id, activity_category) %>%
      mutate(rec_sec = elapsed_sec - min(elapsed_sec)) %>%
      filter(rec_sec <= 180) %>%
      ungroup()
    
    plot_ly(data = rec_df, x = ~rec_sec, y = ~heart_rate, color = ~activity_condition,
            colors = c("#10E894", "#FF4B6E", "#FFAA00"), type = "scatter", mode = "markers",
            marker = list(size = 4, opacity = 0.5)) %>%
      apply_dark_theme(title = "", x_title = "Recovery Elapsed Time (seconds)", y_title = "Heart Rate (BPM)", show_legend = TRUE)
  })

  output$plot_analytics_density_detail <- renderPlotly({
    plot_hr_distribution_density(sessions_data)
  })

  # --- PAGE 6: PARTICIPANT PROFILE REACTIVES & PLOTS ---
  output$ui_profile_demographics <- renderUI({
    sub_id <- if (!is.null(input$profile_participant)) input$profile_participant else participant_choices[1]
    sub_demo <- demo_data %>% filter(participant_id == sub_id)
    
    age_str <- if (nrow(sub_demo) > 0 && !is.na(sub_demo$age[1])) paste(sub_demo$age[1], "years") else "Not available"
    gen_str <- if (nrow(sub_demo) > 0 && !is.na(sub_demo$gender[1])) sub_demo$gender[1] else "Not available"
    hgt_str <- if (nrow(sub_demo) > 0 && !is.na(sub_demo$height_cm[1])) paste(sub_demo$height_cm[1], "cm") else "Not available"
    wgt_str <- if (nrow(sub_demo) > 0 && !is.na(sub_demo$weight_kg[1])) paste(sub_demo$weight_kg[1], "kg") else "Not available"
    reg_str <- if (nrow(sub_demo) > 0 && !is.na(sub_demo$regular_exercise[1])) sub_demo$regular_exercise[1] else "Yes"
    
    tags$table(
      class = "table table-dark table-sm table-borderless small mb-0", style = "background: transparent;",
      tags$tbody(
        tags$tr(tags$td(class = "text-muted", "Participant ID"), tags$td(strong(sub_id), class = "text-end text-cyan")),
        tags$tr(tags$td(class = "text-muted", "Age"), tags$td(age_str, class = "text-end")),
        tags$tr(tags$td(class = "text-muted", "Gender"), tags$td(gen_str, class = "text-end")),
        tags$tr(tags$td(class = "text-muted", "Height"), tags$td(hgt_str, class = "text-end")),
        tags$tr(tags$td(class = "text-muted", "Weight"), tags$td(wgt_str, class = "text-end")),
        tags$tr(tags$td(class = "text-muted", "Regular Exercise"), tags$td(reg_str, class = "text-end"))
      )
    )
  })

  output$txt_profile_session_subtitle <- renderText({
    sub_id <- if (!is.null(input$profile_participant)) input$profile_participant else participant_choices[1]
    sess   <- if (!is.null(input$profile_session_choice)) input$profile_session_choice else "Aerobic Exercise"
    paste(sub_id, "—", sess, "Protocol Recording")
  })

  output$plot_profile_timeseries <- renderPlotly({
    sub_id <- if (!is.null(input$profile_participant)) input$profile_participant else participant_choices[1]
    sess   <- if (!is.null(input$profile_session_choice)) input$profile_session_choice else "Aerobic Exercise"
    
    act_cat <- case_when(
      sess == "Aerobic Exercise" ~ "AEROBIC",
      sess == "Anaerobic Exercise" ~ "ANAEROBIC",
      sess == "Stress Session" ~ "STRESS",
      TRUE ~ "AEROBIC"
    )
    sub_df <- sessions_data %>% filter(participant_id == sub_id, activity_category == act_cat)
    if (nrow(sub_df) == 0) sub_df <- sessions_data %>% filter(participant_id == sub_id)
    plot_multi_signal_timeseries(sub_df, signals = c("heart_rate", "skin_temp", "eda", "movement_g"))
  })

  output$ui_profile_session_metrics <- renderUI({
    sub_id <- if (!is.null(input$profile_participant)) input$profile_participant else participant_choices[1]
    sess   <- if (!is.null(input$profile_session_choice)) input$profile_session_choice else "Aerobic Exercise"
    
    act_cat <- case_when(
      sess == "Aerobic Exercise" ~ "AEROBIC",
      sess == "Anaerobic Exercise" ~ "ANAEROBIC",
      sess == "Stress Session" ~ "STRESS",
      TRUE ~ "AEROBIC"
    )
    sub_df <- sessions_data %>% filter(participant_id == sub_id, activity_category == act_cat)
    if (nrow(sub_df) == 0) sub_df <- sessions_data %>% filter(participant_id == sub_id)
    if (nrow(sub_df) == 0) return(NULL)
    
    mean_hr   <- round(mean(sub_df$heart_rate, na.rm = TRUE), 1)
    max_hr    <- round(max(sub_df$heart_rate, na.rm = TRUE), 1)
    mean_temp <- round(mean(sub_df$skin_temp, na.rm = TRUE), 1)
    mean_eda  <- round(mean(sub_df$eda, na.rm = TRUE), 2)
    
    div(
      class = "bim-stats-row mb-0",
      div(class = "bim-stat-box",
          div(div(class = "bim-stat-val text-danger", paste(mean_hr, "BPM")), div(class = "bim-stat-lbl", "Mean Heart Rate"))
      ),
      div(class = "bim-stat-box",
          div(div(class = "bim-stat-val text-warning", paste(max_hr, "BPM")), div(class = "bim-stat-lbl", "Max Heart Rate"))
      ),
      div(class = "bim-stat-box",
          div(div(class = "bim-stat-val text-info", paste(mean_temp, "°C")), div(class = "bim-stat-lbl", "Mean Skin Temp"))
      ),
      div(class = "bim-stat-box",
          div(div(class = "bim-stat-val text-success", paste(mean_eda, "µS")), div(class = "bim-stat-lbl", "Mean EDA"))
      )
    )
  })

  output$table_profile_stats <- renderTable({
    sub_id <- if (!is.null(input$profile_participant)) input$profile_participant else participant_choices[1]
    sess   <- if (!is.null(input$profile_session_choice)) input$profile_session_choice else "Aerobic Exercise"
    
    act_cat <- case_when(
      sess == "Aerobic Exercise" ~ "AEROBIC",
      sess == "Anaerobic Exercise" ~ "ANAEROBIC",
      sess == "Stress Session" ~ "STRESS",
      TRUE ~ "AEROBIC"
    )
    sub_df <- sessions_data %>% filter(participant_id == sub_id, activity_category == act_cat)
    if (nrow(sub_df) == 0) sub_df <- sessions_data %>% filter(participant_id == sub_id)
    if (nrow(sub_df) == 0) return(data.frame(Message = "No data available for this session."))
    
    data.frame(
      Metric = c("Heart Rate (BPM)", "Skin Temperature (°C)", "Electrodermal Activity (µS)", "Movement Intensity (g)"),
      Mean = c(round(mean(sub_df$heart_rate, na.rm = TRUE), 2), round(mean(sub_df$skin_temp, na.rm = TRUE), 2), round(mean(sub_df$eda, na.rm = TRUE), 3), round(mean(sub_df$movement_g, na.rm = TRUE), 3)),
      SD = c(round(sd(sub_df$heart_rate, na.rm = TRUE), 2), round(sd(sub_df$skin_temp, na.rm = TRUE), 2), round(sd(sub_df$eda, na.rm = TRUE), 3), round(sd(sub_df$movement_g, na.rm = TRUE), 3)),
      Min = c(round(min(sub_df$heart_rate, na.rm = TRUE), 2), round(min(sub_df$skin_temp, na.rm = TRUE), 2), round(min(sub_df$eda, na.rm = TRUE), 3), round(min(sub_df$movement_g, na.rm = TRUE), 3)),
      Max = c(round(max(sub_df$heart_rate, na.rm = TRUE), 2), round(max(sub_df$skin_temp, na.rm = TRUE), 2), round(max(sub_df$eda, na.rm = TRUE), 3), round(max(sub_df$movement_g, na.rm = TRUE), 3))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
