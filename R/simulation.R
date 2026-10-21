# R/simulation.R
# Scenario Simulation Engine and Comparison Visualizations
# Body in Motion — Human Body Response Simulator

suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
  library(ranger)
})

#' Run Physiological Scenario Simulation using Trained ML Model
simulate_physiological_response <- function(
  activity_type = "Aerobic Exercise",
  intensity_pct = 60,
  duration_min = 20,
  participant_id = "Average",
  rf_model = NULL,
  baseline_data = NULL
) {
  # 1. Validation & Range Check
  duration_min <- max(5, min(duration_min, 45))
  intensity_pct <- max(20, min(intensity_pct, 100))
  
  # Time vector in 30-second steps
  total_sec <- duration_min * 60
  time_steps <- seq(0, total_sec, by = 30)
  n_steps <- length(time_steps)
  
  # Zone boundaries (minutes)
  # Standard exercise protocol: Rest (first 3 min), Exercise, Recovery (last 3 min)
  rest_end_min <- 3.0
  recov_start_min <- max(rest_end_min + 2, duration_min - 3.0)
  
  # Baseline starting values (empirical rest averages from PhysioNet dataset)
  base_hr   <- 72.0
  base_temp <- 34.2
  base_eda  <- 0.28
  
  # If a specific participant is chosen and baseline data exists, customize starting values
  if (!is.null(baseline_data) && participant_id != "Average") {
    sub_rest <- baseline_data %>% filter(participant_id == !!participant_id, activity_condition == "Rest")
    if (nrow(sub_rest) > 10) {
      base_hr   <- mean(sub_rest$heart_rate, na.rm = TRUE)
      base_temp <- mean(sub_rest$skin_temp, na.rm = TRUE)
      base_eda  <- mean(sub_rest$eda, na.rm = TRUE)
    }
  }
  
  # Target HR delta according to activity and intensity
  intensity_factor <- intensity_pct / 100.0
  max_hr_delta <- case_when(
    grepl("Aerobic", activity_type, ignore.case = TRUE)   ~ 85.0 * intensity_factor,
    grepl("Anaerobic", activity_type, ignore.case = TRUE) ~ 105.0 * intensity_factor,
    grepl("Stress", activity_type, ignore.case = TRUE)    ~ 35.0 * intensity_factor,
    TRUE ~ 10.0 * intensity_factor
  )
  
  # Movement intensity profile (g)
  base_mv <- case_when(
    grepl("Aerobic", activity_type, ignore.case = TRUE)   ~ 0.5 + 0.6 * intensity_factor,
    grepl("Anaerobic", activity_type, ignore.case = TRUE) ~ 0.8 + 1.1 * intensity_factor,
    grepl("Stress", activity_type, ignore.case = TRUE)    ~ 0.08 + 0.05 * intensity_factor,
    TRUE ~ 0.05
  )
  
  # Generate simulated time series using autoregressive RF predictions
  sim_df <- data.frame(
    elapsed_sec = time_steps,
    time_min = time_steps / 60.0,
    zone = case_when(
      (time_steps / 60.0) <= rest_end_min ~ "Rest",
      (time_steps / 60.0) >= recov_start_min ~ "Recovery",
      TRUE ~ "Exercise"
    )
  )
  
  sim_hr <- numeric(n_steps)
  sim_temp <- numeric(n_steps)
  sim_eda <- numeric(n_steps)
  sim_mv <- numeric(n_steps)
  
  # Initial step
  sim_hr[1]   <- base_hr
  sim_temp[1] <- base_temp
  sim_eda[1]  <- base_eda
  sim_mv[1]   <- 0.05
  
  current_hr <- base_hr
  
  for (i in 2:n_steps) {
    t_min <- sim_df$time_min[i]
    z <- sim_df$zone[i]
    
    # Movement profile
    mv <- if (z == "Exercise") base_mv * (0.95 + 0.1 * sin(i / 3)) else 0.05
    sim_mv[i] <- mv
    
    # Feature vector for ML model prediction
    # In exercise conditions, workload increases HR towards intensity demand
    if (z == "Exercise") {
      exercise_target <- base_hr + max_hr_delta
      prog <- min(1.0, (t_min - rest_end_min) / 4.0) # 4 min adaptation ramp
      workload_hr <- base_hr + (exercise_target - base_hr) * (1 - exp(-2.5 * prog))
      input_hr <- max(current_hr, workload_hr * 0.95)
    } else if (z == "Recovery") {
      rec_prog <- min(1.0, (t_min - recov_start_min) / (duration_min - recov_start_min))
      input_hr <- base_hr + (current_hr - base_hr) * exp(-1.8 * rec_prog)
    } else {
      input_hr <- base_hr
    }

    if (!is.null(rf_model)) {
      feat <- data.frame(
        prev_hr_mean = input_hr,
        prev_hr_last = input_hr,
        prev_hr_sd = ifelse(z == "Exercise", 3.2, 1.1),
        prev_hr_min = input_hr - 2,
        prev_hr_max = input_hr + 2,
        prev_eda_mean = sim_eda[i - 1],
        prev_eda_sd = 0.04,
        prev_eda_slope = ifelse(z == "Exercise", 0.005, -0.002),
        prev_temp_mean = sim_temp[i - 1],
        prev_temp_sd = 0.05,
        prev_mv_mean = mv,
        prev_mv_max = mv * 1.3,
        prev_mv_sd = 0.1,
        activity_condition = factor(
          if (z == "Rest") "Rest" 
          else if (z == "Exercise" && grepl("Aerobic", activity_type)) "Aerobic Exercise"
          else if (z == "Exercise" && grepl("Anaerobic", activity_type)) "Anaerobic Exercise"
          else if (z == "Exercise" && grepl("Stress", activity_type)) "Stress Session"
          else "Rest",
          levels = c("Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session")
        ),
        time_elapsed_min = t_min
      )
      
      pred_hr <- suppressWarnings(predict(rf_model, data = feat)$predictions)
      current_hr <- round(pred_hr, 1)
    } else {
      current_hr <- round(input_hr, 1)
    }
    
    sim_hr[i] <- round(current_hr, 1)
    
    # Skin temperature dynamics (vasodilation and sweat evaporation)
    if (z == "Exercise") {
      temp_drift <- 0.4 * (1 - exp(-i / 10))
      sim_temp[i] <- round(base_temp + temp_drift, 2)
    } else if (z == "Recovery") {
      sim_temp[i] <- round(sim_temp[i - 1] - 0.03, 2)
    } else {
      sim_temp[i] <- base_temp
    }
    
    # EDA sympathetic sweat response dynamics
    if (z == "Exercise") {
      eda_rise <- (intensity_factor * 1.5) * (1 - exp(-i / 8))
      sim_eda[i] <- round(base_eda + eda_rise + 0.02 * sin(i / 2), 3)
    } else if (z == "Recovery") {
      sim_eda[i] <- round(max(base_eda, sim_eda[i - 1] - 0.04), 3)
    } else {
      sim_eda[i] <- base_eda
    }
  }
  
  sim_df$heart_rate <- sim_hr
  sim_df$skin_temp  <- sim_temp
  sim_df$eda        <- sim_eda
  sim_df$movement_g <- sim_mv
  
  # Summary outcome metrics
  summary_metrics <- list(
    peak_hr   = max(sim_hr),
    avg_temp  = round(mean(sim_temp), 1),
    peak_eda  = max(sim_eda),
    total_mov = round(sum(sim_mv) * (30 / 60), 1), # g * min
    rest_end  = rest_end_min,
    recov_start = recov_start_min,
    duration  = duration_min
  )
  
  list(simulated = sim_df, metrics = summary_metrics)
}

#' Plot Simulated vs Real Heart Rate with Protocol Zone Shading
plot_simulated_vs_real_hr <- function(sim_df, real_df = NULL, rest_end = 3, recov_start = 17, total_dur = 20) {
  p <- plot_ly()
  
  # Real Observed Data line (if available for comparison)
  if (!is.null(real_df) && nrow(real_df) > 0) {
    p <- p %>% add_lines(
      x = real_df$elapsed_sec / 60,
      y = real_df$heart_rate,
      name = "Real Measured Data",
      line = list(color = "#00CFFF", width = 2),
      opacity = 0.9
    )
  }
  
  # Simulated Model Heart Rate Line
  p <- p %>% add_lines(
    x = sim_df$time_min,
    y = sim_df$heart_rate,
    name = "Simulated Model (ranger RF)",
    line = list(color = "#FF4B6E", width = 2.5, dash = "dash")
  )
  
  # Background Protocol Zone Rectangles
  zone_shapes <- list(
    list(type = "rect", fillcolor = "rgba(59, 130, 246, 0.15)", line = list(width = 0),
         x0 = 0, x1 = rest_end, y0 = 40, y1 = 215, layer = "below"),
    list(type = "rect", fillcolor = "rgba(239, 68, 68, 0.16)", line = list(width = 0),
         x0 = rest_end, x1 = recov_start, y0 = 40, y1 = 215, layer = "below"),
    list(type = "rect", fillcolor = "rgba(16, 232, 148, 0.15)", line = list(width = 0),
         x0 = recov_start, x1 = total_dur, y0 = 40, y1 = 215, layer = "below")
  )
  
  # Annotations and layout
  p <- p %>% layout(
    shapes = zone_shapes,
    annotations = list(
      list(x = rest_end / 2, y = 200, text = "Rest", showarrow = FALSE, font = list(color = "#60A5FA", size = 11)),
      list(x = (rest_end + recov_start) / 2, y = 200, text = "Exercise Protocol", showarrow = FALSE, font = list(color = "#F87171", size = 11)),
      list(x = (recov_start + total_dur) / 2, y = 200, text = "Recovery", showarrow = FALSE, font = list(color = "#34D399", size = 11))
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(color = "#8EA3BF", family = "Inter, sans-serif"),
    xaxis = list(
      title = "Time (minutes)",
      range = c(0, total_dur),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = "#163456"
    ),
    yaxis = list(
      title = "Heart Rate (BPM)",
      range = c(50, 205),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = "#163456"
    ),
    legend = list(
      orientation = "h",
      x = 0.5, xanchor = "center", y = 1.15,
      bgcolor = "rgba(11, 29, 48, 0.8)",
      font = list(color = "#FFFFFF", size = 10)
    ),
    margin = list(l = 50, r = 30, t = 35, b = 40)
  ) %>% config(displayModeBar = FALSE, responsive = TRUE)
  
  p
}

#' Plot Predicted Multi-Signal Simulation
plot_predicted_signals_simulation <- function(sim_df) {
  p <- plot_ly() %>%
    add_lines(
      x = sim_df$time_min, y = sim_df$heart_rate,
      name = "Heart Rate (BPM)",
      line = list(color = "#FF4B6E", width = 2),
      yaxis = "y"
    ) %>%
    add_lines(
      x = sim_df$time_min, y = sim_df$skin_temp,
      name = "Skin Temp (°C)",
      line = list(color = "#00D2FF", width = 1.8),
      yaxis = "y2"
    ) %>%
    add_lines(
      x = sim_df$time_min, y = sim_df$eda,
      name = "EDA (µS)",
      line = list(color = "#FFAA00", width = 1.8),
      yaxis = "y3"
    ) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#8EA3BF", family = "Inter, sans-serif", size = 11),
      xaxis = list(title = "Time (minutes)", gridcolor = "rgba(22, 52, 86, 0.4)", linecolor = "#163456"),
      yaxis = list(
        title = "HR (BPM)", titlefont = list(color = "#FF4B6E"), tickfont = list(color = "#FF4B6E"),
        gridcolor = "rgba(22, 52, 86, 0.4)", side = "left"
      ),
      yaxis2 = list(
        title = "Temp (°C)", titlefont = list(color = "#00D2FF"), tickfont = list(color = "#00D2FF"),
        overlaying = "y", side = "right", showgrid = FALSE
      ),
      yaxis3 = list(
        title = "EDA (µS)", titlefont = list(color = "#FFAA00"), tickfont = list(color = "#FFAA00"),
        overlaying = "y", side = "right", position = 0.95, showgrid = FALSE
      ),
      legend = list(
        orientation = "h", x = 0.5, xanchor = "center", y = 1.15,
        bgcolor = "rgba(11, 29, 48, 0.8)", font = list(color = "#FFFFFF", size = 10)
      ),
      margin = list(l = 50, r = 70, t = 35, b = 40)
    ) %>% config(displayModeBar = FALSE, responsive = TRUE)
  
  p
}
