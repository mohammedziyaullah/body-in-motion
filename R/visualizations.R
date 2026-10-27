# R/visualizations.R
# Interactive Plotly and Visual Analytics Chart Generators
# Dark Medical-Analytics Aesthetic (#061321, #0B1D30, #00CFFF)

suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
})

# Custom Dark Theme Layout Template
DARK_BG <- "#061321"
CARD_BG <- "#0B1D30"
BORDER_COLOR <- "#163456"
TEXT_WHITE <- "#FFFFFF"
TEXT_MUTED <- "#8EA3BF"
CYAN_ACCENT <- "#00CFFF"
BLUE_PRIMARY <- "#087BFF"

# Metric Color Palette
COLOR_HR   <- "#FF4B6E" # Red
COLOR_TEMP <- "#00D2FF" # Cyan
COLOR_EDA  <- "#FFAA00" # Yellow/Orange
COLOR_ACC  <- "#10E894" # Green
COLOR_BVP  <- "#A855F7" # Violet

#' Common dark plotly layout configuration
apply_dark_theme <- function(p, title = "", x_title = "", y_title = "", show_legend = TRUE) {
  p %>%
    layout(
      title = list(text = title, font = list(color = TEXT_WHITE, size = 15, family = "Inter, -apple-system, sans-serif")),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED, family = "Inter, -apple-system, sans-serif", size = 11),
      xaxis = list(
        title = list(text = x_title, font = list(color = TEXT_MUTED, size = 12)),
        color = TEXT_MUTED,
        gridcolor = "rgba(22, 52, 86, 0.4)",
        zerolinecolor = "rgba(22, 52, 86, 0.6)",
        showline = TRUE,
        linecolor = BORDER_COLOR
      ),
      yaxis = list(
        title = list(text = y_title, font = list(color = TEXT_MUTED, size = 12)),
        color = TEXT_MUTED,
        gridcolor = "rgba(22, 52, 86, 0.4)",
        zerolinecolor = "rgba(22, 52, 86, 0.6)",
        showline = TRUE,
        linecolor = BORDER_COLOR
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.12,
        font = list(color = TEXT_WHITE, size = 10),
        bgcolor = "rgba(11, 29, 48, 0.6)",
        bordercolor = BORDER_COLOR,
        borderwidth = 1
      ),
      showlegend = show_legend,
      margin = list(l = 50, r = 50, t = 45, b = 45)
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

#' Build Continuous Timeline across Selected Sessions/Conditions
#' Prevents duplicate timestamps and unnatural zigzags when multiple sessions are selected
build_continuous_timeline <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  session_order <- c("AEROBIC", "ANAEROBIC", "STRESS")
  df$session_factor <- factor(df$activity_category, levels = session_order)
  df <- df %>% arrange(session_factor, elapsed_sec)
  
  sessions_present <- levels(df$session_factor)[levels(df$session_factor) %in% unique(df$activity_category)]
  
  df$timeline_sec <- 0
  current_offset <- 0
  
  for (sess in sessions_present) {
    idx <- which(df$activity_category == sess)
    if (length(idx) > 0) {
      sess_elapsed <- df$elapsed_sec[idx]
      min_e <- min(sess_elapsed)
      df$timeline_sec[idx] <- current_offset + (sess_elapsed - min_e)
      current_offset <- max(df$timeline_sec[idx]) + 1
    }
  }
  df
}

#' Normalized secondary signals (0 - 1 scale for clean multi-signal comparison)
#' Preserves missing values (NA) as NA without fabricating artificial measurements
norm_scale <- function(x) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  if (length(x_clean) == 0) return(rep(NA_real_, length(x)))
  rng <- range(x_clean)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[2] <= rng[1]) {
    val <- x_clean[1]
    norm_val <- if (val >= 0 && val <= 1.5) val / 1.5 else 0.5
    res <- rep(norm_val, length(x))
    res[is.na(x)] <- NA_real_
    return(res)
  }
  res <- (x - rng[1]) / (rng[2] - rng[1])
  res[is.na(x)] <- NA_real_
  res
}

#' Plot Multi-Signal Time Series with Adaptive Axis and Clean Waveform Rendering
plot_multi_signal_timeseries <- function(df, signals = c("heart_rate", "skin_temp", "eda", "movement_g", "bvp"), time_range = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(plot_ly(type = "scatter", mode = "markers") %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        text = "No data available for current selection",
        showarrow = FALSE, font = list(color = TEXT_MUTED, size = 14)
      ))
    ))
  }
  
  # Ensure timeline is continuous if not already computed
  if (!("timeline_sec" %in% colnames(df))) {
    df <- build_continuous_timeline(df)
  }
  
  # Determine time column: prefer continuous timeline_sec
  t_sec <- if ("timeline_sec" %in% colnames(df)) df$timeline_sec else df$elapsed_sec
  df$plot_time_min <- t_sec / 60
  
  # Ensure strictly distinct, monotonically increasing time points to guarantee line rendering
  df <- df %>% distinct(plot_time_min, .keep_all = TRUE) %>% arrange(plot_time_min)
  time_min <- df$plot_time_min
  
  # Filter signals to only those requested and present with at least one non-NA value
  valid_signals <- c()
  for (s in signals) {
    if (s %in% colnames(df) && any(!is.na(df[[s]]))) {
      valid_signals <- c(valid_signals, s)
    }
  }
  
  if (length(valid_signals) == 0) {
    sig_names <- sapply(signals, function(s) {
      switch(s, "heart_rate" = "Heart Rate", "skin_temp" = "Skin Temperature",
             "eda" = "EDA", "movement_g" = "Movement", "bvp" = "BVP", s)
    })
    sub_txt <- if ("participant_id" %in% colnames(df) && length(unique(df$participant_id)) == 1) unique(df$participant_id)[1] else "selected participant"
    cond_txt <- if ("activity_condition" %in% colnames(df)) paste(unique(df$activity_condition), collapse = ", ") else "selected condition"
    
    msg <- paste0(
      paste(sig_names, collapse = " / "), " is not recorded for participant ", sub_txt, " during ", cond_txt, ".\n",
      "(Heart Rate, EDA, Movement, and BVP are recorded for this session. Select another signal or try participant S01/S04)."
    )
    
    return(plot_ly(type = "scatter", mode = "markers") %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        text = msg,
        showarrow = FALSE, font = list(color = "#FCD34D", size = 13)
      ))
    ))
  }
  
  is_single_signal <- length(valid_signals) == 1
  single_sig <- if (is_single_signal) valid_signals[1] else NULL
  
  p <- plot_ly()
  
  # Case 1: ONLY BVP SELECTED
  if (is_single_signal && single_sig == "bvp") {
    # Plot actual BVP samples in chronological order connected with a solid line
    p <- p %>% add_lines(
      x = time_min, y = df$bvp,
      name = "BVP Signal",
      line = list(color = COLOR_BVP, width = 1.8, dash = "solid"),
      yaxis = "y",
      hovertemplate = "<b>BVP Signal</b>: %{y:.3f}<br>Time: %{x:.2f} min<extra></extra>"
    )
    
    y_min <- min(df$bvp, na.rm = TRUE)
    y_max <- max(df$bvp, na.rm = TRUE)
    pad <- if (y_max > y_min) (y_max - y_min) * 0.1 else 0.1
    
    yaxis_cfg <- list(
      title = "BVP Signal",
      titlefont = list(color = COLOR_BVP),
      tickfont = list(color = COLOR_BVP),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = BORDER_COLOR,
      side = "left",
      range = c(y_min - pad, y_max + pad)
    )
    yaxis2_cfg <- list(visible = FALSE)
  }
  # Case 2: ONLY HEART RATE SELECTED
  else if (is_single_signal && single_sig == "heart_rate") {
    p <- p %>% add_lines(
      x = time_min, y = df$heart_rate,
      name = "Heart Rate (BPM)",
      line = list(color = COLOR_HR, width = 2, dash = "solid"),
      yaxis = "y",
      hovertemplate = "<b>Heart Rate</b>: %{y:.1f} BPM<br>Time: %{x:.2f} min<extra></extra>"
    )
    yaxis_cfg <- list(
      title = "Heart Rate (BPM)",
      titlefont = list(color = COLOR_HR),
      tickfont = list(color = COLOR_HR),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = BORDER_COLOR,
      side = "left"
    )
    yaxis2_cfg <- list(visible = FALSE)
  }
  # Case 3: ONLY SKIN TEMP SELECTED
  else if (is_single_signal && single_sig == "skin_temp") {
    p <- p %>% add_lines(
      x = time_min, y = df$skin_temp,
      name = "Skin Temp (°C)",
      line = list(color = COLOR_TEMP, width = 1.8, dash = "solid"),
      yaxis = "y",
      hovertemplate = "<b>Skin Temp</b>: %{y:.2f} °C<br>Time: %{x:.2f} min<extra></extra>"
    )
    yaxis_cfg <- list(
      title = "Skin Temp (°C)",
      titlefont = list(color = COLOR_TEMP),
      tickfont = list(color = COLOR_TEMP),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = BORDER_COLOR,
      side = "left"
    )
    yaxis2_cfg <- list(visible = FALSE)
  }
  # Case 4: ONLY EDA SELECTED
  else if (is_single_signal && single_sig == "eda") {
    p <- p %>% add_lines(
      x = time_min, y = df$eda,
      name = "EDA (µS)",
      line = list(color = COLOR_EDA, width = 1.8, dash = "solid"),
      yaxis = "y",
      hovertemplate = "<b>EDA</b>: %{y:.3f} µS<br>Time: %{x:.2f} min<extra></extra>"
    )
    yaxis_cfg <- list(
      title = "EDA (µS)",
      titlefont = list(color = COLOR_EDA),
      tickfont = list(color = COLOR_EDA),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = BORDER_COLOR,
      side = "left"
    )
    yaxis2_cfg <- list(visible = FALSE)
  }
  # Case 5: ONLY MOVEMENT SELECTED
  else if (is_single_signal && single_sig == "movement_g") {
    y_min <- min(df$movement_g, na.rm = TRUE)
    y_max <- max(df$movement_g, na.rm = TRUE)
    pad <- if (y_max > y_min) (y_max - y_min) * 0.15 else 0.25
    
    p <- p %>% add_lines(
      x = time_min, y = df$movement_g,
      name = "Movement",
      line = list(color = COLOR_ACC, width = 2, dash = "solid"),
      yaxis = "y",
      hovertemplate = "<b>Movement</b>: %{y:.2f} g<br>Time: %{x:.2f} min<extra></extra>"
    )
    yaxis_cfg <- list(
      title = "Movement (g)",
      titlefont = list(color = COLOR_ACC),
      tickfont = list(color = COLOR_ACC),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = BORDER_COLOR,
      side = "left",
      range = c(max(0, y_min - pad), y_max + pad)
    )
    yaxis2_cfg <- list(visible = FALSE)
  }
  # Case 6: MULTIPLE SIGNALS INCLUDING HEART RATE
  else if ("heart_rate" %in% valid_signals) {
    p <- p %>% add_lines(
      x = time_min, y = df$heart_rate,
      name = "Heart Rate (BPM)",
      line = list(color = COLOR_HR, width = 2, dash = "solid"),
      yaxis = "y"
    )
    
    if ("skin_temp" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$skin_temp),
        name = "Skin Temp (Norm)",
        line = list(color = COLOR_TEMP, width = 1.5, dash = "solid"),
        yaxis = "y2"
      )
    }
    if ("eda" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$eda),
        name = "EDA (Norm)",
        line = list(color = COLOR_EDA, width = 1.5, dash = "solid"),
        yaxis = "y2"
      )
    }
    if ("movement_g" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$movement_g),
        name = "Movement (Norm)",
        line = list(color = COLOR_ACC, width = 1.6, dash = "solid"),
        yaxis = "y2"
      )
    }
    if ("bvp" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$bvp),
        name = "BVP (Norm)",
        line = list(color = COLOR_BVP, width = 1.5, dash = "solid"),
        yaxis = "y2"
      )
    }
    
    yaxis_cfg <- list(
      title = "Heart Rate (BPM)",
      titlefont = list(color = COLOR_HR),
      tickfont = list(color = COLOR_HR),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = BORDER_COLOR,
      side = "left"
    )
    yaxis2_cfg <- list(
      title = "Other Signals (Normalized 0-1)",
      titlefont = list(color = CYAN_ACCENT),
      tickfont = list(color = CYAN_ACCENT),
      overlaying = "y",
      side = "right",
      range = c(0, 1.05),
      showgrid = FALSE
    )
  }
  # Case 7: MULTIPLE SIGNALS NOT INCLUDING HEART RATE
  else {
    if ("skin_temp" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$skin_temp),
        name = "Skin Temp (Norm)",
        line = list(color = COLOR_TEMP, width = 1.5, dash = "solid"),
        yaxis = "y"
      )
    }
    if ("eda" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$eda),
        name = "EDA (Norm)",
        line = list(color = COLOR_EDA, width = 1.5, dash = "solid"),
        yaxis = "y"
      )
    }
    if ("movement_g" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$movement_g),
        name = "Movement (Norm)",
        line = list(color = COLOR_ACC, width = 1.6, dash = "solid"),
        yaxis = "y"
      )
    }
    if ("bvp" %in% valid_signals) {
      p <- p %>% add_lines(
        x = time_min, y = norm_scale(df$bvp),
        name = "BVP (Norm)",
        line = list(color = COLOR_BVP, width = 1.5, dash = "solid"),
        yaxis = "y"
      )
    }
    
    yaxis_cfg <- list(
      title = "Normalized Signals (0-1)",
      titlefont = list(color = CYAN_ACCENT),
      tickfont = list(color = CYAN_ACCENT),
      gridcolor = "rgba(22, 52, 86, 0.4)",
      linecolor = BORDER_COLOR,
      side = "left",
      range = c(0, 1.05)
    )
    yaxis2_cfg <- list(visible = FALSE)
  }
  
  xaxis_cfg <- list(
    title = "Time (minutes)",
    gridcolor = "rgba(22, 52, 86, 0.4)",
    linecolor = BORDER_COLOR,
    color = TEXT_MUTED
  )
  if (!is.null(time_range) && length(time_range) == 2 && time_range[2] > time_range[1]) {
    xaxis_cfg$range <- time_range
  }
  
  p <- p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED, family = "Inter, sans-serif", size = 11),
      xaxis = xaxis_cfg,
      yaxis = yaxis_cfg,
      yaxis2 = yaxis2_cfg,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.15,
        bgcolor = "rgba(11, 29, 48, 0.7)",
        font = list(color = TEXT_WHITE, size = 10)
      ),
      margin = list(l = 50, r = 50, t = 35, b = 40)
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
  
  p
}

#' Plot Distribution Curves for Selected Signal across Activity Conditions
plot_signal_distribution_density <- function(df, signal = "heart_rate") {
  signal_info <- list(
    heart_rate = list(name = "Heart Rate", unit = "Heart Rate (BPM)", col = "heart_rate", min_val = 35, max_val = 220),
    bvp        = list(name = "BVP Signal", unit = "BVP Signal", col = "bvp", min_val = -Inf, max_val = Inf),
    skin_temp  = list(name = "Skin Temp", unit = "Skin Temp (°C)", col = "skin_temp", min_val = 20, max_val = 45),
    eda        = list(name = "EDA", unit = "EDA (µS)", col = "eda", min_val = 0, max_val = 100),
    movement_g = list(name = "Movement", unit = "Movement (g)", col = "movement_g", min_val = 0, max_val = 20)
  )
  
  info <- if (signal %in% names(signal_info)) signal_info[[signal]] else signal_info$heart_rate
  col_name <- info$col
  
  if (is.null(df) || nrow(df) == 0 || !(col_name %in% colnames(df)) || !("activity_condition" %in% colnames(df)) || all(is.na(df[[col_name]]))) {
    sub_txt <- if ("participant_id" %in% colnames(df) && length(unique(df$participant_id)) == 1) unique(df$participant_id)[1] else "selected participant"
    return(plot_ly(type = "scatter", mode = "markers") %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        text = paste0("No valid ", tolower(info$name), " recorded for participant ", sub_txt, " in this condition\n(Select Heart Rate, EDA, Movement, or choose participant S01/S04)"),
        showarrow = FALSE, font = list(color = "#FCD34D", size = 12)
      ))
    ))
  }
  
  activities <- c("Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session")
  colors <- c("Rest" = "#3B82F6", "Aerobic Exercise" = "#10E894", "Anaerobic Exercise" = "#FF4B6E", "Stress Session" = "#FFAA00")
  
  p <- plot_ly()
  traces_added <- 0
  
  for (act in activities) {
    vec <- df[[col_name]][df$activity_condition == act]
    valid_vec <- vec[!is.na(vec) & is.finite(vec) & vec >= info$min_val & vec <= info$max_val]
    if (length(valid_vec) >= 5 && sd(valid_vec, na.rm = TRUE) > 1e-4) {
      dens <- density(valid_vec, n = 100)
      p <- p %>% add_trace(
        x = dens$x, y = dens$y,
        name = act,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        fillcolor = paste0(colors[act], "33"),
        line = list(color = colors[act], width = 2)
      )
      traces_added <- traces_added + 1
    }
  }
  
  if (traces_added == 0) {
    # If constant values exist (e.g. constant movement protocol level), display uniform level bar
    valid_all <- df[[col_name]][!is.na(df[[col_name]]) & is.finite(df[[col_name]])]
    if (length(valid_all) > 0) {
      cond_summary <- df %>%
        filter(!is.na(.data[[col_name]])) %>%
        group_by(activity_condition) %>%
        summarise(val = mean(.data[[col_name]], na.rm = TRUE), .groups = "drop")
      
      p_bar <- plot_ly(
        cond_summary, x = ~activity_condition, y = ~val,
        type = "bar",
        marker = list(color = "#10E894", line = list(color = "#00CFFF", width = 1.5))
      ) %>% layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = TEXT_MUTED, family = "Inter, sans-serif"),
        xaxis = list(title = "Condition", gridcolor = "rgba(22, 52, 86, 0.4)", linecolor = BORDER_COLOR),
        yaxis = list(title = info$unit, gridcolor = "rgba(22, 52, 86, 0.4)", linecolor = BORDER_COLOR),
        annotations = list(list(
          x = 0.5, y = 1.08, xref = "paper", yref = "paper",
          text = paste("Uniform protocol", tolower(info$name), "level recorded across condition"),
          showarrow = FALSE, font = list(color = CYAN_ACCENT, size = 11)
        )),
        margin = list(l = 50, r = 30, t = 35, b = 40)
      ) %>% config(displayModeBar = FALSE, responsive = TRUE)
      return(p_bar)
    }
    
    return(plot_ly(type = "scatter", mode = "markers") %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        text = paste("Insufficient", tolower(info$name), "variation to generate density curve"),
        showarrow = FALSE, font = list(color = TEXT_MUTED, size = 13)
      ))
    ))
  }
  
  apply_dark_theme(p, title = "", x_title = info$unit, y_title = "Density", show_legend = TRUE)
}

#' Plot Heart Rate Distribution Curves across Activity Conditions (Backward Compatibility)
plot_hr_distribution_density <- function(df) {
  plot_signal_distribution_density(df, signal = "heart_rate")
}

#' Plot Correlation Heatmap with Meaningful Variable Names
plot_correlation_heatmap <- function(df, signals = c("heart_rate", "skin_temp", "eda", "movement_g", "bvp")) {
  num_cols <- c("heart_rate", "skin_temp", "eda", "movement_g", "bvp")
  existing_cols <- intersect(signals, intersect(num_cols, colnames(df)))
  
  if (is.null(df) || nrow(df) < 5 || length(existing_cols) < 2) {
    return(plot_ly(type = "scatter", mode = "markers") %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        text = "Select at least 2 signals with valid data to view correlation",
        showarrow = FALSE, font = list(color = TEXT_MUTED, size = 13)
      ))
    ))
  }
  
  sub_df <- df[, existing_cols, drop = FALSE]
  cor_mat <- suppressWarnings(cor(sub_df, use = "pairwise.complete.obs"))
  
  if (all(is.na(cor_mat))) {
    return(plot_ly() %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        text = "Insufficient variable variance for correlation in current selection",
        showarrow = FALSE, font = list(color = TEXT_MUTED, size = 13)
      ))
    ))
  }
  
  cor_mat[!is.finite(cor_mat)] <- 0
  diag(cor_mat) <- 1.0
  
  name_map <- c(
    "heart_rate" = "Heart Rate (BPM)",
    "skin_temp"  = "Skin Temp (°C)",
    "eda"        = "EDA (µS)",
    "movement_g" = "Movement (g)",
    "bvp"        = "BVP"
  )
  labels <- unname(name_map[existing_cols])
  labels[is.na(labels)] <- existing_cols[is.na(labels)]
  
  # Scientific Diverging Colorscale (Navy -> White -> Red/Cyan)
  colorscale <- list(
    list(0, "#087BFF"),
    list(0.5, "#0B1D30"),
    list(1, "#FF4B6E")
  )
  
  plot_ly(
    x = labels, y = labels, z = cor_mat,
    type = "heatmap",
    colorscale = colorscale,
    zmin = -1, zmax = 1,
    text = round(cor_mat, 2),
    hoverinfo = "x+y+text"
  ) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED, family = "Inter, sans-serif"),
      xaxis = list(color = TEXT_WHITE, tickfont = list(color = TEXT_WHITE, size = 10)),
      yaxis = list(color = TEXT_WHITE, tickfont = list(color = TEXT_WHITE, size = 10)),
      margin = list(l = 85, r = 30, t = 30, b = 60)
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

#' Movement vs Heart Rate Scatter Plot with Linear Regression
plot_movement_vs_hr <- function(df) {
  if (is.null(df) || nrow(df) < 10) return(plot_ly())
  
  sub_df <- df %>%
    filter(!is.na(movement_g), !is.na(heart_rate), movement_g > 0, heart_rate > 40) %>%
    sample_n(min(800, n())) # sampled for smooth rendering
  
  fit <- lm(heart_rate ~ movement_g, data = sub_df)
  r_val <- cor(sub_df$movement_g, sub_df$heart_rate)
  p_val <- summary(fit)$coefficients[2, 4]
  p_text <- ifelse(p_val < 0.001, "p < 0.001", sprintf("p = %.3f", p_val))
  annotation_text <- sprintf("R = %.2f\n%s", r_val, p_text)
  
  plot_ly() %>%
    add_markers(
      data = sub_df,
      x = ~movement_g, y = ~heart_rate,
      marker = list(color = "rgba(0, 207, 255, 0.45)", size = 5, line = list(color = "rgba(0, 207, 255, 0.8)", width = 1)),
      name = "Observed"
    ) %>%
    add_lines(
      x = sub_df$movement_g, y = predict(fit),
      line = list(color = COLOR_HR, width = 2),
      name = "Trend Line"
    ) %>%
    layout(
      annotations = list(
        list(
          x = 0.85, y = 0.9, xref = "paper", yref = "paper",
          text = annotation_text,
          showarrow = FALSE,
          bgcolor = "rgba(11, 29, 48, 0.85)",
          bordercolor = BORDER_COLOR,
          font = list(color = CYAN_ACCENT, size = 12)
        )
      )
    ) %>%
    apply_dark_theme(title = "", x_title = "Movement Intensity (g)", y_title = "Heart Rate (BPM)", show_legend = FALSE)
}

#' Activity Condition Comparison (Box / Violin / Bar)
plot_activity_comparison <- function(df, metric = "heart_rate", chart_type = "box") {
  if (is.null(df) || nrow(df) == 0) return(plot_ly())
  
  metric_label <- case_when(
    metric == "heart_rate" ~ "Heart Rate (BPM)",
    metric == "skin_temp"  ~ "Skin Temperature (°C)",
    metric == "eda"        ~ "EDA (µS)",
    metric == "movement_g" ~ "Movement Intensity (g)",
    TRUE ~ metric
  )
  
  sub_df <- df %>% filter(!is.na(.data[[metric]]))
  
  colors <- c(
    "Rest" = "#3B82F6",
    "Aerobic Exercise" = "#10E894",
    "Anaerobic Exercise" = "#FF4B6E",
    "Stress Session" = "#FFAA00"
  )
  
  p <- plot_ly()
  
  if (chart_type == "box") {
    for (act in names(colors)) {
      dat <- sub_df %>% filter(activity_condition == act)
      if (nrow(dat) > 0) {
        p <- p %>% add_boxplot(
          y = dat[[metric]], name = act,
          marker = list(color = colors[act]),
          line = list(color = colors[act]),
          fillcolor = paste0(colors[act], "44")
        )
      }
    }
  } else if (chart_type == "violin") {
    for (act in names(colors)) {
      dat <- sub_df %>% filter(activity_condition == act)
      if (nrow(dat) > 0) {
        p <- p %>% add_trace(
          y = dat[[metric]], name = act, type = "violin",
          box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          fillcolor = paste0(colors[act], "55"),
          line = list(color = colors[act])
        )
      }
    }
  } else {
    # Bar chart of mean ± standard error
    agg <- sub_df %>%
      group_by(activity_condition) %>%
      summarise(
        mean_val = mean(.data[[metric]], na.rm = TRUE),
        se_val = sd(.data[[metric]], na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    p <- p %>% add_bars(
      x = agg$activity_condition, y = agg$mean_val,
      error_y = list(value = agg$se_val, color = TEXT_WHITE),
      marker = list(color = colors[agg$activity_condition])
    )
  }
  
  apply_dark_theme(p, title = "", x_title = "Activity Condition", y_title = metric_label, show_legend = FALSE)
}
