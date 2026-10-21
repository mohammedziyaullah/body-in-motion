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

#' Plot Multi-Signal Time Series with Dual Y-Axis
plot_multi_signal_timeseries <- function(df, signals = c("heart_rate", "skin_temp", "eda", "movement_g", "bvp")) {
  if (is.null(df) || nrow(df) == 0) {
    return(plot_ly() %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                                annotations = list(list(text = "No data available for current selection",
                                                        showarrow = FALSE, font = list(color = TEXT_MUTED, size = 14)))))
  }
  
  df <- df %>% arrange(elapsed_sec)
  time_min <- df$elapsed_sec / 60
  
  p <- plot_ly()
  
  # Heart Rate (Primary Axis, BPM)
  if ("heart_rate" %in% signals && any(!is.na(df$heart_rate))) {
    p <- p %>% add_lines(
      x = time_min, y = df$heart_rate,
      name = "Heart Rate (BPM)",
      line = list(color = COLOR_HR, width = 2),
      yaxis = "y"
    )
  }
  
  # Normalized secondary signals (0 - 1 scale for clean multi-signal comparison)
  norm_scale <- function(x) {
    x_clean <- x[!is.na(x) & is.finite(x)]
    if (length(x_clean) == 0) return(rep(0.5, length(x)))
    rng <- range(x_clean)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[2] <= rng[1]) return(rep(0.5, length(x)))
    res <- (x - rng[1]) / (rng[2] - rng[1])
    res[is.na(res) | !is.finite(res)] <- 0.5
    res
  }
  
  if ("skin_temp" %in% signals && any(!is.na(df$skin_temp))) {
    p <- p %>% add_lines(
      x = time_min, y = norm_scale(df$skin_temp),
      name = "Skin Temp (Norm)",
      line = list(color = COLOR_TEMP, width = 1.5, dash = "solid"),
      yaxis = "y2"
    )
  }
  
  if ("eda" %in% signals && any(!is.na(df$eda))) {
    p <- p %>% add_lines(
      x = time_min, y = norm_scale(df$eda),
      name = "EDA (Norm)",
      line = list(color = COLOR_EDA, width = 1.5),
      yaxis = "y2"
    )
  }
  
  if ("movement_g" %in% signals && any(!is.na(df$movement_g))) {
    p <- p %>% add_lines(
      x = time_min, y = norm_scale(df$movement_g),
      name = "Movement (Norm)",
      line = list(color = COLOR_ACC, width = 1.5),
      yaxis = "y2"
    )
  }
  
  if ("bvp" %in% signals && any(!is.na(df$bvp))) {
    p <- p %>% add_lines(
      x = time_min, y = norm_scale(df$bvp),
      name = "BVP (Norm)",
      line = list(color = COLOR_BVP, width = 1.2, dash = "dot"),
      yaxis = "y2"
    )
  }
  
  p <- p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED, family = "Inter, sans-serif", size = 11),
      xaxis = list(
        title = "Time (minutes)",
        gridcolor = "rgba(22, 52, 86, 0.4)",
        linecolor = BORDER_COLOR,
        color = TEXT_MUTED
      ),
      yaxis = list(
        title = "Heart Rate (BPM)",
        titlefont = list(color = COLOR_HR),
        tickfont = list(color = COLOR_HR),
        gridcolor = "rgba(22, 52, 86, 0.4)",
        linecolor = BORDER_COLOR,
        side = "left"
      ),
      yaxis2 = list(
        title = "Other Signals (Normalized)",
        titlefont = list(color = CYAN_ACCENT),
        tickfont = list(color = CYAN_ACCENT),
        overlaying = "y",
        side = "right",
        range = c(0, 1.05),
        showgrid = FALSE
      ),
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

#' Plot Heart Rate Distribution Curves across Activity Conditions
plot_hr_distribution_density <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !("activity_condition" %in% colnames(df))) {
    return(plot_ly() %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
  }
  
  activities <- c("Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session")
  colors <- c("Rest" = "#3B82F6", "Aerobic Exercise" = "#10E894", "Anaerobic Exercise" = "#FF4B6E", "Stress Session" = "#FFAA00")
  
  p <- plot_ly()
  
  for (act in activities) {
    sub_df <- df %>% filter(activity_condition == act, !is.na(heart_rate), is.finite(heart_rate), heart_rate > 35, heart_rate < 220)
    if (nrow(sub_df) > 5 && sd(sub_df$heart_rate, na.rm = TRUE) > 0.1) {
      dens <- density(sub_df$heart_rate, n = 100)
      p <- p %>% add_trace(
        x = dens$x, y = dens$y,
        name = act,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        fillcolor = paste0(colors[act], "33"),
        line = list(color = colors[act], width = 2)
      )
    }
  }
  
  apply_dark_theme(p, title = "", x_title = "Heart Rate (BPM)", y_title = "Density", show_legend = TRUE)
}

#' Plot Correlation Heatmap
plot_correlation_heatmap <- function(df) {
  num_cols <- c("heart_rate", "skin_temp", "eda", "movement_g", "bvp")
  existing_cols <- intersect(num_cols, colnames(df))
  
  if (length(existing_cols) < 2) {
    return(plot_ly() %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
  }
  
  sub_df <- df[, existing_cols] %>% na.omit()
  if (nrow(sub_df) < 10) {
    return(plot_ly() %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
  }
  
  cor_mat <- suppressWarnings(cor(sub_df, use = "pairwise.complete.obs"))
  cor_mat[!is.finite(cor_mat)] <- 0
  diag(cor_mat) <- 1.0
  labels <- c("HR", "Skin Temp", "EDA", "Move", "BVP")[match(existing_cols, num_cols)]
  
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
      xaxis = list(color = TEXT_WHITE, tickfont = list(color = TEXT_WHITE)),
      yaxis = list(color = TEXT_WHITE, tickfont = list(color = TEXT_WHITE)),
      margin = list(l = 50, r = 30, t = 30, b = 30)
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
