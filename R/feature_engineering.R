# R/feature_engineering.R
# Sliding Window Feature Extraction with Strict Zero-Leakage Guarantee
# Body in Motion — Human Body Response Simulator

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

#' Extract 30-second non-overlapping or stepped windows for next-window HR forecasting
#' Strictly ensures all features are derived from [t - window_size, t]
#' and target is derived from [t, t + window_size].
create_windowed_features <- function(session_df, window_size = 30, step_size = 30) {
  if (is.null(session_df) || nrow(session_df) < (window_size * 2)) {
    return(data.frame())
  }
  
  # Ensure sorted by time
  session_df <- session_df %>% arrange(elapsed_sec)
  max_sec <- max(session_df$elapsed_sec, na.rm = TRUE)
  
  window_starts <- seq(from = window_size, to = max_sec - window_size, by = step_size)
  if (length(window_starts) == 0) return(data.frame())
  
  feature_list <- list()
  
  for (t in window_starts) {
    # Past observation window: [t - window_size, t)
    past_win <- session_df %>% filter(elapsed_sec >= (t - window_size) & elapsed_sec < t)
    # Future target window: [t, t + window_size)
    fut_win  <- session_df %>% filter(elapsed_sec >= t & elapsed_sec < (t + window_size))
    
    if (nrow(past_win) < (window_size * 0.7) || nrow(fut_win) < (window_size * 0.7)) {
      next
    }
    
    # Target: average HR in future window
    target_hr <- mean(fut_win$heart_rate, na.rm = TRUE)
    if (is.na(target_hr) || target_hr <= 30 || target_hr >= 230) next
    
    # Past window features (STRICTLY past measurements)
    hr_vals <- past_win$heart_rate[!is.na(past_win$heart_rate)]
    eda_vals <- past_win$eda[!is.na(past_win$eda)]
    temp_vals <- past_win$skin_temp[!is.na(past_win$skin_temp)]
    mv_vals <- past_win$movement_g[!is.na(past_win$movement_g)]
    
    if (length(hr_vals) == 0) next
    
    hr_mean <- mean(hr_vals)
    hr_last <- tail(hr_vals, 1)
    hr_sd   <- if (length(hr_vals) > 1) sd(hr_vals) else 0
    hr_min  <- min(hr_vals)
    hr_max  <- max(hr_vals)
    
    eda_mean <- if (length(eda_vals) > 0) mean(eda_vals) else 0.5
    eda_sd   <- if (length(eda_vals) > 1) sd(eda_vals) else 0
    eda_slope <- if (length(eda_vals) > 1) (tail(eda_vals, 1) - head(eda_vals, 1)) / window_size else 0
    
    temp_mean <- if (length(temp_vals) > 0) mean(temp_vals) else 34.0
    temp_sd   <- if (length(temp_vals) > 1) sd(temp_vals) else 0
    
    mv_mean <- if (length(mv_vals) > 0) mean(mv_vals) else 0.1
    mv_max  <- if (length(mv_vals) > 0) max(mv_vals) else 0.1
    mv_sd   <- if (length(mv_vals) > 1) sd(mv_vals) else 0
    
    activity_cond <- past_win$activity_condition[nrow(past_win)]
    participant_id <- past_win$participant_id[1]
    
    feature_list[[length(feature_list) + 1]] <- data.frame(
      participant_id    = participant_id,
      window_time_sec   = t,
      time_elapsed_min  = round(t / 60, 2),
      activity_condition= activity_cond,
      prev_hr_mean      = hr_mean,
      prev_hr_last      = hr_last,
      prev_hr_sd        = hr_sd,
      prev_hr_min       = hr_min,
      prev_hr_max       = hr_max,
      prev_eda_mean     = eda_mean,
      prev_eda_sd       = eda_sd,
      prev_eda_slope    = eda_slope,
      prev_temp_mean    = temp_mean,
      prev_temp_sd      = temp_sd,
      prev_mv_mean      = mv_mean,
      prev_mv_max       = mv_max,
      prev_mv_sd        = mv_sd,
      target_hr         = target_hr,
      stringsAsFactors  = FALSE
    )
  }
  
  if (length(feature_list) == 0) return(data.frame())
  bind_rows(feature_list)
}

#' Generate ML feature table across all sessions
prepare_ml_dataset <- function(all_sessions_df, window_size = 30, step_size = 15) {
  if (is.null(all_sessions_df) || nrow(all_sessions_df) == 0) {
    return(data.frame())
  }
  
  # Group by participant and session/activity
  sessions <- split(all_sessions_df, list(all_sessions_df$participant_id, all_sessions_df$activity_category), drop = TRUE)
  
  feature_tables <- list()
  for (key in names(sessions)) {
    sub_df <- sessions[[key]]
    feats <- create_windowed_features(sub_df, window_size = window_size, step_size = step_size)
    if (nrow(feats) > 0) {
      feature_tables[[key]] <- feats
    }
  }
  
  if (length(feature_tables) == 0) return(data.frame())
  
  combined_features <- bind_rows(feature_tables)
  combined_features$activity_condition <- as.factor(combined_features$activity_condition)
  
  combined_features
}
