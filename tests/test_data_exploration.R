# tests/test_data_exploration.R
# Comprehensive Unit & Verification Suite for Data Exploration Chart Rendering & Filtering

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(plotly)
})

cat("=== EXECUTING DATA EXPLORATION VERIFICATION SUITE ===\n")

# Load dependencies
source("R/visualizations.R")
sessions_data <- readRDS("data/processed/physiological_sessions.rds")

# Helper function: build continuous timeline
build_continuous_timeline <- function(df) {
  if (nrow(df) == 0) return(df)
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

# Test 1: Single condition - Anaerobic Exercise has unique timestamps
sub_ana <- sessions_data %>% filter(participant_id == "f01", activity_condition == "Anaerobic Exercise")
chained_ana <- build_continuous_timeline(sub_ana)
expect_false(any(duplicated(chained_ana$timeline_sec)))
expect_gt(nrow(chained_ana), 1000)
cat("  [PASS] Test 01: Anaerobic Exercise has unique, strictly increasing timestamps\n")

# Test 2: Single condition - Stress Session has unique timestamps
sub_str <- sessions_data %>% filter(participant_id == "f01", activity_condition == "Stress Session")
chained_str <- build_continuous_timeline(sub_str)
expect_false(any(duplicated(chained_str$timeline_sec)))
expect_gt(nrow(chained_str), 1000)
cat("  [PASS] Test 02: Stress Session has unique, strictly increasing timestamps\n")

# Test 3: Single condition - Rest has unique timestamps after continuous timeline
sub_rest <- sessions_data %>% filter(participant_id == "f01", activity_condition == "Rest")
chained_rest <- build_continuous_timeline(sub_rest)
expect_false(any(duplicated(chained_rest$timeline_sec)))
expect_equal(nrow(chained_rest), 540)
cat("  [PASS] Test 03: Rest condition chains all sessions without duplicate timestamps\n")

# Test 4: Single condition - Aerobic Exercise has unique timestamps
sub_aer <- sessions_data %>% filter(participant_id == "f01", activity_condition == "Aerobic Exercise")
chained_aer <- build_continuous_timeline(sub_aer)
expect_false(any(duplicated(chained_aer$timeline_sec)))
expect_gt(nrow(chained_aer), 1000)
cat("  [PASS] Test 04: Aerobic Exercise has unique timestamps\n")

# Test 5: Multiple conditions - Aerobic + Anaerobic has no duplicate timestamps
sub_multi <- sessions_data %>% filter(participant_id == "f01", activity_condition %in% c("Aerobic Exercise", "Anaerobic Exercise"))
chained_multi <- build_continuous_timeline(sub_multi)
expect_false(any(duplicated(chained_multi$timeline_sec)))
expect_equal(nrow(chained_multi), nrow(sub_multi))
cat("  [PASS] Test 05: Multiple conditions (Aerobic + Anaerobic) chain cleanly with zero duplicates\n")

# Test 6: All 4 conditions combined have zero duplicate timestamps
sub_all <- sessions_data %>% filter(participant_id == "f01")
chained_all <- build_continuous_timeline(sub_all)
expect_false(any(duplicated(chained_all$timeline_sec)))
expect_equal(nrow(chained_all), nrow(sub_all))
cat("  [PASS] Test 06: All 4 conditions combined chain into continuous timeline with zero duplicates\n")

# Test 7: Multi-signal plot builds correctly for Anaerobic without area fill or solid block
p_ana <- plot_multi_signal_timeseries(chained_ana, signals = c("heart_rate", "movement_g", "eda"))
pb_ana <- plotly_build(p_ana)
expect_gt(length(pb_ana$x$data), 0)
for (tr in pb_ana$x$data) {
  expect_true(is.null(tr$fill) || tr$fill == "none" || tr$fill == "")
  expect_equal(tr$mode, "lines")
}
cat("  [PASS] Test 07: Anaerobic multi-signal chart renders pure lines with no fill\n")

# Test 8: Multi-signal plot builds correctly for Stress without area fill or solid block
p_str <- plot_multi_signal_timeseries(chained_str, signals = c("heart_rate", "movement_g", "skin_temp"))
pb_str <- plotly_build(p_str)
expect_gt(length(pb_str$x$data), 0)
for (tr in pb_str$x$data) {
  expect_true(is.null(tr$fill) || tr$fill == "none" || tr$fill == "")
  expect_equal(tr$mode, "lines")
}
cat("  [PASS] Test 08: Stress Session multi-signal chart renders pure lines with no fill\n")

# Test 9: Empty selection produces informative message
p_empty <- plot_multi_signal_timeseries(chained_ana[0, ])
pb_empty <- plotly_build(p_empty)
expect_true(any(grepl("No data available", unlist(pb_empty$x$layout$annotations))))
cat("  [PASS] Test 09: Empty selection produces informative annotation message\n")

# Test 10: Normalization preserves NA values (does not fabricate 0.5)
x_test <- c(10, NA, 20, NA, 30)
norm_res <- norm_scale(x_test)
expect_true(is.na(norm_res[2]))
expect_true(is.na(norm_res[4]))
expect_equal(norm_res[1], 0.0)
expect_equal(norm_res[5], 1.0)
cat("  [PASS] Test 10: norm_scale preserves NA values and does not fabricate measurements\n")

# Test 11: Correlation heatmap displays meaningful variable names
p_heat <- plot_correlation_heatmap(chained_ana, signals = c("heart_rate", "skin_temp", "eda", "movement_g"))
pb_heat <- plotly_build(p_heat)
tr_heat <- pb_heat$x$data[[1]]
expect_true("Heart Rate (BPM)" %in% tr_heat$x)
expect_true("Movement (g)" %in% tr_heat$x)
cat("  [PASS] Test 11: Correlation Heatmap displays full meaningful variable labels\n")

# Test 12: Heart Rate distribution handles filtered single condition
p_dens <- plot_hr_distribution_density(chained_ana)
pb_dens <- plotly_build(p_dens)
expect_gt(length(pb_dens$x$data), 0)
expect_equal(pb_dens$x$data[[1]]$name, "Anaerobic Exercise")
cat("  [PASS] Test 12: Heart Rate distribution reflects filtered Anaerobic Exercise condition\n")

cat("\nALL 12 DATA EXPLORATION TESTS PASSED SUCCESSFULLY!\n")
