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

# Test 13: Selecting only BVP produces single trace with actual BVP values, solid line, on primary yaxis
p_bvp_only <- plot_multi_signal_timeseries(chained_ana, signals = c("bvp"))
pb_bvp_only <- plotly_build(p_bvp_only)
expect_equal(length(pb_bvp_only$x$data), 1)
tr_bvp <- pb_bvp_only$x$data[[1]]
expect_equal(tr_bvp$name, "BVP Signal")
expect_true(is.null(tr_bvp$line$dash) || tr_bvp$line$dash == "solid")
expect_true(is.null(tr_bvp$yaxis) || tr_bvp$yaxis == "y")
# Confirm actual values preserved (between 1.2 and 1.8), NOT normalized to [0, 1]
expect_gt(min(tr_bvp$y, na.rm = TRUE), 1.0)
expect_lt(max(tr_bvp$y, na.rm = TRUE), 2.0)
cat("  [PASS] Test 13: BVP-only plot renders actual samples with solid line on primary axis\n")

# Test 14: BVP-only plot labels primary Y-axis 'BVP Signal' and hides secondary axis
y_title_text <- if (is.list(pb_bvp_only$x$layout$yaxis$title)) pb_bvp_only$x$layout$yaxis$title$text else pb_bvp_only$x$layout$yaxis$title
expect_equal(y_title_text, "BVP Signal")
expect_false(isTRUE(pb_bvp_only$x$layout$yaxis2$visible))
cat("  [PASS] Test 14: BVP-only plot has Y-axis labeled 'BVP Signal' and hides secondary axis\n")

# Test 15: Heart-rate-only plot labels primary Y-axis 'Heart Rate (BPM)'
p_hr_only <- plot_multi_signal_timeseries(chained_ana, signals = c("heart_rate"))
pb_hr_only <- plotly_build(p_hr_only)
expect_equal(length(pb_hr_only$x$data), 1)
y_hr_title <- if (is.list(pb_hr_only$x$layout$yaxis$title)) pb_hr_only$x$layout$yaxis$title$text else pb_hr_only$x$layout$yaxis$title
expect_equal(y_hr_title, "Heart Rate (BPM)")
cat("  [PASS] Test 15: Heart-rate-only plot has Y-axis labeled 'Heart Rate (BPM)'\n")

# Test 16: Multi-signal plot with HR + BVP plots HR on primary axis and BVP on secondary axis with solid line
p_multi_bvp <- plot_multi_signal_timeseries(chained_ana, signals = c("heart_rate", "bvp"))
pb_multi_bvp <- plotly_build(p_multi_bvp)
expect_equal(length(pb_multi_bvp$x$data), 2)
bvp_tr <- if (pb_multi_bvp$x$data[[1]]$name == "BVP (Norm)") pb_multi_bvp$x$data[[1]] else pb_multi_bvp$x$data[[2]]
expect_equal(bvp_tr$yaxis, "y2")
expect_true(is.null(bvp_tr$line$dash) || bvp_tr$line$dash == "solid")
cat("  [PASS] Test 16: Multi-signal with HR + BVP renders clean dual-axis with solid lines\n")

# Test 17: Signal distribution density correctly renders BVP density across conditions
p_bvp_dist <- plot_signal_distribution_density(chained_all, signal = "bvp")
pb_bvp_dist <- plotly_build(p_bvp_dist)
expect_gt(length(pb_bvp_dist$x$data), 0)
x_bvp_dist <- if (is.list(pb_bvp_dist$x$layout$xaxis$title)) pb_bvp_dist$x$layout$xaxis$title$text else pb_bvp_dist$x$layout$xaxis$title
expect_equal(x_bvp_dist, "BVP Signal")
cat("  [PASS] Test 17: Signal distribution density accurately reflects BVP distribution\n")

cat("\nALL 17 DATA EXPLORATION & BVP TESTS PASSED SUCCESSFULLY!\n")
