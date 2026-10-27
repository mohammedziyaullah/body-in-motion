# tests/test_simulation_animation.R
# Automated Verification Suite for Simulation Animation Playback & Movement Visualization

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(plotly)
})

cat("=== EXECUTING SIMULATION ANIMATION & MOVEMENT TEST SUITE ===\n")

source("R/simulation.R")
source("R/visualizations.R")
rf_model <- readRDS("models/rf_hr_model.rds")
sessions_data <- readRDS("data/processed/physiological_sessions.rds")

# Test 1: Simulation workflow produces valid time-series and metrics for Aerobic
sim_aer <- simulate_physiological_response("Aerobic Exercise", 70, 20, "Average", rf_model, sessions_data)
expect_equal(nrow(sim_aer$simulated), 41)
expect_equal(sim_aer$metrics$duration, 20)
expect_gt(sim_aer$metrics$peak_hr, 100)
cat("  [PASS] Test 01: Simulation computes full 20-min aerobic trajectory (41 steps)\n")

# Test 2: Simulation workflow produces valid time-series for Anaerobic with higher peak HR
sim_ana <- simulate_physiological_response("Anaerobic Exercise", 85, 15, "Average", rf_model, sessions_data)
expect_equal(nrow(sim_ana$simulated), 31)
expect_gt(sim_ana$metrics$peak_hr, sim_aer$metrics$peak_hr)
cat("  [PASS] Test 02: Simulation reflects high-intensity anaerobic demands correctly\n")

# Test 3: Simulation workflow adapts timeline when duration changes (e.g. 35 mins)
sim_35 <- simulate_physiological_response("Stress Session", 50, 35, "Average", rf_model, sessions_data)
expect_equal(nrow(sim_35$simulated), 71)
expect_equal(sim_35$metrics$duration, 35)
cat("  [PASS] Test 03: Duration change dynamically updates timeline steps (71 steps for 35 min)\n")

# Test 4: Graph 1 (Simulated vs Real HR) builds progressive slice with current cursor & marker
cur_step <- 15 # 7.5 minutes in
sim_sub <- sim_aer$simulated[1:cur_step, ]
real_df <- sessions_data %>% filter(participant_id == "f01", activity_condition == "Aerobic Exercise")
p_hr <- plot_simulated_vs_real_hr(sim_sub, real_df, sim_aer$metrics$rest_end, sim_aer$metrics$recov_start, sim_aer$metrics$duration)
pb_hr <- plotly_build(p_hr)
expect_gt(length(pb_hr$x$data), 1)
# Check fixed x-axis range [0, 20]
expect_equal(pb_hr$x$layout$xaxis$range, c(0, 20))
# Check current time marker trace
has_marker <- any(sapply(pb_hr$x$data, function(d) identical(d$mode, "markers")))
expect_true(has_marker)
cat("  [PASS] Test 04: Graph 1 animates progressively with cursor, marker, and fixed x-axis\n")

# Test 5: Graph 2 (Predicted Signals) builds progressive slice with fixed duration
p_sig <- plot_predicted_signals_simulation(sim_sub, total_dur = sim_aer$metrics$duration)
pb_sig <- plotly_build(p_sig)
expect_equal(pb_sig$x$layout$xaxis$range, c(0, 20))
expect_gte(length(pb_sig$x$data), 3) # HR, Temp, EDA
cat("  [PASS] Test 05: Graph 2 animates HR, Temp, and EDA with fixed timeline\n")

# Test 6: Summary metrics match final simulation when completed
m_final <- sim_aer$metrics
cur_peak_hr <- max(sim_sub$heart_rate, na.rm = TRUE)
expect_lte(cur_peak_hr, m_final$peak_hr)
# At 100% completion:
sub_100 <- sim_aer$simulated[1:nrow(sim_aer$simulated), ]
expect_equal(max(sub_100$heart_rate), m_final$peak_hr)
expect_equal(round(mean(sub_100$skin_temp), 1), m_final$avg_temp)
expect_equal(max(sub_100$eda), m_final$peak_eda)
cat("  [PASS] Test 06: Progressive outcome metrics smoothly converge to final summary metrics\n")

# Test 7: Movement visualization inspection & verification
sub_mv <- sessions_data %>% filter(participant_id == "f01", activity_condition == "Aerobic Exercise")
sub_mv_chained <- build_continuous_timeline(sub_mv)
p_mv <- plot_multi_signal_timeseries(sub_mv_chained, signals = c("movement_g"))
pb_mv <- plotly_build(p_mv)
tr_mv <- pb_mv$x$data[[1]]
expect_equal(tr_mv$name, "Movement")
expect_true(is.null(tr_mv$line$dash) || tr_mv$line$dash == "solid")
# Y-axis title
y_title <- if (is.list(pb_mv$x$layout$yaxis$title)) pb_mv$x$layout$yaxis$title$text else pb_mv$x$layout$yaxis$title
expect_equal(y_title, "Movement (g)")
# Authentic data value preserved (0.85 for Aerobic), not fabricated
expect_equal(unique(tr_mv$y), 0.85)
cat("  [PASS] Test 07: Movement visualization preserves genuine data (0.85g), solid line, 'Movement' legend\n")

# Test 8: Movement distribution handles constant protocol level with informative chart
p_mv_dist <- plot_signal_distribution_density(sub_mv_chained, signal = "movement_g")
pb_mv_dist <- plotly_build(p_mv_dist)
expect_gt(length(pb_mv_dist$x$data), 0)
cat("  [PASS] Test 08: Movement distribution displays uniform protocol level without error\n")

cat("\nALL 8 SIMULATION ANIMATION & MOVEMENT TESTS PASSED SUCCESSFULLY!\n")
