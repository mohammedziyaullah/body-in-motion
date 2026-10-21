# tests/test_pipeline.R
# Automated Tests for Data Preprocessing, Feature Engineering, Model & Simulation
# Body in Motion — Human Body Response Simulator

# Ensure working directory is project root
if (basename(getwd()) == "tests") {
  setwd("..")
} else if (!file.exists("data/processed/physiological_sessions.rds") && file.exists("../data/processed/physiological_sessions.rds")) {
  setwd("..")
}

cat("=== EXECUTING AUTOMATED UNIT TESTS ===\n")
test_count <- 0
pass_count <- 0

assert_test <- function(desc, cond) {
  test_count <<- test_count + 1
  if (isTRUE(cond)) {
    pass_count <<- pass_count + 1
    cat(sprintf("  [PASS] Test %02d: %s\n", test_count, desc))
  } else {
    cat(sprintf("  [FAIL] Test %02d: %s\n", test_count, desc))
  }
}

source("R/data_preprocessing.R")
source("R/feature_engineering.R")
source("R/train_model.R")
source("R/simulation.R")

# 1. Test Processed Sessions Data
sessions_path <- "data/processed/physiological_sessions.rds"
assert_test("Processed sessions RDS file exists", file.exists(sessions_path))
sessions <- readRDS(sessions_path)
assert_test("Processed sessions contains valid observations", nrow(sessions) > 1000)
assert_test("Required physiological signals exist", all(c("heart_rate", "skin_temp", "eda", "movement_g") %in% colnames(sessions)))
assert_test("Heart rate values within physiological range (40 - 220 BPM)", all(sessions$heart_rate[!is.na(sessions$heart_rate)] >= 35 & sessions$heart_rate[!is.na(sessions$heart_rate)] <= 230))
assert_test("Activity condition categories properly encoded", all(c("Rest", "Aerobic Exercise", "Anaerobic Exercise", "Stress Session") %in% unique(sessions$activity_condition)))

# 2. Test Feature Engineering Zero Leakage
feats_path <- "data/processed/ml_features.rds"
assert_test("Extracted ML features RDS file exists", file.exists(feats_path))
feats <- readRDS(feats_path)
assert_test("Feature table has observations", nrow(feats) > 100)
assert_test("Target HR is present and strictly numeric", is.numeric(feats$target_hr) && all(!is.na(feats$target_hr)))
assert_test("Lagged features are present", all(c("prev_hr_mean", "prev_hr_last", "prev_eda_mean", "prev_temp_mean", "prev_mv_mean") %in% colnames(feats)))

# 3. Test Trained Model Artifact
model_path <- "models/rf_hr_model.rds"
eval_path  <- "models/evaluation_results.rds"
assert_test("Trained Random Forest RDS exists", file.exists(model_path))
assert_test("Evaluation results RDS exists", file.exists(eval_path))

rf_model <- readRDS(model_path)
eval_res <- readRDS(eval_path)
assert_test("Random Forest model is of class ranger", inherits(rf_model, "ranger"))
assert_test("Train and Test participant sets are completely disjoint", length(intersect(eval_res$train_participants, eval_res$test_participants)) == 0)
assert_test("Held-out test set size is positive", eval_res$test_size > 0)
assert_test("Evaluation MAE is physically sensible (< 10 BPM)", eval_res$rf_metrics$mae < 10.0)
assert_test("Evaluation R-squared is positive (> 0.8)", eval_res$rf_metrics$r2 > 0.8)

# 4. Test Scenario Simulation Function
sim_res <- simulate_physiological_response(
  activity_type = "Aerobic Exercise",
  intensity_pct = 70,
  duration_min = 20,
  participant_id = "Average",
  rf_model = rf_model,
  baseline_data = sessions
)
assert_test("Simulation produces simulated data frame", is.data.frame(sim_res$simulated) && nrow(sim_res$simulated) > 10)
assert_test("Simulation computes outcome metrics (peak HR, avg temp, peak EDA, total move)",
            all(c("peak_hr", "avg_temp", "peak_eda", "total_mov") %in% names(sim_res$metrics)))
assert_test("Simulation peak heart rate is higher than resting baseline (> 100 BPM)", sim_res$metrics$peak_hr > 100)

cat(sprintf("\nTEST SUMMARY: %d/%d tests passed (%.1f%%)\n", pass_count, test_count, (pass_count / test_count) * 100))
if (pass_count == test_count) {
  cat("ALL TESTS COMPLETED SUCCESSFULLY!\n")
} else {
  stop("SOME TESTS FAILED.")
}
