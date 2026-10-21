# R/run_pipeline.R
# Complete Autonomous Data & ML Execution Pipeline
# Body in Motion — Human Body Response Simulator

cat("=== BODY IN MOTION: PIPELINE INITIALIZATION ===\n")
t_start <- Sys.time()

# 1. Source required modules
source("R/data_preprocessing.R")
source("R/feature_engineering.R")
source("R/train_model.R")

raw_dir       <- "data/raw"
processed_dir <- "data/processed"
models_dir    <- "models"
reports_dir   <- "reports"

dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)

# 2. Run Data Preprocessing
cat("\n[1/3] Processing raw PhysioNet records and demographic data...\n")
res <- process_all_datasets(raw_dir, processed_dir)

if (is.null(res) || nrow(res$sessions) == 0) {
  stop("FATAL: No physiological sessions could be processed.")
}

cat("  Processed observations:", nrow(res$sessions), "\n")
cat("  Processed sessions:", nrow(res$summary), "\n")
cat("  Participants included:", paste(unique(res$sessions$participant_id), collapse = ", "), "\n")
cat("  Activities included:", paste(unique(res$sessions$activity_condition), collapse = ", "), "\n")

# 3. Run Feature Engineering
cat("\n[2/3] Extracting 30-second windowed lag features (strict zero-leakage)...\n")
features_df <- prepare_ml_dataset(res$sessions, window_size = 30, step_size = 15)
saveRDS(features_df, file.path(processed_dir, "ml_features.rds"))
cat("  Extracted ML window observations:", nrow(features_df), "\n")

# 4. Train & Evaluate Random Forest Model
cat("\n[3/3] Training and evaluating Random Forest regressor with disjoint participant split...\n")
model_res <- train_and_evaluate_rf(features_df, test_participant_ratio = 0.25, seed = 42)

saveRDS(model_res$model, file.path(models_dir, "rf_hr_model.rds"))
saveRDS(model_res$evaluation, file.path(models_dir, "evaluation_results.rds"))

# 5. Output Evaluation Report
eval <- model_res$evaluation
report_lines <- c(
  "==================================================",
  "BODY IN MOTION — ML MODEL EVALUATION REPORT",
  paste("Generated:", Sys.time()),
  "==================================================",
  "",
  "MODEL SPECIFICATION:",
  "  Algorithm: Random Forest Regression (ranger R package)",
  "  Objective: Next-Window Mean Heart Rate Prediction",
  "  Window Size: 30 seconds",
  "  Target: Future window [t, t+30s] mean heart rate (BPM)",
  "  Features: Lagged HR (mean, last, sd, min, max), EDA (mean, sd, slope),",
  "            Skin Temp (mean, sd), Movement (mean, max, sd), Activity condition,",
  "            and elapsed duration.",
  "  Leakage Prevention: All features strictly computed from prior window [t-30s, t].",
  "  Validation Strategy: Disjoint Participant-Level Holdout Split (zero subject leakage)",
  "",
  paste("TRAINING SET:  ", eval$train_size, "windows from participants:", paste(eval$train_participants, collapse = ", ")),
  paste("HELD-OUT TEST SET:", eval$test_size, "windows from participants:", paste(eval$test_participants, collapse = ", ")),
  "",
  "HELD-OUT TEST METRICS:",
  paste("  Random Forest MAE: ", round(eval$rf_metrics$mae, 2), "BPM"),
  paste("  Random Forest RMSE:", round(eval$rf_metrics$rmse, 2), "BPM"),
  paste("  Random Forest R²:  ", round(eval$rf_metrics$r2, 4)),
  paste("  Random Forest MAPE:", round(eval$rf_metrics$mape, 2), "%"),
  "",
  "BASELINE COMPARISONS (on identical held-out test participants):",
  paste("  Persistence Baseline (last observed HR) MAE: ", round(eval$persist_metrics$mae, 2), "BPM"),
  paste("  Persistence Baseline RMSE:                  ", round(eval$persist_metrics$rmse, 2), "BPM"),
  paste("  Window Mean Baseline MAE:                   ", round(eval$mean_metrics$mae, 2), "BPM"),
  paste("  Window Mean Baseline RMSE:                  ", round(eval$mean_metrics$rmse, 2), "BPM"),
  "",
  "FEATURE IMPORTANCE (Impurity-based):"
)

for (i in 1:min(10, nrow(eval$importance))) {
  report_lines <- c(report_lines, sprintf("  %2d. %-20s : %.2f", i, eval$importance$feature[i], eval$importance$importance[i]))
}

report_lines <- c(
  report_lines,
  "",
  "PERFORMANCE BY ACTIVITY CONDITION:"
)

for (i in 1:nrow(eval$activity_eval)) {
  row <- eval$activity_eval[i, ]
  report_lines <- c(report_lines, sprintf("  %-20s (n=%3d) | MAE: %5.2f BPM | RMSE: %5.2f BPM", row$activity_condition, row$n, row$mae, row$rmse))
}

writeLines(report_lines, file.path(reports_dir, "model_evaluation_report.txt"))
cat(paste(report_lines, collapse = "\n"), "\n")

cat("\nPipeline finished successfully in", round(difftime(Sys.time(), t_start, units = "secs"), 1), "seconds!\n")
