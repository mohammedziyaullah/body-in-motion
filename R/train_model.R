# R/train_model.R
# Random Forest Model Training and Evaluation Pipeline
# Body in Motion — Human Body Response Simulator

suppressPackageStartupMessages({
  library(dplyr)
  library(ranger)
})

#' Train Random Forest Model with Disjoint Participant Split
train_and_evaluate_rf <- function(features_df, test_participant_ratio = 0.25, seed = 42) {
  set.seed(seed)
  
  if (nrow(features_df) < 50) {
    stop("Insufficient feature observations for training (minimum 50 required).")
  }
  
  unique_participants <- unique(features_df$participant_id)
  n_test <- max(1, round(length(unique_participants) * test_participant_ratio))
  
  # Select held-out test participants
  test_participants <- sample(unique_participants, size = n_test)
  train_participants <- setdiff(unique_participants, test_participants)
  
  message("Participant-Level Split:")
  message("  Train Participants (", length(train_participants), "): ", paste(train_participants, collapse = ", "))
  message("  Held-out Test Participants (", length(test_participants), "): ", paste(test_participants, collapse = ", "))
  
  train_df <- features_df %>% filter(participant_id %in% train_participants)
  test_df  <- features_df %>% filter(participant_id %in% test_participants)
  
  # Predictor formula
  formula_rf <- target_hr ~ prev_hr_mean + prev_hr_last + prev_hr_sd + prev_hr_min + 
    prev_hr_max + prev_eda_mean + prev_eda_sd + prev_eda_slope + 
    prev_temp_mean + prev_temp_sd + prev_mv_mean + prev_mv_max + 
    prev_mv_sd + activity_condition + time_elapsed_min
  
  # Train Random Forest via ranger
  message("Training ranger Random Forest model on ", nrow(train_df), " observations...")
  rf_model <- ranger(
    formula = formula_rf,
    data = train_df,
    num.trees = 150,
    mtry = 4,
    min.node.size = 5,
    importance = "impurity",
    seed = seed
  )
  
  # Predictions on Held-Out Test Set
  test_preds <- predict(rf_model, data = test_df)$predictions
  
  # Baseline predictions on Held-Out Test Set
  persist_preds <- test_df$prev_hr_last
  mean_preds    <- test_df$prev_hr_mean
  
  # Calculate Metrics
  calc_metrics <- function(actual, pred) {
    err <- actual - pred
    mae  <- mean(abs(err), na.rm = TRUE)
    rmse <- sqrt(mean(err^2, na.rm = TRUE))
    r2   <- 1 - (sum(err^2, na.rm = TRUE) / sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE))
    mape <- mean(abs(err / actual), na.rm = TRUE) * 100
    list(mae = mae, rmse = rmse, r2 = r2, mape = mape)
  }
  
  rf_metrics      <- calc_metrics(test_df$target_hr, test_preds)
  persist_metrics <- calc_metrics(test_df$target_hr, persist_preds)
  mean_metrics    <- calc_metrics(test_df$target_hr, mean_preds)
  
  # Per-activity performance breakdown
  activity_eval <- test_df %>%
    mutate(pred_rf = test_preds, error_rf = abs(target_hr - test_preds)) %>%
    group_by(activity_condition) %>%
    summarise(
      n = n(),
      mae = round(mean(error_rf, na.rm = TRUE), 2),
      rmse = round(sqrt(mean((target_hr - pred_rf)^2, na.rm = TRUE)), 2),
      mean_actual = round(mean(target_hr, na.rm = TRUE), 1),
      mean_pred = round(mean(pred_rf, na.rm = TRUE), 1),
      .groups = "drop"
    )
  
  # Variable Importance
  imp <- importance(rf_model)
  imp_df <- data.frame(
    feature = names(imp),
    importance = as.numeric(imp)
  ) %>% arrange(desc(importance))
  
  # Evaluation results package
  evaluation <- list(
    train_size = nrow(train_df),
    test_size = nrow(test_df),
    train_participants = train_participants,
    test_participants = test_participants,
    rf_metrics = rf_metrics,
    persist_metrics = persist_metrics,
    mean_metrics = mean_metrics,
    activity_eval = activity_eval,
    importance = imp_df,
    test_predictions = data.frame(
      participant_id = test_df$participant_id,
      activity_condition = test_df$activity_condition,
      time_elapsed_min = test_df$time_elapsed_min,
      actual_hr = test_df$target_hr,
      predicted_hr = test_preds,
      baseline_last_hr = persist_preds,
      residual = test_df$target_hr - test_preds
    ),
    timestamp = Sys.time()
  )
  
  list(model = rf_model, evaluation = evaluation)
}
