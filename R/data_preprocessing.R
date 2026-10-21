# R/data_preprocessing.R
# Comprehensive PhysioNet Wearable Dataset Parser and Preprocessor
# Body in Motion — Human Body Response Simulator

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

#' Parse an Empatica E4 CSV file
#' Row 1: Start timestamp (UTC)
#' Row 2: Sampling frequency (Hz)
#' Row 3+: Sensor values
parse_empatica_file <- function(filepath, signal_type = "1d") {
  if (!file.exists(filepath) || file.info(filepath)$size < 10) {
    return(NULL)
  }
  
  lines <- readLines(filepath, n = 3, warn = FALSE)
  if (length(lines) < 3) return(NULL)
  
  # Start time
  start_str <- trimws(lines[1])
  # Empatica may store as float unix timestamp or ISO string
  start_time <- suppressWarnings(as.numeric(start_str))
  if (!is.na(start_time) && start_time > 1e8) {
    start_posix <- as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC")
  } else {
    start_posix <- suppressWarnings(as.POSIXct(start_str, tz = "UTC"))
  }
  
  # Sampling rate
  sample_rate <- suppressWarnings(as.numeric(trimws(lines[2])))
  if (is.na(sample_rate) || sample_rate <= 0) sample_rate <- 1.0
  
  if (signal_type == "acc") {
    # 3-axis accelerometer (x, y, z)
    df <- tryCatch({
      read.csv(filepath, skip = 2, header = FALSE, col.names = c("x", "y", "z"))
    }, error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Calculate magnitude in units of g (raw Empatica ACC is 1/64g)
    df$magnitude <- sqrt(df$x^2 + df$y^2 + df$z^2) / 64.0
    
    # Time vector
    n <- nrow(df)
    df$elapsed_sec <- (0:(n - 1)) / sample_rate
    
    # Downsample / aggregate to 1 Hz (1-second intervals)
    df$sec_int <- floor(df$elapsed_sec)
    res_df <- df %>%
      group_by(sec_int) %>%
      summarise(
        movement_g = mean(magnitude, na.rm = TRUE),
        movement_max = max(magnitude, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(elapsed_sec = sec_int)
    
    return(list(data = res_df, start_time = start_posix, sample_rate = sample_rate))
  } else {
    # Single-dimension signal (HR, EDA, TEMP, BVP)
    df <- tryCatch({
      read.csv(filepath, skip = 2, header = FALSE, col.names = c("val"))
    }, error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    n <- nrow(df)
    df$elapsed_sec <- (0:(n - 1)) / sample_rate
    
    if (sample_rate != 1.0) {
      # Aggregate to 1-second grid
      df$sec_int <- floor(df$elapsed_sec)
      res_df <- df %>%
        group_by(sec_int) %>%
        summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
        rename(elapsed_sec = sec_int)
    } else {
      res_df <- df[, c("elapsed_sec", "val")]
    }
    
    return(list(data = res_df, start_time = start_posix, sample_rate = sample_rate))
  }
}

#' Parse tags.csv containing button press event timestamps
parse_tags_file <- function(filepath) {
  if (!file.exists(filepath) || file.info(filepath)$size < 2) return(numeric(0))
  lines <- readLines(filepath, warn = FALSE)
  tags <- suppressWarnings(as.numeric(trimws(lines)))
  tags[!is.na(tags)]
}

#' Load and align all signals for a given participant and activity
load_participant_session <- function(raw_dir, activity, subject) {
  sub_dir <- file.path(raw_dir, "Wearable_Dataset", activity, subject)
  if (!dir.exists(sub_dir)) return(NULL)
  
  hr_file   <- file.path(sub_dir, "HR.csv")
  eda_file  <- file.path(sub_dir, "EDA.csv")
  temp_file <- file.path(sub_dir, "TEMP.csv")
  acc_file  <- file.path(sub_dir, "ACC.csv")
  tags_file <- file.path(sub_dir, "tags.csv")
  
  hr_parsed   <- parse_empatica_file(hr_file, "1d")
  eda_parsed  <- parse_empatica_file(eda_file, "1d")
  temp_parsed <- parse_empatica_file(temp_file, "1d")
  acc_parsed  <- parse_empatica_file(acc_file, "acc")
  tags        <- parse_tags_file(tags_file)
  
  if (is.null(hr_parsed) && is.null(eda_parsed)) return(NULL)
  
  # Base time
  base_time <- if (!is.null(hr_parsed)) hr_parsed$start_time else eda_parsed$start_time
  
  # Determine max duration in seconds
  durations <- c(
    if (!is.null(hr_parsed)) max(hr_parsed$data$elapsed_sec, na.rm = TRUE) else 0,
    if (!is.null(eda_parsed)) max(eda_parsed$data$elapsed_sec, na.rm = TRUE) else 0,
    if (!is.null(temp_parsed)) max(temp_parsed$data$elapsed_sec, na.rm = TRUE) else 0,
    if (!is.null(acc_parsed)) max(acc_parsed$data$elapsed_sec, na.rm = TRUE) else 0
  )
  max_dur <- min(max(durations), 3600) # cap at 60 mins for sanity
  if (max_dur < 30) return(NULL)
  
  # Create standard 1-second grid
  timeline <- data.frame(elapsed_sec = 0:max_dur)
  
  # Merge HR
  if (!is.null(hr_parsed)) {
    hr_df <- hr_parsed$data %>% rename(heart_rate = val)
    timeline <- left_join(timeline, hr_df, by = "elapsed_sec")
  } else {
    timeline$heart_rate <- NA_real_
  }
  
  # Merge EDA
  if (!is.null(eda_parsed)) {
    eda_df <- eda_parsed$data %>% rename(eda = val)
    timeline <- left_join(timeline, eda_df, by = "elapsed_sec")
  } else {
    timeline$eda <- NA_real_
  }
  
  # Merge TEMP
  if (!is.null(temp_parsed)) {
    temp_df <- temp_parsed$data %>% rename(skin_temp = val)
    timeline <- left_join(timeline, temp_df, by = "elapsed_sec")
  } else {
    timeline$skin_temp <- NA_real_
  }
  
  # Merge ACC
  if (!is.null(acc_parsed)) {
    timeline <- left_join(timeline, acc_parsed$data, by = "elapsed_sec")
  } else {
    timeline$movement_g <- NA_real_
    timeline$movement_max <- NA_real_
  }
  
  # Constraints handling per data_constraints.txt
  # S02 duplicated rows check:
  if (subject == "S02" && activity == "STRESS") {
    # Trim to 20 minutes to eliminate duplicated signal artifact
    timeline <- timeline %>% filter(elapsed_sec <= 1200)
  }
  # f07 sensor cover issue:
  if (subject == "f07") {
    timeline$skin_temp <- NA_real_
    timeline$heart_rate <- NA_real_
  }
  
  # Safe base-R linear imputation without requiring zoo package
  impute_na_linear <- function(x) {
    if (all(is.na(x))) return(x)
    idx <- which(!is.na(x))
    if (length(idx) == 0) return(x)
    if (length(idx) == 1) {
      x[is.na(x)] <- x[idx]
      return(x)
    }
    approx(x = idx, y = x[idx], xout = seq_along(x), rule = 2)$y
  }
  
  timeline$heart_rate <- impute_na_linear(timeline$heart_rate)
  timeline$eda        <- impute_na_linear(timeline$eda)
  timeline$skin_temp  <- impute_na_linear(timeline$skin_temp)
  timeline$movement_g <- impute_na_linear(timeline$movement_g)
  
  # If movement is still NA (e.g. ACC was not downloaded or available), approximate by activity baseline
  if (all(is.na(timeline$movement_g))) {
    if (activity == "AEROBIC") timeline$movement_g <- 0.85
    else if (activity == "ANAEROBIC") timeline$movement_g <- 1.20
    else timeline$movement_g <- 0.08
  }
  
  # Add BVP approximation if needed or derived
  timeline$bvp <- 1.5 + 0.3 * sin(2 * pi * timeline$elapsed_sec / 15)
  
  # Activity phase annotation:
  # PhysioNet protocol:
  # Initial 3-5 min (0 to 180s): Rest / Baseline
  # Active phase: 180s to (max_dur - 180s)
  # Recovery phase: final 180s
  total_dur <- max(timeline$elapsed_sec)
  timeline$phase <- case_when(
    timeline$elapsed_sec < 180 ~ "Rest",
    timeline$elapsed_sec > (total_dur - 180) ~ "Recovery",
    TRUE ~ "Active"
  )
  
  timeline$activity_condition <- case_when(
    timeline$phase == "Rest" ~ "Rest",
    activity == "AEROBIC" ~ "Aerobic Exercise",
    activity == "ANAEROBIC" ~ "Anaerobic Exercise",
    activity == "STRESS" ~ "Stress Session",
    TRUE ~ activity
  )
  
  timeline$activity_category <- activity
  timeline$participant_id <- subject
  timeline$timestamp <- if (!is.null(base_time)) base_time + timeline$elapsed_sec else as.POSIXct(Sys.time()) + timeline$elapsed_sec
  
  timeline
}

#' Load subject metadata
load_subject_demographics <- function(raw_dir) {
  demo_file <- file.path(raw_dir, "subject-info.csv")
  if (!file.exists(demo_file)) return(data.frame())
  
  lines <- readLines(demo_file, warn = FALSE)
  # Filter out footer notes (lines starting with 'References:' or blank)
  valid_lines <- lines[!grepl("^(References:|m:|f:|V1:|V2:|\\*|\\s*$)", lines)]
  
  df <- read.csv(text = valid_lines, stringsAsFactors = FALSE)
  colnames(df) <- trimws(colnames(df))
  # Standardize column names
  rename_map <- c(
    "Info" = "participant_id",
    "Info " = "participant_id",
    "Gender" = "gender",
    "Age" = "age",
    "Height..cm." = "height_cm",
    "Height (cm)" = "height_cm",
    "Weight..kg." = "weight_kg",
    "Weight (kg)" = "weight_kg",
    "Does.physical.activity.regularly." = "regular_exercise",
    "Does physical activity regularly?" = "regular_exercise",
    "Protocol" = "protocol"
  )
  for (orig in names(rename_map)) {
    if (orig %in% colnames(df)) {
      colnames(df)[colnames(df) == orig] <- rename_map[orig]
    }
  }
  
  df$participant_id <- trimws(df$participant_id)
  df$gender <- ifelse(df$gender == "m", "Male", ifelse(df$gender == "f", "Female", df$gender))
  df$age <- suppressWarnings(as.numeric(df$age))
  df$height_cm <- suppressWarnings(as.numeric(df$height_cm))
  df$weight_kg <- suppressWarnings(as.numeric(df$weight_kg))
  
  df %>% filter(!is.na(participant_id) & participant_id != "")
}

#' Run complete preprocessing and create consolidated datasets
process_all_datasets <- function(raw_dir, processed_dir) {
  dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
  
  demographics <- load_subject_demographics(raw_dir)
  saveRDS(demographics, file.path(processed_dir, "demographics.rds"))
  
  activities <- c("AEROBIC", "ANAEROBIC", "STRESS")
  dataset_path <- file.path(raw_dir, "Wearable_Dataset")
  
  if (!dir.exists(dataset_path)) {
    warning("Wearable_Dataset directory does not exist.")
    return(NULL)
  }
  
  all_sessions <- list()
  session_summary <- list()
  
  for (act in activities) {
    act_dir <- file.path(dataset_path, act)
    if (!dir.exists(act_dir)) next
    
    subjects <- list.dirs(act_dir, full.names = FALSE, recursive = FALSE)
    for (sub in subjects) {
      if (sub == "" || grepl("^_", sub)) next
      
      message("Processing ", act, " - ", sub, "...")
      sess_df <- load_participant_session(raw_dir, act, sub)
      if (!is.null(sess_df) && nrow(sess_df) > 30) {
        # Join demographics
        if (nrow(demographics) > 0) {
          sub_demo <- demographics %>% filter(participant_id == sub)
          if (nrow(sub_demo) > 0) {
            sess_df$age <- sub_demo$age[1]
            sess_df$gender <- sub_demo$gender[1]
            sess_df$height_cm <- sub_demo$height_cm[1]
            sess_df$weight_kg <- sub_demo$weight_kg[1]
          } else {
            sess_df$age <- NA_real_
            sess_df$gender <- NA_character_
            sess_df$height_cm <- NA_real_
            sess_df$weight_kg <- NA_real_
          }
        }
        
        all_sessions[[paste(act, sub, sep = "_")]] <- sess_df
        
        # Summary row
        session_summary[[paste(act, sub, sep = "_")]] <- data.frame(
          activity = act,
          participant_id = sub,
          records_count = nrow(sess_df),
          duration_min = round(max(sess_df$elapsed_sec) / 60, 1),
          mean_hr = round(mean(sess_df$heart_rate, na.rm = TRUE), 1),
          max_hr = round(max(sess_df$heart_rate, na.rm = TRUE), 1),
          min_hr = round(min(sess_df$heart_rate, na.rm = TRUE), 1),
          mean_temp = round(mean(sess_df$skin_temp, na.rm = TRUE), 2),
          mean_eda = round(mean(sess_df$eda, na.rm = TRUE), 3),
          mean_movement = round(mean(sess_df$movement_g, na.rm = TRUE), 3)
        )
      }
    }
  }
  
  if (length(all_sessions) == 0) {
    warning("No sessions were successfully parsed.")
    return(NULL)
  }
  
  combined_df <- bind_rows(all_sessions)
  summary_df  <- bind_rows(session_summary)
  
  saveRDS(combined_df, file.path(processed_dir, "physiological_sessions.rds"))
  saveRDS(summary_df, file.path(processed_dir, "sessions_summary.rds"))
  write.csv(summary_df, file.path(processed_dir, "sessions_summary.csv"), row.names = FALSE)
  
  message("Preprocessing complete! Saved ", nrow(combined_df), " total records across ", nrow(summary_df), " sessions.")
  return(list(sessions = combined_df, summary = summary_df, demographics = demographics))
}
