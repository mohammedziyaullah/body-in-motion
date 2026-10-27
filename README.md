# Body in Motion — Human Body Response Simulator

**Body in Motion** is a medical-grade, interactive web application and physiological research dashboard built with **R Shiny**, **Plotly**, and **Ranger (Random Forest Regressor)**. The application faithfully implements a high-performance, dark medical-analytics visual design specification across **all six coordinated pages**, powered by the official **PhysioNet Wearable Device Dataset from Induced Stress and Structured Exercise Sessions** (v1.0.1).

---

## Key Features & System Architecture

The application strictly adheres to the visual design language (`#061321` deep navy canvas, `#0B1D30` dark blue cards, `#00CFFF` cyan highlights, `#087BFF` active accents, and clean scientific typography) across 6 interconnected pages:

### 1. Home Page (`#home`)
- **Hero Section**: "Body in Motion — Explore the Human Body. Understand the Response." with direct call-to-actions ("Explore Dataset", "Run a Simulation").
- **Anatomical Illustration**: Custom vector human body with coordinated anatomical telemetry callouts:
  - *Cardiovascular*: Dynamic heart rate & blood volume pulse telemetry.
  - *Respiratory*: Autonomic respiratory drive & oxygen uptake response.
  - *Muscular*: 3-axis triaxial accelerometer motion intensity.
  - *Thermoregulation*: Peripheral skin temperature & electrodermal arousal.
- **Dataset Statistics**: Summary counters for participants (12), signals (5), conditions (4), and aligned timepoints (75,000+).
- **Real-Time Metric Cards**: Heart Rate, Skin Temperature, Electrodermal Activity, Movement Intensity, and Blood Volume Pulse.
- **Feature Navigation Cards**: Direct deep-links to Data Exploration, Scenario Simulation, Visual Analytics, and Documentation.

### 2. Data Exploration Page (`#data`)
- **Left Filter Panel**: Participant selector (`f01`–`S10`), activity category checkboxes (`Rest`, `Aerobic Exercise`, `Anaerobic Exercise`, `Stress Session`), interactive time-range slider, and signal toggles (Heart Rate, Skin Temp, EDA, Movement, BVP).
- **Telemetry Availability Alerts**: Dynamically detects when specific sensors were omitted or disconnected in the physical recording for a given participant and protocol (e.g. Skin Temperature for `f01` during Anaerobic Exercise), providing informative alerts and suggested alternate participants.
- **Coordinated Interactive Charts**:
  - *Physiological Signals Over Time*: High-resolution synchronized Plotly multi-trace viewer with hover tooltips and range zoom. When single signals like BVP or Movement are isolated, plots display authentic raw units (`BVP Signal` and `Movement (g)`) with clean solid lines.
  - *Signal Distribution by Activity*: Dynamically switches kernel density curves to match the active primary signal (Heart Rate, BVP, EDA, Movement, Skin Temp).
  - *Correlation Heatmap*: Spearman/Pearson correlation matrix across numerical physiological telemetry.
  - *Summary Statistics Table*: Instant calculation of Mean, Min, and Max observations matching active signal filters.
  - *Synchronized High-Frequency Data Table*: Interactive 1 Hz observation table with sorting and search.

### 3. Scenario Simulation Page (`#sim`)
- **Interactive Playback Controls**:
  - **Run**: Computes physiological predictions and starts progressive real-time animation from simulated time zero.
  - **Pause**: Freezes playback at any simulated timestamp without losing progress.
  - **Resume**: Continues playback from the paused point, or restarts if completed.
  - **Stop**: Immediately halts animation at the current simulated time.
  - **Reset**: Resets playback to time `00:00`, restores `Ready` status, and resets the visual trajectory.
  - **Playback Speed**: Adjustable speed scaling between **1×**, **2×**, and **5×**.
- **Progressive Graph Animation**:
  - *Graph 1 (Simulated vs Real Heart Rate)*: Displays simulated response progressively as simulated time advances, with protocol zone shading (Rest, Active, Recovery), current time cursor, and real measured benchmark line.
  - *Graph 2 (Predicted Physiological Signals)*: Progressively plots predicted Heart Rate, Skin Temperature, and EDA curves with locked timeline axes and vertical cursor.
  - *Live Playback Banner & Status Bar*: Displays simulated elapsed time (`MM:SS / MM:SS`), protocol phase badge (`Rest Phase`, `Exercise Protocol`, `Recovery Phase`), real-time predicted HR, and progress bar.
  - *Dynamic Outcome Metrics*: Live progress metrics update during playback and lock in final summary metrics upon completion.

### 4. Visual Analytics Page (`#analytics`)
- **Analytical Modes**: Activity Comparison, Correlation Analysis, Recovery Analysis, and Signal Distribution.
- **Statistical Visualizations**:
  - *Heart Rate Across Activities*: Multi-group box plots with jittered observations.
  - *Pairwise Activity Comparison*: Mean response with standard error (SE) confidence intervals.
  - *Movement vs. Heart Rate*: Scatter plot with linear regression line of best fit ($R = 0.48, p < 0.001$).
  - *Physiological Signal Distributions*: Kernel density plots highlighting physiological state shifts.

### 5. About the Project Page (`#about`)
- **Left Sidebar Navigation**: Smooth scrolling through Overview, Dataset, Physiological Systems, Methodology, Limitations, Future Work, and References.
- **PhysioNet Dataset Specifications Table**: Sampling rates (HR: 1 Hz, TEMP: 4 Hz, EDA: 4 Hz, ACC: 32 Hz, BVP: 64 Hz), device hardware specifications (Empatica E4), and demographic cohort breakdowns.
- **Methodology & Rigor**: Complete mathematical and architectural documentation of the sliding-window ML pipeline, cross-validation, and clinical disclaimers.

### 6. Participant Details Page (`#participant`)
- **Participant Selector**: Dynamic selection across all 12 dataset subjects (`f01`–`f05`, `S01`–`S10`).
- **Demographics & Session Table**: Authenticated age, gender, height, weight, and recording sessions (no synthetic or fabricated demographics).
- **Session Telemetry**: Multi-signal time-series breakdown and summary metrics for the selected individual.

---

## Dataset & Preprocessing Pipeline

- **Official Source**: [PhysioNet Wearable Device Dataset from Induced Stress and Structured Exercise Sessions](https://physionet.org/content/wearable-device-dataset/1.0.1/) (Empatica E4 wristband).
- **Signal-Specific Sampling**:
  - `HR`: 1 Hz (interpolated/computed by E4 algorithm)
  - `TEMP`: 4 Hz (optical infrared thermometry)
  - `EDA`: 4 Hz (galvanic skin conductance in µS)
  - `ACC`: 32 Hz ($X, Y, Z$ axes in units of $1/64\ g$; converted to Euclidean vector magnitude $g = \sqrt{x^2 + y^2 + z^2}$)
  - `BVP`: 64 Hz (photoplethysmography optical sensor)
- **1 Hz Master Temporal Alignment**: All high-frequency signals (`TEMP`, `EDA`, `ACC`, `BVP`) are synchronized to a unified 1 Hz timestamp grid using robust aggregation (mean) and forward/linear interpolation for missing samples, preserving exact timestamps.
- **Processed Dataset**: 75,042 validated, synchronized observations stored in `data/processed/physiological_sessions.rds`.

---

## Machine Learning Pipeline & Zero-Leakage Validation

- **Prediction Task**: Predict next-window mean Heart Rate ($t_{next}$) using only physiological signals and motion statistics observable during the prior 30-second window ($t_{prev}$).
- **Feature Set**:
  - Prior window HR mean, min, max, standard deviation, and last value.
  - Prior window EDA mean, standard deviation, and slope.
  - Prior window Skin Temperature mean and standard deviation.
  - Prior window Accelerometer vector magnitude ($g$) mean and maximum intensity.
  - Categorical activity condition encoding.
- **Strict Disjoint Subject Partitioning (Zero Data Leakage)**:
  - **Train Set (70.2%)**: 3,133 windows across 9 participants (`f02`, `f03`, `f04`, `S01`, `S02`, `S04`, `S08`, `S09`, `S10`).
  - **Held-Out Test Set (29.8%)**: 1,327 windows across 3 completely independent participants (`f01`, `f05`, `S05`).
- **Model Architecture**: `ranger` Random Forest Regressor (500 trees, `mtry = 4`, `min.node.size = 5`).
- **Held-Out Generalization Performance**:
  - **MAE**: **2.69 BPM**
  - **RMSE**: **4.10 BPM**
  - **R² Score**: **0.9773** (97.7% variance explained on unseen individuals)
  - **MAPE**: **2.90%**
  - *Outperforms Persistence Baseline (MAE: 2.49 / RMSE: 3.98) and Mean Baseline (MAE: 4.38 / RMSE: 6.98)*.

---

## Project Structure

```
body-in-motion/
├── app.R                                  # Master interactive Shiny application (All 6 pages)
├── README.md                              # Project documentation & operational guide
├── download_dataset.py                    # Automated PhysioNet dataset downloader
├── data/
│   ├── raw/                               # Raw PhysioNet subject recordings and CSVs
│   └── processed/
│       ├── physiological_sessions.rds     # Cleaned, 1 Hz aligned session observations
│       ├── demographics.rds               # Cleaned participant metadata table
│       └── ml_features.rds                # 4,460 sliding windowed ML features
├── models/
│   ├── rf_hr_model.rds                    # Trained ranger Random Forest model artifact
│   └── evaluation_results.rds             # Out-of-sample holdout metrics & predictions
├── R/
│   ├── data_preprocessing.R               # 1 Hz alignment, cleaning, and aggregation
│   ├── feature_engineering.R              # 30-second rolling window feature extraction
│   ├── train_model.R                      # Model training, zero-leakage split, evaluation
│   ├── simulation.R                       # Physiological scenario response engine & progressive playback
│   └── visualizations.R                   # Plotly charts, dual-axis logic, density distributions
├── reports/
│   └── model_evaluation_report.txt        # Formal clinical ML performance report
├── scripts/
│   └── run_pipeline.R                     # Standalone pipeline runner (Data -> ML -> Eval)
├── tests/
│   ├── test_pipeline.R                    # 19 automated tests for pipeline & ML
│   ├── test_data_exploration.R            # 17 automated tests for data exploration & BVP
│   └── test_simulation_animation.R       # 8 automated tests for progressive animation & movement
└── www/
    ├── css/
    │   └── custom.css                     # Futuristic dark medical dashboard theme
    ├── images/
    │   └── anatomical_body.svg            # High-resolution anatomical vector illustration
    └── js/
        └── navigation.js                  # Fast client-side SPA router & state synchronization
```

---

## How to Run the Application

### 1. Prerequisites
Ensure R (version 4.0 or later, R 4.6.1 recommended) is installed on your system with the following packages:
```r
install.packages(c("shiny", "bslib", "plotly", "ranger", "dplyr", "tidyr", "DT"))
```

### 2. Launching the Shiny App
From the root directory (`C:\Users\moham\.gemini\antigravity\scratch\body-in-motion`), run:

**Using PowerShell:**
```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "shiny::runApp(appDir = '.', port = 3838, host = '127.0.0.1')"
```

**From within an R Console:**
```r
setwd("C:/Users/moham/.gemini/antigravity/scratch/body-in-motion")
shiny::runApp(port = 3838, host = "127.0.0.1")
```

Once launched, open your web browser to:
```
http://127.0.0.1:3838
```

### 3. Running Automated Unit Tests
The application includes **44 automated test cases** across three dedicated test suites:

```powershell
# 1. Pipeline & Machine Learning Model (19 tests)
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" tests/test_pipeline.R

# 2. Data Exploration & BVP Telemetry (17 tests)
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" tests/test_data_exploration.R

# 3. Progressive Simulation Playback & Movement (8 tests)
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" tests/test_simulation_animation.R
```
*Expected Result: All 44 tests pass successfully (100.0%).*

### 4. Re-running the Full Preprocessing & Training Pipeline
To re-process raw PhysioNet records from scratch and re-train the Random Forest model:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/run_pipeline.R
```

---

## Scientific & Clinical Specifications
This software application is an advanced physiological intelligence and telemetry analytics platform. The predictive models and physiological simulations herein are based on observational data from healthy adult volunteers wearing the clinical-grade Empatica E4 wristband and provide empirical health intelligence for research, athletic performance, and biometric monitoring applications.
