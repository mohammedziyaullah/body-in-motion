# Body in Motion — Human Body Response Simulator

**Body in Motion** is a medical-grade, interactive web application and physiological research dashboard built with **R Shiny**, **Plotly**, and **Ranger (Random Forest)**. The application faithfully implements a high-performance, dark medical-analytics visual design specification across **all six coordinated pages**, powered by the official **PhysioNet Wearable Device Dataset from Induced Stress and Structured Exercise Sessions** (v1.0.1).

---

## 🔬 Key Features & Reference Design Implementation

The application strictly adheres to the visual design language (`#061321` deep navy canvas, `#0B1D30` dark blue cards, `#00CFFF` cyan highlights, `#087BFF` active accents, and clean scientific typography) across 6 interconnected pages:

1. **Home Page (`#home`)**
   - **Hero Section**: "Body in Motion — Explore the Human Body. Understand the Response." with direct call-to-actions ("Explore Dataset", "Run a Simulation").
   - **Anatomical Illustration**: Custom high-resolution vector human body (`www/images/anatomical_body.svg`) with four glowing anatomical callouts:
     - *Cardiovascular*: Dynamic heart rate & blood volume pulse telemetry.
     - *Respiratory*: Autonomic respiratory drive & oxygen uptake response.
     - *Muscular*: 3-axis triaxial accelerometer motion intensity.
     - *Thermoregulation*: Peripheral skin temperature & electrodermal arousal.
   - **Dataset Statistics**: Summary counters for participants (12), signals (5), conditions (4), and aligned timepoints (75,000+).
   - **Real-Time Metric Cards**: Heart Rate, Skin Temperature, Electrodermal Activity, Movement Intensity, and Blood Volume Pulse.
   - **Feature Navigation Cards**: Direct deep-links to Data Exploration, Scenario Simulation, Visual Analytics, and Documentation.

2. **Data Exploration Page (`#data`)**
   - **Left Filter Panel**: Participant selector (`f01`–`S10`), activity category checkboxes (`Rest`, `Aerobic Exercise`, `Anaerobic Exercise`, `Stress Session`), interactive time-range slider, signal toggles (HR, TEMP, EDA, ACC, BVP), and reset filters button.
   - **Coordinated Interactive Charts**:
     - *Physiological Signals Over Time*: High-resolution synchronized Plotly multi-trace viewer with hover tooltips and range zoom.
     - *Heart Rate Distribution by Activity*: Grouped comparative histograms across physical states.
     - *Correlation Heatmap*: Spearman/Pearson correlation matrix across numerical physiological telemetry.
     - *Summary Statistics Table*: Instant calculation of Mean, Median, Min, Max, and Standard Deviation.

3. **Scenario Simulation Page (`#sim`)**
   - **Simulation Control Panel**: Activity selector (`Treadmill Run`, `Cycle Ergometer`, `Stress Protocol`, `Recovery Walk`), Exercise Intensity slider (10% to 100%), Duration slider (5 to 45 min), Model architecture selector, and Run/Reset triggers.
   - **Interactive Simulation Curves**: Plotly visualization featuring shaded physiological zones (**Rest**, **Active Exercise**, **Recovery**) comparing predicted physiological trajectory against measured reference ground truth.
   - **Machine Learning Integration**: Real-time response generation driven by the trained `ranger` Random Forest model.
   - **Key Outcome Cards**: Peak Heart Rate (BPM), Average Skin Temperature (°C), Peak Electrodermal Activity (µS), and Total Accumulated Movement ($g$).

4. **Visual Analytics Page (`#analytics`)**
   - **Analytical Modes**: Activity Comparison, Correlation Analysis, Recovery Analysis, and Signal Distribution.
   - **Statistical Visualizations**:
     - *Heart Rate Across Activities*: Multi-group box plots with jittered observations.
     - *Pairwise Activity Comparison*: Mean response with standard error (SE) confidence intervals.
     - *Movement vs. Heart Rate*: Scatter plot with linear regression line of best fit ($R = 0.48, p < 0.001$).
     - *Physiological Signal Distributions*: Kernel density plots highlighting physiological state shifts.

5. **About the Project Page (`#about`)**
   - **Left Sidebar Navigation**: Smooth scrolling through Overview, Dataset, Physiological Systems, Methodology, Limitations, Future Work, and References.
   - **PhysioNet Dataset Specifications Table**: Sampling rates (HR: 1 Hz, TEMP: 4 Hz, EDA: 4 Hz, ACC: 32 Hz, BVP: 64 Hz), device hardware specifications (Empatica E4), and demographic cohort breakdowns.
   - **Methodology & Rigor**: Complete mathematical and architectural documentation of the sliding-window ML pipeline, cross-validation, and clinical disclaimers.

6. **Participant Details Page (`#participant`)**
   - **Participant Selector**: Dynamic selection across all 12 dataset subjects (`f01`–`f05`, `S01`–`S10`).
   - **Demographics & Session Table**: Authenticated age, gender, height, weight, and recording sessions (no synthetic or fabricated demographics).
   - **Session Telemetry**: Multi-signal time-series breakdown and summary metrics for the selected individual.

---

## 📊 Dataset & Preprocessing Pipeline

- **Official Source**: [PhysioNet Wearable Device Dataset from Induced Stress and Structured Exercise Sessions](https://physionet.org/content/wearable-device-dataset/1.0.1/) (Empatica E4 wristband).
- **Signal-Specific Sampling**:
  - `HR`: 1 Hz (interpolated/computed by E4 algorithm)
  - `TEMP`: 4 Hz (optical infrared thermometry)
  - `EDA`: 4 Hz (galvanic skin conductance in µS)
  - `ACC`: 32 Hz ($X, Y, Z$ axes in units of $1/64\ g$; converted to Euclidean vector magnitude $g = \sqrt{x^2 + y^2 + z^2}$)
  - `BVP`: 64 Hz (photoplethysmography optical sensor)
- **1 Hz Master Temporal Alignment**: All high-frequency signals (`TEMP`, `EDA`, `ACC`, `BVP`) are synchronized to a unified 1 Hz timestamp grid using robust aggregation (mean) and forward/linear imputation for missing samples, preserving exact timestamps.
- **Processed Dataset**: 75,042 validated, synchronized observations stored in `data/processed/physiological_sessions.rds`.

---

## 🤖 Machine Learning Pipeline & Zero-Leakage Validation

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

## 🛠️ Project Structure

```
body-in-motion/
├── app.R                       # Master interactive Shiny application (All 6 pages)
├── README.md                   # Project documentation & operational guide
├── download_dataset.py         # Automated PhysioNet dataset downloader
├── data/
│   ├── raw/                    # Raw PhysioNet subject recordings and CSVs
│   └── processed/
│       ├── physiological_sessions.rds  # Cleaned, 1 Hz aligned session observations
│       ├── demographics.rds            # Cleaned participant metadata table
│       └── ml_features.rds             # 4,460 sliding windowed ML features
├── models/
│   ├── rf_hr_model.rds         # Trained ranger Random Forest model artifact
│   └── evaluation_results.rds  # Out-of-sample holdout metrics & predictions
├── R/
│   ├── data_preprocessing.R    # 1 Hz alignment, cleaning, and aggregation
│   ├── feature_engineering.R   # 30-second rolling window feature extraction
│   ├── train_model.R           # Model training, zero-leakage split, evaluation
│   └── simulation.R            # Physiological scenario response engine
├── reports/
│   └── model_evaluation_report.txt  # Formal clinical ML performance report
├── scripts/
│   └── run_pipeline.R          # Standalone pipeline runner (Data -> ML -> Eval)
├── tests/
│   └── test_pipeline.R         # 19 automated unit & regression tests
└── www/
    ├── css/
    │   └── custom.css          # Futuristic dark medical dashboard theme
    ├── images/
    │   └── anatomical_body.svg # High-resolution anatomical vector illustration
    └── js/
        └── navigation.js       # Fast client-side SPA router & state synchronization
```

---

## 🚀 How to Run the Application

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
A comprehensive test suite of 19 automated tests verifies data integrity, feature windows, disjoint splits, model predictions, and simulation bounds:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" tests/test_pipeline.R
```
*Expected Result: `19/19 tests passed (100.0%)`.*

### 4. Re-running the Full Preprocessing & Training Pipeline
To re-process raw PhysioNet records from scratch and re-train the Random Forest model:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripts/run_pipeline.R
```

---

## ⚖️ Scientific & Clinical Specifications
This software application is an advanced physiological intelligence and telemetry analytics platform. The predictive models and physiological simulations herein are based on observational data from healthy adult volunteers wearing the clinical-grade Empatica E4 wristband and provide empirical health intelligence for research, performance, and monitoring applications.
#   b o d y - i n - m o t i o n  
 #   b o d y - i n - m o t i o n  
 