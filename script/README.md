# Scripts directory

This folder contains scripts used to process gaze data, generate heatmaps and run ML interpretability experiments.

Structure
- `0_exploratory_analysis/` — exploratory analysis scripts and notebooks
- `1_data_processing/` — data loading and processing scripts
- `2_heatmap/` — heatmap generation helper and main scripts
- `3_ML_interpretability/` — LIME + CNN experiments helpers
- `requirements.R` — helper to load packages (not a reproducible installer)

Overview
This directory contains the scripts used to preprocess gaze recordings, generate heatmaps and run ML interpretability experiments. Use these scripts when you want to recreate the data processing / image generation / model interpretability steps described in the project.

Running
1. Install dependencies listed in `script/requirements.R` (preferably using `renv` or the packages listed there).
2. Run one consumer from command line:

```bash
Rscript script/run_data_processing.R 1-lucien
```

Other quick entry points

- Generate heatmaps for all consumers (uses `double_loop` behaviour):

```bash
Rscript script/run_heatmaps.R heatmap_corrected
```

- Run ML interpretability scripts (fake or real heatmaps):

```bash
Rscript script/run_ml_interpretability.R fake
Rscript script/run_ml_interpretability.R real
```

Notes
- The scripts expect raw gaze files under `data/gazedata/<consumer>/` with `ScreenRecorderPath.dat` files and supporting CSV / RData files in `data/` and `experience/`.
- Install packages listed in `script/requirements.R` (preferably use `renv` to pin versions).
