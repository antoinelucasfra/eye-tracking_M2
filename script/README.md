# Scripts directory

This folder contains scripts used to process gaze data, generate heatmaps and run ML interpretability experiments.

Structure
- `0_exploratory_analysis/` — exploratory analysis scripts and notebooks
- `1_data_processing/` — data loading and processing scripts
- `2_heatmap/` — heatmap generation helper and main scripts
- `3_ML_interpretability/` — LIME + CNN experiments helpers
- `requirements.R` — helper to load packages (not a reproducible installer)

Refactor notes
- Reusable helper functions were moved to `R/helpers_data_process.R` and cleaned.
- `script/1_data_processing/main_data_process.R` now exposes `process_consumer()` which returns the raw and corrected data frames.
- `script/run_data_processing.R` is a small CLI wrapper that saves results to `outputs/processed_<consumer>.rds`.

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

Testing
- There are no unit tests in this repository by project decision.

Notes
- The scripts still expect raw gaze files under `data/gazedata/<consumer>/` with `ScreenRecorderPath.dat` files.
- For reproducibility, add a `renv.lock` or list package versions in the top-level README.
