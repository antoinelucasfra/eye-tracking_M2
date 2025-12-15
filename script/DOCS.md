Scripts documentation
---------------------

This file summarises the important entry points and helpers in `script/`.

Key helpers (in `R/`):

- `R/helpers_data_process.R` — gaze pre-processing, time filters, PCA clustering and correction helpers.
- `R/helpers_heatmap.R` — `heatmap_generator()` and `heatmap_generator_bigger()` plus a wrapper exposing the `double_loop` behaviour.
- `R/helpers_ml.R` — `loader_img()` to load heatmap images and build arrays suitable for Keras.

CLI entry points (recommended):

- `script/run_data_processing.R <consumer>` — process a single consumer and save an RDS in `outputs/`.
- `script/run_heatmaps.R [method]` — generate heatmaps (uses `double_loop` behaviour); default method: `heatmap_corrected`.
- `script/run_ml_interpretability.R fake|real` — run ML interpretability pipeline on fake or real heatmaps.

Notes & recommendations
- Prefer using `renv` to pin package versions and make the project reproducible.
- `script/requirements.R` lists packages used across scripts but is not an installer.
- The code still expects raw gaze files under `data/gazedata/<consumer>/` and supporting CSV/ RData files in `experience/` and `data/`.
