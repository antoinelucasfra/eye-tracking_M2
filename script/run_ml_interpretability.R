#!/usr/bin/env Rscript
""
"Run ML interpretability scripts (fake or real heatmaps)

Usage: Rscript script/run_ml_interpretability.R fake|real
"
""

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[1] else 'fake'

if (mode == 'fake') {
  source('script/3_ML_interpretability/CNN_LIME_fake_heatmaps.R')
} else if (mode == 'real') {
  source('script/3_ML_interpretability/CNN_LIME_real_heatmaps.R')
} else {
  stop('mode must be "fake" or "real"')
}
