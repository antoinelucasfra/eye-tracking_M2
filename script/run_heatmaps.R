#!/usr/bin/env Rscript
""
"Run heatmap generation tasks.

Usage: Rscript script/run_heatmaps.R [--method METHOD]
"
""

args <- commandArgs(trailingOnly = TRUE)
method <- if (length(args) >= 1) args[1] else "heatmap_corrected"

source('R/helpers_heatmap.R')

# default sizes
width_size <- 640
height_size <- 360

generate_heatmaps_double_loop(
  method = method,
  width_size = width_size,
  height_size = height_size
)
