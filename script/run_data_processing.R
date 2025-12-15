#!/usr/bin/env Rscript
""
"Run data processing for one consumer from command line.

Usage: Rscript script/run_data_processing.R <consumer_name>
"
""

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop('Please provide consumer name as first argument')
}
consumer <- args[1]

source('R/helpers_data_process.R')
source('script/1_data_processing/main_data_process.R')

# Example defaults; in practice use a config file or explicit args
start_time_vec <- matrix(5, nrow = 1, ncol = 20)
end_time_vec <- matrix(60, nrow = 1, ncol = 20)

res <- process_consumer(
  consumers_name = consumer,
  screen_size = c(1080, 1920),
  start_time_vec = start_time_vec,
  end_time_vec = end_time_vec
)

saveRDS(
  res,
  file = file.path('outputs', paste0('processed_', consumer, '.rds'))
)
