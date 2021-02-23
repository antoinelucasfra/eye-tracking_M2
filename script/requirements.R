# script to install and load necessary packages/items to run the project 

# load library and script (do install.packages(package_name) if the package is not already installed)

# divers
library(tidyr)
library(FactoMineR)
library(tidyverse)
library(googledrive)

# Heatmaps
library(truncnorm)
library(png)
library(grid)
library(ggplot2)

# Algo keras
library(tensorflow)
library(keras)
library(png)
library(stringr)
library(lime)
library(magick)
library(reticulate)
library(abind)
library(gtools)

# first install of keras and tensorflow in your Rstudio ?
# execute the following lines to complete installation
install_tensorflow()
install_keras()

# load script helpers for the project 

source("script/1_data_processing/helpers_data_process.R")
source("script/2_heatmap/helpers_heatmap_generator.R")
source("script/1_data_processing/load_data.R")


