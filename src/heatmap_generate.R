######## script to process raw data from GazeRecorder #######
# obtain a nice heatmap + stimuli output #

# loading necessary packages

library(FactoMineR)
library(tidyverse)

# load raw data from GazeRecorder

data_gaze_raw <- read.table("data/record_correctionAndStimuli.txt")

# load data from real observed points

df_real <- data.frame(x_real = c(1.2, 28.4, 1.2, 28.4, 15.8 ),
                      y_real = c(16.4, 16.4, 1.2, 1.2, 8.7),
                      name = as.factor(1:5))


# function to pre-process raw data_gaze 
gaze_manage <- function(data,screen_size){
  gaze_df <- data.frame(
    x = data[,1]*screen_size[1],
    y = data[,2]*screen_size[2],
    t = (data[,3] - data[,3][1])/1000
  )
  return(gaze_df)
}

data_gaze <- gaze_manage(data_gaze_raw,c(31,17.4))

# function to run classification on the gaze points 

gaze_classif <- function(data,pca_weights,clust_number){
  res_pca <- FactoMineR::PCA(data,col.w = pca_weights,graph = F)
  res_hcpc <- FactoMineR::HCPC(res_pca,nb.clust = clust_number,consol = T,graph = F)
  return(res_hcpc$data.clust)
}

data_class <- gaze_classif(data_gaze,c(1,1,5),5)

df_join <- full_join(data_class, df_real, by=c("clust"="name")) %>% 
  rename(x_eye = x, y_eye = y, group = clust)

# function to apply correction of calibration 

gaze_calibrate <- function(data){
  
  # barycenter calcul
  bary_coord <- data %>% group_by(group) %>% 
    summarise(mean_x_eye = mean(x_eye), 
              mean_y_eye = mean(y_eye))
  
  
  
  return()
}
gaze_calibrate(data_class,df_real)








