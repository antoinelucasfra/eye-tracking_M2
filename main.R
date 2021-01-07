# source code function
source("script/1_data_processing/data_process.R")
source("script/2_heatmap/generate_fake_heatmaps.R")


library(tidyr)


#Position of calibration square
square_pos = read.csv("experience/25_square_position.csv", sep =";", header = TRUE, row.names = 1, dec = ".", colClasses = c("col" = "factor", "xvec" = "numeric", 'yvec' = "numeric"))
square_pos$name = rownames(square_pos)


################################################
# define area number we were looking on screen for calibration phase 
area_number = dim(square_pos)[1]


# define your own screen size (height, width)
screen_size_input = c(9,16)


# Load data from the study

list_consumers_path = list.files("data/Gazedata/", full.names = TRUE)
list_consumers_name = list.files("data/Gazedata/", full.names = FALSE)

n_consumers = length(list_consumers_path)
row_number = n_consumers * length(list.files(list_consumers_path[1], full.names = TRUE))

df = data.frame()
col = c("x", "y", "t", "stimu", "consu", "rank")


for (k in 1:n_consumers){
  path_temp = list_consumers_path[k]
  list_stimuli = list.files(path_temp, full.names = FALSE)
  n_stimuli = length(list_stimuli)
    
  for (i in 1:n_stimuli){
    
    path_stimuli = paste0(path_temp[k],"/", list_stimuli[i], "/ScreenRecorderPath.dat")
    temp_txt <- read.table(path_stimuli, skip = 1)
    
    temp_txt = gaze_preprocess(temp_txt)
    
    df_temp = cbind(temp_txt[,], 
                    as.factor(list_stimuli[i]),
                    as.factor(list_consumers_name[k]),
                    as.factor(gsub("([0-9]+).*$", "\\1", list_stimuli[i])))
    colnames(df_temp) = col
    
    t = split_time(df_temp, time_sep = 10)
    
    df = rbind(df,df_temp)
  }
} 

# supprimer les 5 premieres secondes d'enregistrement : temps de recalage
df = df[df$t>5,]

consu_lvl = levels(df$consu)
stimu_lvl = levels(df$stimu)

for (k in consu_lvl){
  correction_temp = df[(df$stimu == "0_correction") & (df$consu == k),c("x", "y", "t")]
  data_class <- gaze_classif(correction_temp, clust_number = area_number)
  
  df_join <- full_join(data_class, df_real, by=c("clust"="name")) %>% 
    rename(x_eye = x, y_eye = y, group = clust)
  
  
  
  
  
  
  # get correction data with barycenter correction
  df_trans <- gaze_correct_bary(df_join)
  #get only barycenter data for each observed area
  df_bary <- unique(df_trans[,c("group","mean_x_eye","mean_y_eye","x_real","y_real")])
  
  dist_weight <- gaze_dist_weight_df(df_real,df_bary,stimuli)
  
  # separate each object created in dist_weight
  real_pivot <- dist_weight[[1]]
  bary_pivot <- dist_weight[[2]]
  weight <- dist_weight[[4]]
  dist <- dist_weight[[3]]
  
  # execute the linear combination
  stimuli_correct <- gaze_stimuli_combi(stimuli,real_pivot,bary_pivot,weight) 
}










