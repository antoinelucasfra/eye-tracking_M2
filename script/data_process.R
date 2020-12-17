######## script to process raw data from GazeRecorder #######
# obtain a nice heatmap + stimuli output #

# loading necessary packages

library(FactoMineR)
library(tidyverse)

# load raw data from GazeRecorder

data_gaze_raw <- read.table("data/record_23points_correction.txt")

################################################
# define area number we were looking at physical
area_number = 23
################################################

# load data from real observed points : add it manually
xvec <- c(1.6,29.4,29.4,1.6,15.6,8,24,24,8,1.6,29.4,29.4,1.6,15.6,29.4,15.6,1.6,8,24,8,24,24,8)
yvec <- c(16.5,16.5,1,1,8.7,16.5,16.5,1,1,13,13,4.3,4.3,16.5,8.7,1,8.7,8.7,8.7,13,13,4.3,4.3)
df_real <- data.frame(x_real=xvec,y_real=yvec,name=as.factor(1:area_number))
# df_real <- data.frame(x_real = c(1.2, 28.4, 1.2, 28.4, 15.8 ),
#                       y_real = c(16.4, 16.4, 1.2, 1.2, 8.7),
#                       name = as.factor(1:5))


# function to pre-process raw data_gaze 
gaze_manage <- function(data,screen_size){
  gaze_df <- data.frame(
    x = data[,1]*screen_size[1],
    y = (1 -data[,2])*screen_size[2],
    t = (data[,3] - data[,3][1])/1000
  )
  return(gaze_df)
}

data_gaze <- gaze_manage(data_gaze_raw,c(31,17.4))

# function to split the data in two parts : correction and stimuli
split_time <- function(data,time_sep){
  df_calibration <- data %>% filter(t < time_sep)
  df_stimuli <- data %>% filter(t >= time_sep)
  return(list(df_calibration,df_stimuli))
}

split <- split_time(data_gaze,12)

#get correction data
correction <- split[[1]]

# get stimuli data
stimuli <- split[[2]]

# function to run classification on the correction  
gaze_classif <- function(data,pca_weights,clust_number){
  res_pca <- FactoMineR::PCA(data,col.w = pca_weights,graph = F)
  res_hcpc <- FactoMineR::HCPC(res_pca,nb.clust = clust_number,consol = T,graph = F)
  return(res_hcpc$data.clust)
}

data_class <- gaze_classif(correction,c(1,1,5),area_number)

# add coordinates of real data
df_join <- full_join(data_class, df_real, by=c("clust"="name")) %>% 
  rename(x_eye = x, y_eye = y, group = clust)

# function to apply correction on correction data

gaze_correct_bary <- function(data){
  
  # barycenter calcul
  bary_coord <- data %>% group_by(group) %>% 
    summarise(mean_x_eye = mean(x_eye), 
              mean_y_eye = mean(y_eye))
  
  trans_mat <- full_join(data,bary_coord,by="group") %>% 
    mutate(x_diff = x_real-mean_x_eye,
           y_diff = y_real-mean_y_eye) %>% 
    mutate(x_trans = x_eye+x_diff,
           y_trans = y_eye+y_diff)
    
  return(trans_mat)
}

# get correction data with barycenter correction
df_trans <- gaze_correct_bary(df_join)

#get only barycenter data for each observed area
df_bary <- unique(df_trans[,c("group","mean_x_eye","mean_y_eye","x_real","y_real")])

# function to get dist and weights to apply linear combination on our stimuli data

gaze_dist_weight_df <- function(data_real,data_barycenter,data_stimuli,nb_clust){
  
  N <- dim(data_stimuli)[1]
  
  # get dataframe with same values for real points
  df_real_dup <- data_real %>% 
    pivot_wider(names_from = name, 
                values_from = c(x_real,y_real)) %>% 
    slice(rep(1:n(),each=N))
  
  # get barycenter in row vector
  df_bary_dup <- data_barycenter %>% 
    select(mean_x_eye,mean_y_eye,group) %>% 
    pivot_wider(names_from = group, 
                values_from = c(mean_x_eye,mean_y_eye)) %>% 
    slice(rep(1:n(),each=N))
  
  # bind the 3 dataframe
  # df_bind_3 <- cbind(stimuli,df_real_dup,df_bary_dup)
  # create the distance variable from the (observed) barycenters
  
  dist <- data.frame(temp=rep(0,N)) # create an empty dataframe to add columns
  
  # loop to compute the distance between stimuli data and barycenter data for each cluster
  for (k in 1:nb_clust){
    
    # compute euclidean distance
    dist_temp <- sqrt((data_stimuli$x - df_bary_dup[,k])^2 +
                       (data_stimuli$y - df_bary_dup[,nb_clust+k])^2)
    # rename the columns
    colnames(dist_temp) <- paste0("dist",k)
    dist <- cbind(dist,dist_temp)
  }
  dist <- dist[,-1] # erase the pseudo columns useful to create the dataframe at first
  
  # get max distance between each clusters per line
  dist$max_dist <- apply(dist,1,max)
  print(dist)
  # create the 5 weights variable from each clusters
  
  weights <- data.frame(temp=rep(0,N))
  name_weights <- c()
  for (k in 1:nb_clust){
    
    # compute a weight ratio
    weight_temp <- 1 - dist[,k] / dist$max_dist
    name_weights[k] <- paste0("weight",k)
    # rename the columns
    weights <- cbind(weights,weight_temp)
  }
  weights <- weights[,-1] 
  colnames(weights) <- name_weights
  
  return(list(df_real_dup,df_bary_dup,dist,weights))
}

dist_weight <- gaze_dist_weight_df(df_real,df_bary,stimuli,area_number)

real_pivot <- dist_weight[[1]]
bary_pivot <- dist_weight[[2]]
weight <- dist_weight[[4]]
dist <- dist_weight[[3]]

# function to apply the linear combination on stimuli data from the 
# correction data 

gaze_stimuli_combi <- function(data_stimuli,data_real,data_bary,data_weight,nb_clust){
  
  stimuli_correct <- data.frame(x = data_stimuli$x,
                                y = data_stimuli$y)
  for (k in 1:nb_clust)
    {
    stimuli_correct[,1] <- stimuli_correct[,1] + (data_real[k]-data_bary[k])*data_weight[k]
    stimuli_correct[,2] <- stimuli_correct[,2] + (data_real[nb_clust+k]-data_bary[nb_clust+k])*data_weight[k]
  }
  colnames(stimuli_correct) <- c("new_x","new_y")
  return(stimuli_correct)
}

stimuli_correct <- gaze_stimuli_combi(stimuli,real_pivot,bary_pivot,weight,area_number) 

# save file in data folder
save(stimuli_correct,file="data/stimuli_rdy.RData")








