######## script to process raw data from GazeRecorder #######
# obtain a nice heatmap + stimuli output #

# loading necessary packages

library(FactoMineR)
library(tidyverse)

# load raw data from GazeRecorder
# insert your own path for the GazeRecorder txt file  
data_gaze_raw <- read.table("data/record_23points_correction.txt")


# define time of eye-tracking evaluation (in seconds)
# start_time = 
# stop time =  

#' @param data 
#' @param screen_size 
#' function to pre-process raw data_gaze 
#' take as input the raw data to pre-process
#' return the pre-process data with screen size taken into account 
#' and time units changed from milliseconds to seconds
gaze_preprocess <- function(data,screen_size=screen_size_input){
  gaze_df <- data.frame(
    x = data[,1]*screen_size[2],
    y = (1 -data[,2])*screen_size[1],
    t = (data[,3] - data[,3][1])/1000
  )
  return(gaze_df)
}

#' function to split the data in two parts : correction and stimuli
#' @param data 
#' @param time_sep
#' take as input the pre-process data and 
#' return a list of the two elements splitted
split_time <- function(data,time_sep){
  df_calibration <- data %>% filter(t < time_sep)
  df_stimuli <- data %>% filter(t >= time_sep)
  return(list(df_calibration,df_stimuli))
}

#' function to run classification on the correction  
#' @param data 
#' @param pca_weights 
#' @param clust_number 
#' take as input the correction (early) phase of eye-tracking experiment
gaze_classif <- function(data,pca_weights=c(1,1,5),clust_number){
  res_pca <- FactoMineR::PCA(data,col.w = pca_weights,graph = F)
  res_hcpc <- FactoMineR::HCPC(res_pca,nb.clust = clust_number,consol = T,graph = F)
  return(res_hcpc$data.clust)
}

#' function to apply the correction method on correction data
#' @param data 
#' take as input the joined dataframe of observed and real point issued from the evaluation
#' and return a dataframe with the transformed columns according the correction employed
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


#' function to get dist and weights to apply linear combination on our stimuli data
#'
#' @param data_real 
#' @param data_barycenter 
#' @param data_stimuli 
#' @param nb_clust 
#' take as input 3 dataframe : the real points physically available on screen ; the real
#' points with the mean of observed points during correction phase ; 
#' and the stimuli data observed. And also the number of class decided earlier (matching the number
#' of real point physically displayed on screen).
#' return the different dataframes that are useful to apply the linear combination correction
gaze_dist_weight_df <- function(data_real,data_barycenter,data_stimuli,nb_clust=area_number){
  
  nb_line_stimuli  <- dim(data_stimuli)[1]
  
  # get dataframe with same values for real points
  df_real_dup <- data_real %>% 
    pivot_wider(names_from = name, 
                values_from = c(x_real,y_real)) %>% 
    slice(rep(1:n(),each=nb_line_stimuli))
  
  # get barycenter in row vector
  df_bary_dup <- data_barycenter %>% 
    select(mean_x_eye,mean_y_eye,group) %>% 
    pivot_wider(names_from = group, 
                values_from = c(mean_x_eye,mean_y_eye)) %>% 
    slice(rep(1:n(),each=nb_line_stimuli))
  
  # bind the 3 dataframe
  # df_bind_3 <- cbind(stimuli,df_real_dup,df_bary_dup)
  # create the distance variable from the (observed) barycenters
  
  dist <- data.frame(temp=rep(0,nb_line_stimuli)) # create an empty dataframe to add columns
  
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
  # create the 5 weights variable from each clusters
  
  weights <- data.frame(temp=rep(0,nb_line_stimuli))
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

#' function to apply the linear combination on stimuli data from the 
#' correction data 
#'
#' @param data_stimuli 
#' @param data_real 
#' @param data_bary 
#' @param data_weight 
#' @param nb_clust 
#'
#'take as input the 4 dataframes : the stimuli data points ; the real data points
#'pivoted for each class ; the pivot with barycenter for each class ; the weights 
#'calculated according our method
gaze_stimuli_combi <- function(data_stimuli,data_real,data_bary,data_weight,nb_clust=area_number){
  
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

################################################## 
######## execution of function definition ######## 
################################################## 

# execute the pre-process function
data_gaze <- gaze_preprocess(data_gaze_raw)

# execute the split function 
split <- split_time(data_gaze,12)

#get correction data
correction <- split[[1]]
# get stimuli data
stimuli <- split[[2]]

# define weights you want to attribute to each variable for PCA
pca_weights_input = c(1,1,5)

# execute the classification function 
data_class <- gaze_classif(correction)

# add coordinates of real data
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

############
####save####
############

# save file in data folder
save(stimuli_correct,file="data/stimuli_rdy.RData")


