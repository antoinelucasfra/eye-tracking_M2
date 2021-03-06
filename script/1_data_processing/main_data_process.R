# script to process heatmap from the googledrive data obtained

#Position of calibration square

square_pos <- read.csv("experience/25_square_position.csv", sep =";", header = TRUE, row.names = 1, dec = ".", colClasses = c("col" = "factor", "xvec" = "numeric", 'yvec' = "numeric"))
square_pos$name <- rownames(square_pos)

################################################
# define area number we were looking on screen for calibration phase 
area_number <- dim(square_pos)[1]

# Load data from the study for the concerned consumer

consumers_path <- paste0("data/gazedata/", consumers_name)

list_stimuli <- list.files(consumers_path, full.names = FALSE)
n_stimuli <- length(list_stimuli)

# define an empty to dataframe to fill in the loop

df <- data.frame()
col <- c("x", "y", "t", "stimu", "consu", "rank")

for (i in 1:n_stimuli){
  
  path_stimuli <- paste0(consumers_path,"/", list_stimuli[i], "/ScreenRecorderPath.dat")
  temp_txt <- read.table(path_stimuli, skip = 1)
  
  temp_txt <- gaze_preprocess(temp_txt, screen_size = screen_size_input)
  
  df_temp <- cbind(temp_txt[,], 
                  as.factor(list_stimuli[i]),
                  as.factor(consumers_name),
                  as.factor(gsub("([0-9]+).*$", "\\1", list_stimuli[i])))
  colnames(df_temp) <- col
  
  df <- rbind(df,df_temp)
}

stimu_lvl <- levels(df$stimu)


correction <- df[(df$stimu == "0_correction") ,c("x", "y", "t")]
correction <- remove_first_time(correction, t0 = start_time_vec[1,1])
correction <- remove_last_time(correction, t1 = end_time_vec[1,1])

data_class <- gaze_classif(correction,pca_weights = c(1,1,50), clust_number = area_number)
data_class %>% group_by(clust) %>% rename(group=clust) %>% arrange(t)

# join each class points with real points position
df_join <- full_join(data_class, square_pos, by=c("clust"="name")) %>% 
  rename(x_eye = x, y_eye = y, group = clust)


#### supposing classification is well done

# get correction data with barycenter correction
df_trans <- gaze_correct_bary(df_join)
df_bary <- unique(df_trans[,c("group","mean_x_eye","mean_y_eye","xvec","yvec")])


#get only barycenter data for each observed area
df_corrected <- data.frame()
col <- c("x", "y", "t", "stimu")

for (k in 2:length(stimu_lvl)){
  
  stimuli_data <- df[df$stimu == stimu_lvl[k],]
  stimuli_data <- remove_first_time(stimuli_data, t0 = start_time_vec[1,k])
  stimuli_data <- remove_last_time(stimuli_data, t1 = end_time_vec[1,k])
  dist_weight <- gaze_dist_weight_df(square_pos,df_bary,stimuli_data)
  
  # separate each object created in dist_weight
  real_pivot <- dist_weight[[1]]
  bary_pivot <- dist_weight[[2]]
  weight <- dist_weight[[4]]
  dist <- dist_weight[[3]]
  
  # execute the linear combination
  stimuli_correct <- gaze_stimuli_combi(stimuli_data,real_pivot,bary_pivot,weight) 
  stimuli_correct$stimu <- stimu_lvl[k]
  df_corrected <- rbind(df_corrected, stimuli_correct)
}

