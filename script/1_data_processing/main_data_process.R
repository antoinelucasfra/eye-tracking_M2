# source code function
source("script/1_data_processing/data_process.R")
source("script/2_heatmap/generate_fake_heatmaps.R")


library(tidyr)

########################### parameters #######################

consumers_name = "1-Flavie"
screen_size_input = c(9,16)

# default parameters
start_time = rep(5,17)
end_time = rep(60,17)


#Position of calibration square
square_pos = read.csv("experience/25_square_position.csv", sep =";", header = TRUE, row.names = 1, dec = ".", colClasses = c("col" = "factor", "xvec" = "numeric", 'yvec' = "numeric"))
square_pos$name = rownames(square_pos)


################################################
# define area number we were looking on screen for calibration phase 
area_number = dim(square_pos)[1]


# Load data from the study for one consumer

consumers_path = paste0("data/Gazedata/", consumers_name)

df = data.frame()
col = c("x", "y", "t", "stimu", "consu", "rank")

list_stimuli = list.files(consumers_path, full.names = FALSE)
n_stimuli = length(list_stimuli)
  
for (i in 1:n_stimuli){
  
  path_stimuli = paste0(consumers_path,"/", list_stimuli[i], "/ScreenRecorderPath.dat")
  temp_txt <- read.table(path_stimuli, skip = 1)
  
  temp_txt = gaze_preprocess(temp_txt, screen_size = screen_size_input)
  
  df_temp = cbind(temp_txt[,], 
                  as.factor(list_stimuli[i]),
                  as.factor(consumers_name),
                  as.factor(gsub("([0-9]+).*$", "\\1", list_stimuli[i])))
  colnames(df_temp) = col
  
  df = rbind(df,df_temp)
}

stimu_lvl = levels(df$stimu)

correction = df[(df$stimu == "0_correction") ,c("x", "y", "t")]
correction = remove_first_time(correction, t0 = start_time[1])
correction = remove_last_time(correction, t1 = end_time[1])

data_class <- gaze_classif(correction,pca_weights = c(1,1,50), clust_number = area_number)
levels(data_class$clust)
data_class %>% group_by(clust) %>% rename(group=clust) %>% arrange(t)

# on fait une seq nb_clust (1:26) qu'on mutate sur le df summarisé et ensuite on allonge tout ça en pivot_long
# pour corriger l'ordre des classe : a faire 

#### autre idée :
# on affecte a chaque cluster, le point reel dont la distance cluster -> point réel est la plus faible.

    
  
# joindre les classe avec les vrai points 
df_join <- full_join(data_class, square_pos, by=c("clust"="name")) %>% 
  rename(x_eye = x, y_eye = y, group = clust)


  
#### suppose que la classif est bien faites : 

# get correction data with barycenter correction
df_trans <- gaze_correct_bary(df_join)
df_bary <- unique(df_trans[,c("group","mean_x_eye","mean_y_eye","xvec","yvec")])


#get only barycenter data for each observed area
df_corrected = data.frame()
col = c("x", "y", "t", "stimu")

for (k in 2:length(stimu_lvl)){
  
  stimuli_data <- df[df$stimu == stimu_lvl[k],]
  stimuli_data = remove_first_time(stimuli_data, t0 = start_time[k])
  stimuli_data = remove_last_time(stimuli_data, t1 = end_time[k])
  dist_weight <- gaze_dist_weight_df(square_pos,df_bary,stimuli_data)
  
  # separate each object created in dist_weight
  real_pivot <- dist_weight[[1]]
  bary_pivot <- dist_weight[[2]]
  weight <- dist_weight[[4]]
  dist <- dist_weight[[3]]
  
  # execute the linear combination
  stimuli_correct <- gaze_stimuli_combi(stimuli_data,real_pivot,bary_pivot,weight) 
  stimuli_correct$stimu <- stimu_lvl[k]
  df_corrected = rbind(df_corrected, stimuli_correct)
}


head(df_corrected)

levels(as.factor(df_corrected$stimu))

df_corrected %>% group_by(stimu) %>% count()



