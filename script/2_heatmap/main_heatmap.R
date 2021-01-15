# script to execute the heatmap generation 

library(gsheet)

########################### parameters #######################

consumers_name = "4-thomas"
consumers_number = 1
screen_size_input = c(9,16)

# default parameters
start_time = rep(5,17)
end_time = rep(60,17)

##############################################################

# download time_user_exp from google drive 

drive_path = "Eyetracking/1ter_resultats_exp/time_user_exp"
folder_path = "data/time_user_exp.csv"

drive_download(drive_path, path = folder_path, overwrite = TRUE)

time_user_exp  = read.csv(folder_path, sep = ",", header = TRUE)


start_time_vec = time_user_exp[,grepl( "start_time", col, fixed = TRUE)]
end_time_vec = time_user_exp[,grepl( "end_time", col, fixed = TRUE)]
  
# script to download all the screen recorder path from google drive 
source("script/1_data_processing/download_googleDrive_data.R")

# script to obtain the gaze data on the stimuli (16)
source("script/1_data_processing/main_data_process.R")

# sourcing helpers script for heatmap
source("script/2_heatmap/helpers_heatmap_generator.R")

# create heatmap folder
dir.create(paste0("data/gazedata/",consumers_name,"/heatmap"))

# extract the letters name from stimu_lvl to match each heatmap to their stimuli
list_img <- list.files("experience/cockpit_utile/")

# load the exp plan 
load("experience/plan_exp.RData")
# get the list order for a given consumer
consumers_list <- df_random[consumers_number,] 

# get the list_img in the order for a given consumer with their full path 
list_img_order <- paste0("experience/cockpit_utile/",
                         list_img[order(match(substr(list_img,start=1,stop = 3),
                                              consumers_list))])

# execute heatmap function 

for(i in 2:length(stimu_lvl)){
  # if no correction, execute the following lines
  col <- colnames(df_corrected)
  df_corrected <- df[,1:4]
  colnames(df_corrected) <- col
  heatmap_generator(df_corrected[as.character(df_corrected$stimu) == stimu_lvl[i],],
                    path_img = list_img_order[i-1], 
                    width_size = 640, height_size = 360, transparency_img = 0.6)
}



