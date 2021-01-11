# script to execute the heatmap generation 


########################### parameters #######################

consumers_name = "2-tonio"
screen_size_input = c(9,16)

# default parameters
start_time = rep(5,17)
end_time = rep(60,17)

##############################################################

# script to obtain the gaze data on the stimuli (16)
source("script/1_data_processing/main_data_process.R")


# sourcing helpers script for heatmap
source("script/2_heatmap/helpers_heatmap_generator.R")

# execute heatmap function 

for(i in 2:length(stimu_lvl)){
  
  heatmap_generator(df_corrected[df_corrected$stimu == stimu_lvl[i],])
  
}






