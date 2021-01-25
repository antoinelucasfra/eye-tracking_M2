# source code function
source("script/1_data_processing/helpers_data_process.R")
consumers_name = "28-chloe"


#Position of calibration square
square_pos = read.csv("experience/25_square_position.csv", 
                      sep =";", header = TRUE, row.names = 1, dec = ".", 
                      colClasses = c("col" = "factor", "xvec" = "numeric", 'yvec' = "numeric"))
square_pos$name = rownames(square_pos)

correspondance = read.csv("experience/correspondance_stimu_couleur.csv", sep =";")

square_correspondance = left_join(square_pos,correspondance,by="col")


################################################
# define area number we were looking on screen for calibration phase 
area_number = dim(square_pos)[1]

# Load data from the study for the concerned consumer

consumers_path = paste0("data/gazedata/", consumers_name)

# define an empty to dataframe to fill in the loop

df = data.frame()
col = c("x", "y", "t", "stimu", "consu", "rank")

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

# rename df avec les noms des stimulis A FAIRE DANS MAIN 

plan_exp <- read.csv("experience/plan_exp.csv")[,-1]
consumers_list <- plan_exp[consumers_number,]


levels(df$stimu) <- c("0_correction",as.numeric(consumers_list))
consumers_list

time_user_exp


df_result1 <- df %>% filter(stimu %in% "Result1")


folder_path = "data/time_user_exp.csv"


time_user_exp  = read.csv(folder_path, sep = ",", header = TRUE)
col = colnames(time_user_exp)

start_time_vec = time_user_exp[time_user_exp$ordre == consumers_number ,grepl( "start_time", col, fixed = TRUE)]
end_time_vec = time_user_exp[time_user_exp$ordre == consumers_number ,grepl( "end_time", col, fixed = TRUE)]

 
df_result1_rm = remove_first_time(df_result1, t0 = start_time_vec[1,2])
df_result1_rm = remove_last_time(df_result1_rm, t1 = end_time_vec[1,2])

df_calib <- df_result1_rm %>% filter(t < 10 + start_time_vec[1,2])

df_result1_rm_nocalib <- df_result1_rm %>% filter(t > 10 + start_time_vec[1,2] )





ggplot() +
  geom_point(df_result1_rm_nocalib,aes(x,y)) +
  geom_point(square_correspondance %>% filter(stimu=))



ggplot(df_calib, aes(x = x, y = y) ) +
  
  # theme_void() +
  
  stat_density_2d(aes(fill = ..density..), 
                  geom = "raster", 
                  contour = F, 
                  contour_var = "ndensity",
                  n = 200, # use to adjust density estimation and grid quality 
                  # h = c(5,5),
                  alpha = 0.8) +
  
  scale_fill_distiller(palette= "Spectral", direction=-1) 
  scale_x_continuous(limits = c(0, 16)) +
  scale_y_continuous(limits = c(0, 9)) +
  
  # coord_fixed(ratio = 1, xlim = c(0,16), ylim = c(0,9))+
  
  theme(
    legend.position='none', 
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot(df_result1_rm_nocalib, aes(x = x, y = y) ) +
    
    # theme_void() +
    
    stat_density_2d(aes(fill = ..density..), 
                    geom = "raster", 
                    contour = F, 
                    contour_var = "ndensity",
                    n = 200, # use to adjust density estimation and grid quality 
                    # h = c(5,5),
                    alpha = 0.8) +
    
    scale_fill_distiller(palette= "Spectral", direction=-1) 
  # scale_x_continuous(limits = c(0, 16)) +
  # scale_y_continuous(limits = c(0, 9)) +
  
  # coord_fixed(ratio = 1, xlim = c(0,16), ylim = c(0,9))+
  
  theme(
    legend.position='none', 
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )







