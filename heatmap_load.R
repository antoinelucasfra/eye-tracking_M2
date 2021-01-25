# Load heatmaps data in a nice dataframe and their associated liking 

# run each directory and keep in memory the file names and labelling 
# example : a dataframe with 2 columns : stimuli and label ?

time_user_exp  = read.csv("data/time_user_exp.csv", sep = ",", header = TRUE)
col = colnames(time_user_exp)

time_user_exp$nom <- paste0(time_user_exp$ordre,"-",time_user_exp$nom)

time_user <- time_user_exp %>% select(nom, starts_with("stimu"),ends_with("liking")) 

colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)] <- paste0(colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)],"_label")

colnames(time_user) <- str_replace_all(colnames(time_user),"X","stimu")


time_user <- as.data.frame(apply(X =  time_user, FUN = as.character, MARGIN = 2))

time_user <- time_user %>% pivot_longer(cols= -nom,
                           
                           names_to = c("stimu",".value"),
                           
                           names_sep = "_", 
                           
                           values_ptypes = list(.value=character())) 

method_name = "basic"
path_abs = "data/input_ML_"

loader_heatmap <- function (path_abs = "data/input_ML_",method_name){
  
  time_user$path_img = paste0(path_abs,method_name,"/",time_user$nom,"_",time_user$label)
  
  # list_heatmap <- list.files(paste0(path_abs,method_name))
  
  # load the img for each consumer folder 
  
  img_vec <- paste0(path_abs,method_name,"/",
                    list.files(paste0(path_abs,method_name)))
  
  
  
  
  img <- lapply(img_vec, FUN = readPNG) # list with the 16 img per evaluator
  liking <- 
  length(img)
  
}

readPNG("data/input_ML_basic/112.png")

for (k in 1:length(consumers_name_vec)){
  list_heatmap_consu <- list.files(paste0("data/gazedata/",
                                          consumers_name_vec[k],
                                          "/heatmap"))
  heatmap_full_list <- rbind(list_heatmap_consu)
}


list_files <- list.files("data/input_ML_basic/heatmap1/")


readPNG(list_files[1])





