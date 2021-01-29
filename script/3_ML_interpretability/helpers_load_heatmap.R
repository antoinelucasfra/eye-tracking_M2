#' load heatmap function
#'
#' @param path_abs absolute path of the heatmap locations
#' @param method_name name of the method employed to generate heatmaps
#' @param consumers_data the dataframe with consumers informations 
#' @param height_size height of the generated heatmaps
#' @param width_size width of the generated heatmaps 

loader_img <- function (path_abs = "data/inputs_ML/", 
                        method_name, 
                        consumers_data,
                        height_size=360, 
                        width_size=640){
  
  col = colnames(consumers_data)
  
  # create the id (like .png start name) 
  consumers_data$nom <- paste0(consumers_data$ordre,"-",consumers_data$nom)
  
  # keep only selected columns
  time_user <- consumers_data %>% select(nom, starts_with("stimu"),ends_with("liking")) 
  
  # manage colnames 
  colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)] <- paste0(colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)],"_label")
  colnames(time_user) <- str_replace_all(colnames(time_user),"X","stimu")
  
  # convert into character
  time_user <- as.data.frame(apply(X =  time_user, FUN = as.character, MARGIN = 2))
  
  # make a pivot table to have the right shape of the data
  time_user <- time_user %>% pivot_longer(cols = -nom,
                                          names_to = c("stimu",".value"),
                                          names_sep = "_", 
                                          values_ptypes = list(.value=character())) 

  img_path = list.files(paste0(path_abs,method_name),full.names = TRUE)
  img_path_short <- gsub(".png","",
                         list.files(paste0(path_abs,method_name),full.names = FALSE))
  
  # load the img according the img_path column 
  img <- lapply(img_path, FUN = readPNG) # list with the img per evaluator
  
  # Creation of img as an array, input for the model with explicative variables (images)
  heat_img <- list()
  heat_img$x <- array(0, c(length(img), height_size, width_size,3)) # image size 

  # reshape heat_img$x
  for (k in 1:length(img)) {
    heat_img$x[k,,,] <- img[[k]][,,] 
  }
  
  # add a second element in the list which is the variable to explain : liking output
  # need to ensure the fact that the liking vector is in the same order as the heat_img array
  
  # create the consu_id to match with path_img labels 
  time_user$consu_id = paste0(time_user$nom, "_", time_user$label)
  
  # keep only relevant columns 
  consumers_data_ordered <- time_user %>% 
    filter(consu_id %in% img_path_short) %>% 
    select(consu_id,liking)
  
  # reorder the liking vector like the image order
  consumers_data_final <- consumers_data_ordered[match(img_path_short,
                                                       consumers_data_ordered$consu_id),]

  # recode the liking information in a keras-friendly output 
  heat_img$y <- as.factor(consumers_data_final$liking)
  heat_img$y <- as.integer(recode(heat_img$y , "aime" = "1", "aime_pas" = "2"))
  
  heat_img$img_path_short = img_path_short
  
  # delete these line because info is in img already in the same order as the images 
  # heat_img$name = name
  
  return(heat_img)
}
