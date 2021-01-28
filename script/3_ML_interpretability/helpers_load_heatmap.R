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
  

  img_path = list.files(paste0(path_abs,method_name),full.names = TRUE)
  
  name <- list.files(paste0(path_abs,method_name),full.names = FALSE)
  name <- gsub(".png","",name)
  
  # load the img according the img_path column 
  img <- lapply(img_path, FUN = readPNG) # list with the img per evaluator
  
  # Creation of img as an array, input for the model with explicative variables (images)
  heat_img <- list()
  heat_img$x <- array(0, c(length(img), height_size, width_size,3)) # image size 


  # reshape heat_img$x
  for (k in 1:length(img)) {
    heat_img$x[k,,,] <- img[[k]][,,] 
  }
  
  # add a second element in the array which is the variable to explain : liking output
  # need to ensure the fact that the liking vector is in the same order like the heat_img array
  
  consumers_data$consu_id = paste0(consumers_data$nom, "_", consumers_data$label)
  
  consumers_data <- consumers_data %>% filter(consu_id %in% name)
  
  heat_img$y <- as.factor(consumers_data$liking)
  
  heat_img$y <- as.numeric(recode(heat_img$y , "aime" = "1", "aime_pas" = "2"))
  
  heat_img$img_path = img_path
  
  heat_img$name = name
  
  return(heat_img)
}

