#' load heatmap function
#'
#' @param path_abs absolute path of the heatmap locations
#' @param method_name name of the method employed to generate heatmaps
#' @param consumers_data the dataframe with consumers informations 
#' @param height_size height of the generated heatmaps
#' @param width_size width of the generated heatmaps 

loader_heatmap <- function (path_abs = "data/input_ML_",method_name, consumers_data, height_size=360, 
                            width_size=640){
  
  time_user$path_img = paste0(path_abs,method_name,"/",time_user$nom,"_",time_user$label,".png")
  
  # load the img according the path_img column 
  
  img <- lapply(time_user$path_img[1:16], FUN = readPNG) # list with the 16 img per evaluator
  
  # Creation of img as an array, input for the model with explicative variables (images)
  heat_img <- list()
  heat_img$x <- array(0, c(length(img), height_size, width_size,3)) # image size 
  
  dim(heat_img$x)
  dim(img[[1]])
  
  # reshape heat_img$x
  for (k in 1:length(img)) {
    heat_img$x[k,,,] <- img[[k]][,,] 
  }
  
  # add a second element in the array which is the variable to explain : liking output
  # need to ensure the fact that the liking vector is in the same order like the heat_img array
  
  heat_img$y <- time_user$liking[1:16]
  return(heat_img)
}