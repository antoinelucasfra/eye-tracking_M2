

#' loader data from the experience
#'
#' @param path the data path
#' @param screen_size_input size of the screen, (x,y)
#'
#' @return a dataframe with all data,  c( x, y, t, stimu, consu, rank)
#'
#' @examples
#' loader_data()
loader_data = function (path = "data/gazedata/", screen_size_input = c(9,16)){
  
  list_consu = list.files(path)
  
  df =  data.frame()
  col = c("x", "y", "t", "stimu", "consu", "rank")
  for (k in list_consu){
    path_temp = paste0(path, k)
    
    list_stimuli = list.files(path_temp, full.names = FALSE)
    n_stimuli = length(list_stimuli)
    
    # define an empty to dataframe to fill in the loop
    
    df_temp = data.frame()
  
    for (i in 1:n_stimuli){
      
      path_stimuli = paste0(path_temp,"/", list_stimuli[i], "/ScreenRecorderPath.dat")
      temp_txt <- read.table(path_stimuli, skip = 1)
      
      temp_txt = gaze_preprocess(temp_txt, screen_size = screen_size_input)
      
      df_tempbis = cbind(temp_txt[,], 
                      as.factor(list_stimuli[i]),
                      as.factor(k),
                      as.factor(gsub("([0-9]+).*$", "\\1", list_stimuli[i])))
      colnames(df_tempbis) = col
      
      df_temp = rbind(df_temp,df_tempbis)
    }
    df = rbind(df, df_temp)
  }
  
return(df)
} 

