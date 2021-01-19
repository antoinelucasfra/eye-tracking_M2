
#' Function to generate heatmaps from the stimuli dataframe 
#'
#' @param data dataframe to generate heatmaps
#' @param path_img path of the stimuli picture to have in a vector
#' @param width_size number of pixels on width heatmap
#' @param height_size number of pixels on height heatmap
#' @param add_img TRUE/FALSE argument to decide whether or not to add the background stimuli on the heatmap
#' @param transparency_img coefficient to modulate the transparency of the heatmap 

heatmap_generator = function(data,
                             path_img = "experience/stimuli_img/Audi e-tron- COCKPIT.png", 
                             width_size = 160, 
                             height_size = 90, 
                             add_img = TRUE, 
                             transparency_img = 0.8){
  
  #get the background image
  img <- readPNG(path_img)
  img <- rasterGrob(img, interpolate=TRUE)

  if (add_img == TRUE){
    
    heatmap = ggplot(data, aes(x = new_x, y = new_y) ) +
      
      theme_void() +
      annotation_custom(img, xmin=0, xmax=16, ymin=0, ymax=9) +
      
      stat_density_2d(aes(fill = ..density..), 
                      geom = "raster", 
                      contour = FALSE, 
                      alpha = transparency_img) +
      
      scale_fill_distiller(palette= "Spectral", direction=-1) +
      scale_x_continuous(limits = c(0, 16)) +
      scale_y_continuous(limits = c(0, 9)) +
      
      coord_fixed(ratio = 1, xlim = c(0,16), ylim = c(0,9)) +
      
      theme(
        legend.position='none', 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )
  }
  
  else {
    
    heatmap = ggplot(data, aes(x = new_x, y = new_y) ) +
      
      theme_void() +
      
      stat_density_2d(aes(fill = ..density..), 
                      geom = "raster", 
                      contour = FALSE, 
                      alpha = transparency_img) +
      
      scale_fill_distiller(palette= "Spectral", direction=-1) +
      scale_x_continuous(limits = c(0, 16)) +
      scale_y_continuous(limits = c(0, 9)) +
      
      coord_fixed(ratio = 1, xlim = c(0,16), ylim = c(0,9))+
      
      theme(
        legend.position='none', 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  } 
    
  # save heatmap in the folder
  filename <- stimu_lvl[i]
  png(file = paste0("data/gazedata/",consumers_name,"/heatmap/",filename,".png"), 
      width = width_size, 
      height = height_size)
  plot(heatmap)
  dev.off()
}
