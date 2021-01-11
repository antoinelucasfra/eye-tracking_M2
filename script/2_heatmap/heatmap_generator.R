library(truncnorm)
library(png)
library(grid)
library(ggplot2)


heatmap_generator = function(path_img = "img/flav_thvnrd_LI.png", width_size = 50, height_size = 50, sup_img = TRUE, transparancy_img = 0.8, record_file = data){
  
  #get the background image
  img <- readPNG(path_img)
  img <- rasterGrob(img, interpolate=TRUE)

  if (sup_img == TRUE){
    heatmap = ggplot(data, aes(x = x, y = y) ) +
      theme_void() +
      annotation_custom(img, xmin=0, xmax=16, ymin=0, ymax=9) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = transparancy_img) +
      scale_fill_distiller(palette= "Spectral", direction=-1) +
      scale_x_continuous(limits = c(0, 16)) +
      scale_y_continuous(limits = c(0, 9)) +
      coord_fixed(ratio = 1, xlim = c(0,16), ylim = c(0,9))+
      theme(
        legend.position='none', 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        )
    }
  else {
    heatmap = ggplot(data, aes(x=x, y=y) ) +
      theme_void() +
    #annotation_custom(img, xmin=0, xmax=16, ymin=0, ymax=9) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = transp) +
      scale_fill_distiller(palette= "Spectral", direction=-1) +
      scale_x_continuous(limits = c(0, 16)) +
      scale_y_continuous(limits = c(0, 9)) +
      coord_fixed(ratio = 1, xlim = c(0,16), ylim = c(0,9))+
      theme(
        legend.position='none', 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
      )
  }
    
  # save la heatmaps en format png
  png(file="img/fake_img/fake.png", width=width_size, height=height_size)
  plot(heatmap)
  dev.off()
}

