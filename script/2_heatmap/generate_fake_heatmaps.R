library(truncnorm)
library(png)
library(grid)
library(ggplot2)

## function that is generating a simulated cognitive process through a heatmap
one_record <- function(repetition = 10, nb_clust = 7, like = 1, x_pos = 0.3){
  
  # take as input : 
  # repetition : the number of points per cluster
  # cluster : the number of area fixed on screen 
  # liking appreciation : 0 for a left-side structure, 1 for a right-side structure
  
  df <- data.frame(x = NULL ,y = NULL, like = NULL)
  
  # if like == 1, then 0.3<x<1, right side part of the screen
  if (like == 1){
    for (k in 1:nb_clust){  
      y <- rnorm(n = repetition,mean = runif(1), sd = 0.05)
      x <- rtruncnorm(n = repetition, a = 0, b = 1, mean = runif(1,x_pos,1), sd = 0.05)
      
      x <- x * 31
      y <- y * 17.4
      like <- rep(1,repetition)
      df_temp <- data.frame(x,y, like)
      df <- rbind(df,df_temp)
    }
  }
  
  # if like == 0, then 0<x<0.7 left part side of the screen
  else{
    for (k in 1:nb_clust){  
      y <- rnorm(n = repetition,mean = runif(1), sd=0.05)
      x <- rtruncnorm(n=repetition, a=0, b=1, mean=runif(1,0,(1-x_pos)), sd=0.05)
      
      x <- x * 31
      y <- y * 17.4
      like <- rep(0,repetition)
      df_temp <- data.frame(x,y, like)
      df <- rbind(df,df_temp)
    }
  }
  return(df)
}

# create fake heatmap
fake_generator <- function(nb_record = 1, img = "663.png", 
                          width_size = 50, height_size = 50, 
                          sup_img = TRUE, transp = 0.8){

  # get background image :
  img_path <- paste0("experience/cockpit_utile/",img)
  img <- readPNG(img_path)
  img <- rasterGrob(img, interpolate=TRUE)
  
  # loop to create nb_record of liking appreciation, default is equal to 100 
  for (k in 1:nb_record){
    if (k%%2 == 0){
      l = 1
    }
    else {
      l = 0
    }
    
    # use the function one_record that generates coordinates data (x,y) according the liking appreciation 
    record <- one_record(like = l)
    
    # heatmap generation
    if (sup_img == TRUE){
      heatmap <- ggplot(record, aes(x=x, y=y) ) +
        theme_void() +
        annotation_custom(img, xmin=0, xmax=31, ymin=0, ymax=17.4) +
        stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = transp) +
        scale_fill_distiller(palette= "Spectral", direction=-1) +
        scale_x_continuous(limits = c(0, 31)) +
        scale_y_continuous(limits = c(0, 17.4)) +
        coord_fixed(ratio = 1, xlim = c(-0,31), ylim = c(-0,17.4))+
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
    
    # generate heatmap witout the stimuli background
    else {
      heatmap <- ggplot(record, aes(x=x, y=y) ) +
        theme_void() +
        stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, alpha = transp) +
        scale_fill_distiller(palette= "Spectral", direction=-1) +
        scale_x_continuous(limits = c(0, 31)) +
        scale_y_continuous(limits = c(0, 17.4)) +
        coord_fixed(ratio = 1, xlim = c(-0,31), ylim = c(-0,17.4))+
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
   
    # save heatmaps in .png format 
    png(file = paste0("data/imputs_ML/fake_img_med/fake",k,".png"), 
        width = width_size, 
        height = height_size)
    plot(heatmap)
    dev.off()
  }
}
