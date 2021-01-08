
library(truncnorm)
library(png)
library(grid)
library(ggplot2)

## fonction qui génère une trajectoire du regard
one_record = function(repetition = 10, nb_clust = 7, like = 1, x_pos = 0.3){
  # prend en entree : 
  # repetition : le nombre de points par cluster
  # cluster : le nombre de zone fixé sur l'ecran 
  # like : 0 pour fixation a gauche, 1 pour une fixation a droite
  
  
  df = data.frame(x = NULL ,y = NULL, like = NULL)
  
  # si like ==1, alors 0.3<x<1, parti a droite de l'ecran
  if (like==1){
    for (k in 1:nb_clust){  
      y = rnorm(n = repetition,mean = runif(1), sd=0.05)
      x = rtruncnorm(n=repetition, a=0, b=1, mean=runif(1,x_pos,1), sd=0.05)
      
      x = x * 31
      y = y * 17.4
      like = rep(1,repetition)
      df_temp = data.frame(x,y, like)
      df = rbind(df,df_temp)
    }
  }
  
  # si like == 0, alors 0<x<0.7 parti a gauche de l'ecran
  else{
    for (k in 1:nb_clust){  
      y = rnorm(n = repetition,mean = runif(1), sd=0.05)
      x = rtruncnorm(n=repetition, a=0, b=1, mean=runif(1,0,(1-x_pos)), sd=0.05)
      
      x = x * 31
      y = y * 17.4
      like = rep(0,repetition)
      df_temp = data.frame(x,y, like)
      df = rbind(df,df_temp)
    }
  }
  return(df)
}


fake_generator = function(nb_record = 1, img = "flav_thvnrd_LI.png", 
                          width_size = 50, height_size = 50, sup_img = TRUE, transp = 0.8){

  # importer image, par default le liknedIn de flavie : 
  img_path = paste0("img/old/",img)
  img <- readPNG(img_path)
  img <- rasterGrob(img, interpolate=TRUE)
  
  # boucle pour créer n record, par default 100
  for (k in 1:nb_record){
    if (k%%2 == 0){
      l = 1
    }
    else {
      l = 0
    }
    
    #utilise la fonction one record qui génère des coordonnées x,y selon le like 
    record = one_record(like = l)
    
    #génère une heatmaps
    if (sup_img == TRUE){
      heatmap = ggplot(record, aes(x=x, y=y) ) +
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
    else {
      heatmap = ggplot(record, aes(x=x, y=y) ) +
        theme_void() +
        #annotation_custom(img, xmin=0, xmax=31, ymin=0, ymax=17.4) +
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
   
    # save la heatmaps en format png
    png(file=paste0("img/fake_img/fake",k,".png"), width=width_size, height=height_size)
    plot(heatmap)
    dev.off()
  }
}



