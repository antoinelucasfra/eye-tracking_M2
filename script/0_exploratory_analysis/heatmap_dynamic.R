
library(eyetrackingR)

library(parameters)
library(NbClust)

library(plyr)

source("script/requirements.R")
source("script/1_data_processing/helpers_data_process.R")

data = read.table("data/gazedata/18-damien/Result2/ScreenRecorderPath.dat", skip = 1, header = FALSE)

data = data[,1:3]

data = gaze_preprocess(data, c(9,16))

data = remove_first_time(data,15)
data = remove_last_time(data,27.51)


path_img ="experience/cockpit_utile/112.png"
img <- readPNG(path_img)
img <- rasterGrob(img, interpolate=TRUE)

data.pca = PCA(data, col.w = c(1,1,1))

data.hcpc = HCPC(res = data.pca, nb.clust = -1)



find_hull <- function(df) df[chull(df$x, df$y), ]
poly <- ddply(data.hcpc$data.clust, "clust", find_hull)

plot <- ggplot(data = data.hcpc$data.clust, aes(x = x,  y = y, colour=clust, fill = clust)) +
  annotation_custom(img, xmin=0, xmax=16, ymin=0, ymax=9) +
  geom_point() + 
  geom_polygon(data = poly, alpha = 0.5) +
  labs(x = "x", y = "y")+
  
  coord_fixed(ratio = 1, xlim = c(-10, 18), ylim = c(-3, 12)) 
plot

cluster = data.hcpc$data.clust


ggplot()+
  annotation_custom(img, xmin=0, xmax=16, ymin=0, ymax=9) +
  geom_point(data = cluster, aes(x = x,  y = y, colour = clust)) + 
  geom_polygon(data = poly, aes(x = x,  y = y, colour=clust, fill = clust), alpha = 0.5) +
  coord_fixed(ratio = 1, xlim = c(-10, 18), ylim = c(-3, 12)) 


ggplot() +
    annotation_custom(img, xmin=0, xmax=16, ymin=0, ymax=9) +
  
  stat_density_2d_filled(data = cluster , aes(x = x,  y = y, fill = ..density..), 
                  geom = "raster", 
                  contour = FALSE,
                  contour_var = "count",
                  alpha = 0.8) +
  geom_point(data = data.hcpc$data.clust, aes(x = x,  y = y, colour = t, shape = clust)) +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  scale_x_continuous(limits = c(-10, 18)) +
  scale_y_continuous(limits = c(-3, 12)) 
  
 




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
