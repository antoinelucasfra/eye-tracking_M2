library(eyetrackingR)
library(parameters)
install.packages("NbClust")
library(NbClust)

data = read.table("data/gazedata/2-romane/Result5/ScreenRecorderPath.dat", skip = 1, header = FALSE)
data = data[,1:3]
data = gaze_preprocess(data, c(9,16))
data = remove_first_time(data,15)
data = remove_last_time(data,27.51)
path_img ="experience/cockpit_utile/112.png"
img <- readPNG(path_img)
img <- rasterGrob(img, interpolate=TRUE)
data.pca = PCA(data, col.w = c(1,1,1000))
data.hcpc = HCPC(res = data.pca, nb.clust = -1)
data.classif = cluster_analysis(data, package = "NbClust")

install.packages("see")
library(see)
plot(data.classif)

ggplot() +  theme_void() +
  annotation_custom(img, xmin=0, xmax=16, ymin=0, ymax=9) +  
  stat_density_2d_filled(data = data.hcpc$data.clust, 
                         aes(x = x,  y = y, fill = ..density..),
                         geom = "raster",
                         contour = FALSE,
                         contour_var = "count",
                         alpha = 0.8) +
  
  geom_point(data = data.hcpc$data.clust, aes(x = x,  y = y, colour = t, shape = clust)) +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  scale_x_continuous(limits = c(-10, 18)) +
  scale_y_continuous(limits = c(-3, 12)) +  
  coord_fixed(ratio = 1, xlim = c(-10, 18), ylim = c(-3, 12))







