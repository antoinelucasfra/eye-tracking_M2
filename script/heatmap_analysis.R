#### Detourer automatiquement ####
library(SuperpixelImageSegmentation)
library(OpenImageR)

init = Image_Segmentation$new()

im = OpenImageR::readImage("/Volumes/THESE/Eye-tracking/Eye-tracking_AOI/600 x /Citroen DS3.png")
plot(as.raster(im))

spx = init$spixel_segmentation(input_image = im,
                               superpixel = 130, 
                               AP_data = TRUE,
                               use_median = TRUE,
                               sim_wL = 2, 
                               sim_wA = 20,
                               sim_wB = 4,
                               sim_color_radius = 11,
                               return_labels_2_dimensionsional = TRUE,
                               # kmeans_method = "kmeans",
                               # kmeans_initializer = "kmeans++",
                               # kmeans_num_init = 3,
                               # kmeans_max_iters = 100,
                               verbose = TRUE)


OpenImageR::imageShow(spx$AP_image_data)