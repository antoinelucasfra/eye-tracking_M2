library(tensorflow)
library(keras)
library(png)
library(stringr)
library(lime)
library(magick)
library(reticulate)
library(abind)

source("generate_fake_heatmaps.R")


height_size = 180
width_size = 320
channel = 3

## Generate fake heatMaps if needed 
list_files = list.files("img/fake_img/", pattern = "*.png", full.names = TRUE)
if (length(list_files)==0) {
  fake_generator(100,
                 img = "left_right.png", 
                 height_size = height_size, 
                 width_size = width_size, 
                 sup_img = FALSE)
}


# Load heatMaps data
list_files = list.files("img/fake_img/", pattern = "*.png", full.names = TRUE)

# extract number in picture names
numextract <- function(string){str_extract(string, "[-+]?[0-9]*\\.?[0-9]+")
}
list_number = as.numeric(numextract(list_files))


# Convertion in png
image = lapply(X =list_files, FUN =readPNG)
# on les convert en RG (utile si on veut visualiser)
image_rg = lapply(image, grid::rasterGrob)


# Creation heat_img as array : 
heat_img <- list()
heat_img$x <- array(0, c(length(image), height_size, width_size,3)) # image size 

# reshape heat_img$x
for (k in 1:length(image)) {
  heat_img$x[k,,,] <- image[[k]][,,] 
}

# y is a serie of (0,1) according the name of the picture :pair then 0, else 1 
heat_img$y = ifelse(list_number %% 2 == 0, 1,0)




##################################

######### dataset train et test ##

##################################


n_train = round(length(list_files) * 0.7)
ind_train = sample(1:length(list_files),n_train )

#train
x_train <- heat_img$x[ind_train,,,]
y_train <- as.array(as.integer(heat_img$y))[ind_train]

#test
x_test <- heat_img$x[-ind_train,,,]
y_test <- as.array(as.integer(heat_img$y))[-ind_train]


###  keras algorithm

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(height_size,width_size,channel)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")

model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = "accuracy"
)


history <- model %>% 
  fit(
    x = x_train, y = y_train,
    epochs = 4,
    validation_data = list(x_test,y_test),
    verbose = 1
  )

plot(history)



#### Lime Interpretability #####


img_path <- "img/fake_img/fake43.png"
img_path2 <- "img/fake_img/fake100.png"

image_prep2 <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(180, 320))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}

plot_superpixels(img_path, n_superpixels = 10, weight = 10)

explainer2 <- lime(c(img_path, img_path2), model = model, preprocess =  image_prep2)
explanation2 <- explain(c(img_path, img_path2), explainer2,
                        n_labels = 2, n_features = 10, weight = 10,n_superpixels = 10,
                        background = "white")

exp <- as.data.frame(explanation2)
desagreable <- exp[exp$case == "fake43.png",]
plot_image_explanation(desagreable)
agreable <- exp[exp$case == "fake100.png",]
plot_image_explanation(agreable)
