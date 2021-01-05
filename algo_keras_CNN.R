library(tensorflow)
library(keras)
library(png)
library(stringr)
library(lime)
library(magick)
library(reticulate)
library(abind)


#peut etre pas utile
library(imager)


source("generate_fake_heatmaps.R")


height_size = 180
width_size = 320
channel = 3

### only if requiered
# fake_generator(100,img = "left_right.png", height_size = height_size, width_size = width_size, sup_img = TRUE)


# on load la list d'image 
list_files = list.files("img/fake_img/", pattern = "*.png", full.names = TRUE)


numextract <- function(string){str_extract(string, "[-+]?[0-9]*\\.?[0-9]+")
}
list_number = as.numeric(numextract(list_files))


# on les converti en png
image = lapply(X =list_files, FUN =readPNG)
# on les convert en RG (utile si on veut visualiser)
image_rg = lapply(image, grid::rasterGrob)


# Creation heat_img en array : 
heat_img <- list()
heat_img$x <- array(0, c(length(image), height_size, width_size,3)) # image size 

# reshape heat_img$x
for (k in 1:length(image)) {
  heat_img$x[k,,,] <- image[[k]][,,] 
}

# y etant une serie de 1 ou 0 en variable catégorielle
heat_img$y = ifelse(list_number %% 2 == 0, 1,0)

# jdd train et test

n_train = round(length(list_files) * 0.7)
ind_train = sample(1:length(list_files),n_train )

#train
x_train <- heat_img$x[ind_train,,,]
y_train <- as.array(as.integer(heat_img$y))[ind_train]

#test
x_test <- heat_img$x[-ind_train,,,]
y_test <- as.array(as.integer(heat_img$y))[-ind_train]

# ####### Algo muriel ########
# 
# # on créé un model CNN
# model <- keras_model_sequential()
# model %>%
#   layer_flatten(input_shape = c(height_size, width_size, 3)) %>%   # 70*70 pixels *3 couleurs
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dense(units = 1, activation = 'sigmoid')
# 
# model %>% compile(
#   optimizer = 'adam',
#   loss = 'binary_crossentropy',
#   metrics = c('accuracy')
# )
# 
# model %>% fit(x_train, y_train, epochs = 10) # on refait n fois le model
# 
# score <- model %>% evaluate(x_train, y_train, verbose = 1)
# score
# 
# 
# # prediction sur nos data
# model %>%keras::predict_classes(x_train, verbose = 1)


### Algo keras : avec troisieme dimension 
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


summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = "accuracy"
)


history <- model %>% 
  fit(
    x = x_train, y = y_train,
    epochs = 10,
    validation_data = list(x_test,y_test),
    verbose = 1
  )

plot(history)





#### Interpretability #####

# lime

img_path <- "img/fake_img/fake1.png"
img_path2 <- "img/fake_img/fake2.png"

image_prep2 <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(180, 320))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}

explainer2 <- lime(c(img_path, img_path2), model = model, preprocess =  image_prep2)
explanation2 <- explain(c(img_path, img_path2), explainer2,
                        n_labels = 2, n_features = 10, weight = 5,
                        background = "white")

exp <- as.data.frame(explanation2)
desagreable <- exp[exp$case == "nom de la voiture classé dans j'aime",]
plot_image_explanation(desagreable)
agreable <- exp[exp$case == "nom de la voiture classé dans j'aime pas.jpg",]
plot_image_explanation(agreable)

### exemple 

img_path <- system.file('extdata', 'produce.png', package = 'lime')
# load a predefined image classifier
model <- application_vgg16(
  weights = "imagenet",
  include_top = TRUE
)

# create a function that prepare images for the model
img_preprocess <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(224,224))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind, c(arrays, list(along = 1)))
}

# Create an explainer (lime recognise the path as an image)
explainer <- lime(img_path, as_classifier(model, unlist(labels)), img_preprocess)

# Explain the model (can take a long time depending on your system)
explanation <- explain(img_path, explainer, n_labels = 2, n_features = 10, n_superpixels = 70)

plot_image_explanation(explanation)





