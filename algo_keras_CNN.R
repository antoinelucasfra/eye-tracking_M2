library(tensorflow)
library(keras)
library(png)
library(stringr)

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


## modif plus nécessaire
heat_img <- list()
heat_img$x <- array(0, c(length(image), height_size, width_size,3)) # image size 

# reshape heat_img$x
for (k in 1:length(image)) {
  heat_img$x[k,,,] <- image[[k]][,,] 
}

# y etant une serie de 1 ou 0 en variable catégorielle
heat_img$y = ifelse(list_number %% 2 == 0, 1,0)

x_train <- heat_img$x
y_train <- as.array(as.integer(heat_img$y))


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
summary(model)

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
    verbose = 1
  )

plot(history)


