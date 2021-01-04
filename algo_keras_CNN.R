library(tensorflow)
library(keras)
library(png)

# on load la list d'image 
list_files = list.files("img/fake_img/", pattern = "*.png", full.names = TRUE)

# on les converti en png
image = lapply(X =list_files, FUN =readPNG)
# on les convert en RG (utile si on veut visualiser)
image_rg = lapply(image, grid::rasterGrob)


## modif plus nécessaire
# heat_img <- list()
# heat_img$x <- array(0, c(length(image), 70, 70,3)) # image size : 70*70
# 
# for (k in 1:length(image)) {
#   heat_img$x[k,,] <- image[[k]][,,] 
# }

# on passe notre liste d'image en un array 4d
image_matrix = simplify2array(image)
# on réarrange la strucuture de cet array : verifier la validité de cette ligne 
image_matrix = aperm(image_matrix, c(4,1,2,3))

heat_img$x = image_matrix
# y etant une serie de 1 ou 0 en variable catégorielle
heat_img$y = rep(c(1,0),length(image)/2)

x_train <- heat_img$x
y_train <- as.array(as.integer(heat_img$y))


dim(x_train)
dim(y_train)


# Algo muriel 

# on créé un model CNN
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(70, 70, 3)) %>%   # 70*70 pixels *3 couleurs
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

model %>% fit(x_train, y_train, epochs = 50) # on refait 50 fois le model

score <- model %>% evaluate(x_train, y_train, verbose = 0)
score



### Algo keras : avec troisieme dimension 


model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(70,70,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")
summary(model)

model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")


summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

dim(cifar$train$x)
class(cifar$train$x)
class(x_train)
dim(x_train)

dim(cifar$train$y)
class(cifar$train$y)
class(y_train)
dim(y_train)


history <- model %>% 
  fit(
    x = x_train, y = y_train,
    epochs = 10,
    verbose = 2
  )

plot(history)




#### code muriel ##


# traitement 
Img_train <- lapply(file_list, readJPEG)
#plot(as.raster(Img_train[[1]][,,1]))

heat_img <- list()
heat_img$x <- array(0, c(length(Img_train), 28, 28)) #28*28pixels
for (k in 1:length(Img_train)) {
  heat_img$x[k,,] <- Img_train[[k]][,,1] 
}

heat_img$y <- as.array(as.integer(Y))



x_train <- heat_img$x
y_train <- heat_img$y


Algomodel <- keras_model_sequential() %>% 
layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
              input_shape = c(32,32,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")
summary(model)

model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")


summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

history <- model %>% 
  fit(
    x = x_train, y = y_train,
    epochs = 10,
    verbose = 2
  )


plot(history)




#####################
### exemple keras : 
#####################


cifar <- dataset_cifar10()
dim(cifar$test$x)

class_names <- c('airplane', 'automobile', 'bird', 'cat', 'deer',
                 'dog', 'frog', 'horse', 'ship', 'truck')

index <- 1:30

par(mfcol = c(5,6), mar = rep(1, 4), oma = rep(0.2, 4))
cifar$train$x[index,,,] %>% 
  purrr::array_tree(1) %>%
  purrr::set_names(class_names[cifar$train$y[index] + 1]) %>% 
  purrr::map(as.raster, max = 255) %>%
  purrr::iwalk(~{plot(.x); title(.y)})

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(32,32,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")
summary(model)

model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")


summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

dim(cifar$train$y)

history <- model %>% 
  fit(
    x = cifar$train$x, y = cifar$train$y,
    epochs = 10,
    validation_data = unname(cifar$test),
    verbose = 2
  )



plot(history)