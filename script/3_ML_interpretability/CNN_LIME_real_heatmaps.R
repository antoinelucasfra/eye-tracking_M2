source("script/requirements.R")

# Script to load heatmaps data and their 
# associated liking in a compatible shape for CNN model inputs

# loading labeled data 
time_user_exp <- read.csv("data/time_user_exp.csv", sep = ",", header = TRUE)

##################################
##### CNN ########################
##################################

# sourcing pre-process script 
source("script/3_ML_interpretability/helpers_load_heatmap.R")

### load the heatmaps from the concerned folder ###

# temp variable declaration

method_name <- "raw_image"

path_abs <- "data/inputs_ML/"

channel <- 3
height_size <- 360
width_size <- 640

# execute function

cnn_input <- loader_img(method_name = method_name, consumers_data = time_user_exp)

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
  layer_dense(units = 1, activation = "softmax")

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

summary(model)

# train the model and evaluate it

history <- model %>%
  fit(
    x = cnn_input$x, y = cnn_input$y,
    epochs = 4,
    validation_split = 0.2,
    verbose = 1
  )

plot(history)
