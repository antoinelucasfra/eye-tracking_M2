# Scrip to load heatmaps data in a compatible and their associated liking in a compatible shape for model inputs

# loading labeled data 
time_user_exp  = read.csv("data/time_user_exp.csv", sep = ",", header = TRUE)
col = colnames(time_user_exp)

# create the id (like .png start name) 
time_user_exp$nom <- paste0(time_user_exp$ordre,"-",time_user_exp$nom)

# keep only selected columns
time_user <- time_user_exp %>% select(nom, starts_with("stimu"),ends_with("liking")) 

# manage colnames 
colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)] <- paste0(colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)],"_label")
colnames(time_user) <- str_replace_all(colnames(time_user),"X","stimu")

# convert into character
time_user <- as.data.frame(apply(X =  time_user, FUN = as.character, MARGIN = 2))

# make a pivot table to have the right shape of the data
time_user <- time_user %>% pivot_longer(cols = -nom,
                           names_to = c("stimu",".value"),
                           names_sep = "_", 
                           values_ptypes = list(.value=character())) 


##################################
##### CNN ########################
##################################

# sourcing pre-process script 
source("script/3_ML_interpretability/helpers_load_heatmap.R")

### load the heatmaps from the concerned folder ###

# temp variable declaration
method_name = "basic"
path_abs = "data/input_ML_"
channel = 3
height_size = 360
width_size = 640

# execute function
cnn_input <- loader_heatmap(method_name = method_name, consumers_data = time_user)


#### create train/test dataset ###

# get full lists of img
list_files <- list.files(path = paste0(path_abs,method_name))

n_train = round(length(list_files) * 0.7)
ind_train = sample(1:length(list_files), n_train )

#train
x_train <- cnn_input$x[ind_train,,,]
y_train <- as.array(cnn_input$y)[ind_train]

#test
x_test <- cnn_input$x[-ind_train,,,]
y_test <- as.array(cnn_input$y)[-ind_train]


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
    epochs = 4, batch_size = 4,
    validation_data = list(x_test,y_test),
    verbose = 1
  )

plot(history)

























