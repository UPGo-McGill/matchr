### ML experiments #############################################################

library(tidyverse)
library(keras)
use_virtualenv("~/tf_env", required = TRUE)

class_names <- c("inside", "outside", "other")
train_image_files_path <- "/Users/dwachsmuth/Desktop/img_testing/ML"

# number of output classes (i.e. fruits)
output_n <- length(class_names)

# image size to scale down to (original images are 100 x 100 px)
img_width <- img_height <- 50
target_size <- c(img_width, img_height)

# RGB = 3 channels
channels <- 3

# define batch size
batch_size <- 32

train_data_gen <- image_data_generator(rescale = 1/255, validation_split = 0.3)

# training images
train_image_array_gen <- 
  flow_images_from_directory(
    train_image_files_path, train_data_gen, subset = 'training', 
    target_size = target_size, class_mode = "categorical", 
    classes = class_names, batch_size = batch_size, seed = 42)

# validation images
valid_image_array_gen <- 
  flow_images_from_directory(
    train_image_files_path, train_data_gen, subset = 'validation',
    target_size = target_size, class_mode = "categorical", 
    classes = class_names, batch_size = batch_size, seed = 42)

# Number of samples and epochs
train_samples <- train_image_array_gen$n
valid_samples <- valid_image_array_gen$n
epochs <- 20

# initialise model
model <- 
  keras_model_sequential() %>% 
  # add layers
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", 
                input_shape = c(img_width, img_height, channels)) %>%
  layer_activation("relu") %>%
  
  # Second hidden layer
  layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  # Project outputs from dense layer onto output layer
  layer_dense(output_n) %>% 
  layer_activation("softmax")

# compile
model %>% 
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
    metrics = "accuracy")

# fit
hist <- 
  model %>% 
  fit(train_image_array_gen, epochs = epochs, 
      validation_data = valid_image_array_gen)
