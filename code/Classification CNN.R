project_root <- if (dir.exists(file.path(getwd(), "data"))) {
  getwd()
} else if (dir.exists(file.path(getwd(), "..", "data"))) {
  file.path(getwd(), "..")
} else {
  stop("Set the working directory to the repository root or code directory.")
}
project_root <- normalizePath(project_root, winslash = "/")
source(file.path(project_root, "code", "Image Loading.R"))
source(file.path(project_root, "code", "Model Saver.R"))
library(keras3)

###########################cnn for classification on images already cropped


class_names <- c(
  "Comminuted",
  "Greenstick",
  "Healthy",
  "Linear",
  "Oblique Displaced",
  "Oblique",
  "Segmental",
  "Spiral",
  "Transverse Displaced",
  "Transverse"
)

####### resizing cropped images to same dimensions
target_h <- 150
target_w <- 150

#####train
Cell_Images_resized <- lapply(Fracture_Images, function(img) {
  resize(img, w = target_w, h = target_h)})
#####validation
Cell_Images_resized_test <- lapply(valid_Fracture_Images, function(img) {
  resize(img, w = target_w, h = target_h)
})

display(Cell_Images_resized[[5]])
display(Cell_Images_resized_test[[2]])


##################### Converting images to arrays #####################

##### Training

image_arrays <- lapply(Cell_Images_resized, as.array)

image_dim <- dim(image_arrays[[1]])

x_train <- array(
  unlist(image_arrays, use.names = FALSE),
  dim = c(image_dim, length(image_arrays))
)

x_train <- aperm(x_train, c(4, 1, 2, 3))


##### Validation

image_arrays_test <- lapply(Cell_Images_resized_test, as.array)

image_dim_test <- dim(image_arrays_test[[1]])

x_test <- array(
  unlist(image_arrays_test, use.names = FALSE),
  dim = c(image_dim_test, length(image_arrays_test))
)

x_test <- aperm(x_test, c(4, 1, 2, 3))
################## Class labels ##################

y_train <- as.integer(train_Fracture_list$V1)
y_test  <- as.integer(valid_Fracture_list$V1)


##################### Check dimensions #####################

dim(x_train)
dim(x_test)

length(y_train)
length(y_test)



###########Class weighting #######################
class_counts <- table(factor(y_train, levels = 0:9))

class_weights <- length(y_train) /
  (length(class_counts) * as.numeric(class_counts))

class_weights <- setNames(class_weights, as.character(0:9))

class_weights

################## CNN model ##################

num_classes <- length(class_names)

model <- keras_model_sequential() |>
  layer_conv_2d(
    filters = 16,
    kernel_size = c(3, 3),
    activation = "relu",
    padding = "same",
    input_shape = dim(x_train)[2:4]
  ) |>
  layer_max_pooling_2d(pool_size = c(2, 2)) |>
  layer_conv_2d(
    filters = 32,
    kernel_size = c(3, 3),
    activation = "relu",
    padding = "same"
  ) |>
  layer_max_pooling_2d(pool_size = c(2, 2)) |>
  layer_conv_2d(
    filters = 64,
    kernel_size = c(3, 3),
    activation = "relu",
    padding = "same"
  ) |>
  layer_max_pooling_2d(pool_size = c(2, 2)) |>
  layer_conv_2d(
    filters = 128,
    kernel_size = c(3, 3),
    activation = "relu",
    padding = "same"
  ) |>
  layer_max_pooling_2d(pool_size = c(2, 2)) |>
  layer_conv_2d(
    filters = 256,
    kernel_size = c(3, 3),
    activation = "relu",
    padding = "same"
  ) |>
  layer_flatten() |>
  layer_dense(units = 256, activation = "relu") |>
  layer_dropout(rate = 0.4) |>
  layer_dense(units = num_classes, activation = "softmax")


################## Compile model ##################

model |> compile(
  optimizer = optimizer_adam(learning_rate = 0.0002),
  loss = "sparse_categorical_crossentropy",
  metrics = c("accuracy")
)


################## Train model ##################

history <- model |> fit(
  x = x_train,
  y = y_train,
  epochs = 25,
  batch_size = 32,
  validation_data = list(x_test, y_test),
  class_weight = class_weights
)


################## Model summary ##################

summary(model)

################## Plot training history ##################

plot(history)


############ Model eval ###########
# Predict classes for validation images
pred_prob <- predict(model, x_test)
y_pred <- max.col(pred_prob) - 1


# Convert numerical predictions to fracture names
y_pred_names <- class_names[y_pred + 1]

# Check predicted class distribution
table(y_pred_names)

# Confusion matrix
table(
  Actual = factor(y_test, levels = 0:9),
  Predicted = factor(y_pred, levels = 0:9)
)

# Evaluate overall validation performance
model |> evaluate(x_test, y_test)

save_fracture_models(
  project_root,
  classification_model = model,
  class_names = class_names
)