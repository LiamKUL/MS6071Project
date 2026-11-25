library(keras3)
library(EBImage)

project_root <- if (dir.exists(file.path(getwd(), "data"))) {
  getwd()
} else if (dir.exists(file.path(getwd(), "..", "data"))) {
  file.path(getwd(), "..")
} else {
  stop("Set the working directory to the repository root or code directory.")
}
project_root <- normalizePath(project_root, winslash = "/")

source(file.path(project_root, "code", "Detection_CNN_Data_Prep.R"))
source(file.path(project_root, "code", "Model Saver.R"))

set.seed(123)
tensorflow::set_random_seed(123)

############## CNN #######################

model <- keras_model_sequential(input_shape = c(image_size, image_size, 3)) |>
  layer_conv_2d(filters = 16, kernel_size = 3, padding = "same",
                activation = "relu") |>
  layer_max_pooling_2d(pool_size = 2) |>
  layer_conv_2d(filters = 32, kernel_size = 3, padding = "same",
                activation = "relu") |>
  layer_max_pooling_2d(pool_size = 2) |>
  layer_conv_2d(filters = 64, kernel_size = 3, padding = "same",
                activation = "relu") |>
  layer_max_pooling_2d(pool_size = 2) |>
  layer_conv_2d(filters = 128, kernel_size = 3, padding = "same",
                activation = "relu") |>
  layer_max_pooling_2d(pool_size = 2) |>
  layer_conv_2d(filters = 16, kernel_size = 3, padding = "same",
                activation = "relu") |>
  layer_conv_2d(filters = 32, kernel_size = 3, padding = "same",
                activation = "relu") |>
  layer_conv_2d(filters = 64, kernel_size = 3, padding = "same",
                activation = "relu") |>
  layer_conv_2d(filters = 5, kernel_size = 1, padding = "same",
                activation = "sigmoid")

model |> summary()


######### loss function ############

fracture_loss <- function(y_true, y_pred) {
  eps <- 1e-7

  obj_true <- y_true[,,,1:1]
  obj_pred <- y_pred[,,,1:1]
  obj_pred <- op_minimum(op_maximum(obj_pred, eps), 1 - eps)

  positive_loss <- -op_sum(obj_true * op_log(obj_pred)) /
    op_maximum(op_sum(obj_true), 1)

  negative_loss <- -op_sum((1 - obj_true) * op_log(1 - obj_pred)) /
    op_maximum(op_sum(1 - obj_true), 1)

  obj_loss <- 0.5 * positive_loss + 0.5 * negative_loss

  x_true <- y_true[,,,2:2]
  y_true_c <- y_true[,,,3:3]
  w_true <- y_true[,,,4:4] * grid_size
  h_true <- y_true[,,,5:5] * grid_size

  x_pred <- y_pred[,,,2:2]
  y_pred_c <- y_pred[,,,3:3]
  w_pred <- y_pred[,,,4:4] * grid_size
  h_pred <- y_pred[,,,5:5] * grid_size

  true_x1 <- x_true - w_true / 2
  true_y1 <- y_true_c - h_true / 2
  true_x2 <- x_true + w_true / 2
  true_y2 <- y_true_c + h_true / 2

  pred_x1 <- x_pred - w_pred / 2
  pred_y1 <- y_pred_c - h_pred / 2
  pred_x2 <- x_pred + w_pred / 2
  pred_y2 <- y_pred_c + h_pred / 2

  inter_w <- op_maximum(
    0,
    op_minimum(true_x2, pred_x2) - op_maximum(true_x1, pred_x1)
  )
  inter_h <- op_maximum(
    0,
    op_minimum(true_y2, pred_y2) - op_maximum(true_y1, pred_y1)
  )
  intersection <- inter_w * inter_h

  true_area <- w_true * h_true
  pred_area <- w_pred * h_pred
  union <- true_area + pred_area - intersection
  iou <- intersection / (union + eps)

  enclosing_w <- op_maximum(true_x2, pred_x2) -
    op_minimum(true_x1, pred_x1)
  enclosing_h <- op_maximum(true_y2, pred_y2) -
    op_minimum(true_y1, pred_y1)
  enclosing_area <- enclosing_w * enclosing_h

  giou <- iou - (enclosing_area - union) / (enclosing_area + eps)
  box_loss_per_cell <- 1 - giou
  box_loss <- op_sum(box_loss_per_cell * obj_true) /
    op_maximum(op_sum(obj_true), 1)

  obj_loss + box_loss
}


#### compile model
model |> compile(optimizer = "adam", loss = fracture_loss)

history <- model |> fit(
  x = X_train,
  y = y_train,
  validation_data = list(X_valid, y_valid),
  epochs = 7,
  batch_size = 16
)

plot(history)

y_valid_pred <- predict(model, X_valid, batch_size = 16)
y_test_pred <- predict(model, X_test, batch_size = 16)

save_fracture_models(
  project_root,
  detection_model = model
)
