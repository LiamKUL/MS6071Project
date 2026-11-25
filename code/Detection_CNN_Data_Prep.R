library(EBImage)

project_root <- if (dir.exists(file.path(getwd(), "data"))) {
  getwd()
} else if (dir.exists(file.path(getwd(), "..", "data"))) {
  file.path(getwd(), "..")
} else {
  stop("Set the working directory to the repository root or code directory.")
}
project_root <- normalizePath(project_root, winslash = "/")
source(file.path(project_root, "code", "Image Loading.R"))


######################## preparing data ######################
image_size <- 256
grid_size <- 16
num_outputs <- 5

## putting yolo info into array for cnn to train on

y_train <- array(
  0,
  dim = c(length(train_img_names), grid_size, grid_size, num_outputs)
)


# filling arrays
for (i in seq_len(nrow(train_Fracture_list))) {
  img_id <- train_Fracture_list$Image[i]
  x <- train_Fracture_list$V2[i]
  y <- train_Fracture_list$V3[i]
  width <- train_Fracture_list$V4[i]
  height <- train_Fracture_list$V5[i]

  grid_x <- min(floor(x * grid_size) + 1, grid_size)
  grid_y <- min(floor(y * grid_size) + 1, grid_size)

  y_train[img_id, grid_y, grid_x, 1] <- 1
  y_train[img_id, grid_y, grid_x, 2] <- x * grid_size - floor(x * grid_size)
  y_train[img_id, grid_y, grid_x, 3] <- y * grid_size - floor(y * grid_size)
  y_train[img_id, grid_y, grid_x, 4] <- width
  y_train[img_id, grid_y, grid_x, 5] <- height
}


dim(y_train)

# putting yolo info into array for cnn to validate on
y_valid <- array(
  0,
  dim = c(length(valid_img_names), grid_size, grid_size, num_outputs)
)

for (i in seq_len(nrow(valid_Fracture_list))) {
  img_id <- valid_Fracture_list$Image[i]
  x <- valid_Fracture_list$V2[i]
  y <- valid_Fracture_list$V3[i]
  width <- valid_Fracture_list$V4[i]
  height <- valid_Fracture_list$V5[i]

  grid_x <- min(floor(x * grid_size) + 1, grid_size)
  grid_y <- min(floor(y * grid_size) + 1, grid_size)

  y_valid[img_id, grid_y, grid_x, 1] <- 1
  y_valid[img_id, grid_y, grid_x, 2] <- x * grid_size - floor(x * grid_size)
  y_valid[img_id, grid_y, grid_x, 3] <- y * grid_size - floor(y * grid_size)
  y_valid[img_id, grid_y, grid_x, 4] <- width
  y_valid[img_id, grid_y, grid_x, 5] <- height
}


# function to resize images and put as array

prepare_image <- function(file) {
  img <- readImage(file)
  img <- resize(img, w = image_size, h = image_size)
  as.array(img)
}

# Prepare training images
X_train <- array(
  0,
  dim = c(length(train_img_names), image_size, image_size, 3)
)

for (i in seq_along(train_img_names)) {
  X_train[i,,,] <- prepare_image(train_img_names[i])

  if (i %% 100 == 0) {
    cat("Training images processed:", i, "\n")
  }
}

# Prepare validation images
X_valid <- array(
  0,
  dim = c(length(valid_img_names), image_size, image_size, 3)
)

for (i in seq_along(valid_img_names)) {
  X_valid[i,,,] <- prepare_image(valid_img_names[i])

  if (i %% 50 == 0) {
    cat("Validation images processed:", i, "\n")
  }
}

dim(X_train)
dim(X_valid)

range(X_train)
range(X_valid)


######## test
## putting yolo info into array for cnn to test on

y_test <- array(
  0,
  dim = c(length(test_img_names), grid_size, grid_size, num_outputs)
)

for (i in seq_len(nrow(test_Fracture_list))) {
  img_id <- test_Fracture_list$Image[i]
  x <- test_Fracture_list$V2[i]
  y <- test_Fracture_list$V3[i]
  width <- test_Fracture_list$V4[i]
  height <- test_Fracture_list$V5[i]

  grid_x <- min(floor(x * grid_size) + 1, grid_size)
  grid_y <- min(floor(y * grid_size) + 1, grid_size)

  y_test[img_id, grid_y, grid_x, 1] <- 1
  y_test[img_id, grid_y, grid_x, 2] <- x * grid_size - floor(x * grid_size)
  y_test[img_id, grid_y, grid_x, 3] <- y * grid_size - floor(y * grid_size)
  y_test[img_id, grid_y, grid_x, 4] <- width
  y_test[img_id, grid_y, grid_x, 5] <- height
}


dim(y_test)

# Prepare testing images
X_test <- array(
  0,
  dim = c(length(test_img_names), image_size, image_size, 3)
)

for (i in seq_along(test_img_names)) {
  X_test[i,,,] <- prepare_image(test_img_names[i])

  if (i %% 100 == 0) {
    cat("Testing images processed:", i, "\n")
  }
}

dim(X_test)
range(X_test)
