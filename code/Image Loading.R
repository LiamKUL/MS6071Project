library(EBImage)
library(tidyverse)
library(magick)

set.seed(4)
keras3::set_random_seed(4)

project_root <- if (dir.exists(file.path(getwd(), "data"))) {
  getwd()
} else if (dir.exists(file.path(getwd(), "..", "data"))) {
  file.path(getwd(), "..")
} else {
  stop("Set the working directory to the repository root or code directory.")
}
project_root <- normalizePath(project_root, winslash = "/")
dataset_path <- file.path(project_root, "data",
                          "HBFMID"
)

######## Bone Fractures Detections

##################### TRAINING SET ######################

# Load images
train_img_names <- list.files(
  path = file.path(dataset_path, "train", "images"),
  pattern = "\\.jpg$",
  full.names = TRUE
)

train_images <- lapply(train_img_names, readImage)

# Dimensions of images
train_dimensions <- data.frame(unique(lapply(train_images, dim)))

# Load txt files
train_txt_names <- list.files(
  file.path(dataset_path, "train", "labels"),
  pattern = "\\.txt$",
  full.names = TRUE
)

train_txtfiles <- lapply(train_txt_names, read.table)

# Check images and txt files are indexed correctly
dir_strip <- function(x) {
  tools::file_path_sans_ext(basename(x))
}

# Add image number to each instance of a fracture
for (n in seq_along(train_txtfiles)) {
  train_txtfiles[[n]] <- cbind(
    train_txtfiles[[n]],
    Image = n
  )
}

# Create fracture list
train_Fracture_list <- do.call(rbind, train_txtfiles)

# Add fracture numbers
train_Fracture_list$Fracture_Number <- seq_len(
  nrow(train_Fracture_list)
)


##################### VALIDATION SET ######################

# Load images
valid_img_names <- list.files(
  path = file.path(dataset_path, "valid", "images"),
  pattern = "\\.jpg$",
  full.names = TRUE
)

valid_images <- lapply(valid_img_names, readImage)

# Dimensions of images
valid_dimensions <- data.frame(unique(lapply(valid_images, dim)))

# Load txt files
valid_txt_names <- list.files(
  file.path(dataset_path, "valid", "labels"),
  pattern = "\\.txt$",
  full.names = TRUE
)

valid_txtfiles <- lapply(valid_txt_names, read.table)


# Add image number to each instance of a fracture
for (n in seq_along(valid_txtfiles)) {
  valid_txtfiles[[n]] <- cbind(
    valid_txtfiles[[n]],
    Image = n
  )
}

# Create fracture list
valid_Fracture_list <- do.call(rbind, valid_txtfiles)

# Add fracture numbers
valid_Fracture_list$Fracture_Number <- seq_len(
  nrow(valid_Fracture_list)
)


##################### TEST SET ######################

# Load images
test_img_names <- list.files(
  path = file.path(dataset_path, "test", "images"),
  pattern = "\\.jpg$",
  full.names = TRUE
)

test_images <- lapply(test_img_names, readImage)

# Dimensions of images
test_dimensions <- data.frame(unique(lapply(test_images, dim)))

# Load txt files
test_txt_names <- list.files(
  file.path(dataset_path, "test", "labels"),
  pattern = "\\.txt$",
  full.names = TRUE
)

test_txtfiles <- lapply(test_txt_names, read.table)

# Add image number to each instance of a fracture
for (n in seq_along(test_txtfiles)) {
  test_txtfiles[[n]] <- cbind(
    test_txtfiles[[n]],
    Image = n
  )
}

# Create fracture list
test_Fracture_list <- do.call(rbind, test_txtfiles)

# Add fracture numbers
test_Fracture_list$Fracture_Number <- seq_len(
  nrow(test_Fracture_list)
)



##################### CROPPING TRAIN FRACTURES ###################

crop_Fr <- function(n) {
  as_EBImage(
    image_read(train_img_names[[train_Fracture_list[n, 6]]]) %>%
      image_crop(
        geometry = geometry_area(
          width = train_Fracture_list[n, 4] * 640,
          height = train_Fracture_list[n, 5] * 640,
          x_off = (train_Fracture_list[n, 2] * 640) -
            (train_Fracture_list[n, 4] * 640) / 2,
          y_off = (train_Fracture_list[n, 3] * 640) -
            (train_Fracture_list[n, 5] * 640) / 2
        )
      )
  )
}

Fracture_Images <- list()

n <- 1
while (n < nrow(train_Fracture_list) + 1) {
  Fracture_Images[[n]] <- crop_Fr(n)
  n <- n + 1
}


##################### CROPPING VALIDATION FRACTURES ###################

crop_valid_Fr <- function(n) {
  as_EBImage(
    image_read(valid_img_names[[valid_Fracture_list[n, 6]]]) %>%
      image_crop(
        geometry = geometry_area(
          width = valid_Fracture_list[n, 4] * 640,
          height = valid_Fracture_list[n, 5] * 640,
          x_off = (valid_Fracture_list[n, 2] * 640) -
            (valid_Fracture_list[n, 4] * 640) / 2,
          y_off = (valid_Fracture_list[n, 3] * 640) -
            (valid_Fracture_list[n, 5] * 640) / 2
        )
      )
  )
}

valid_Fracture_Images <- list()

n <- 1
while (n < nrow(valid_Fracture_list) + 1) {
  valid_Fracture_Images[[n]] <- crop_valid_Fr(n)
  n <- n + 1
}


##################### CROPPING TEST FRACTURES ###################

crop_test_Fr <- function(n) {
  as_EBImage(
    image_read(test_img_names[[test_Fracture_list[n, 6]]]) %>%
      image_crop(
        geometry = geometry_area(
          width = test_Fracture_list[n, 4] * 640,
          height = test_Fracture_list[n, 5] * 640,
          x_off = (test_Fracture_list[n, 2] * 640) -
            (test_Fracture_list[n, 4] * 640) / 2,
          y_off = (test_Fracture_list[n, 3] * 640) -
            (test_Fracture_list[n, 5] * 640) / 2
        )
      )
  )
}

test_Fracture_Images <- list()

n <- 1
while (n < nrow(test_Fracture_list) + 1) {
  test_Fracture_Images[[n]] <- crop_test_Fr(n)
  n <- n + 1
}


##################### DATA VALIDATION #####################

# Check image and label file counts
stopifnot(
  length(train_img_names) == length(train_txt_names),
  length(valid_img_names) == length(valid_txt_names),
  length(test_img_names) == length(test_txt_names)
)

# Check image and label filenames match in order
stopifnot(
  identical(unname(sapply(train_img_names, dir_strip)),
            unname(sapply(train_txt_names, dir_strip))),
  identical(unname(sapply(valid_img_names, dir_strip)),
            unname(sapply(valid_txt_names, dir_strip))),
  identical(unname(sapply(test_img_names, dir_strip)),
            unname(sapply(test_txt_names, dir_strip)))
)

# Check fracture list row counts match cropped image counts
stopifnot(
  nrow(train_Fracture_list) == length(Fracture_Images),
  nrow(valid_Fracture_list) == length(valid_Fracture_Images),
  nrow(test_Fracture_list) == length(test_Fracture_Images)
)



##################### DISPLAY CROPPED IMAGES ###################

display(train_images[[1]])
display(Fracture_Images[[1]])
display(Fracture_Images[[2]])

display(channel(train_images[[1]], "Gray"))