model_directory <- function(project_root) file.path(project_root, "models")

save_fracture_models <- function(project_root, classification_model = NULL,
                                 detection_model = NULL, class_names = NULL) {
  if (!requireNamespace("keras3", quietly = TRUE)) {
    stop("Install keras3 to save the trained models.")
  }
  directory <- model_directory(project_root)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(classification_model)) {
    keras3::save_model(classification_model,
      filepath = file.path(directory, "classification.keras"))
  }
  if (!is.null(detection_model)) {
    keras3::save_model(detection_model,
      filepath = file.path(directory, "detection.keras"))
  }
  if (!is.null(class_names)) {
    saveRDS(class_names, file.path(directory, "class_names.rds"))
  }
  invisible(directory)
}

load_fracture_models <- function(project_root) {
  directory <- model_directory(project_root)
  classification_path <- file.path(directory, "classification.keras")
  detection_path <- file.path(directory, "detection.keras")
  class_names_path <- file.path(directory, "class_names.rds")
  has_models <- file.exists(classification_path) || file.exists(detection_path)
  if (has_models && !requireNamespace("keras3", quietly = TRUE)) {
    stop("Install keras3 to load the saved models.")
  }
  list(
    classification = if (file.exists(classification_path)) keras3::load_model(classification_path) else NULL,
    detection = if (file.exists(detection_path)) keras3::load_model(detection_path, compile = FALSE) else NULL,
    classes = if (file.exists(class_names_path)) readRDS(class_names_path) else NULL
  )
}

predict_fracture <- function(image_path, model, fracture_classes) {
  if (is.null(model)) stop("No classification model is saved yet. Run Classification CNN.R first.")
  if (!requireNamespace("EBImage", quietly = TRUE)) {
    stop("Install Bioconductor package EBImage to process uploaded images.")
  }
  image <- EBImage::readImage(image_path)
  if (length(dim(image)) == 2L) image <- EBImage::rgbImage(image, image, image)
  if (length(dim(image)) != 3L || dim(image)[3] != 3L) {
    stop("Image must be grayscale or have three colour channels.")
  }
  image <- EBImage::resize(image, w = 150, h = 150)
  image_array <- array(as.array(image), dim = c(1L, dim(image)))
  probabilities <- as.numeric(stats::predict(model, image_array)[1, ])
  predicted_index <- which.max(probabilities)
  list(
    predicted_class = fracture_classes[[predicted_index]],
    confidence = probabilities[[predicted_index]],
    probabilities = probabilities
  )
}