project_root <- if (dir.exists(file.path(getwd(), "data"))) {
  getwd()
} else if (dir.exists(file.path(getwd(), "..", "data"))) {
  file.path(getwd(), "..")
} else {
  stop("Set the working directory to the repository root or code directory.")
}
project_root <- normalizePath(project_root, winslash = "/")

if (!exists("y_test_pred") || !exists("y_test")) {
  source(file.path(project_root, "code", "Detection_CNN.R"))
}

eps <- 1e-7

ious <- numeric(0)

y_pred <- y_test_pred

for (i in seq_len(dim(y_test)[1])) {
  for (gy in seq_len(grid_size)) {
    for (gx in seq_len(grid_size)) {
      if (y_test[i, gy, gx, 1] == 0) next

      tx <- y_test[i, gy, gx, 2]
      ty <- y_test[i, gy, gx, 3]
      tw <- y_test[i, gy, gx, 4] * grid_size
      th <- y_test[i, gy, gx, 5] * grid_size

      px <- y_pred[i, gy, gx, 2]
      py <- y_pred[i, gy, gx, 3]
      pw <- y_pred[i, gy, gx, 4] * grid_size
      ph <- y_pred[i, gy, gx, 5] * grid_size

      tx1 <- tx - tw / 2
      ty1 <- ty - th / 2
      tx2 <- tx + tw / 2
      ty2 <- ty + th / 2

      px1 <- px - pw / 2
      py1 <- py - ph / 2
      px2 <- px + pw / 2
      py2 <- py + ph / 2

      iw <- max(0, min(tx2, px2) - max(tx1, px1))
      ih <- max(0, min(ty2, py2) - max(ty1, py1))
      intersection <- iw * ih

      union <- tw * th + pw * ph - intersection
      ious <- c(ious, intersection / (union + eps))
    }
  }
}


cat("Number of actual boxes:", length(ious), "\n")
cat("Mean IoU:", round(mean(ious), 4), "\n")
cat("Median IoU:", round(median(ious), 4), "\n")
cat("Min IoU:", round(min(ious), 4), "\n")
cat("Max IoU:", round(max(ious), 4), "\n")
cat("Boxes with IoU >=0.5:",
    round(mean(ious >= 0.5) * 100, 2), "%\n")


library(ggplot2)

iou_data <- data.frame(IoU = ious)

ggplot(iou_data, aes(x = IoU)) +
  geom_histogram(
    binwidth = 0.05,
    boundary = 0,
    colour = "black",
    fill = "lightblue"
  ) +
  geom_vline(
    xintercept = 0.5,
    colour = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Distribution of Intersection over Union(IoU)",
    x = "IoU",
    y = "Frequency"
  ) +
  theme_minimal()


actual_cells <- which(y_test[,,,1] == 1, arr.ind = TRUE)

predicted_boxes <- data.frame(
  Image = actual_cells[,1],
  Grid_Row = actual_cells[,2],
  Grid_Col = actual_cells[,3],
  Objectness = y_pred[cbind(actual_cells, 1)],
  X = y_pred[cbind(actual_cells, 2)],
  Y = y_pred[cbind(actual_cells, 3)],
  Width = y_pred[cbind(actual_cells, 4)],
  Height = y_pred[cbind(actual_cells, 5)]
)


n <- 3
x <- 379
y <- 419
w <- 136
h <- 94

display(
  as_EBImage(
    image_read(test_img_names[[test_Fracture_list[n, 6]]]) %>%
      image_crop(
        geometry = geometry_area(
          width = w,
          height = h,
          x_off = x - w / 2,
          y_off = y - h / 2
        )
      )
  )
)
