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

print("now")

eps <- 1e-7

## actual and predicted objectness scores

obj_true <- y_test[,,,1]
obj_pred <- y_test_pred[,,,1]


# Positive and negative predictions
positive_predictions <- obj_pred[obj_true == 1]
negative_predictions <- obj_pred[obj_true == 0]

cat("Positive predictions:\n")
print(summary(positive_predictions))

cat("\nNegative predictions:\n")
print(summary(negative_predictions))

cat("Positive cells:", sum(obj_true == 1), "\n")
cat("Negative cells:", sum(obj_true == 0), "\n")


# iterate over thresholds to find value that splits
thresholds <- seq(0, 1, by = 0.01)

for (threshold in thresholds) {
  positive_rate <- mean(positive_predictions >= threshold)
  negative_rate <- mean(negative_predictions < threshold)

  precision <- sum(positive_predictions >= threshold) /
    (sum(positive_predictions >= threshold) +
       sum(negative_predictions >= threshold))

  cat("\nThreshold:", threshold, "\n")
  cat("Positive detection rate:", round(positive_rate * 100, 2), "%\n")
  cat("Negative classification rate:", round(negative_rate * 100, 2), "%\n")
  cat("Precision:", round(precision * 100, 2), "%\n")
}


## histograms
hist(
  positive_predictions,
  breaks = 30,
  xlim = c(0, 1),
  main = "Positive Objectness Predictions",
  xlab = "Objectness"
)

hist(
  negative_predictions,
  breaks = 30,
  xlim = c(0, 1),
  main = "Negative Objectness Predictions",
  xlab = "Objectness"
)


y_test_pred[1,,,1]
y_test[1,,,1]
display(test_images[[1]])
