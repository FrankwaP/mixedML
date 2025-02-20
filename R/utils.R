.get_left_side_string <- function(spec) {
  stopifnot(class(spec) == "formula")
  return(deparse(spec[[2]]))
}

.get_right_side_string <- function(spec) {
  stopifnot(class(spec) == "formula")
  return(deparse(spec[[3]]))
}

.spec_formula_to_labels <- function(spec) {
  stopifnot(class(spec) == "formula")
  left <- .get_left_side_string(spec)
  right <- .get_right_side_string(spec)
  right <- gsub(" ", "", right)
  right <- strsplit(right, "+", fixed = TRUE)[[1]]
  return(list(y_label = left, x_labels = right))
}

.reshape_for_rnn <- function(spec, data, subject) {
  stopifnot(class(spec) == "formula")
  stopifnot(class(data) == "data.frame")
  stopifnot(class(subject) == "character")

  labels <- .spec_formula_to_labels(spec)
  x_labels <- labels[["x_labels"]]
  y_label <- labels[["y_label"]]

  rnn_data <- unname(split(data, data[subject]))
  X <- lapply(rnn_data, function(df) {
    as.matrix(df[x_labels])
  })
  Y <- lapply(rnn_data, function(df) {
    as.matrix(df[y_label])
  })
  return(list(X = X, Y = Y))
}

.reshape_pred_of_rnn <- function(pred, data, subject) {
  return(unsplit(pred, data[subject]))
}
