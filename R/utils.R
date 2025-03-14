.get_left_side_string <- function(spec) {
  stopifnot(rlang::is_bare_formula(spec))
  return(deparse(spec[[2]]))
}

.get_right_side_string <- function(spec) {
  stopifnot(rlang::is_bare_formula(spec))
  return(deparse(spec[[3]]))
}

.spec_formula_to_labels <- function(spec) {
  left <- .get_left_side_string(spec)
  right <- .get_right_side_string(spec)
  right <- gsub(" ", "", right)
  right <- strsplit(right, "+", fixed = TRUE)[[1]]
  return(list(y_label = left, x_labels = right))
}

.reshape_for_rnn <- function(spec, data, subject) {
  stopifnot(rlang::is_bare_formula(spec))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  #
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
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  stopifnot(subject %in% names(data))
  stopifnot(is.list(pred) | is.vector(pred))
  stopifnot(length(pred) == max(data[subject]))
  #
  return(unsplit(pred, data[subject]))
}


.check_sorted_data <- function(data, subject, time) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  stopifnot(subject %in% names(data))
  stopifnot(is.character(time))
  stopifnot(time %in% names(data))
  #
  data_order <- order(data[, subject], data[, time])
  if (!all(data_order == 1:length(data_order))) {
    stop("Please sort the data by subject and time beforehand!")
  }
}
