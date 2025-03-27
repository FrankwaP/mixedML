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
  right <- gsub("\\+*\\b1\\b\\+*", "", right)
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
  stopifnot(length(pred) == length(unique(data[[subject]])))
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


.clean_control <- function(
  control,
  control_name,
  mandatory_names,
  avoid_names
) {
  stopifnot(is.list(control))
  stopifnot(is.character(control_name))
  stopifnot(is.null(mandatory_names) || is.vector(mandatory_names))
  stopifnot(is.null(avoid_names) || is.vector(avoid_names))
  #############
  diff_ <- setdiff(mandatory_names, names(control))
  if (length(diff_) != 0) {
    stop(paste(control_name, "must contain", diff_))
  }
  #
  inter_ <- intersect(avoid_names, names(control))
  if (length(inter_) > 0) {
    warning(
      paste(
        "Parameter",
        inter_,
        "of",
        control_name,
        "are already defined in MixedML and will be ignored."
      )
    )
    control[inter_] <- NULL
  }
  return(control)
}


# reticulate ----
.set_r_attr_to_py_obj <- function(py_obj, name, r_value) {
  reticulate::py_set_attr(py_obj, name, reticulate::r_to_py(r_value))
}


.get_r_attr_from_py_obj <- function(py_obj, name) {
  return(reticulate::py_to_r(reticulate::py_get_attr(py_obj, name)))
}
