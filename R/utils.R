# formula sides ----

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

# array reshaping ----

.reshape_for_rnn <- function(spec, data, subject) {
  stopifnot(rlang::is_bare_formula(spec))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  #
  labels <- .spec_formula_to_labels(spec)
  x_labels <- labels[["x_labels"]]
  y_label <- labels[["y_label"]]

  # unname so reticulate convert tis as a list (not a dict)
  rnn_data <- unname(split(data, data[subject]))
  # matric so reticulate convert is as a numpy array
  X <- lapply(rnn_data, function(df) {
    as.matrix(df[x_labels])
  })
  y <- lapply(rnn_data, function(df) {
    as.matrix(df[y_label])
  })
  return(list(X = X, y = y))
}

.reshape_pred_of_rnn <- function(pred, data, subject) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  stopifnot(subject %in% names(data))
  stopifnot(is.list(pred) | is.vector(pred))
  stopifnot(length(pred) == length(unique(data[[subject]])))
  #
  names(pred) <- unique(data[[subject]])
  pred <- unsplit(pred, data[[subject]])
  return(unsplit(pred, data[subject]))
}

# data check ----

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


is.single.integer <- function(x) {
  return(is.integer(x) & length(x) == 1)
}

is.single.numeric <- function(x) {
  return(is.numeric(x) & length(x) == 1)
}

is.named.vector <- function(x) {
  return(is.vector(x) & ((length(x) == 0) | is.character(names(x))))
}

.check_control <- function(
  controls,
  mandatory_names_checks = NULL,
  avoid_names = NULL
) {
  stopifnot(is.named.vector(mandatory_names_checks))
  stopifnot(all(sapply(mandatory_names_checks, is.function)))
  stopifnot(is.null(avoid_names) | is.vector(avoid_names))
  #
  control_name <- as.character(as.list(match.call())[['controls']])
  #
  if (!is.named.vector(controls)) {
    stop(sprintf("\"%s\" must me a named list", control_name))
  }
  #
  for (i in 1:length(mandatory_names_checks)) {
    name <- names(mandatory_names_checks)[[i]]
    check <- mandatory_names_checks[[i]]
    if (name %in% names(controls)) {
      if (!check(controls[[name]])) {
        stop(sprintf(
          '\"%s\" parameter of \"%s\" does not respect this condiction: %s',
          name,
          control_name,
          deparse(check)
        ))
      }
    } else {
      stop(sprintf(
        '\"%s\" parameter of \"%s\" is missing',
        name,
        control_name
      ))
    }
  }
  #
  inter_ <- intersect(avoid_names, names(controls))
  if (length(inter_) > 0) {
    warning(
      paste(
        "Parameter",
        inter_,
        "of",
        control_name,
        "will be ignored."
      )
    )
    controls[inter_] <- NULL
  }
  #
  return(controls)
}

# reticulate ----

# .fix_integer <- function(r_value) {
#   if (round(r_value) == r_value) {
#     r_value <- as.integer(r_value)
#   }
#   return(r_value)
# }
#
# .fix_integers_in_control <- function(control) {
#   for (n in names(control)) {
#     control[[n]] <- .fix_integer(control[[n]])
#   }
# }

.set_r_attr_to_py_obj <- function(py_obj, name, r_value) {
  reticulate::py_set_attr(py_obj, name, reticulate::r_to_py(r_value))
}


.get_r_attr_from_py_obj <- function(py_obj, name) {
  return(reticulate::py_to_r(reticulate::py_get_attr(py_obj, name)))
}
