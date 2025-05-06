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

.fix_integer <- function(value) {
  if (is.numeric(value)) {
    if (all(round(value) == value)) {
      value <- as.integer(value)
    }
  }
  return(value)
}

.fix_integers_in_controls <- function(controls) {
  for (n in names(controls)) {
    controls[[n]] <- .fix_integer(controls[[n]])
  }
  return(controls)
}

.check_controls_with_function <- function(controls, controls_function) {
  names_controls <- names(controls)
  params_function <- formalArgs(controls_function)
  if (!setequal(names_controls, params_function)) {
    control_name <- as.character(as.list(match.call())[['controls']])
    function_name <- as.character(as.list(match.call())[['controls_function']])
    stop(sprintf(
      "\"%s\" should be set with the function \"%s\"\n",
      control_name,
      function_name
    ))
  }
}


.check_control <- function(
  controls,
  mandatory_names_checks = NULL,
  avoid_names = NULL,
  convert_integer = TRUE
) {
  control_name <- as.character(as.list(match.call())[['controls']])
  #
  stopifnot(is.named.vector(controls))
  stopifnot(is.named.vector(mandatory_names_checks))
  stopifnot(all(sapply(mandatory_names_checks, is.function)))
  stopifnot(is.null(avoid_names) | is.vector(avoid_names))
  stopifnot(is.logical(convert_integer))
  #
  if (convert_integer) {
    controls <- .fix_integers_in_controls(controls)
  }
  #
  inter_mand <- intersect(names(mandatory_names_checks), names(controls))
  if (!setequal(inter_mand, names(mandatory_names_checks))) {
    stop(sprintf(
      '\"%s\" parameters of \"%s\" is missing',
      inter_mand,
      control_name
    ))
  }
  #
  inter_avoid <- intersect(avoid_names, names(controls))
  if (length(inter_avoid) > 0) {
    warning(sprintf(
      "'\"%s\" parameter of \"%s\" will be ignored",
      inter_avoid,
      control_name
    ))
    controls[inter_avoid] <- NULL
  }
  #
  for (i in 1:length(mandatory_names_checks)) {
    name <- names(mandatory_names_checks)[[i]]
    check <- mandatory_names_checks[[i]]
    if (!check(controls[[name]])) {
      stop(sprintf(
        '\"%s\" parameter of \"%s\" does not respect this condiction: %s',
        name,
        control_name,
        deparse(check)
      ))
    }
  }
  return(controls)
}

# reticulate ----

.prompt_environment <- function() {
  if (!(reticulate::virtualenv_exists() | reticulate::condaenv_exists())) {
    list_venv <- reticulate::virtualenv_list()
  }
}


.set_r_attr_to_py_obj <- function(py_obj, name, r_value) {
  reticulate::py_set_attr(py_obj, name, reticulate::r_to_py(r_value))
}


.get_r_attr_from_py_obj <- function(py_obj, name) {
  return(reticulate::py_to_r(reticulate::py_get_attr(py_obj, name)))
}
