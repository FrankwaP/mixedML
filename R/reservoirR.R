# I am sorry but the reservoirR package has no real interest, since it is just
# a wrapper for reticulate which is not complicated at all.
# Also it is actually quite confusing since it uses a single function to create
# all the different Nodes (which have different parameters)…
# So yeah I directly use reticulate

# reticulate ----
reservoirpy <- reticulate::import("reservoirpy", convert = FALSE)
.get_reservoir <- reservoirpy$nodes$Reservoir
.get_ridge <- reservoirpy$nodes$Ridge
.link_nodes <- reservoirpy$link

.param_reservoir <- formalArgs(.get_reservoir)
.param_ridge <- formalArgs(.get_ridge)


# utils ----

.call_with_control <- function(func, control) {
  inter_params <- intersect(formalArgs(func), names(control))
  return(do.call(func, control[inter_params]))
}


# initialization ----

.prepare_control_reservoir <- function(control) {
  .clean_control(
    control,
    "control_reservoir",
    mandatory_names = c("units", "lr", "sr", "ridge", "warmup"),
    avoid_names = c()
  )
  #
  stopifnot(control$units == round(control$units))
  stopifnot(0 < control$units)
  control$units <- as.integer(control$units)
  #
  stopifnot(is.numeric(control$lr))
  stopifnot(0 < control$lr & control$lr < 1)
  #
  stopifnot(is.numeric(control$sr))
  stopifnot(0 < control$sr)
  #
  stopifnot(is.numeric(control$ridge))
  stopifnot(0 < control$ridge)
  #
  stopifnot(control$warmup == round(control$warmup))
  stopifnot(0 < control$warmup)
  control$warmup <- as.integer(control$warmup)
  #
  return(control)
}

.initiate_reservoirR <- function(
  fixed_spec,
  subject,
  control
) {
  control <- .prepare_control_reservoir(control)
  # with this method, "input_bias" and "bias"
  # will be used for both reservoir and ridge
  # so far no problem as I never had this use case
  reservoir <- .call_with_control(.get_reservoir, control)
  # control_reservoir <- control[intersect(names(control), .param_reservoir)]
  # reservoir <- do.call(.get_reservoir, control_reservoir)
  ridge <- .call_with_control(.get_ridge, control)
  # control_ridge <- control[intersect(names(control), .param_ridge)]
  # ridge <- do.call(.get_ridge, control_ridge)
  model <- .link_nodes(reservoir, ridge)
  # Adding the remaining attributes to the model so we can use them for fit and predict
  params_union <- union(.param_reservoir, .param_ridge)
  control_final <- control[setdiff(names(control), params_union)]
  control_final$fixed_spec <- fixed_spec
  control_final$subject <- subject
  for (name in names(control_final)) {
    .set_r_attr_to_py_obj(model, name, control_final[[name]])
  }
  return(model)
}

# fitting/training ----
.fit_reservoirR <- function(model, data, pred_rand) {
  model_attr <- .get_r_attr_from_py_obj(model, "__dict__")
  fixed_spec <- model_attr$fixed_spec
  subject <- model_attr$subject
  # !!! offsetting is not implemented in LCMM
  # BUT for linear models, fitting "f(X)+offset" on Y is equivalent to
  # fitting f(X) on "Y-offset"
  # so that is the method used so far
  left <- .get_left_side_string(fixed_spec)
  data[left] <- data[left] - pred_rand
  data_reshaped <- .reshape_for_rnn(fixed_spec, data, subject)
  #
  control <- c(
    list(X = data_reshaped[["X"]], Y = data_reshaped[["Y"]]),
    model_attr
  )
  model <- .call_with_control(model$fit, control)
  pred_fixed <- .predict_reservoirR(model, data, subject, data_reshaped)
  return(list("model" = model, "pred_fixed" = pred_fixed))
}


# prediction ----
.predict_reservoirR <- function(model, data, subject, data_reshaped = NULL) {
  model_attr <- .get_r_attr_from_py_obj(model, "__dict__")
  if (is.null(data_reshaped)) {
    data_reshaped <- .reshape_for_rnn(fixed_spec, data, subject)
  } else {
    # shortcut to avoid redoing this operation
    stopifnot(setequal(names(data_reshaped), c('X', 'Y')))
  }
  #
  control <- c(list(X = data_reshaped[["X"]], model_attr))
  pred_fixed <- .call_with_control(model$run, control)
  pred_fixed <- reticulate::py_to_r(pred_fixed)
  pred_fixed <- .reshape_pred_of_rnn(pred_fixed, data, subject)
  return(pred_fixed)
}
