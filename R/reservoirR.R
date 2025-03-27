# I am sorry but the reservoirR package has no real interest, since it is just
# a wrapper for reticulate which is not complicated at all.
# Also it is actually quite confusing since it uses a single function to create
# all the different Nodes (which have different parameters)…
# So yeah I directly use reticulate

# # reticulate ----
# reservoirpy <- reticulate::import("reservoirpy", convert = FALSE)
# reservoirpy$verbosity(as.integer(0))

mprsrvr <- reticulate::import_from_path(
  "multiproc_reservoir",
  path = this.path::this.dir(),
  convert = TRUE,  # try FALSE if you've got problems
  delay_load = FALSE
)


# .get_reservoir <- reservoirpy$nodes$Reservoir
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
    mandatory_names = c("units", "lr", "sr", "ridge", "warmup", "seeds"),
    avoid_names = c()
  )
  #
  stopifnot(control$units == round(control$units))
  stopifnot(0. < control$units)
  control$units <- as.integer(control$units)
  #
  stopifnot(is.numeric(control$lr))
  stopifnot(0. <= control$lr & control$lr <= 1.)
  #
  stopifnot(is.numeric(control$sr))
  #
  stopifnot(is.numeric(control$ridge))
  stopifnot(0 <= control$ridge)
  #
  stopifnot(control$warmup == round(control$warmup))
  stopifnot(0 <= control$warmup)
  control$warmup <- as.integer(control$warmup)
  #
  stopifnot(is.vector(control$seeds))
  stopifnot(is.numeric(control$seeds))
  return(control)
}

.initiate_single_reservoir <- function(fixed_spec, subject, control) {
  reservoir <- .call_with_control(.get_reservoir, control)
  ridge <- .call_with_control(.get_ridge, control)
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

.initiate_reservoir <- function(
  fixed_spec,
  subject,
  control
) {
  control <- .prepare_control_reservoir(control)
  # with this method, "input_bias" and "bias"
  # will be used for both reservoir and ridge
  # so far no problem as I never had this use case
  model_list <- list()
  for (seed in control$seeds) {
    tmp_control <- control
    tmp_control$seed <- as.integer(seed)
    tmp_control$seeds <- NULL
    model_list <- c(
      model_list,
      .initiate_single_reservoir(fixed_spec, subject, tmp_control)
    )
  }
  return(model_list)
}

# fitting/training ----
.fit_single_reservoir <- function(model, data, pred_rand) {
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
  pred_fixed <- .predict_single_reservoir(model, data, subject, data_reshaped)
  return(list("model" = model, "pred_fixed" = pred_fixed))
}

.combine_fit <- function(...) {
  list_result <- list(...)
  models <- list()
  preds <- c()
  for (res in list_result) {
    models <- c(models, res$model)
    preds <- cbind(preds, res$pred_fixed)
  }
  return(list("model" = models, "pred_fixed" = rowMeans(preds)))
}

.fit_reservoir <- function(model, data, pred_rand) {
  res1 <- .fit_single_reservoir(model[[1]], data, pred_rand)
  res2 <- .fit_single_reservoir(model[[2]], data, pred_rand)
  check <- .combine_fit(res1, res2)
  result <- foreach::foreach(
    reservoir = iter(model),
    .combine = .combine_fit,
    .export = c(".fit_single_reservoir"),
    .verbose = TRUE
  ) %dopar%
    {
      devtools::load_all("R/utils.R")
      .fit_single_reservoir(reservoir, data, pred_rand)
    }
  return(result)
}

# prediction ----
.predict_single_reservoir <- function(
  model,
  data,
  subject,
  data_reshaped = NULL
) {
  model_attr <- .get_r_attr_from_py_obj(model, "__dict__")
  if (is.null(data_reshaped)) {
    data_reshaped <- .reshape_for_rnn(model_attr$fixed_spec, data, subject)
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

.predict_reservoir <- function(model, data, subject, data_reshaped = NULL) {
  result <- foreach(
    reservoir = iter(model),
    .combine = mean,
    .export = c(".predict_single_reservoir")
  ) %dopar%
    {
      devtools::load_all("R/utils.R")
      .predict_single_reservoir(
        reservoir,
        data,
        subject,
        data_reshaped
      )
    }
  return(result)
}
