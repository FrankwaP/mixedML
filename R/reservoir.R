# reticulate ----
.load_package <- function() {
  .activate_environment()
  pypath <- sprintf('%s/%s', here::here(), '/python/')
  stopifnot(dir.exists(pypath))
  module <- 'reservoir_ensemble'
  stopifnot(file.exists(sprintf('%s/%s.py', pypath, module)))
  retipy <- reticulate::import_from_path(module, pypath, convert = TRUE)
  return(retipy)
}


# parameters check/modification ----
esn_ctrls <- function(
  units = NULL,
  lr = 1.0,
  sr = NULL,
  ridge = 0.0,
  feedback = FALSE
) {
  units <- .fix_integer(units)
  stopifnot(is.single.integer(units))
  stopifnot(is.single.numeric(lr))
  stopifnot(is.single.numeric(sr))
  stopifnot(is.single.numeric(ridge))
  stopifnot(is.logical(feedback))
  return(as.list(environment()))
}

ensemble_ctrls <- function(
  seed_list = c(1, 2, 3),
  agg_func = 'median',
  n_procs = 1
) {
  seed_list <- .fix_integer(seed_list)
  n_procs <- .fix_integer(n_procs)
  stopifnot(is.integer(seed_list))
  stopifnot(is.character(agg_func))
  stopifnot(is.single.integer(n_procs))
  return(as.list(environment()))
}

fit_ctrls <- function(warmup = 0, stateful = TRUE, reset = FALSE) {
  warmup <- .fix_integer(warmup)
  stopifnot(is.single.integer(warmup))
  stopifnot(is.logical(stateful))
  stopifnot(is.logical(reset))
  return(as.list(environment()))
}

predict_ctrls <- function(stateful = TRUE, reset = FALSE) {
  stopifnot(is.logical(stateful))
  stopifnot(is.logical(reset))
  return(as.list(environment()))
}


# recipes  ----
.initiate_ens <- function(
  fixed_spec,
  subject,
  esn_controls = esn_ctrls(),
  ensemble_controls = ensemble_ctrls(),
  fit_controls = fit_ctrls(),
  predict_controls = predict_ctrls()
) {
  retipy <- .load_package()
  .check_controls_with_function(esn_controls, esn_ctrls)
  .check_controls_with_function(ensemble_controls, ensemble_ctrls)
  .check_controls_with_function(fit_controls, fit_ctrls)
  .check_controls_with_function(predict_controls, predict_ctrls)

  stopifnot(is.named.vector(fit_controls))
  stopifnot(is.named.vector(predict_controls))
  controls <- c(list(esn_controls = esn_controls), ensemble_controls)
  model <- do.call(retipy$get_esn_ensemble, controls)
  # Adding the remaining attributes to the model so we can use them for fit and predict
  .set_r_attr_to_py_obj(model, "fixed_spec", fixed_spec)
  .set_r_attr_to_py_obj(model, "subject", subject)
  .set_r_attr_to_py_obj(model, "fit_controls", fit_controls)
  .set_r_attr_to_py_obj(model, "predict_controls", predict_controls)
  return(model)
}


# fitting/training ----
.fit_reservoir <- function(model, data, pred_rand) {
  # !!! offsetting is not implemented in LCMM
  # BUT for linear models, fitting "f(X)+offset" on Y is equivalent to
  # fitting f(X) on "Y-offset"
  # so that is the method used so far
  fixed_spec <- .get_r_attr_from_py_obj(model, "fixed_spec")
  subject <- .get_r_attr_from_py_obj(model, "subject")
  left <- .get_left_side_string(fixed_spec)
  data[left] <- data[left] - pred_rand
  data_reshaped <- .reshape_for_rnn(fixed_spec, data, subject)
  #
  controls <- c(
    data_reshaped,
    list(fit_controls = .get_r_attr_from_py_obj(model, "fit_controls"))
  )
  do.call(model$fit, controls)
  pred_fixed <- .predict_reservoir(model, data, subject, data_reshaped)
  return(list("model" = model, "pred_fixed" = pred_fixed))
}


# prediction ----
.predict_reservoir <- function(
  model,
  data,
  subject,
  data_reshaped = NULL
) {
  if (is.null(data_reshaped)) {
    fixed_spec <- .get_r_attr_from_py_obj(model, "fixed_spec")
    data_reshaped <- .reshape_for_rnn(fixed_spec, data, subject)
  } else {
    # shortcut to avoid redoing this operation if already done in "fit"
    stopifnot(setequal(names(data_reshaped), c('X', 'y')))
  }
  #
  controls <- c(
    data_reshaped["X"],
    list(predict_controls = .get_r_attr_from_py_obj(model, "predict_controls"))
  )
  pred_fixed <- do.call(model$predict, controls)
  pred_fixed <- .reshape_pred_of_rnn(pred_fixed, data, subject)
  return(pred_fixed)
}
