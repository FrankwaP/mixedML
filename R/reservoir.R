# reticulate ----
# REMINDER: use .fix_integer/.fix_integers_in_controls (and integer without "L" is not a real integer! )

pypath <- sprintf('%s/%s', here::here(), '/python/')
stopifnot(dir.exists(pypath))
module <- 'reservoir_ensemble'
stopifnot(file.exists(sprintf('%s/%s.py', pypath, module)))

retipy <- reticulate::import_from_path(module, pypath)


# parameters check/modification ----

.prepare_esn_controls <- function(esn_controls) {
  return(.check_control(
    esn_controls,
    mandatory_names_checks = list(
      units = is.single.integer,
      lr = is.single.numeric,
      sr = is.single.numeric,
      ridge = is.single.numeric
    ),
    avoid_names = c('seed')
  ))
  return(esn_controls)
}


.prepare_ensemble_controls <- function(ensemble_controls) {
  return(.check_control(
    ensemble_controls,
    mandatory_names_checks = list(
      seed_list = is.vector,
      agg_func = is.character,
      n_procs = is.single.integer
    ),
    avoid_names = c()
  ))
  return(ensemble_controls)
}


# recipes  ----

.initiate_ens <- function(
  fixed_spec,
  subject,
  esn_controls,
  ensemble_controls,
  fit_controls,
  predict_controls
) {
  esn_controls <- .prepare_esn_controls(esn_controls)
  ensemble_controls <- .prepare_ensemble_controls(ensemble_controls)
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
    fit_controls = .get_r_attr_from_py_obj(model, "fit_controls")
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
    predict_controls = .get_r_attr_from_py_obj(model, "predict_controls")
  )
  pred_fixed <- do.call(model$predict, controls)
  pred_fixed <- .reshape_pred_of_rnn(pred_fixed, data, subject)
  return(pred_fixed)
}
