# utility ----

# initialization ----

.initiate_reservoirR_from_control <- function(control) {
  stopifnot(is.list(control))
  mandatory <- formalArgs(.initiate_reservoirR)
  diff_ <- setdiff(mandatory, names(control))
  if (length(diff_) != 0) {
    stop(paste0("control_hlme must contain ", diff_))
  }
  return(.initiate_reservoirR(
    control$units,
    control$lr,
    control$sr,
    control$ridge
  ))
}


.test_initiate_reservoirR <- function(
  units,
  lr,
  sr,
  ridge
) {
  stopifnot(is.numeric(units))
  stopifnot(is.numeric(lr))
  stopifnot(is.numeric(sr))
  stopifnot(is.numeric(ridge))
}

.initiate_reservoirR <- function(
  units,
  lr,
  sr,
  ridge
) {
  .test_initiate_reservoirR(units, lr, sr, ridge)

  reservoir <- reservoirnet::createNode(
    "Reservoir",
    units = units,
    lr = lr,
    sr = sr
  )
  readout <- reservoirnet::createNode("Ridge", ridge = ridge)
  model <- reservoirnet::link(reservoir, readout)
  return(model)
}


# fitting/training ----

.test_fit_reservoirR <- function(
  model,
  fixed_spec,
  data,
  subject,
  time,
  pred_rand
) {
  .test_predict_reservoirR(model, fixed_spec, data, subject, time)
  stopifnot(is.numeric(pred_rand))
  stopifnot(is.vector(pred_rand))
}


.fit_reservoirR <- function(model, fixed_spec, data, subject, time, pred_rand) {
  .test_fit_reservoirR(model, fixed_spec, data, subject, time, pred_rand)
  # !!! offsetting is not implemented in LCMM
  # BUT for linear models, fitting "f(X)+offset" on Y is equivalent to
  # fitting f(X) on "Y-offset"
  # so that is the method used so far
  left <- .get_left_side_string(fixed_spec)
  data[left] <- data[left] - pred_rand
  train_data <- .reshape_for_rnn(fixed_spec, data, subject)
  model_fit <- reservoirnet::reservoirR_fit(
    model,
    X = train_data[["X"]],
    Y = train_data[["Y"]],
    stateful = FALSE,
    warmup = 0
  )
  model <- model_fit$fit
  pred_fixed <- reservoirnet::predict_seq(
    node = model,
    X = train_data[["X"]],
    stateful = FALSE
  )
  pred_fixed <- .reshape_pred_of_rnn(pred_fixed, data, subject)
  return(list("model" = model, "pred_fixed" = pred_fixed))
}


# prediction ----

.test_predict_reservoirR <- function(
  model,
  fixed_spec,
  data,
  subject,
  time
) {
  .check_sorted_data(data, subject, time)
  #
  stopifnot(inherits(model, "reservoirpy.model.Model"))
  stopifnot(rlang::is_bare_formula(fixed_spec))
}


.predict_reservoirR <- function(model, fixed_spec, data, subject, time) {
  .test_predict_reservoirR(model, fixed_spec, data, subject, time)
  train_data <- .reshape_for_rnn(fixed_spec, data, subject)
  pred_fixed <- reservoirnet::predict_seq(
    node = model,
    X = train_data[["X"]],
    stateful = FALSE
  )
  pred_fixed <- .reshape_pred_of_rnn(pred_fixed, data, subject)
  return(pred_fixed)
}
