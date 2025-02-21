# usethis::use_test("reservoirs")
library(reticulate)
rpy <- import("reservoirpy")
invisible(rpy$verbosity(0))  # no need to be too verbose here


initiate_reservoirR <- function(spec, data, subject, units, lr, sr, ridge) {
  stopifnot(class(spec) == "formula")
  stopifnot(class(data) == "data.frame")
  stopifnot(class(subject) == "character")
  stopifnot(class(units) == "numeric")
  stopifnot(class(lr) == "numeric")
  stopifnot(class(sr) == "numeric")
  stopifnot(class(ridge) == "numeric")

  reservoir <- reservoirnet::createNode("Reservoir",
                                        units = units,
                                        lr = lr,
                                        sr = sr)
  readout <- reservoirnet::createNode("Ridge", ridge = ridge)
  model <- reservoirnet::link(reservoir, readout)
  # .GlobalEnv$model <- model
  return(model)
}

fit_reservoirR <- function(model, spec, data, subject, pred_rand) {
  # !!! offsetting is not implemented in LCMM
  # BUT for linear models, fitting "f(X)+offset" on Y is equivalent to fitting f(X) on "Y-offset"
  # so that is the method used so far
  left <- .get_left_side_string(spec)
  data[left] <- data[left] - pred_rand
  train_data <- .reshape_for_rnn(spec, data, subject)
  model_fit <- reservoirnet::reservoirR_fit(
    model,
    X = train_data[["X"]],
    Y = train_data[["Y"]],
    stateful = FALSE,
    warmup = 0
  )
  model <- model_fit$fit
  pred_fixed <- reservoirnet::predict_seq(node = model,
                                          X = train_data[["X"]],
                                          stateful = FALSE)
  pred_fixed <- .reshape_pred_of_rnn(pred_fixed, data, subject)
  return(list("model" = model, "pred_fixed" = pred_fixed))
}
