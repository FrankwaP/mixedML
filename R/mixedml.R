# https://github.com/reservoirpy/reservoirR/blob/main/tests/testthat/tests.R

# initialization ----

MIXEDML_CLASS <- "MixedML_Model"

.test_reservoir_mixedml <- function(
  fixed_spec,
  random_spec,
  cor,
  data,
  subject,
  time,
  control_hlme,
  control_reservoir,
  control_mixedml
) {
  stopifnot(rlang::is_bare_formula(fixed_spec))
  stopifnot(rlang::is_bare_formula(random_spec))
  stopifnot(
    .get_left_side_string(fixed_spec) == .get_left_side_string(random_spec)
  )
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  stopifnot(subject %in% names(data))
  stopifnot(is.character(time))
  stopifnot(time %in% names(data))
  stopifnot(
    is.null(cor) | (rlang::is_bare_formula(cor) & (cor[0:2] %in% c("AR", "BM")))
  )

  .check_sorted_data(data, subject, time)
}

.prepare_control_mixedml <- function(control) {
  stopifnot(is.list(control))
  mandatory <- c("patience", "conv_ratio_thresh")
  diff_ <- setdiff(mandatory, names(control))
  if (length(diff_) != 0) {
    stop(paste0("control must contain ", diff_))
  }
  conv_ratio_thresh <- control[["conv_ratio_thresh"]]
  patience <- control[["patience"]]
  stopifnot(is.numeric(conv_ratio_thresh))
  stopifnot(0 <= conv_ratio_thresh & conv_ratio_thresh < 1)
  stopifnot(round(patience) == patience)
  stopifnot(0 <= patience)
  return(control)
}

reservoir_mixedml <- function(
  fixed_spec,
  random_spec,
  cor,
  data,
  subject,
  time,
  control_hlme,
  control_reservoir,
  control_mixedml
) {
  .test_reservoir_mixedml(
    fixed_spec,
    random_spec,
    cor,
    data,
    subject,
    time,
    control_hlme,
    control_reservoir,
    control_mixedml
  )
  control_mixedml <- .prepare_control_mixedml(control_mixedml)
  #
  random_model <- .initiate_random_hlme(
    random_spec,
    cor,
    data,
    subject,
    time,
    control_hlme
  )
  fixed_model <- .initiate_reservoirR(
    fixed_spec,
    subject,
    control_reservoir
  )
  conv_ratio_thresh <- control_mixedml[["conv_ratio_thresh"]]
  patience <- control_mixedml[["patience"]]

  ##
  target_name <- .get_left_side_string(fixed_spec)
  pred_rand <- rep(0, nrow(data))
  istep <- 0
  mse_list <- c()
  mse_min <- Inf
  while (TRUE) {
    print(paste0("step#", istep, ": fixed effects"))
    fixed_results <- .fit_reservoirR(fixed_model, data, pred_rand)
    fixed_model <- fixed_results$model
    pred_fixed <- fixed_results$pred_fixed
    #
    print(paste0("step#", istep, ": mixed effects"))
    random_results <- .fit_random_hlme(random_model, data, pred_fixed)
    random_model <- random_results$model
    pred_rand <- random_results$pred_rand
    #
    residuals <- pred_fixed + pred_rand - data[[target_name]]
    mse <- mean(residuals**2)
    print(paste0("step#", istep, ": MSE = ", mse))
    mse_list <- c(mse_list, mse)

    if (mse < (1 - conv_ratio_thresh) * mse_min) {
      count_conv <- 0
    } else {
      count_conv <- count_conv + 1
      if (count_conv > patience) {
        break
      }
    }
    if (mse < mse_min) {
      mse_min <- mse
    }
    istep <- istep + 1
  }

  output <- (list(
    "subject" = subject,
    "time" = time,
    "fixed_spec" = fixed_spec,
    "random_spec" = random_spec,
    "fixed_model" = fixed_model,
    "random_model" = random_model,
    "mse_list" = mse_list,
    "residuals" = residuals,
    "call" = match.call()
  ))
  class(output) <- MIXEDML_CLASS
  return(output)
}

# prediction ----

.test_predict <- function(model, data) {
  stopifnot(inherits(model, MIXEDML_CLASS))
  stopifnot(names(data) == names(model$random_model$data))
}

predict <- function(model, data) {
  .test_predict(model, data)
  #
  pred_fixed <- .predict_reservoirR(
    model$fixed_model,
    data,
    model$subject,
  )
  pred_mixed <- .predict_random_hlme(model$random_model, data)
  return(pred_fixed + pred_mixed)
}

# summary
plot_conv <- function(model, ylog = TRUE) {
  plot(
    1:length(model$mse_list),
    model$mse_list,
    type = "o",
    xlab = model$time,
    ylab = "MSE",
    ylog = ylog
  )
}
