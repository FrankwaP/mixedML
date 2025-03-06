# https://github.com/reservoirpy/reservoirR/blob/main/tests/testthat/tests.R
.check_reservoir_mixedml_args <- function(
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
  stopifnot(is.data.frame(data))
  .check_sorted_data(data, subject, time)
  stopifnot(is.character(subject))
  stopifnot(subject %in% names(data))
  stopifnot(is.character(time))
  stopifnot(is.null(cor) | is.call(cor))
  stopifnot(time %in% names(data))
  stopifnot(is.list(control_hlme))
  stopifnot(is.list(control_reservoir))
  stopifnot(is.list(control_mixedml))
}

.check_sorted_data <- function(data, subject, time) {
  data_order <- order(data[, subject], data[, time])
  if (!all(data_order == 1:length(data_order))) {
    stop("Please sort the data by subject and time beforehand!")
  }
}

.prepare_control_hlme <- function(
  control_hlme,
  random_spec,
  cor,
  data,
  subject,
  time
) {
  params <- names(control_hlme)
  stopifnot(!is.null(params))
  avoid <- c("fixed_spec", "random_spec", "data", "subject", "var.time", "cor")
  inter <- intersect(avoid, params)
  if (length(inter) > 0) {
    warning(
      paste0(
        "Parameter ",
        inter,
        " of control_hlme are already defined for mixedml and will be ignored.\n"
      )
    )
  }
  control_hlme$random_spec <- random_spec
  control_hlme$data <- data
  control_hlme$subject <- subject
  control_hlme$var.time <- time
  control_hlme$cor <- cor
  return(control_hlme)
}

.prepare_control_reservoir <- function(
  control_reservoir,
  fixed_spec,
  data,
  subject,
  time
) {
  control_reservoir$fixed_spec <- fixed_spec
  control_reservoir$data <- data
  control_reservoir$subject <- subject
  # control_reservoir$time <- time
  return(control_reservoir)
}

.prepare_control_mixedml <- function(control_mixedml) {
  diff_ <- setdiff(names(control_mixedml), c('patience', 'conv_ratio_thresh'))
  if (length(diff_) != 0) {
    stop(paste0('control_mixedml must contain ', diff_))
  }
  conv_ratio_thresh <- control_mixedml[['conv_ratio_thresh']]
  patience <- control_mixedml[['patience']]
  stopifnot(is.numeric(conv_ratio_thresh))
  stopifnot(0 <= conv_ratio_thresh & conv_ratio_thresh < 1)
  stopifnot(is.numeric(patience))
  stopifnot(0 <= patience)
  return(control_mixedml)
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
  .check_reservoir_mixedml_args(
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

  control_hlme <- .prepare_control_hlme(
    control_hlme,
    random_spec,
    cor,
    data,
    subject,
    time
  )
  control_reservoir <- .prepare_control_reservoir(
    control_reservoir,
    fixed_spec,
    data,
    subject,
    time
  )
  control_mixedml <- .prepare_control_mixedml(control_mixedml)

  #
  target_name <- .get_left_side_string(fixed_spec)
  random_model <- do.call(initiate_random_hlme, control_hlme)
  fixed_model <- do.call(initiate_reservoirR, control_reservoir)
  conv_ratio_thresh <- control_mixedml[['conv_ratio_thresh']]
  patience <- control_mixedml[['patience']]

  ########
  data_fixed <- data
  pred_rand <- rep(0, nrow(data))
  istep <- 0
  mse_list <- c()
  mse_min <- Inf
  while (TRUE) {
    print(paste0("step#", istep, ": fixed_spec effects"))
    fixed_results <- fit_reservoirR(
      fixed_model,
      fixed_spec,
      data,
      subject,
      pred_rand
    )
    fixed_model <- fixed_results$model
    pred_fixed <- fixed_results$pred_fixed
    #
    random_results <- fit_random_hlme(random_model, data, pred_fixed)
    random_model <- random_results$model
    pred_rand <- random_results$pred_rand
    #
    mse <- mean((pred_fixed + pred_rand - data[[target_name]])**2)
    mse_list <- c(mse_list, mse)

    if (mse < (1 - conv_ratio_thresh) * mse_min) {
      count_conv <- 0
    } else {
      count_conv <- count_conv + 1
      if (count_conv > patience) {
        print(mse_list)
        break
      }
    }
    if (mse < mse_min) {
      mse_min <- mse
    }
  }
  return(
    list(
      "fixed_model" = fixed_model,
      "random_model" = random_model,
      "mse_list" = mse_list
    )
  )
}
