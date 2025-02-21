# https://github.com/reservoirpy/reservoirR/blob/main/tests/testthat/tests.R
.check_reservoir_mixedml_args <- function(spec,
                                          data,
                                          subject,
                                          time,
                                          conv_ratio_thresh,
                                          patience,
                                          list_hlme_args,
                                          list_reservoir_args) {
  stopifnot(rlang::is_bare_formula(spec))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  stopifnot(subject %in% names(data))
  stopifnot(is.character(time))
  stopifnot(time %in% names(data))
  stopifnot(is.numeric(conv_ratio_thresh))
  stopifnot(0 <= conv_ratio_thresh & conv_ratio_thresh < 1)
  stopifnot(is.numeric(patience))
  stopifnot(0 <= patience)
  stopifnot(is.list(list_hlme_args))
  stopifnot(is.list(list_reservoir_args))
}

.prepare_list_hlme_args <- function(list_hlme_args, spec, data, subject, time) {
  params <- names(list_hlme_args)
  stopifnot(!is.null(params))
  avoid <- c("fixed", "random", "data", "subject", "var.time")
  inter <- intersect(avoid, params)
  if (length(inter) > 0) {
    warning(
      paste0(
        "Parameter ",
        inter,
        " of list_hlme_args are already defined for mixedml and will be ignored.\n"
      )
    )
  }
  list_hlme_args$full_random <- spec
  list_hlme_args$data <- data
  list_hlme_args$subject <- subject
  list_hlme_args$var.time <- time
  return(list_hlme_args)
}

.prepare_list_reservoir_args <- function(list_reservoir_args,
                                         spec,
                                         data,
                                         subject,
                                         time) {
  list_reservoir_args$spec <- spec
  list_reservoir_args$data <- data
  list_reservoir_args$subject <- subject
  # list_reservoir_args$time <- time
  return(list_reservoir_args)
}

reservoir_mixedml <- function(spec,
                              data,
                              subject,
                              time,
                              conv_ratio_thresh,
                              patience,
                              list_hlme_args,
                              list_reservoir_args) {
  .check_reservoir_mixedml_args(
    spec,
    data,
    subject,
    time,
    conv_ratio_thresh,
    patience,
    list_hlme_args,
    list_reservoir_args
  )
  list_hlme_args <- .prepare_list_hlme_args(list_hlme_args, spec, data, subject, time)
  list_reservoir_args <- .prepare_list_reservoir_args(list_reservoir_args, spec, data, subject, time)

  #
  left <- .get_left_side_string(spec)
  random_hlme <- do.call(initiate_random_hlme, list_hlme_args)
  fixed_model <- do.call(initiate_reservoirR, list_reservoir_args)

  ########
  data_fixed <- data
  pred_rand <- rep(0, nrow(data))
  istep <- 0
  mse_list <- c()
  mse_min <- Inf
  while (TRUE) {
    print(paste0("step#", istep, ": fixed effects"))
    fixed_results <- fit_reservoirR(fixed_model, spec, data, subject, pred_rand)
    fixed_model <- fixed_results$model
    pred_fixed <- fixed_results$pred_fixed
    #
    random_results <- fit_random_hlme(random_hlme, data, pred_fixed)
    random_hlme <- random_results$model
    pred_rand <- random_results$pred_rand
    #
    mse <- mean((pred_fixed + pred_rand - data[[left]]) ** 2)
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
      "random_model" = random_hlme,
      "mse_list" = mse_list
    )
  )
}
