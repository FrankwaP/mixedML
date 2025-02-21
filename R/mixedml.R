# devtools::install_github(repo = "reservoirpy/reservoirR")
# library(tidymodels)
# library(recipes)
# usethis::use_test("mixedml")
set.seed(666)

####################################
## test of do.call (it works!)
# sub_test <- function(b, a) {
#   print('COUCOU!')
#   print(a)
#   print(b)
# }
#
# test <- function(list_sub_test_args) {
#   do.call(sub_test, list_sub_test_args)
# }
#
#
# test(list(a=2, b=10))
####################################


# https://github.com/reservoirpy/reservoirR/blob/main/tests/testthat/tests.R


.check_args <- function(spec, data, subject, time, conv_ratio_thresh, patience, list_hlme_args) {
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
}

.check_list_hlme_args <- function(list_hlme_args, spec, data, subject, time) {
  stopifnot(is.list(list_hlme_args))
  params <- names(list_hlme_args)
  stopifnot(!is.null(params))
  avoid <- c("fixed", "random", "data", "subject", "var.time")
  inter <- intersect(avoid, params)
  if (length(inter) > 0) {
    warning(paste0("Parameter ", inter, " of list_hlme_args are already defined for mixedml and will be ignored.\n"))
  }
  list_hlme_args$full_random <- spec
  list_hlme_args$data <- data
  list_hlme_args$subject <- subject
  list_hlme_args$var.time <- time
  return(list_hlme_args)
}

mixedml <- function(spec, data, subject, time, conv_ratio_thresh, patience, list_hlme_args) {
  .check_args(spec, data, subject, time, conv_ratio_thresh, patience, list_hlme_args)
  list_hlme_args <- .check_list_hlme_args(list_hlme_args, spec, data, subject, time)
  #
  left <- .get_left_side_string(spec)
  random_hlme <- do.call(initiate_random_hlme, list_hlme_args)
  fixed_model <- initiate_reservoirR(spec, data, subject, units = 20, lr = 0.1, sr = 1.3, ridge = 1e-3)
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
    mse <- mean((pred_fixed + pred_rand - data[[left]])**2)
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
  return(list("fixed_model" = fixed_model, "random_model" = random_hlme, "mse_list" = mse_list))
}
