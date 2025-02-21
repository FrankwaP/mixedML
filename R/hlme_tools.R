# suppressWarnings(suppressMessages(library(this.path)))
# library(ini)
library(lcmm)

# devtools::load_all("/home/francois/Documents/github/lcmm_local/")

PRED_FIXED <- "__PRED_FIXED"
PRED_RAND <- "__PRED_RAND"

################################
# utiliser "browser()" dans le code
# puis "devtools::test()" ou "devtools::test_file(…)" dans la console!

.check_initiate_random_hlme <- function(full_random, data, subject, var.time) {
  stopifnot(rlang::is_bare_formula(full_random))
  if (length(full_random) != 3) {
    stop("A left side must be defined for 'random' formula")
  }
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  stopifnot(is.character(var.time))
}

initiate_random_hlme <- function(full_random,
                                 data,
                                 subject,
                                 var.time,
                                 idiag = FALSE,
                                 cor = NULL,
                                 maxiter) {
  .check_initiate_random_hlme(full_random, data, subject, var.time)
  # preparing the hlme formula inputs
  left <- .get_left_side_string(full_random)
  right <- .get_right_side_string(full_random)
  fixed_ <- as.formula(paste0(left, "~1"))
  random_ <- as.formula(paste0("~", right))

  # empty run (maxiter = 0) to initialize the model
  random_hlme <- eval(substitute(
    hlme(
      fixed = fixed_,
      random = random_,
      data = data,
      cor = cor,
      idiag = idiag,
      subject = subject,
      var.time = var.time,
      maxiter = 0,
      posfix = c(1),
    )
  )) # trick to substitute cor=cor by its value
  random_hlme$best["intercept"] <- 0.
  if (missing(maxiter)) {
    random_hlme$call$maxiter <- NULL
  } else {
    random_hlme$call$maxiter <- maxiter
  }
  return(random_hlme)
}

.check_fit_random_hlme <- function(random_hlme, data, pred_fixed) {
  stopifnot(class(random_hlme) == "hlme")
  stopifnot(random_hlme$best["intercept"] == 0.)
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(pred_fixed))
  stopifnot(is.vector(pred_fixed))
}

fit_random_hlme <- function(random_hlme, data, pred_fixed) {
  .check_fit_random_hlme(random_hlme, data, pred_fixed)
  # !!! offsetting is not implemented in LCMM
  # BUT for linear models, fitting "f(X)+offset" on Y is equivalent to fitting f(X) on "Y-offset"
  # so that is the method used so far
  left <- .get_left_side_string(random_hlme$call$fixed)
  data[left] <- data[left] - pred_fixed
  random_hlme <- update(random_hlme, data = data, B = random_hlme$best)
  stopifnot(random_hlme$best["intercept"] == 0.)
  return(list("model" = random_hlme, "pred_rand" = random_hlme$pred[["pred_ss"]]))
}

forecast_hlme <- function(random_hlme, data, pred_fixed) {
  stopifnot(class(random_hlme) == "hlme")
  stopifnot(class(pred_fixed) == "numeric")
  var.time <- random_hlme$var.time
  subject <- colnames(random_hlme$pred)[1]

  # trick to simplify the RE 'rowSums' calculation
  x_labels <- random_hlme$Xnames
  intercept <- x_labels[1]
  data[intercept] <- 1

  # initialization with the marginal effects
  # we have checked that calculating the marginal prediction gives 0 with
  data[PRED_RAND] <- as.vector(predictY(random_hlme, newdata = data, marg = TRUE)$pred)
  if (max(abs(data[PRED_RAND])) > 0) {
    message("The marginal effects are different from 0: this is not a 100% random effects model.")
  } else {
    message("The marginal effects are all equal to 0: this is a 100% random effects model.")
  }
  time_unq <- sort(unique(data[[var.time]]))
  for (i_time in time_unq[-1]) {
    prev_data <- data[data[var.time] < i_time, ]
    tryCatch(
      # we let hlme find out if he can predict or not
      {
        ui <- predictRE(random_hlme, newdata = prev_data)
        actual_data <- data[data[var.time] == i_time, ]
        for (i_row in rownames(actual_data)) {
          actual_subject <- actual_data[i_row, subject]
          ui_subject <- ui[ui[, subject] == actual_subject, ]
          if (nrow(ui_subject) == 1) {
            reffects <- rowSums(actual_data[i_row, x_labels] * ui_subject[, x_labels])
            data[i_row, PRED_RAND] <- data[i_row, PRED_RAND] + reffects
          } else if (nrow(ui_subject) > 1) {
            stop("Problem with method!")
          }
        }
      },
      error = function(e) {

      }
    )
  }
  return(data[[PRED_RAND]])
}
