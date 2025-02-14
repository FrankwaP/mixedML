# suppressWarnings(suppressMessages(library(this.path)))
# library(ini)
library(lcmm)

# devtools::load_all("/home/francois/Documents/github/lcmm_local/")

PRED_FIXED <- "__PRED_FIXED"
PRED_RAND <- "__PRED_RAND"

################################
# utiliser "browser()" dans le code
# puis "devtools::test()" ou "devtools::test_file(…)" dans la console!

initiate_random_hlme <- function(random,
                                 data,
                                 subject,
                                 var.time,
                                 idiag = FALSE,
                                 cor = NULL,
                                 maxiter) {
  # preparing the hlme formula inputs
  if (length(random) != 3) {
    stop("A left side must be defined for 'random' formula")
  }
  left <- .get_left_side_string(random)
  right <- .get_right_side_string(random)
  fixed_ <- as.formula(paste0(left, "~1"))
  random_ <- as.formula(paste0("~", right))

  # empty run (maxiter = 0) to initialize the model
  # the eval is used to handle the transfer of parameters to hlme
  command <- substitute(
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
    ),
    env = as.list(environment())
  ) # used to debug the command, we could just run eval directly
  random_hlme <- eval(command)
  random_hlme$best["intercept"] <- 0.
  if (missing(maxiter)) {
    random_hlme$call$maxiter <- NULL
  } else {
    random_hlme$call$maxiter <- maxiter
  }
  return(random_hlme)
}

fit_random_hlme <- function(random_hlme, data, pred_fixed) {
  left <- deparse(random_hlme$call$fixed[[2]])
  # !!! offsetting is not implemented in LCMM
  # BUT for linear models, fitting "f(X)+offset" on Y is equivalent to fitting f(X) on "Y-offset"
  # so that is the method used so far
  data[left] <- data[left] - pred_fixed
  random_hlme <- update(random_hlme, data = data, B = random_hlme$best)
  if (random_hlme$best["intercept"] <- 0.) {
    stop("This model should have a 0 fixed intercept.")
  }
  return(list("model" = random_hlme, "pred_rand" = random_hlme$pred["pred_ss"]))
}

forecast_hlme <- function(random_hlme, data, pred_fixed) {
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
