library(lcmm)

PRED_FIXED <- "__PRED_FIXED"
PRED_RAND <- "__PRED_RAND"


# initialization ----
.prepare_control_hlme <- function(control) {
  stopifnot(is.list(control))

  mandatory <- c("maxiter")
  diff_ <- setdiff(mandatory, names(control))
  if (length(diff_) != 0) {
    stop(paste0("control_hlme must contain ", diff_))
  }
  stopifnot(control$maxiter == round(control$maxiter))
  stopifnot(control$maxiter > 0)
  #
  avoid <- c("fixed", "random", "data", "cor", "idiag", "subject", "var.time")
  inter <- intersect(avoid, names(control))
  if (length(inter) > 0) {
    warning(
      paste0(
        "Parameter ",
        inter,
        " are already defined in MixedML and will be ignored.\n"
      )
    )
  }
  control[avoid] <- NULL
  return(control)
}


.test_initiate_random_hlme <- function(
  random_spec,
  cor,
  data,
  subject,
  var.time
) {
  stopifnot(rlang::is_bare_formula(random_spec))
  if (length(random_spec) != 3) {
    stop("A left side must be defined for 'random' formula")
  }
  stopifnot(
    is.null(cor) | (rlang::is_bare_formula(cor) & (cor[0:2] %in% c("AR", "BM")))
  )
  stopifnot(is.data.frame(data))
  stopifnot(is.character(subject))
  stopifnot(subject %in% names(data))
  stopifnot(is.character(var.time))
  stopifnot(var.time %in% names(data))
}

.initiate_random_hlme <- function(
  random_spec,
  cor,
  data,
  subject,
  var.time,
  control
) {
  .test_initiate_random_hlme(random_spec, cor, data, subject, var.time)
  control <- .prepare_control_hlme(control)

  # preparing the hlme formula inputs
  left <- .get_left_side_string(random_spec)
  right <- .get_right_side_string(random_spec)
  control$fixed <- as.formula(paste0(left, "~1"))
  control$random <- as.formula(paste0("~", right))
  control$cor <- cor
  control$data <- data
  control$subject <- subject
  control$var.time <- var.time
  control$posfix <- c(1)
  # initialization with maxiter = 0
  maxiter_backup <- control$maxiter
  control$maxiter <- 1
  random_hlme <- do.call(hlme, control)
  random_hlme$best[["intercept"]] <- 0. # "$" does not work (conversion to list)
  random_hlme$call$maxiter <- maxiter_backup
  return(random_hlme)
}


# training ----
.check_fit_random_hlme <- function(random_hlme, data, pred_fixed) {
  stopifnot(class(random_hlme) == "hlme")
  stopifnot(random_hlme$best["intercept"] == 0.)
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(pred_fixed))
  stopifnot(is.vector(pred_fixed))
}

.fit_random_hlme <- function(random_hlme, data, pred_fixed) {
  .check_fit_random_hlme(random_hlme, data, pred_fixed)
  # !!! offsetting is not implemented in LCMM
  # BUT for linear models, fitting "f(X)+offset" on Y is equivalent
  # to fitting f(X) on "Y-offset"
  # so that is the method used so far
  left <- .get_left_side_string(random_hlme$call$fixed)
  data[left] <- data[left] - pred_fixed
  random_hlme <- update(random_hlme, data = data, B = random_hlme$best)
  stopifnot(random_hlme$best["intercept"] == 0.)
  return(list(
    "model" = random_hlme,
    "pred_rand" = random_hlme$pred[["pred_ss"]]
  ))
}

# prediction ----

.predict_random_hlme <- function(random_hlme, data) {
  stopifnot(class(random_hlme) == "hlme")
  var.time <- random_hlme$var.time
  subject <- colnames(random_hlme$pred)[1]

  # trick to simplify the RE 'rowSums' calculation
  x_labels <- random_hlme$Xnames
  intercept <- x_labels[1]
  data[intercept] <- 1

  # initialization with the marginal effects
  # we have checked that calculating the marginal prediction gives 0 with
  data[PRED_RAND] <- as.vector(
    predictY(random_hlme, newdata = data, marg = TRUE)$pred
  )
  if (max(abs(data[PRED_RAND])) > 0) {
    message(
      "The marginal effects are different from 0: ",
      "this is not a 100% random effects model."
    )
  } else {
    message(
      "The marginal effects are all equal to 0: ",
      "this is a 100% random effects model."
    )
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
            reffects <- rowSums(
              actual_data[i_row, x_labels] * ui_subject[, x_labels]
            )
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
