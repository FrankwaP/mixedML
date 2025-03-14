spec_formula <- Y ~ X1 + X2 + X3

to_scale <- c("X1", "X2", "X3")
data <- data_hlme
data[, to_scale] <- scale(data[, to_scale])

test_that("reservoir works", {
  model <- .initiate_reservoirR(
    # spec_formula,
    # data,
    # subject = "ID",
    units = 20,
    lr = 0.3,
    sr = 1.1,
    ridge = 1e-3
  )
  pred_rand <- rnorm(nrow(data))
  fit_result <- .fit_reservoirR(
    model,
    spec_formula,
    data,
    "ID",
    "Time",
    pred_rand
  )
  expect_named(fit_result, c("model", "pred_fixed"))
  # expect_equal(class(fit_result$model), class(model))
  expect_vector(fit_result$pred_fixed)
})
