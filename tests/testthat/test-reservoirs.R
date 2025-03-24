spec_formula <- Y ~ X1 + X2 + X3

to_scale <- c("X1", "X2", "X3")
data <- lcmm::data_hlme
data[, to_scale] <- scale(data[, to_scale])

test_that("reservoir works", {
  model <- .initiate_reservoirR(
    fixed_spec = spec_formula,
    subject = "ID",
    control = list(units = 20, lr = 0.3, sr = 1.1, ridge = 1e-3, warmup = 2)
  )

  pred_rand <- rnorm(nrow(data))
  fit_result <- .fit_reservoirR(
    model,
    data,
    pred_rand
  )
  expect_named(fit_result, c("model", "pred_fixed"))
  # expect_equal(class(fit_result$model), class(model))
  expect_vector(fit_result$pred_fixed)
})
