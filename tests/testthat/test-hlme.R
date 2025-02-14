test_that("hlme_full_use", {
  eps <- 0.001
  data <- data_hlme
  ##########
  k <- 0.5
  results0 <- fit_hlme_first_iteration(
    random = Y ~ X1 + X2 + X3,
    data = data,
    pred_fixed = k * data[["Y"]],
    subject = "ID",
    var.time = "Time",
  )
  expect_type(results0, "list")
  expect_named(results0, c("model", "pred_rand"))
  #########
  k <- 0.8
  results1 <- fit_hlme_next_iteration(
    random_hlme = results0$model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  expect_true(mean(abs(results1$model$best - results0$model$best)) > eps)
  #########
  k <- 0.5
  results2 <- fit_hlme_next_iteration(
    random_hlme = results0$model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  expect_true(mean(abs(results2$model$best - results0$model$best)) < eps)
  ##########
  results3 <- forecast_hlme(
    random_hlme = results0$model,
    data = data,
    pred_fixed = data[["Y"]] + 10
  )
  expect_vector(results3, ptype = NULL, size = nrow(data))
  expect_type(results3, "double")
})
