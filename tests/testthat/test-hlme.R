test_that("hlme_full_use", {
  eps <- 0.01  # correlated with list_hlme_args
  data <- data_hlme
  ##########
  model <- initiate_random_hlme(
    random = Y ~ X1 + X2 + X3,
    data = data,
    subject = "ID",
    var.time = "Time",
    maxiter = 10
  )
  expect_type(model, "list")
  expect_s3_class(model, "hlme")
  #########
  k <- 0.5
  results0 <- fit_random_hlme(
    random_hlme = model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  #########
  k <- 0.8
  results1 <- fit_random_hlme(
    random_hlme = results0$model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  expect_true(mean(abs(
    results1$model$best - results0$model$best
  )) > eps)
  #########
  k <- 0.5
  results2 <- fit_random_hlme(
    random_hlme = results0$model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  expect_true(mean(abs(
    results2$model$best - results0$model$best
  )) < eps)
  ##########
  k <- 0.8
  results3 <- forecast_hlme(
    random_hlme = results0$model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  expect_vector(results3, ptype = NULL, size = nrow(data))
  expect_type(results3, "double")
})
