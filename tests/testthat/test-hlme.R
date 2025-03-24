test_that("hlme_full_use", {
  eps <- 0.01 # correlated with list_hlme_args
  data <- lcmm::data_hlme
  ##########
  model <- .initiate_random_hlme(
    random_spec = Y ~ X1 + X2 + X3,
    cor = NULL,
    data = data,
    subject = "ID",
    var.time = "Time",
    control = list(maxiter = 1000, idiag = TRUE)
  )
  expect_type(model, "list")
  expect_s3_class(model, "hlme")
  #########
  k <- 0.5
  results0 <- .fit_random_hlme(
    random_hlme = model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  expect_type(results0, "list")
  expect_named(results0, c("model", "pred_rand"))
  expect_s3_class(results0$model, "hlme")
  expect_vector(results0$pred_rand)
  #########
  k <- 0.8
  results1 <- .fit_random_hlme(
    random_hlme = results0$model,
    data = data,
    pred_fixed = k * data[["Y"]]
  )
  expect_true(
    mean(abs(
      results1$model$best - results0$model$best
    )) >
      eps
  )
  ##########
  k <- 0.5
  results3 <- .predict_random_hlme(
    random_hlme = results0$model,
    data = data
  )
  expect_vector(results3, ptype = NULL, size = nrow(data))
  expect_type(results3, "double")
})
