spec_formula <- Y ~ X1 + X2 + X3

to_scale <- c("X1", "X2", "X3")
data <- lcmm::data_hlme
data[, to_scale] <- scale(data[, to_scale])


test_that("esn works", {
  model <- .initiate_ens(
    fixed_spec = spec_formula,
    subject = "ID",
    esn_controls = esn_ctrls(
      units = 50,
      sr = 0.1,
      lr = 0.2,
      ridge = 0.001
    ),
    ensemble_controls = ensemble_ctrls(
      seed_list = c(1L, 2L),
      agg_func = 'median',
      n_procs = 2L
    ),
    fit_controls = fit_ctrls(warmup = 2),
    predict_controls = predict_ctrls()
  )

  pred_rand <- rnorm(nrow(data))
  fit_result <- .fit_reservoir(
    model,
    data,
    pred_rand
  )
  expect_named(fit_result, c("model", "pred_fixed"))
  expect_vector(fit_result$pred_fixed)
})
