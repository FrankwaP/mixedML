spec_formula <- y_mixed ~ x1 + x2 + x3

to_scale <- c('x1', 'x2', 'x3')
data <- data_mixedml
data[, to_scale] <- scale(data[, to_scale])

test_that("mixedml works", {
  output <- reservoir_mixedml(
    spec = spec_formula,
    data = data,
    subject = 'subject',
    time = 'time',
    conv_ratio_thresh = 0.01,
    patience = 1,
    list_hlme_args = list(subject = "willraiseawarning", var.time = "willraiseawarning"),
    list_reservoir_args = list(
      units = 20,
      lr = 0.1,
      sr = 1.3,
      ridge = 1e-3
    )
  )

  expect_named(output, c("fixed_model", "random_model", "mse_list"))
})
