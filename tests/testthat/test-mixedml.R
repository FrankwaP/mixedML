spec_formula <- y_mixed ~ x1 + x2 + x3

to_scale <- c('x1', 'x2', 'x3')
data <- data_mixedml
data[, to_scale] <- scale(data[, to_scale])

test_that("mixedml works", {
  output <- mixedml(
    spec = spec_formula,
    data = data,
    subject = 'subject',
    time = 'time',
    conv_ratio_thresh = 0.01,
    patience = 1,
    list_hlme_args = list(subject = "willraiseawarning", var.time = "willraiseawarning")
  )
  expect_named(output, c("fixed_model", "random_model", "mse_list"))
})

