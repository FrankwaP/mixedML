fixed_spec <- y_mixed ~ x1 + x2 + x3
random_spec <- y_mixed ~ x1 + x2
subject = 'subject'
time = 'time'

to_scale <- c('x1', 'x2', 'x3')
data <- data_mixedml
data[, to_scale] <- scale(data[, to_scale])

test_that("mixedml works", {
  output <- reservoir_mixedml(
    fixed_spec = fixed_spec,
    random_spec = random_spec,
    data = data,
    subject = subject,
    time = time,
    cor = NULL,
    control_hlme = list(
      subject = "willraiseawarning",
      var.time = "willraiseawarning"
    ),
    control_reservoir = list(
      units = 20,
      lr = 0.1,
      sr = 1.3,
      ridge = 1e-3
    ),
    control_mixedml = list(conv_ratio_thresh = 0.01, patience = 1)
  )

  expect_named(output, c("fixed_model", "random_model", "mse_list"))
})
