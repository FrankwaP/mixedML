spec_formula <- Y ~ X1 + X2 + X3

test_that("mixedml works", {
  mixedml(
    spec = spec_formula,
    data = data_hlme,
    conv_thresh = 0.01,
    list_hlme_args = list(subject = "ID", var.time = "Time")
  )
})
