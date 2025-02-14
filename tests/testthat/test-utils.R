spec_formula <- Y1 ~ X1 + X2 + X3

test_that(".get_left_side_string", {
  expect_equal(.get_left_side_string(spec_formula), "Y1")
})

test_that(".get_right_side_string", {
  expect_equal(.get_right_side_string(spec_formula), "X1 + X2 + X3")
})


test_that("spec_formula_to_labels", {
  labels <- .spec_formula_to_labels(spec_formula)

  expect_named(labels, c("y_label", "x_labels"))

  expect_equal(labels[["y_label"]], "Y1")

  expect_vector(labels[["x_labels"]])
  expect_equal(labels[["x_labels"]], c("X1", "X2", "X3"))
})
