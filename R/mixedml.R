# devtools::install_github(repo = "reservoirpy/reservoirR")
library(neuralnet)
library(reservoirnet)
# usethis::use_test("mixedml")

###### test of do.call (it works!)
# sub_test <- function(b, a) {
#   print('COUCOU!')
#   print(a)
#   print(b)
# }
#
# test <- function(list_sub_test_args) {
#   do.call(sub_test, list_sub_test_args)
# }
#
#
# test(list(a=2, b=10))


# https://github.com/reservoirpy/reservoirR/blob/main/tests/testthat/tests.R

source <- reservoirnet::createNode("Input")
readout <- reservoirnet::createNode("Ridge")
reservoir <- reservoirnet::createNode("Reservoir", units = 100, lr = 0.2, sr = 0.8)

model <- reservoirnet::link(reservoir, readout)

mixedml <- function(spec, data, conv_thresh, list_hlme_args) {
  names_test <- names(list_hlme_args)
  if (("random" %in% names_test) | ("data" %in% names_test)) {
    stop("random and data are not necessary in list_hlme_args.")
  }
  list_hlme_args$random <- spec
  list_hlme_args$data <- data
  random_hlme <- do.call(initiate_random_hlme, list_hlme_args)
  #
  left <- .get_left_side_string(spec)
  # labels <- .spec_formula_to_labels(spec)
  ########
  data_fixed <- data
  istep <- 0
  while (TRUE) {
    nn <- neuralnet(spec,
      data = data_fixed, hidden = c(20,10, 5, 3),
      linear.output = TRUE, threshold = 0.01
    )
    print(nn)
    browser()
    pred_fixed <- compute(nn, data_fixed)$net.result

    random_result <- fit_random_hlme(random_hlme, data, pred_fixed)
    random_hlme <- random_result$model
    pred_rand <- random_result$pred_rand

    break
  }
}
