

.get_left_side_string <- function(spec_formula) {
  return(deparse(spec_formula[[2]]))
}

.get_right_side_string <- function(spec_formula) {
  return(deparse(spec_formula[[3]]))
}

.spec_formula_to_labels <- function(spec_formula) {
  left <- .get_left_side_string(spec_formula)

  right <-  .get_right_side_string(spec_formula)
  right <-  gsub(" ", "", right)
  right <- strsplit(right, "+", fixed = TRUE)[[1]]

  return(list(y_label = left, x_labels = right))
}
