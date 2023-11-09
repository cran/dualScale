ds_input_check <- function(input, solutions) {
  if (!any(input == round(input))) {
    stop("Values must be an integer")
  }

  if (!is.null(solutions) && solutions != round(solutions)) {
    stop("Solutions must be an integer")
  }
}

ds_cf_check <- function(input, solutions) {
  ds_input_check(input, solutions)
}

ds_mc_input <- function(input, solutions, mode = NULL) {
  out <- FALSE
  if (any(is.na(input)) || any(unlist(apply(input, 2, tabulate)) == 0)) {
    message(
      glue::glue(
        "You have NA values.
        The input is transformed with the function ds_mc_check.
        The mode selected is: {mode}"
      )
    )
    out <- TRUE
  }
  if (!out) {
    ds_input_check(input, solutions)
  }
  out
}

ds_mcf_input <- function(input, solutions, mode, crit) {
  out <- FALSE
  if (is.na(crit)) {
    stop("A value must be assigned in crit")
  }
  if (crit > ncol(input)) {
    ncol <- ncol(input)
    stop(glue::glue("Item Criterium is bigger than {ncol}"))
  }
  out <- ds_mc_input(input, solutions, mode)
  out
}
