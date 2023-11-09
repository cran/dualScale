#' Print of Dual Scale analysis
#'
#' @description `print` method for package "dualScale"
#'
#' @param x An dualScale object for which a summary is desired
#' @param ... Arguments to be passed to methods
#'
#' @return A print of the available information from the object
#' @export
#'
#' @seealso [print()]
#'
#' @examples
#' print(ds_cf(curricula))
#' print(ds_cf(preferences))
#' print(ds_mc(singaporean))
#' print(ds_mcf(singaporean, crit = 1))
#' print(ds_pc(christmas))
#' print(ds_ro(goverment))
print.dualScale <- function(x, ...) {
  if (class(x)[1] == "ds_cf") {
    .print_cf(x)
  } else if (class(x)[1] == "ds_mc") {
    .print_mc(x)
  } else if (class(x)[1] == "ds_mcf") {
    .print_mcf(x)
  } else if (class(x)[1] == "ds_pc") {
    .print_pc(x)
  } else if (class(x)[1] == "ds_ro") {
    .print_ro(x)
  }
}

.print_cf <- function(x, ...) {
  print(glue::glue(
    "

    Call: {capture.output(x$call)}

    Type of Analysis: ds_cf

    Results:

    Dual Scaling - Contingency and frequency data analysis

    "
  ))
  print(round(x$out, 4), row.names = FALSE, digits = 4)
  print(glue::glue(
    "

    Distribution of Order 0 Approximation

    "
  ))
  print(data.frame(round(x$appro0, 4)), row.names = TRUE)
  for (k in seq_len(x$solutions)) {
    print(glue::glue(
      "

      Distribution of Order {k} Approximation

      "
    ))
    print(data.frame(round(x$approx[, , k], 4)), row.names = TRUE)
  }
  print(glue::glue(
    "

    Distribution of Order 0 Residual Matrix

    "
  ))
  print(data.frame(round(x$residual0, 4)), row.names = TRUE)
  for (k in seq_len(x$solutions)) {
    print(glue::glue(
      "

      Distribution of Order {k} Residual Matrix

      "
    ))
    print(data.frame(round(x$residual[, , k], 4)), row.names = TRUE)
  }
  invisible(x)
}

.print_mc <- function(x, ...) {
  print(glue::glue(
    "

    Call: {capture.output(x$call)}

    Type of Analysis: ds_mc

    Results:

    Dual Scaling - Multiple choice data analysis

    "
  ))
  print(round(x$out, 4), row.names = FALSE, digits = 4)
  print(glue::glue(
    "

    Distribution of Information Over {x$solutions} Components

    "
  ))
  print(data.frame(round(x$info, 4), row.names = TRUE))
  for (k in seq_len(x$solutions)) {
    print(glue::glue(
      "

      Inter Item Correlation for Component {k}

      "
    ))
    print(data.frame(x$rij[, , k]), row.names = TRUE, digits = 4)
  }
  invisible(x)
}

.print_mcf <- function(x, ...) {
  print(glue::glue(
    "

    Call: {capture.output(x$call)}

    Type of Analysis: ds_mcf

    Results:

    Dual Scaling - Dual Scaling - Forced multiple choice data analysis

    Forced classification of the criterion item (type A)

    "
  ))
  print(round(x$out_a, 4), row.names = FALSE, digits = 4)
  print(glue::glue(
    "

    Distribution of Information Over {x$solutions_mcf} Components

    "
  ))
  print(data.frame(round(x$info_a, 4)), row.names = FALSE)
  for (k in seq_len(x$solutions_mcf)) {
    print(glue::glue(
      "

      Inter Item Correlation for Component {k}

      "
    ))
    print(data.frame(x$rij_a[, , k]), row.names = TRUE, digits = 4)
  }
  print(glue::glue(
    "

    Dual scaling of non-criterion items by ignoring the criterion item (type B)

    "
  ))
  print(round(x$out_b, 4), row.names = FALSE, digits = 4)
  print(glue::glue(
    "

    Distribution of Information Over {x$solutions_mc} Components

    "
  ))
  print(data.frame(round(x$info_b, 4)), row.names = FALSE)
  for (k in seq_len(x$solutions_mc)) {
    print(glue::glue(
      "

      Inter Item Correlation for Component {k}:

      "
    ))
    print(data.frame(x$rij_b[, , k]), row.names = TRUE, digits = 4)
  }
  print(glue::glue(
    "

    Dual scaling of non-criterion items after eliminating the influence \\
    of the criterion item (type C)

    "
  ))
  print(round(x$out_c, 4), row.names = FALSE, digits = 4)
  print(glue::glue(
    "

    Distribution of Information Over {x$solutions_mcf} Components

    "
  ))
  print(data.frame(round(x$info_c, 4)), row.names = TRUE)
  invisible(x)
}

.print_pc <- function(x, ...) {
  print(glue::glue(
    "

    Call: {capture.output(x$call)}

    Type of Analysis: ds_pc

    Results:

    Dual Scaling - Paired comparison data analysis

    "
  ))
  print(round(x$out, 4), row.names = FALSE, digits = 4)
  print(glue::glue(
    "

    Matrix E

    "
  ))
  print(data.frame(round(x$mat_e, 4)))
  invisible(x)
}

.print_ro <- function(x, ...) {
  print(glue::glue(
    "

    Call: {capture.output(x$call)}

    Type of Analysis: ds_ro

    Results:

    Dual Scaling - Rank order data analysis

    "
  ))
  print(round(x$out, 4), row.names = FALSE, digits = 4)
  print(glue::glue(
    "

    Matrix E

    "
  ))
  print(data.frame(round(x$mat_e, 4)))
  invisible(x)
}
