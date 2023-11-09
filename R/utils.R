#' Selection of solutions for analysis
#'
#' @param sols The number of solutions requested
#' @param max Maximum possible data solutions
#'
#' @return Solutions used in further analysis
#' @keywords internal
ds_select_solutions <- function(sols, max) {
  if (is.null(sols)) {
    max
  } else if (max < sols) {
    message(glue::glue(
      "Solutions asked: {sols}
      Maximum possible solutions: {max}"
    ))
    max
  } else {
    sols
  }
}

#' Generate the dataframe out
#'
#' @param inf List with required row size information
#' @param eig List with the information of the values to be displayed
#' @param op Function where each output is used: (1) ds_cf (2) ds_mc (3)
#' ds_mc_A (4) ds_mc_C (5) ds_pc / ds_ro
#'
#' @return A dataframe with the eigenvalue's information of the process
#' @keywords internal
ds_out_tbl <- function(inf, eig, op) {
  alpha <- NULL
  switch(op,
    {
      component <- seq_len(inf$ncol)
      eigenval <- eig$eigenval
    },
    {
      component <- seq_len(inf$n_options)
      eigenval <- eig$eigenval
      alpha <- (1 - (1 - eigenval) / ((inf$ncol - 1) * eigenval))
    },
    {
      component <- seq_len(inf$solutions)
      eigenval <- eig$adj_eigenval
    },
    {
      component <- seq_len(inf$solutions)
      eigenval <- eig$eigenval[seq_len(inf$solutions)]
    },
    {
      component <- seq_len(inf$stimuli)
      eigenval <- eig$eigenval
    },
    stop("Erroneous op value")
  )
  singuval <- sqrt(eigenval)
  delta <- eigenval / sum(eigenval) * 100
  cumdelta <- cumsum(delta) / sum(delta) * 100
  out <- data.frame(
    Component = component,
    Eigenvalue = eigenval,
    SingValue = singuval,
    Delta = delta,
    CumDelta = cumdelta
  )
  if (any(!is.null(alpha))) out$Alpha <- alpha
  out[seq_len(inf$solutions), ]
}

#' Calculate SVD decomposition of the matrix
#'
#' @param obj A Matrix
#'
#' @return Result of `svd` function
#' @keywords internal
ds_eigen <- function(obj) {
  out <- svd(obj)
  list(
    eigenval = out$d,
    singuval = sqrt(out$d),
    eigenvec = out$u
  )
}
