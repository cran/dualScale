ds_pc_calc <- function(input, solutions) {
  inf <- .pc_info(input, solutions)
  mat <- .pc_mat(inf)
  eig <- ds_eigen(mat$h_mat)
  val <- .pc_values(inf, mat, eig)
  list(
    item_op_lbl = inf$item_op_lbl,
    sub_lbl = inf$row_name,
    solutions = inf$solutions,
    norm_opt = as.data.frame(val$x_val),
    proj_opt = as.data.frame(val$x_adj_val),
    norm_sub = as.data.frame(val$y_val),
    proj_sub = as.data.frame(val$y_adj_val),
    out = ds_out_tbl(inf, eig, 5),
    mat_e = as.data.frame(mat$e_mat)
  )
}

.pc_info <- function(input, solutions) {
  root <- Re(polyroot(c(ncol(input) * -2, -1, 1)))
  stimuli <- floor(root[root > 0])
  max_sol <- stimuli - 1
  sol <- ds_select_solutions(solutions, max_sol)
  list(
    orig_data = as.matrix(input),
    ncol = ncol(input),
    nrow = nrow(input),
    solutions = sol,
    stimuli = stimuli,
    total_sum = sum(input),
    row_sum = rowSums(input),
    col_sum = colSums(input),
    item_op_lbl = paste0("q.", seq_len(stimuli)),
    row_name = paste0("s.", seq_len(nrow(input)))
  )
}

.pc_mat <- function(inf) {
  design <- eba::pcX(nstimuli = inf$stimuli, omitRef = FALSE)
  e_mat <- inf$orig_data %*% design
  e_mat_t <- t(e_mat) %*% e_mat
  h_mat <- 1 / (inf$nrow * inf$stimuli * (inf$stimuli - 1)^2) * e_mat_t
  list(
    e_mat = e_mat,
    h_mat = h_mat
  )
}

.pc_values <- function(inf, mat, eig) {
  x <- eig$eigenvec[, seq_len(inf$solutions)] * (inf$solutions - 1)
  x_adj <- x %*% diag(eig$singuval[seq_len(inf$solutions)], inf$solutions)
  den <- sqrt(1 / (inf$nrow * inf$stimuli * (inf$stimuli - 1)^2))
  y <- (den * mat$e_mat %*% x)[, seq_len(inf$solutions)]
  y <- (1 / eig$singuval[seq_len(inf$solutions)]) * y
  y_adj <- y %*% diag(
    eig$singuval[seq_len(inf$solutions)],
    inf$solutions
  )
  list(
    x_val = x,
    y_val = y,
    x_adj_val = x_adj,
    y_adj_val = y_adj
  )
}
