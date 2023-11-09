ds_ro_calc <- function(input, solutions) {
  inf <- .ro_info(input, solutions)
  mat <- .ro_mat(inf)
  eig <- ds_eigen(mat$h_mat)
  val <- .ro_values(inf, mat, eig)
  eig_r <- ds_eigen(mat$c_mat)
  val_r <- .ro_values_ranks(inf, mat, eig_r)
  list(
    orig_data = inf$orig_data,
    item_op_lbl = inf$item_op_lbl,
    sub_name = inf$row_name,
    solutions = inf$solutions,
    mat_e = mat$e_mat,
    out = ds_out_tbl(inf, eig, 5),
    norm_opt = as.data.frame(val$x_val),
    proj_opt = as.data.frame(val$x_adj_val),
    norm_sub = as.data.frame(val$y_val),
    proj_sub = as.data.frame(val$y_adj_val),
    out_rank = ds_out_tbl(inf, eig_r, 5),
    norm_opt_rank = as.data.frame(val_r$x_val),
    proj_opt_rank = as.data.frame(val_r$x_adj_val),
    norm_rank = as.data.frame(val_r$y_val),
    proj_rank = as.data.frame(val_r$y_adj_val)
  )
}


.ro_info <- function(input, solutions) {
  stimuli <- ncol(input)
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

.ro_mat <- function(inf) {
  e_mat <- unname(inf$stimuli + 1 - (2 * inf$orig_data))
  e_mat_t <- as.matrix(t(e_mat) %*% e_mat)
  h_mat <- 1 / (inf$nrow * inf$stimuli * (inf$stimuli - 1)^2) * e_mat_t
  c_sub_rank <- .c_mat_fn(inf)
  diag_col <- diag(colSums(c_sub_rank))
  diag_row <- diag(rowSums(c_sub_rank))
  f_t <- sum(c_sub_rank)
  i_sqrt_diag_c <- sqrt(1 / diag_col)
  i_sqrt_diag_c[is.infinite(i_sqrt_diag_c)] <- 0
  ones <- matrix(data = 1, nrow = inf$ncol, ncol = inf$ncol)
  mat_1 <- list(
    i_sqrt_diag_c, t(c_sub_rank),
    solve(diag_row), c_sub_rank,
    i_sqrt_diag_c
  )
  c_mat_1 <- Reduce("%*%", mat_1)
  c_mat_2 <- Reduce("%*%", list(sqrt(diag_col), ones, sqrt(diag_col)))
  c_mat <- c_mat_1 - c_mat_2 / f_t
  list(
    e_mat = e_mat,
    h_mat = h_mat,
    c_mat = c_mat,
    c_sub_rank = c_sub_rank
  )
}

.c_mat_fn <- function(inf) {
  out <- matrix(nrow = inf$ncol, ncol = inf$ncol)
  for (j in seq_len(inf$ncol)) {
    for (k in seq_len(inf$ncol)) {
      out[k, j] <- length(which(inf$orig_data[, j] == k))
    }
  }
  t(out)
}

.ro_values <- function(inf, mat, eig) {
  .pc_values(inf, mat, eig)
}

.ro_values_ranks <- function(inf, mat, eig_r) {
  x <- eig_r$eigenvec[, seq_len(inf$solutions)] * (inf$solutions - 1)
  x_adj <- x %*% diag(eig_r$singuval[seq_len(inf$solutions)], inf$solutions)
  y <- .y_fn(mat, eig_r, x)
  y_adj <- y %*% diag(
    eig_r$singuval[seq_len(inf$solutions)],
    inf$solutions
  )
  list(
    x_val = x,
    y_val = y,
    x_adj_val = x_adj,
    y_adj_val = y_adj
  )
}

.y_fn <- function(mat, eigen_r, x) {
  y <- matrix(nrow = nrow(x), ncol = ncol(x))
  for (j in seq_len(ncol(x))) {
    y[, j] <- solve(
      diag(colSums(mat$c_sub_rank))
    ) %*% mat$c_sub_rank %*% x[, j] / eigen_r$singuval[j]
  }
  y
}
