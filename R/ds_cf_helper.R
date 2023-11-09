ds_cf_calc <- function(input, solutions) {
  inf <- .cf_info(input, solutions)
  mat <- .cf_matrix(inf)
  eig <- ds_eigen(mat$mat_c)
  vals <- .cf_values(inf, mat, eig)
  appr <- .cf_approx(inf, eig, vals)
  res <- .cf_residual(inf, appr)
  list(
    total_sub = inf$row_sum,
    total_item = inf$col_sum,
    solutions = inf$solutions,
    col_name = inf$col_name,
    row_name = inf$row_name,
    out = ds_out_tbl(inf, eig, 1),
    norm_opt = as.data.frame(vals$x),
    proj_opt = as.data.frame(vals$x_adj),
    norm_sub = as.data.frame(vals$y),
    proj_sub = as.data.frame(vals$y_adj),
    appro0 = as.data.frame(appr$order_0),
    approx = as.data.frame(appr$order_k),
    residual0 = as.data.frame(res$residual_0),
    residual = as.data.frame(res$residual)
  )
}

.cf_info <- function(input, solutions) {
  max_sol <- min(nrow(input), ncol(input)) - 1
  list(
    orig_data = as.matrix(input),
    ncol = ncol(input),
    nrow = nrow(input),
    solutions = ds_select_solutions(solutions, max_sol),
    total_sum = sum(input),
    row_sum = apply(input, 1, sum),
    col_sum = apply(input, 2, sum),
    col_name = paste0("q.", seq_len(ncol(input))),
    row_name = paste0("s.", seq_len(nrow(input)))
  )
}

.cf_matrix <- function(inf) {
  diag_c <- diag(inf$col_sum)
  diag_r <- diag(inf$row_sum)
  col_sum <- inf$col_sum
  ncol <- inf$ncol
  o_data <- inf$orig_data
  tot_sum <- inf$total_sum
  d_1 <- diag(1 / sqrt(col_sum))
  d_2 <- solve(diag_r)
  d_3 <- sqrt(diag_c)
  mat_1 <- list(d_1, t(o_data), d_2, o_data, d_1)
  mat_2 <- list(d_3, tcrossprod(matrix(1, ncol, 1)), d_3)
  list(
    mat_c = Reduce("%*%", mat_1) - 1 / tot_sum * Reduce("%*%", mat_2),
    d_1 = d_1,
    d_2 = d_2
  )
}

.cf_values <- function(inf, mat, eig) {
  sols <- inf$solutions
  o_data <- inf$orig_data
  d_2 <- mat$d_2
  singvl <- eig$singuval
  w <- apply(eig$eigenvec, 2, .cf_w, inf$total_sum)[, seq_len(sols)]
  x <- mat$d_1 %*% w
  y_mats <- list(d_2, o_data, x, diag(1 / singvl[seq_len(sols)], sols))
  y <- Reduce("%*%", y_mats)
  list(
    x = x,
    y = y,
    x_adj = x %*% diag(singvl[seq_len(sols)], sols),
    y_adj = y %*% diag(singvl[seq_len(sols)], sols)
  )
}

.cf_w <- function(x, y) {
  as.vector(sqrt((y / t(x) %*% x))) * x
}

.cf_approx <- function(inf, eig, vals) {
  len_col <- seq_len(inf$ncol)
  len_sol <- seq_len(inf$solutions)
  ord_0 <- array(dim = c(inf$nrow, inf$ncol))
  ord_k <- array(dim = c(inf$nrow, inf$ncol, inf$solutions))
  for (k in len_sol) {
    for (j in len_col) {
      ord_0[, j] <- (inf$row_sum * inf$col_sum[j]) / inf$total_sum
      val <- eig$singuval[k] * vals$x[j, k] * vals$y[, k]
      ord_k[, j, k] <- ord_0[, j] * val
    }
    if (k == 1) {
      ord_k[, , k] <- ord_0 + ord_k[, , k]
    } else {
      ord_k[, , k] <- ord_k[, , (k - 1)] + ord_k[, , k]
    }
  }
  list(
    order_0 = ord_0,
    order_k = ord_k
  )
}

.cf_residual <- function(inf, appr) {
  dim <- inf$solutions + 1
  res <- array(dim = c(inf$nrow, inf$ncol, dim))
  res[, , 1] <- inf$orig_data - appr$order_0
  for (k in seq(2, dim)) {
    res[, , k] <- inf$orig_data - appr$order_k[, , k - 1]
  }
  list(
    residual_0 = res[, , 1],
    residual = res[, , seq(2, dim), drop = FALSE]
  )
}
