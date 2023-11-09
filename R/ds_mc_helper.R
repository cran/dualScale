ds_mc_calc <- function(input, solutions) {
  inf <- .mc_info(input, solutions)
  mat <- .mc_mat(inf)
  eig <- ds_eigen(mat$mat_svd)
  val <- .mc_values(inf, mat, eig)
  corr <- .mc_corr(inf, mat, val)
  list(
    orig_data = inf$orig_data,
    item_op_lbl = inf$item_op_lbl,
    sub_name = inf$row_name,
    solutions = inf$solutions,
    norm_opt = as.data.frame(val$x_val),
    proj_opt = as.data.frame(val$x_adj_val),
    norm_sub = as.data.frame(val$y_val),
    proj_sub = as.data.frame(val$y_adj_val),
    out = ds_out_tbl(inf, eig, 2),
    item_stat = .mc_stats(inf, eig, corr),
    info = .mc_inf(inf, corr),
    rij = corr$corr_mat[seq_len(inf$ncol), seq_len(inf$ncol), ]
  )
}

.mc_info <- function(input, solutions) {
  max_col_val <- apply(input, 2, max)
  max_sol <- sum(max_col_val) - ncol(input)
  sol <- ds_select_solutions(solutions, max_sol)
  input_temp <- input
  colnames(input_temp) <- paste0("q.", seq_len(ncol(input)))
  item_op_lbl <- vector("list", ncol(input))
  for (j in seq_len(ncol(input))) {
    item_op_lbl[[j]] <- paste(
      colnames(input_temp)[j],
      seq_len(max_col_val[j]),
      sep = ":"
    )
  }
  rownames(input_temp) <- paste0("s.", seq_len(nrow(input)))
  list(
    orig_data = as.matrix(input),
    ncol = ncol(input),
    nrow = nrow(input),
    solutions = sol,
    total_sum = sum(input),
    col_name = colnames(input_temp),
    row_name = rownames(input_temp),
    max_col_val = max_col_val,
    n_options = sum(max_col_val),
    item_op_lbl = unlist(item_op_lbl)
  )
}

.mc_mat <- function(info) {
  aux <- unname(
    c(0, cumsum(info$max_col_val))
  )[seq_len(length(info$max_col_val))]
  mat_f <- Matrix::sparseMatrix(
    x = 1,
    i = seq_len(info$nrow) %*% t(matrix(1, info$ncol, 1)),
    j = t(aux + t(info$orig_data))
  )
  mat_c <- Matrix::t(mat_f) %*% mat_f / info$ncol
  row_col <- info$nrow * info$ncol
  mat_f_c <- apply(mat_f, 2, sum)
  mat_f_r <- apply(mat_f, 1, sum)
  corr <- (mat_f_c^.5 %*% t(mat_f_c^.5)) / row_col
  diag_mat_f_c <- diag(mat_f_c^-.5)
  mat_svd <- (diag_mat_f_c %*% mat_c %*% diag_mat_f_c) - corr
  list(
    mat_f = mat_f,
    mat_c = mat_c,
    row_col = row_col,
    mat_f_c = mat_f_c,
    mat_f_r = mat_f_r,
    mat_svd = mat_svd
  )
}

.mc_values <- function(info, mat, eigen) {
  w <- (mat$row_col / diag(
    t(eigen$eigenvec) %*% eigen$eigenvec
  ))^.5 * eigen$eigenvec[, seq_len(info$solutions)]
  x <- diag(mat$mat_f_c^-.5) %*% w
  x_adj <- x %*% diag(sqrt(eigen$eigenval)[1:info$solutions])
  y_adj <- 1 / mat$mat_f_r * mat$mat_f %*% x
  y <- y_adj %*% diag(1 / eigen$singuval[1:info$solutions])
  list(
    x_val = as.matrix(x),
    y_val = as.matrix(y),
    x_adj_val = as.matrix(x_adj),
    y_adj_val = as.matrix(y_adj)
  )
}

.mc_corr <- function(info, mat, values) {
  seq_r <- seq_len(info$nrow)
  seq_c <- seq_len(info$ncol)
  seq_sol <- seq_len(info$solutions)
  cm1 <- ff::ff(
    initdata = 0,
    dim = c(info$nrow, info$ncol, info$solutions),
    dimorder = c(1, 2, 3)
  )
  for (k in seq_sol) {
    q <- as.matrix(Matrix::t(mat$mat_f) * values$x_adj[, k])
    q <- matrix(q[(q != 0)],
      ncol = info$ncol,
      nrow = info$nrow,
      byrow = TRUE
    )
    cm1[, , k] <- q
  }
  cm <- ff::ff(
    initdata = 0,
    dim = c(info$nrow + 1, info$ncol + 1, info$solutions),
    dimorder = c(1, 2, 3)
  )
  cm[seq_r, seq_c, ] <- cm1[, , seq_sol]
  cm[seq_r, info$ncol + 1, ] <- ff::ffapply(
    MARGIN = c(1, 3),
    AFUN = "sum",
    X = cm1[seq_r, seq_c, seq_sol],
    CFUN = "cbind",
    RETURN = TRUE
  )[, seq_sol]
  cm[info$nrow + 1, , ] <- ff::ffapply(
    MARGIN = c(2, 3),
    AFUN = "sum",
    X = cm[seq_r, , ],
    CFUN = "cbind",
    RETURN = TRUE
  )
  corr_mat <- ff::ff(
    initdata = 0,
    dim = c(info$ncol + 1, info$ncol + 1, info$solutions),
    dimorder = c(1, 2, 3)
  )
  for (k in seq_len(info$solutions)) {
    corr_mat[, , k] <- stats::cor(cm[seq_r, , k])
  }
  list(
    corr_mat = corr_mat,
    cm = cm
  )
}

.mc_stats <- function(info, eigen, corr) {
  ssj_tbl <- array(0, dim = c(info$ncol + 1, 3, info$solutions))
  ssj_tbl[, 1, ] <- t(t(apply(
    corr$cm[seq_len(info$nrow), , ]^2,
    c(2, 3),
    sum
  )) / eigen$eigenval[seq_len(info$solutions)])
  ssj_tbl[, 2, ] <- t(.mc_inf(info, corr))
  ssj_tbl[, 3, ] <- sqrt(ssj_tbl[, 2, ])
  out <- array(0, dim = c(info$ncol, 4, info$solutions))
  out[, 1, ] <- seq_len(info$ncol)
  out[, 2:4, ] <- ssj_tbl[seq_len(info$ncol), , ]
  colnames(out) <- c("ITEM(j)", "SS(j)", "R2(jt)", "R(jt)")
  out
}

.mc_inf <- function(info, corr) {
  out <- t(corr$corr_mat[, info$ncol + 1, ])^2
  out[, info$ncol + 1] <- rowMeans(out[, seq_len(info$ncol)])
  colnames(out) <- paste0("Item", seq_len(info$ncol + 1))
  as.data.frame(out)
}
