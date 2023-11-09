ds_mcf_calc <- function(input, solutions, crit, info_mc) {
  inf <- .mcf_info(input, solutions, crit)
  mat <- .mcf_mat_a(inf)
  eig <- ds_eigen(mat$mat_a)
  xval <- .mcf_x_values_a(inf, mat, eig)
  corr <- .mcf_corr_a(inf, mat, xval)
  tbl <- .mcf_tbl_a(inf, corr, eig)
  space <- .mcf_space(inf, mat, xval)
  proj_sc <- .mcf_proj_score(inf, mat, eig, xval, tbl)
  mtch_pred <- .mcf_match_pred(inf, xval, proj_sc)
  cont <- .mcf_contamination(info_mc, inf, space)
  list(
    ncol = inf$ncol,
    nrow = inf$nrow,
    solutions = inf$solutions,
    sub_name = inf$sub_name,
    item_op_lbl = inf$item_op_lbl,
    info_a = as.data.frame(tbl$tbl_e3),
    out_a = ds_out_tbl(inf, tbl, 3),
    item_stat_a = .mcf_item_stat_a(inf, tbl),
    rij_a = corr$corr_m[
      seq_len(inf$ncol),
      seq_len(inf$ncol),
      seq_len(inf$solutions)
    ],
    norm_opt_a = as.data.frame(xval$x[, seq_len(inf$solutions)]),
    norm_sub_a = as.data.frame(proj_sc$norm_sub_a[, seq_len(inf$solutions)]),
    proj_opt_a = as.data.frame(proj_sc$proj_opt_a[, seq_len(inf$solutions)]),
    proj_sub_a = as.data.frame(proj_sc$proj_sub_a[, seq_len(inf$solutions)]),
    info_c = as.data.frame(tbl$tbl_e3),
    out_c = ds_out_tbl(inf, eig, 4),
    norm_opt_c = as.data.frame(xval$x),
    norm_sub_c = as.data.frame(proj_sc$norm_sub_c),
    proj_opt_c = as.data.frame(proj_sc$proj_opt_c),
    proj_sub_c = as.data.frame(proj_sc$proj_sub_c),
    match = mtch_pred$match,
    pred = mtch_pred$pred,
    comp_cont = cont$comp_cont,
    tot_cont = cont$tot_cont
  )
}

.mcf_info <- function(input, solutions, crit) {
  max_col_val <- apply(input, 2, max)
  max_sol <- as.integer(max_col_val[crit] - 1)
  sol <- ds_select_solutions(solutions, max_sol)
  input_temp <- input
  colnames(input_temp) <- paste0("q.", seq_len(ncol(input)))
  rownames(input_temp) <- paste0("s.", seq_len(nrow(input)))
  item_op_lbl <- vector("list", ncol(input))
  for (j in seq_len(ncol(input))) {
    item_op_lbl[[j]] <- paste(
      colnames(input_temp)[j],
      seq_len(max_col_val[j]),
      sep = ":"
    )
  }
  list(
    orig_data = as.matrix(input),
    ncol = ncol(input),
    nrow = nrow(input),
    solutions = sol,
    total_sum = sum(input),
    sub_sum = rowSums(input),
    item_sum = colSums(input),
    item_name = colnames(input_temp),
    sub_name = rownames(input_temp),
    max_col_val = max_col_val,
    n_opts = sum(max_col_val),
    crit = crit,
    item_op_lbl = unlist(item_op_lbl),
    components = sum(max_col_val) - ncol(input)
  )
}

.mcf_mat_a <- function(inf) {
  k_val <- 5200
  mat_c <- matrix(0, inf$n_opts, inf$n_opts)
  mat_fk <- array(dim = c(inf$nrow, inf$n_opts))
  mat_f <- mat_fk
  for (i in seq_len(inf$nrow)) {
    init_f_k <- 99
    init_f <- 99
    for (j in seq_len(inf$ncol)) {
      f_val_k <- array(0, inf$max_col_val[j])
      f_val <- array(0, inf$max_col_val[j])
      ifelse(j == inf$crit,
        f_val_k[inf$orig_data[i, j]] <- k_val,
        f_val_k[inf$orig_data[i, j]] <- 1
      )
      f_val[inf$orig_data[i, j]] <- 1
      init_f_k <- c(init_f_k, f_val_k)
      init_f <- c(init_f, f_val)
    }
    c_1 <- init_f_k[-1] %*% t(init_f_k[-1])
    mat_c <- mat_c + c_1
    mat_fk[i, ] <- init_f_k[-1]
    mat_f[i, ] <- init_f[-1]
  }
  c_i <- mat_c / (inf$ncol - 1 + k_val)
  f_tot <- inf$nrow * (inf$ncol - 1 + k_val)
  f_c <- colSums(mat_fk)
  diag_sqrt <- diag(sqrt(f_c))
  ones <- array(1, dim = c(inf$n_opts, inf$n_opts))
  corr_term <- (diag_sqrt %*% ones %*% diag_sqrt) / f_tot
  diag_sqrt <- diag(1 / sqrt(f_c), inf$n_opts, inf$n_opts)
  mat_a <- (diag_sqrt %*% c_i %*% diag_sqrt) - corr_term
  list(
    mat_a = mat_a,
    mat_f_k = mat_fk,
    mat_f = mat_f,
    f_tot = f_tot,
    diag_sqrt = diag_sqrt
  )
}


.mcf_x_values_a <- function(info, mat, eigen) {
  w_fn <- function(info, mat, eigen) {
    w <- array(99, dim = c(info$n_opts, info$components))
    for (i in seq_len(info$components)) {
      evec <- eigen$eigenvec[, i]
      w[, i] <- (mat$f_tot / as.vector(t(evec) %*% evec))^.5 * evec
    }
    w
  }
  w <- w_fn(info, mat, eigen)
  x <- mat$diag_sqrt %*% w
  list(
    x = x,
    x_adj = x %*% diag(eigen$singuval[seq_len(info$components)])
  )
}

.mcf_corr_a <- function(info, mat, x_values) {
  corr_1 <- array(99, dim = c(info$nrow, info$ncol, info$solutions))
  corr_2 <- array(99, dim = c(info$nrow, info$ncol + 1, info$solutions))
  corr_3 <- array(99, dim = c(info$nrow + 1, info$ncol + 1, info$solutions))
  corr_mat <- array(99, dim = c(info$ncol + 1, info$ncol + 1, info$solutions))
  corr_1k <- corr_1
  corr_2k <- corr_2
  corr_3k <- corr_3
  corr_matk <- corr_mat
  for (k in seq_len(info$solutions)) {
    for (i in seq_len(info$nrow)) {
      p <- mat$mat_f[i, ] * x_values$x_adj[, k]
      p <- p[p != 0]
      corr_1[i, , k] <- t(p)
      p <- mat$mat_f_k[i, ] * x_values$x_adj[, k]
      p <- p[p != 0]
      corr_1k[i, , k] <- t(p)
    }
    corr_2[, , k] <- cbind(corr_1[, , k], rowSums(corr_1[, , k]))
    corr_3[, , k] <- rbind(corr_2[, , k], colSums(corr_2[, , k]))
    corr_mat[, , k] <- stats::cor(corr_2[, , k])
    corr_2k[, , k] <- cbind(corr_1k[, , k], rowSums(corr_1k[, , k]))
    corr_3k[, , k] <- rbind(corr_2k[, , k], colSums(corr_2k[, , k]))
    corr_matk[, , k] <- stats::cor(corr_2k[, , k])
  }
  list(
    corr_m = corr_mat,
    corr_mk = corr_matk,
    corr_3 = corr_3
  )
}

.mcf_tbl_a <- function(info, corr, eigen) {
  tbl_e1 <- array(99, dim = c(info$solutions, info$ncol + 1))
  for (i in seq_len(info$solutions)) {
    tbl_e1[i, ] <- (corr$corr_mk[, info$ncol + 1, i])^2
  }
  tbl_e1[, info$ncol + 1] <- rowMeans(tbl_e1[, seq_len(info$ncol)])
  tbl_e3 <- array(99, dim = c(info$solutions + 1, info$ncol + 1))
  tbl_e3[, 1] <- 0:info$solutions
  for (i in seq_len(info$solutions)) {
    tbl_e3[i + 1, ] <- (corr$corr_mk[, info$ncol + 1, i])^2
  }
  tbl_e3[, info$ncol + 1] <- rowMeans(tbl_e3[, seq_len(info$ncol)])
  eig <- array(99, info$solutions)
  for (i in seq_len(info$solutions)) {
    eig[i] <- (sum(tbl_e3[i + 1, seq_len(info$ncol)]) - 1) / (info$ncol - 1)
  }
  tbl_e3 <- tbl_e3[2:(info$solutions + 1), 1:(info$ncol + 1)]
  colnames(tbl_e3) <- c(paste("Item", seq_len(info$ncol)), "Average")
  tbl_e2 <- array(99, dim = c(info$ncol + 1, 3, info$solutions))
  for (i in seq_len(info$solutions)) {
    tbl_e2[, 1, i] <- colSums(
      corr$corr_3[seq_len(info$nrow), , i]^2,
    ) / eigen$eigenval[i]
    tbl_e2[, 2, i] <- tbl_e1[i, 1:(info$ncol + 1)]
    tbl_e2[, 3, i] <- tbl_e2[, 2, i]^.5
  }
  list(
    tbl_e3 = tbl_e3,
    tbl_e2 = tbl_e2,
    adj_eigenval = eig
  )
}

.mcf_item_stat_a <- function(info, tbl) {
  item_stats <- array(99, dim = c(info$ncol, 4, info$solutions))
  for (k in seq_len(info$solutions)) {
    item_stats[, , k] <- as.matrix(data.frame(
      Item = seq_len(info$ncol),
      SSj = tbl$tbl_e2[seq_len(info$ncol), 1, k],
      R2 = tbl$tbl_e2[seq_len(info$ncol), 2, k],
      R = tbl$tbl_e2[seq_len(info$ncol), 3, k]
    ))
  }
  item_stats
}

.mcf_space <- function(info, mat, x_values) {
  corr_1c <- array(99, dim = c(info$nrow, info$ncol, info$components))
  corr_2c <- array(99, dim = c(info$nrow, info$ncol + 1, info$components))
  corr_mat <- array(99, dim = c(info$ncol + 1, info$ncol + 1, info$components))
  tbl_e3 <- array(99, dim = c(info$components + 1, info$ncol + 1))
  tbl_e3[, 1] <- 0:info$components
  for (k in seq_len(info$components)) {
    for (i in seq_len(info$nrow)) {
      p <- mat$mat_f_k[i, ] * x_values$x_adj[, k]
      corr_1c[i, , k] <- t(p[p != 0])
    }
    corr_2c[, , k] <- cbind(corr_1c[, , k], rowSums(corr_1c[, , k]))
    corr_mat[, , k] <- stats::cor(corr_2c[, , k])
    tbl_e3[k + 1, ] <- (corr_mat[, info$ncol + 1, k])^2
  }
  tbl_e3[, info$ncol + 1] <- rowMeans(tbl_e3[, seq_len(info$ncol)])
  tbl_e3 <- as.data.frame(
    round(tbl_e3[2:(info$components + 1), seq_len(info$ncol + 1)], 3)
  )
  colnames(tbl_e3) <- c(paste0("q.", seq_len(info$ncol)), "Avge")
  list(
    tbl_e3 = tbl_e3
  )
}


.mcf_proj_score <- function(info, mat, eigen, x_values, tbl) {
  to <- cumsum(info$max_col_val)
  from <- to + 1
  from <- c(1, from[seq_len(info$ncol - 1)])
  f_n <- as.matrix(mat$mat_f[, -(from[info$crit]:to[info$crit])])
  x_n <- as.matrix(x_values$x[-(from[info$crit]:to[info$crit]), ])
  row_diag <- diag(info$sub_sum)
  inv_row_diag <- diag(1 / info$sub_sum)
  singuval <- sqrt(tbl$adj_eigenval[seq_len(info$components)])
  y <- inv_row_diag %*% f_n %*% x_n %*% diag(1 / singuval)
  y_c <- inv_row_diag %*% f_n %*% x_n %*% diag(
    1 / eigen$singuval[seq_len(info$components)]
  )
  constant <- diag(info$solutions)
  for (k in seq_len(info$solutions)) {
    constant[k, k] <- sqrt(
      diag(
        info$total_sum * matrixcalc::matrix.inverse(
          t(y[, k] %*% row_diag %*% y[, k])
        )
      )
    )
  }
  y <- y[, seq_len(info$solutions)] %*% constant
  list(
    norm_sub_a = y,
    proj_sub_a = y %*% diag(singuval[seq_len(info$solutions)]),
    proj_opt_a = x_values$x %*% diag(singuval[seq_len(info$components)]),
    norm_sub_c = y_c,
    proj_opt_c = x_values$x %*% diag(eigen$singuval[seq_len(info$components)]),
    proj_sub_c = y_c %*% diag(eigen$singuval[seq_len(info$components)])
  )
}

.mcf_match_pred <- function(info, x_values, proj_score) {
  match <- array(99, dim = c(
    info$max_col_val[info$crit],
    info$max_col_val[info$crit],
    info$solutions
  ))
  pred <- array(99, info$solutions)
  limits <- cumsum(info$max_col_val)
  from <- ifelse(info$crit == 1, 1, limits[info$crit - 1] + 1)
  to <- limits[info$crit]
  for (k in seq_len(info$solutions)) {
    tbl <- cbind(proj_score$norm_sub_a[, k], info$orig_data[, info$crit])
    tbl_sort <- tbl[order(tbl[, 1]), ]
    opt <- factor(tbl_sort[, 2])
    lbl <- paste0("w", seq_len(info$n_opts))
    nl <- tabulate(tbl_sort[, 2])
    idx <- x_values$x[from:to, k]
    nl <- nl[order(-rank(idx))]
    w_fn <- function(i) {
      array(lbl[i], nl[i])
    }
    w <- factor(do.call(c, lapply(info$max_col_val[info$crit]:1, w_fn)))
    match[, , k] <- rbind(
      table(opt, w), idx
    )[, order(-idx)][seq_len(info$max_col_val[info$crit]), ]
    pred[k] <- sum(diag(match[, , k])) / info$nrow
  }
  list(
    match = match,
    pred = pred
  )
}

.mcf_contamination <- function(info_mc, info, space) {
  avg_mc <- info_mc$info[, info$ncol]
  from <- info$max_col_val[info$crit]
  avf_mcf <- space$tbl_e3[
    from:(info$solutions), info$ncol + 1
  ] * info$ncol / (info$ncol - 1)
  possible <- (info$n_opts - info$ncol) - info$solutions
  if (possible == info_mc$solutions) {
    comp_cont <- 100 * ((avg_mc - avf_mcf) / avf_mcf)
    tot_cont <- 100 * (sum(avg_mc) - sum(avf_mcf)) / sum(avf_mcf)
  }
  list(
    comp_cont = comp_cont,
    tot_cont = tot_cont
  )
}
