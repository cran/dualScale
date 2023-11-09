ds_plot <- function(x, dim1, dim2, type) {
  info <- .plot_info(x, dim1, dim2, type)
  print(glue::glue(
    "

    Call: {capture.output(x$call)}

    Type of Graph: {info$title} of dimensions {dim1} {dim2}

    Cumulative Delta: {info$delta}%

    "
  ))
  plot <- .general_plot(info)
  switch(type,
    "Asy1" = .plot_asy1(info, plot),
    "Asy2" = .plot_asy2(info, plot),
    "Sub" = .plot_sub(info, plot),
    "Ite" = .plot_ite(info, plot)
  )
}

.plot_info <- function(x, dim1, dim2, type) {
  if (class(x)[1] == "ds_mcf") {
    adj_op <- x$proj_opt_a
    adj_su <- x$proj_sub_a
    std_op <- x$norm_opt_a
    std_su <- x$norm_sub_a
    delta1 <- round(x$out_a[dim1, 4], 4)
    delta2 <- round(x$out_a[dim2, 4], 4)
  } else {
    adj_op <- x$proj_opt
    adj_su <- x$proj_sub
    std_op <- x$norm_opt
    std_su <- x$norm_sub
    delta1 <- round(x$out[dim1, 4], 4)
    delta2 <- round(x$out[dim2, 4], 4)
  }
  switch(type,
    "Asy1" = {
      df <- .asy(adj_op, std_su, dim1, dim2)
      colour <- .colour_asy(x)
      title <- "Asymmetric Plot I"
      subtitle <- "proj.op weights and norm.su scores"
      label <- c(x$item_op_lbl, x$sub_lbl)
    },
    "Asy2" = {
      df <- .asy(std_op, adj_su, dim1, dim2)
      colour <- .colour_asy(x)
      title <- "Asymmetric Plot II"
      subtitle <- "norm.op weights and proj.su scores"
      label <- c(x$item_op_lbl, x$sub_lbl)
    },
    "Sub" = {
      df <- .ite_sub(adj_su, dim1, dim2)
      colour <- .colour_sub(x)
      title <- "Only Subjects"
      subtitle <- "Projected Scores"
      label <- x$sub_lbl
    },
    "Ite" = {
      df <- .ite_sub(adj_op, dim1, dim2)
      colour <- .colour_ite(x)
      title <- "Only Items Options"
      subtitle <- "Projected Weights"
      label <- x$item_op_lbl
    }
  )
  max_val <- max(abs(df))
  list(
    xlab = glue::glue(
      "Component {dim1} (delta: {format(delta1, nsmall = 4)}%)"
    ),
    ylab = glue::glue(
      "Component {dim2} (delta: {format(delta2, nsmall = 4)}%)"
    ),
    delta = format(delta1 + delta2, nsmall = 4),
    adj_op = data.frame(dim1 = adj_op[, dim1], dim2 = adj_op[, dim2]),
    adj_su = data.frame(dim1 = adj_su[, dim1], dim2 = adj_su[, dim2]),
    std_op = data.frame(dim1 = std_op[, dim1], dim2 = std_op[, dim2]),
    std_su = data.frame(dim1 = std_su[, dim1], dim2 = std_su[, dim2]),
    df = df,
    limits = c(ceiling(max_val * 2) / 2 * -1, ceiling(max_val * 2) / 2),
    colour = colour,
    title = title,
    subtitle = subtitle,
    label = label
  )
}

.general_plot <- function(info) {
  ggplot2::ggplot(NULL) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0), colour = "grey") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "grey") +
    ggplot2::scale_shape_identity() +
    ggplot2::labs(
      x = info$xlab, y = info$ylab, title = info$title, subtitle = info$subtitle
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(info$limits), max(info$limits)),
      limits = info$limits, expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(min(info$limits), max(info$limits)),
      limits = info$limits,
      expand = c(0, 0)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45),
      axis.text.y = ggplot2::element_text(angle = 45), # , vjust = 0.5, hjust = 1
      legend.position = "none"
    ) +
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(
        x = info$df[, 1],
        y = info$df[, 2],
        colour = info$colour,
        label = info$label
      ),
      seed = 123,
      arrow = grid::arrow(length(grid::unit(0.03, "npc"))),
      box.padding = 0.1,
      max.overlaps = Inf
    )
}

.plot_asy1 <- function(info, plot) {
  plot +
    ggplot2::geom_point(
      ggplot2::aes(x = info$adj_op$dim1, y = info$adj_op$dim2, shape = 16)
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = info$std_su$dim1, y = info$std_su$dim2, shape = 17)
    )
}

.plot_asy2 <- function(info, plot) {
  plot +
    ggplot2::geom_point(
      ggplot2::aes(x = info$adj_su$dim1, y = info$adj_su$dim2, shape = 17)
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = info$std_op$dim1, y = info$std_op$dim2, shape = 16)
    )
}

.plot_sub <- function(info, plot) {
  plot +
    ggplot2::geom_point(
      ggplot2::aes(x = info$adj_su$dim1, y = info$adj_su$dim2, shape = 16)
    )
}

.plot_ite <- function(info, plot) {
  plot +
    ggplot2::geom_point(
      ggplot2::aes(x = info$adj_op$dim1, y = info$adj_op$dim2, shape = 16)
    )
}

.asy <- function(opt1, opt2, dim1, dim2) {
  data.frame(
    df1 = c(opt1[, dim1], opt2[, dim1]),
    df2 = c(opt1[, dim2], opt2[, dim2])
  )
}

.ite_sub <- function(opt, dim1, dim2) {
  data.frame(
    df1 = opt[, dim1],
    df2 = opt[, dim2]
  )
}

.colour_asy <- function(x) {
  qn <- unlist(strsplit(x$item_op_lbl, split = "\\."))
  qn <- qn[qn != "q"]
  qn <- as.integer(substr(qn, start = 1, stop = 1))
  tot <- length(qn) + length(x$sub_lbl)
  n <- c(qn, length(x$sub_lbl))
  color <- n
  if (tot >= 3) {
    n <- c(qn, rep(max(qn) + 1, length(x$sub_lbl)))
    color <- RColorBrewer::brewer.pal(length(unique(n)), "Set3")
  }
  factor(n, labels = color)
}

.colour_sub <- function(x) {
  n <- length(unique(x$sub_lbl))
  factor(n, labels = n)
}

.colour_ite <- function(x) {
  qn <- unlist(strsplit(x$item_op_lbl, split = "\\."))
  qn <- qn[qn != "q"]
  tot <- length(qn) + length(x$sub_lbl)
  n <- as.integer(substr(qn, start = 1, stop = 1))
  color <- n
  if (tot >= 3) {
    color <- RColorBrewer::brewer.pal(length(unique(n)), "Set3")
  }
  factor(n, labels = color)
}

ds_plot_data <- function(x, dim1, dim2, type) {
  info <- .plot_info(x, dim1, dim2, type)
  switch(type,
    "Asy1" = list(
      items = cbind("q_x" = info$adj_op$dim1, "q_y" = info$adj_op$dim2),
      subjects = cbind("s_x" = info$std_su$dim1, "s_y" = info$std_su$dim2)
    ),
    "Asy2" = list(
      items = cbind("q_x" = info$adj_su$dim1, "q_y" = info$adj_su$dim2),
      subjects = cbind("s_x" = info$std_op$dim1, "s_y" = info$std_op$dim2)
    ),
    "Sub" = list(subjects = cbind(info$adj_su$dim1, info$adj_su$dim2)),
    "Ite" = list(subjects = cbind(info$adj_op$dim1, info$adj_op$dim2))
  )
}
