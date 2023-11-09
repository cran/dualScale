#' Function to identify incorrect Multiple Choice input data
#'
#' @param input The input data to be checked
#' @param mode Do you want to use a radical ("rad") correction mode
#' or active ("act") allocations?
#'
#' @return A list with the original input and the converted input
#' @export
#'
#' @examples
#' ds_mc_check(singaporean)
#' ds_mc_check(bad_coded)
ds_mc_check <- function(input, mode = c("rad", "act")) {
  op <- match.arg(mode)
  nrow <- nrow(input)
  ncol <- ncol(input)
  t_input <- as.matrix(input)
  if (any(is.na(input))) {
    switch(op,
      "rad" = {
        glue::glue("Rows deleted: {nrow(input[rowSums(is.na(input)) > 0, ])}")
        t_input <- t_input[stats::complete.cases(t_input), ]
      },
      "act" = {
        for (j in seq_len(ncol)) {
          col_dat <- input[, j]
          if (any(is.na(col_dat))) {
            col_dat[is.na(col_dat)] <- max(col_dat, na.rm = TRUE) + 1
            t_input[, j] <- as.matrix(col_dat)
          }
        }
        t_input <- act_mode(t_input, nrow, ncol)
      }
    )
  }
  structure(
    list(
      initial_data = input,
      t_data = as.data.frame(t_input)
    ),
    class = "ds_mc_check"
  )
}

act_mode <- function(t_input, nrow, ncol) {
  if (!any(is.na(t_input))) {
    for (j in seq_len(ncol)) {
      input_tab <- tabulate(t_input[, j])
      if (sum(as.numeric(input_tab == 0)) > 0) {
        x_pos <- cbind(t_input, seq_len(nrow))
        x_ord <- x_pos[order(x_pos[, j]), ]
        tbl <- table(x_ord[, j], useNA = "ifany")
        len_tbl <- length(tbl)
        new_opt <- seq_len(len_tbl)
        new_code <- 99
        for (k in seq_len(len_tbl)) {
          new_code <- c(new_code, array(new_opt[k], tbl[k]))
        }
        new_code <- new_code[-1]
        x_ord[, j] <- new_code
        t_input <- x_ord[order(x_ord[, (ncol + 1)]), ]
        t_input <- t_input[, -(ncol + 1)]
      }
    }
  }
  return(t_input)
}
