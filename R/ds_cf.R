#' Contingency and frequency data analysis
#'
#' @param input A data set with valid data
#' @param solutions Optional arguments. A number of intended solutions
#'
#' @return
#' \item{call}{Call with all of the specified arguments are specified by
#' their full names}
#' \item{orig_data}{Initial data}
#' \item{item_op_lbl}{Item options labels}
#' \item{sub_lbl}{Subjects options labels}
#' \item{tot_row}{Sum of subject values}
#' \item{tot_row}{Sum of item values}
#' \item{solutions}{Maximum possible solutions}
#' \item{out}{Results obtained}
#' \item{norm_opt}{Normed option weights}
#' \item{proj_opt}{Projected option weights}
#' \item{norm_sub}{Normed subject scores}
#' \item{proj_sub}{Projected subject scores}
#' \item{appro0}{Order 0 approximation for initial data}
#' \item{approx}{Order `k` approximation for each solution}
#' \item{residual0}{Residual matrix for initial data}
#' \item{residual}{Residual matrix `k` for each solution}
#'
#' @export
#'
#' @examples
#' ds_cf(curricula)
#' ds_cf(preferences)
ds_cf <- function(input, solutions = NULL) {
  ds_cf_check(input, solutions)
  calc <- ds_cf_calc(input, solutions)
  structure(
    list(
      call = match.call(),
      orig_data = input,
      item_op_lbl = calc$col_name,
      sub_lbl = calc$row_name,
      tot_row = calc$total_sub,
      tot_col = calc$total_item,
      solutions = calc$solutions,
      out = calc$out,
      norm_opt = calc$norm_opt,
      proj_opt = calc$proj_opt,
      norm_sub = calc$norm_sub,
      proj_sub = calc$proj_sub,
      appro0 = calc$appro0,
      approx = calc$approx,
      residual0 = calc$residual0,
      residual = calc$residual
    ),
    class = c("ds_cf", "dualScale")
  )
}
