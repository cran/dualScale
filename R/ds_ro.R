#' Rank order data analysis
#'
#' @param input A data set with valid data
#' @param solutions Optional argument. A number of intended solutions
#'
#' @return
#' \item{call}{Call with all of the specified arguments are specified by
#' their full names}
#' \item{orig_data}{Initial data}
#' \item{item_op_lbl}{Item options labels}
#' \item{sub_lbl}{Subjects options labels}
#' \item{solutions}{Maximum possible solutions}
#' \item{out}{Results obtained}
#' \item{mat_e}{Matrix E}
#' \item{norm_opt}{Normed option weights}
#' \item{proj_opt}{Projected option weights}
#' \item{norm_sub}{Normed subject scores}
#' \item{proj_sub}{Projected subject scores}
#' \item{out_rank}{Results obtained by rank analysis}
#' \item{norm_opt_rank}{Normed option weights by rank analysis}
#' \item{proj_opt_rank}{Projected option weights by rank analysis}
#' \item{norm_rank}{Normed rank scores}
#' \item{proj_rank}{Projected rank scores}
#'
#' @export
#'
#' @examples
#' ds_ro(goverment)
ds_ro <- function(input, solutions = NULL) {
  ds_input_check(input, solutions)
  calc <- ds_ro_calc(input, solutions)
  structure(
    list(
      call = match.call(),
      orig_data = calc$orig_data,
      item_op_lbl = calc$item_op_lbl,
      sub_lbl = calc$sub_name,
      solutions = calc$solutions,
      mat_e = calc$mat_e,
      out = calc$out,
      norm_opt = calc$norm_opt,
      proj_opt = calc$proj_opt,
      norm_sub = calc$norm_sub,
      proj_sub = calc$proj_sub,
      out_rank = calc$out_rank,
      norm_opt_rank = calc$norm_opt_rank,
      proj_opt_rank = calc$proj_opt_rank,
      norm_rank = calc$norm_rank,
      proj_rank = calc$proj_rank
    ),
    class = c("ds_ro", "dualScale")
  )
}
