#' Paired comparison data analysis
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
#'
#' @export
#'
#' @examples
#' ds_pc(christmas)
ds_pc <- function(input, solutions = NULL) {
  ds_input_check(input, solutions)
  calc <- ds_pc_calc(input, solutions)
  structure(
    list(
      call = match.call(),
      orig_data = input,
      item_op_lbl = calc$item_op_lbl,
      sub_lbl = calc$sub_lbl,
      solutions = calc$solutions,
      out = calc$out,
      mat_e = calc$mat_e,
      norm_opt = calc$norm_opt,
      proj_opt = calc$proj_opt,
      norm_sub = calc$norm_sub,
      proj_sub = calc$proj_sub
    ),
    class = c("ds_pc", "dualScale")
  )
}
