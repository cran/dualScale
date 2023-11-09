#' Multiple choice data analysis
#'
#' @param input A data set with valid data
#' @param solutions Optional argument. A number of intended solutions
#' @param mode Optional argument. In case of NA values, the action to be taken.
#' See help("ds_mc_check") for more information. Radical action by default.
#'
#' @return
#' \item{call}{Call with all of the specified arguments are specified by
#' their full names}
#' \item{orig_data}{Initial data}
#' \item{item_op_lbl}{Item options labels}
#' \item{sub_lbl}{Subjects options labels}
#' \item{solutions}{Maximum possible solutions}
#' \item{out}{Results obtained}
#' \item{item_stat}{Item statistics}
#' \item{info}{Distribution of component}
#' \item{rij}{Inter item correlation}
#' \item{proj_opt}{Projected option weights}
#' \item{proj_sub}{Projected subject scores}
#' \item{norm_opt}{Normed option weights}
#' \item{norm_sub}{Normed subject scores}
#'
#' @export
#'
#' @seealso [dualScale::ds_mc_check()]
#'
#' @examples
#' ds_mc(singaporean)
#' ds_mc(singaporean, solutions = 2)
ds_mc <- function(input, solutions = NULL, mode = c("rad", "act")) {
  op <- match.arg(mode)
  if (ds_mc_input(input, solutions, op)) {
    check <- ds_mc_check(input, op)
    input <- check$t_data
  }
  calc <- ds_mc_calc(input, solutions)
  structure(
    list(
      call = match.call(),
      orig_data = input,
      item_op_lbl = calc$item_op_lbl,
      sub_lbl = calc$sub_name,
      solutions = calc$solutions,
      out = calc$out,
      item_stat = calc$item_stat,
      info = calc$info,
      rij = calc$rij,
      norm_opt = calc$norm_opt,
      proj_opt = calc$proj_opt,
      norm_sub = calc$norm_sub,
      proj_sub = calc$proj_sub
    ),
    class = c("ds_mc", "dualScale")
  )
}
