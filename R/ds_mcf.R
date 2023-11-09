#' Forced multiple choice data analysis
#'
#' @param input A data set with valid data
#' @param solutions Optional argument. A number of intended solutions
#' @param crit Used to determine a criterion item for forced
#' classification analysis
#' @param mode Correction mode to incorrect data.
#'
#' @details There are three types of outputs: Forced classification of the
#' criterion item (type A); dual scaling of non-criterion items by ignoring
#' the criterion item (type B); dual scaling of non-criterion items after
#' eliminating the influence of the criterion item (type C).  These three
#' types correspond to, respectively, dual scaling of data projected onto the
#' subspace of the criterion item, dual scaling of non-criterion items, and
#' dual scaling of data in the complementary space of the criterion item.
#'
#' @seealso [dualScale::ds_mc_check()]
#'
#' @return
#' \item{call}{Call with all of the specified arguments are specified by
#' their full names}
#' \item{orig_data}{Initial data}
#' \item{crit_item}{The criterion item for forced classification}
#' \item{item_op_lbl}{Item options labels}
#' \item{sub_lbl}{Subjects options labels}
#' \item{solutions_mcf}{Maximum possible solutions for forced multiple choice}
#' \item{solutions_mc}{Maximum possible solutions for multiple choice}
#' \item{info_\emph{x}}{Distribution of component information according to
#' output}
#' \item{out_\emph{x}}{Results obtained according to output}
#' \item{item_stat_\emph{x}}{Item statistics according to output (Not type C)}
#' \item{rij_\emph{x}}{Inter item correlation according to output (Not type C)}
#' \item{proj_opt_\emph{x}}{Projected option weights according to output}
#' \item{proj_sub_\emph{x}}{Projected subject scores according to output}
#' \item{norm_opt_\emph{x}}{Normed option weights according to output}
#' \item{norm_sub_\emph{x}}{Normed subject scores according to output}
#' \item{match_missmatch}{Match-mismatch tables}
#' \item{predict}{Percentage of correct classification}
#' \item{comp_cont}{Component contamination}
#' \item{tot_cont}{Total contamination}
#'
#' @export
#'
#' @examples
#' ds_mcf(singaporean, crit = 1)
ds_mcf <- function(input, crit, solutions = NULL, mode = c("rad", "act")) {
  op <- match.arg(mode)
  if (ds_mcf_input(input, solutions, op, crit)) {
    check <- ds_mc_check(input, op)
    input <- check$t_data
  }
  inf_mc <- ds_mc(input[, -crit], solutions)
  inf_mcf <- ds_mcf_calc(input, solutions, crit, inf_mc)
  structure(
    list(
      call = match.call(),
      orig_data = input,
      crit_item = crit,
      item_op_lbl = inf_mcf$item_op_lbl,
      sub_lbl = inf_mcf$sub_name,
      solutions_mcf = inf_mcf$solutions,
      solutions_mc = inf_mc$solutions,
      info_a = inf_mcf$info_a,
      out_a = inf_mcf$out_a,
      item_stat_a = inf_mcf$item_stat_a,
      rij_a = inf_mcf$rij_a,
      proj_opt_a = inf_mcf$proj_opt_a,
      proj_sub_a = inf_mcf$proj_sub_a,
      norm_opt_a = inf_mcf$norm_opt_a,
      norm_sub_a = inf_mcf$norm_sub_a,
      info_b = inf_mc$inf,
      out_b = inf_mc$out,
      item_stat_b = inf_mc$item_stat,
      rij_b = inf_mc$rij[
        seq_len(inf_mcf$ncol - 1),
        seq_len(inf_mcf$ncol - 1),
      ],
      proj_opt_b = inf_mc$proj_opt,
      proj_sub_b = inf_mc$proj_sub,
      norm_opt_b = inf_mc$norm_opt,
      norm_sub_b = inf_mc$norm_sub,
      info_c = inf_mcf$info_a,
      out_c = inf_mcf$out_c,
      proj_opt_c = inf_mcf$proj_opt_c,
      proj_sub_c = inf_mcf$proj_sub_c,
      norm_opt_c = inf_mcf$norm_opt_a,
      norm_sub_c = inf_mcf$norm_sub_c,
      match_missmatch = inf_mcf$match,
      predict = inf_mcf$pred,
      comp_cont = inf_mcf$comp_cont,
      tot_cont = inf_mcf$tot_cont
    ),
    class = c("ds_mcf", "dualScale")
  )
}
