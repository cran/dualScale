#' Summary of Dual Scale analysis
#'
#' @description `summary` method for class "dualScale"
#'
#' @param object An dualScale object for which a summary is desired
#' @param ... Arguments to be passed to methods
#'
#' @return A summary of the available information from the object
#' @export
#'
#' @seealso [summary()]
#' @aliases summary.dualScale
#'
#' @examples
#' summary(ds_cf(curricula))
#' summary(ds_cf(preferences))
#' summary(ds_mc(singaporean))
#' summary(ds_mcf(singaporean, crit = 1))
#' summary(ds_pc(christmas))
#' summary(ds_ro(goverment))
summary.dualScale <- function(object, ...) {
  title <- switch(class(object)[1],
    "ds_cf" = "Dual Scaling - Contingency and frequency data analysis",
    "ds_mc" = "Dual Scaling - Multiple choice data analysis",
    "ds_mcf" = "Dual Scaling - Forced multiple choice data analysis",
    "ds_pc" = "Dual Scaling - Paired comparison data analysis",
    "ds_ro" = "Dual Scaling - Rank order data analysis"
  )
  print(glue::glue(
    "

    {title}

    Call: {capture.output(object$call)}

    Initial Data:

    "
  ))
  print(as.data.frame(object$orig_data))
  print(glue::glue(
    "

    Output Available on Demand:

    "
  ))
  summary(unclass(object))
}
