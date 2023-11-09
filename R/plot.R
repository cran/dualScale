#' Plot of Dual Scale analysis
#'
#' @param x A Dual Scale object
#' @param dim1 Component for the horizontal axis. Default dimension 1
#' @param dim2 Component for the vertical axis. Default dimension 2
#' @param type Graph type
#' \describe{
#' \item{Asy1}{Assymetric graph for projected options versus normed subjects
#' (default)}
#' \item{Asy2}{Assymetric graph for normed options versus projected subjects}
#' \item{Sub}{Only subjects graph}
#' \item{Ite}{Only items graph}
#' }
#' @param ... Arguments to be passed to methods
#'
#' @seealso [plot()],[ggplot2::ggplot2()]
#'
#' @return A plot of the available information from the object
#' @export
#'
#' @examples
#' plot(ds_cf(curricula))
#' plot(ds_mc(singaporean))
#' plot(ds_mcf(singaporean, crit = 1))
#' plot(ds_pc(christmas))
#' plot(ds_ro(goverment))
plot.dualScale <- function(x,
                           dim1 = 1,
                           dim2 = 2,
                           type = c("Asy1", "Asy2", "Sub", "Ite"),
                           ...) {
  plt <- match.arg(type)
  sols <- ifelse(class(x)[1] == "ds_mcf", x$solutions_mcf, x$solutions)
  if (sols < max(dim1, dim2)) {
    stop("It is impossible to plot dim ", max(dim1, dim2))
  }
  ds_plot(x, dim1, dim2, plt)
}



#' Obtain the data used in the graphs
#'
#' @inherit plot.dualScale params
#'
#' @return A dataframe with the data used
#' @export
#'
#' @examples
#' plot_data(ds_cf(curricula))
#' plot_data(ds_mc(singaporean))
#' plot_data(ds_mcf(singaporean, crit = 1))
#' plot_data(ds_pc(christmas))
#' plot_data(ds_ro(goverment))
plot_data <- function(x,
                      dim1 = 1,
                      dim2 = 2,
                      type = c("Asy1", "Asy2", "Sub", "Ite"),
                      ...) {
  plt <- match.arg(type)
  sols <- ifelse(class(x)[1] == "ds_mcf", x$solutions_mcf, x$solutions)
  if (sols < max(dim1, dim2)) {
    stop("It is impossible to plot dim ", max(dim1, dim2))
  }
  ds_plot_data(x, dim1, dim2, plt)
}
