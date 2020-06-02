#' TAS plot
#'
#' Create a plot showing tephra glass shard concentration against depth
#' @param data A data frame containing glass shard concentration data
#' @param ymin The lower depth of the sample
#' @param ymax The upper depth of the sample
#' @param concs Glass shard concentrations
#' @param colour The variable which sets the plotting colour (e.g. ash fall)
#' @return A plot of glass shard concentration against depth
#' @examples
#' plot <- strat_plot(data, Lower_m, Upper_m, Conc_sg, Fall);
#' @export
