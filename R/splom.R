#' Scatterplot matrix
#'
#' Create a scatterplot matrix showing full element geochemistry
#' @param data A data frame containing element geochemistries
#' @param colx The first column containing geochemical data
#' @param coly The final column containing geochemical data
#' @param colour The variable which sets the plotting colour (e.g. sample id)
#' @param symbol The variable which sets the plotting symbol (e.g. site, volcano)
#' @return A scatterplot matrix showing selected element geochemistries
#' @examples
#' plot <- biplot(data, 4, 10, Sample_ID, Site);
#' @export
#'

splom <- function(data, colx, coly, colour, symbol){
  splom <- GGally::ggpairs(data, columns = colx:coly, ggplot2::aes(color = {{colour}}, shape = {{symbol}}));
  return(splom);
}
