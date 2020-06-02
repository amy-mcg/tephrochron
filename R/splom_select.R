#' Scatterplot matrix of selected elements
#'
#' Create a scatterplot matrix showing selected element geochemistries
#' @param data A data frame containing element geochemistries
#' @param cols The names of columns containing selected element geochemistries
#' @param colour The variable which sets the plotting colour (e.g. sample id)
#' @param symbol The variable which sets the plotting symbol (e.g. site, volcano)
#' @return A scatterplot matrix showing selected element geochemistries
#' @examples
#' plot <- splom_select(data, cols = c("SiO2","K2O","Na2O"), Sample_ID);
#'
#' data$K2O_Na2O <- data$K2O / data$Na2O
#' plot <- splom_select(data, cols = c("SiO2","K2O_Na2O"), Sample_ID);
#' @export
splom_select <- function(data, cols, colour, symbol){
  splom_select <- GGally::ggpairs(data, columns = {{cols}}, ggplot2::aes(color = {{colour}}, symbol = {{symbol}}));
  return(splom_select);
}
