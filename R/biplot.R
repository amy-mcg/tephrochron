#' Element biplots
#'
#' Create a simple biplot
#' @param data A data frame containing element geochemistries
#' @param x The variable to be plotted on the x axis
#' @param y The variable to be plotted on the y axis
#' @param colour The variable which sets the plotting colour (e.g. sample id)
#' @param symbol The variable which sets the plotting symbol (e.g. site, volcano)
#' @return A simple biplot
#' @examples
#' plot <- biplot(data, SiO2, FeOt, Sample_ID, Site);
#' @export
biplot <- function(data, colx, coly, colour, symbol){
  biplot <- ggplot2::ggplot(data, ggplot2::aes(x= {{colx}}, y= {{coly}})) +
    ggplot2::geom_point(ggplot2::aes(x= {{colx}}, y= {{coly}}, shape={{symbol}}, colour={{colour}}));
  return(biplot);
}
