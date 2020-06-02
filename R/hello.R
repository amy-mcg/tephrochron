# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

##########
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

##########
#' Scatterplot matrix
#'
#' Create a scatterplot matrix showing all element geochemistries
#' @param data A data frame containing element geochemistries
#' @param colx The first column containing geochemical data
#' @param coly The final column containing geochemical data
#' @param colour The variable which sets the plotting colour (e.g. sample id)
#' @param symbol The variable which sets the plotting symbol (e.g. site, volcano)
#' @return A scatterplot matrix showing selected element geochemistries
#' @examples
#' plot <- biplot(data, 4, 10, Sample_ID, Site);
#' @export
splom <- function(data, colx, coly, colour, symbol){
  splom <- GGally::ggpairs(data, columns = colx:coly, ggplot2::aes(color = {{colour}}, shape = {{symbol}}));
  return(splom);
}


##########
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
