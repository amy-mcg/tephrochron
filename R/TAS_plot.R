#' TAS plot
#'
#' Create a TAS plot, using fields of different magma composition after Le Maitre (2002). Based on code by John Stevenson: https://all-geo.org/volcan01010/2015/01/easily-plot-magma-compositions-tas-diagrams-in-python/
#'
#' Le Maitre RW (2002) Igneous rocks: IUGS classification and glossary of terms: recommendations of the International Union of Geological Sciences Subcommission on the Systematics of igneous rocks, 2nd ed. Cambridge University Press, Cambridge
#'
#' @param data A data frame containing the columns SiO2, K2O, Na2O, Total
#' @param ymin The lower depth of the sample
#' @param ymax The upper depth of the sample
#' @param concs Glass shard concentrations
#' @param colour The variable which sets the plotting colour (e.g. ash fall)
#' @return A plot of glass shard concentration against depth
#' @examples
#' plot <- strat_plot(data, Lower_m, Upper_m, Conc_sg, Fall);
#' @export
#'

TAS <- function(){
  TAS <- ggplot2::ggplot()+
    ggplot2::geom_segment(ggplot2::aes(x =41, y =0, xend = 41, yend = 7))  +
    ggplot2::geom_segment(ggplot2::aes(x =41, y =7, xend = 52.5, yend = 14))  +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =0, xend = 45, yend = 5))  +
    ggplot2::geom_segment(ggplot2::aes(x =41, y =3, xend = 45, yend = 3)) +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =5, xend = 61, yend = 13.5)) +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =5, xend = 52, yend = 5)) +
    ggplot2::geom_segment(ggplot2::aes(x =52, y =5, xend = 69, yend = 8)) +
    ggplot2::geom_segment(ggplot2::aes(x =49.4, y =7.3, xend = 52, yend = 5)) +
    ggplot2::geom_segment(ggplot2::aes(x =52, y =5, xend = 52, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(x =48.4, y =11.5, xend = 53, yend = 9.3)) +
    ggplot2::geom_segment(ggplot2::aes(x =53, y =9.3, xend = 57, yend = 5.9)) +
    ggplot2::geom_segment(ggplot2::aes(x =57, y =5.9, xend = 57, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(x =52.5, y =14, xend = 57.6, yend = 11.7)) +
    ggplot2::geom_segment(ggplot2::aes(x =57.6, y =11.7, xend = 63, yend = 7)) +
    ggplot2::geom_segment(ggplot2::aes(x =63, y =7, xend = 63, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(x =69, y =12, xend = 69, yend = 8)) +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =9.4, xend = 49.4, yend = 7.3)) +
    ggplot2::geom_segment(ggplot2::aes(x =69, y =8, xend = 77, yend = 0)) +
    ggplot2::annotate("text",x = 43, y = 2, label = 'Picro\nbasalt') +
    ggplot2::annotate("text", x = 48.5, y = 2, label = 'Basalt') +
    ggplot2::annotate("text", x = 54.5, y = 2, label = 'Basaltic\nandesite') +
    ggplot2::annotate("text", x = 60, y = 2, label = 'Andesite') +
    ggplot2::annotate("text", x = 68.5, y = 2, label = 'Dacite') +
    ggplot2::annotate("text", x = 76, y = 9, label = 'Rhyolite') +
    ggplot2::annotate("text", x = 64.5, y = 11.5, label = 'Trachyte\n(Q < 20%)\n\nTrachydacite\n(Q > 20%)') +
    ggplot2::annotate("text", x = 53, y = 8, label = 'Basaltic\ntrachyandesite') +
    ggplot2::annotate("text", x = 49, y = 6.2, label = 'Trachy-\nbasalt') +
    ggplot2::annotate("text", x = 57.2, y = 9, label = 'Trachyandesite') +
    ggplot2::annotate("text", x = 49, y = 9.6, label = 'Phonotephrite') +
    ggplot2::annotate("text", x = 53, y = 11.8, label = 'Tephriphonolite') +
    ggplot2::annotate("text", x = 57.5, y = 13.5, label = 'Phonolite') +
    ggplot2::annotate("text", x = 45, y = 8, label = 'Tephrite\n(Ol < 10%)') +
    ggplot2::annotate("text", x = 44, y = 11.5, label = 'Foidite') +
    ggplot2::annotate("text", x = 43.5, y = 6.5, label = 'Basanite\n(Ol > 10%)')
  ;
  return(TAS);
}

TAS_plot <- function(data, colour, symbol){
  #####NORMALISE DATA
  SiO2_n <- (data$SiO2 / data$Total) * 100
  Alkalis_n <- ((data$Na2O + data$K2O) / data$Total) * 100
  #####PLOT
  TAS_plot <- ggplot2::ggplot()+
    ggplot2::geom_segment(ggplot2::aes(x =41, y =0, xend = 41, yend = 7))  +
    ggplot2::geom_segment(ggplot2::aes(x =41, y =7, xend = 52.5, yend = 14))  +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =0, xend = 45, yend = 5))  +
    ggplot2::geom_segment(ggplot2::aes(x =41, y =3, xend = 45, yend = 3)) +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =5, xend = 61, yend = 13.5)) +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =5, xend = 52, yend = 5)) +
    ggplot2::geom_segment(ggplot2::aes(x =52, y =5, xend = 69, yend = 8)) +
    ggplot2::geom_segment(ggplot2::aes(x =49.4, y =7.3, xend = 52, yend = 5)) +
    ggplot2::geom_segment(ggplot2::aes(x =52, y =5, xend = 52, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(x =48.4, y =11.5, xend = 53, yend = 9.3)) +
    ggplot2::geom_segment(ggplot2::aes(x =53, y =9.3, xend = 57, yend = 5.9)) +
    ggplot2::geom_segment(ggplot2::aes(x =57, y =5.9, xend = 57, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(x =52.5, y =14, xend = 57.6, yend = 11.7)) +
    ggplot2::geom_segment(ggplot2::aes(x =57.6, y =11.7, xend = 63, yend = 7)) +
    ggplot2::geom_segment(ggplot2::aes(x =63, y =7, xend = 63, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(x =69, y =12, xend = 69, yend = 8)) +
    ggplot2::geom_segment(ggplot2::aes(x =45, y =9.4, xend = 49.4, yend = 7.3)) +
    ggplot2::geom_segment(ggplot2::aes(x =69, y =8, xend = 77, yend = 0)) +
    ggplot2::annotate("text",x = 43, y = 2, label = 'Picro\nbasalt') +
    ggplot2::annotate("text", x = 48.5, y = 2, label = 'Basalt') +
  ggplot2::annotate("text", x = 54.5, y = 2, label = 'Basaltic\nandesite') +
  ggplot2::annotate("text", x = 60, y = 2, label = 'Andesite') +
  ggplot2::annotate("text", x = 68.5, y = 2, label = 'Dacite') +
  ggplot2::annotate("text", x = 76, y = 9, label = 'Rhyolite') +
  ggplot2::annotate("text", x = 64.5, y = 11.5, label = 'Trachyte\n(Q < 20%)\n\nTrachydacite\n(Q > 20%)') +
  ggplot2::annotate("text", x = 53, y = 8, label = 'Basaltic\ntrachyandesite') +
  ggplot2::annotate("text", x = 49, y = 6.2, label = 'Trachy-\nbasalt') +
  ggplot2::annotate("text", x = 57.2, y = 9, label = 'Trachyandesite') +
  ggplot2::annotate("text", x = 49, y = 9.6, label = 'Phonotephrite') +
  ggplot2::annotate("text", x = 53, y = 11.8, label = 'Tephriphonolite') +
  ggplot2::annotate("text", x = 57.5, y = 13.5, label = 'Phonolite') +
  ggplot2::annotate("text", x = 45, y = 8, label = 'Tephrite\n(Ol < 10%)') +
  ggplot2::annotate("text", x = 44, y = 11.5, label = 'Foidite') +
  ggplot2::annotate("text", x = 43.5, y = 6.5, label = 'Basanite\n(Ol > 10%)') +
    ggplot2::geom_point(data, mapping=ggplot2::aes(x=SiO2_n, y=Alkalis_n, shape={{symbol}}, colour={{colour}}))
  ;
  return(TAS_plot);
}

