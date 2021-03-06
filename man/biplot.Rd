% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biplot.R
\name{biplot}
\alias{biplot}
\title{Element biplots}
\usage{
biplot(data, colx, coly, colour, symbol)
}
\arguments{
\item{data}{A data frame containing element geochemistries}

\item{colour}{The variable which sets the plotting colour (e.g. sample id)}

\item{symbol}{The variable which sets the plotting symbol (e.g. site, volcano)}

\item{x}{The variable to be plotted on the x axis}

\item{y}{The variable to be plotted on the y axis}
}
\value{
A simple biplot
}
\description{
Create a simple biplot
}
\examples{
plot <- biplot(data, SiO2, FeOt, Sample_ID, Site);
}
