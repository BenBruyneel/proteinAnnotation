% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{theme_minimal_adapted}
\alias{theme_minimal_adapted}
\title{theme_minimal_adapted}
\usage{
theme_minimal_adapted(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  axis_line_size = base_line_size/2,
  xAxis = TRUE,
  yAxis = TRUE,
  showLegend = TRUE,
  legend.position = "bottom",
  gridLines = TRUE,
  gridLinesX = TRUE,
  gridLinesY = TRUE,
  titleSize = NA
)
}
\arguments{
\item{base_size}{size of the lettering  of axis, title etc}

\item{base_family}{letter type of axis, title etc}

\item{base_line_size}{width of gridlines, set  to 0 for none}

\item{axis_line_size}{width of axis lines, set to 0 for none}

\item{xAxis}{if TRUE then display xAxis title}

\item{yAxis}{if TRUE then display yAxis title}

\item{showLegend}{if TRUE then show legend}

\item{legend.position}{defines where to place the legend}

\item{gridLines}{if TRUE then display gridlines}

\item{gridLinesX}{if TRUE then display gridlines 'along' the x-axis}

\item{gridLinesY}{if TRUE then display gridlines 'along' the y-axis}

\item{titleSize}{if NA, use default title size, else use titleSize value

To be used as ggplot-object + theme_minimal_adapted()}
}
\value{
theme definition
}
\description{
to be able to use the theme theme_minimal with some adjustments
}
\note{
also part of the personal package 'BBPersonalR'
}
