% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chartsTables.R
\name{relSSBseries}
\alias{relSSBseries}
\title{Relative change in biomass and relative catch as time series with uncertainty}
\usage{
relSSBseries(
  wd,
  fileName,
  facetName,
  newLabel = NULL,
  chooseArea = 0,
  percentile = c(0.025, 0.975),
  percentileColor = "#0096d6",
  percentileAlpha = 0.5,
  lineColor = "black",
  doHist = FALSE,
  dpi = 300,
  imageName = "Relative_timeSeries",
  scales = "fixed",
  outputDir = NULL
)
}
\arguments{
\item{wd}{A working directly where the output of runProjection is saved}

\item{fileName}{List. List of file names in the working directory}

\item{facetName}{List. Plot uses ggplot2 facets. File names can be assigned to facets, thus producing separate plots for a given facet, such a low M scenario vs. a high M scenario.}

\item{newLabel}{List. Replacement label for each file.}

\item{chooseArea}{The area to summarized in the plot. Value of 0 sums quantities across all areas}

\item{percentile}{Vector of length two indicating centered percent of observations to represent uncertainty in time series plots. For example, 95\% centered observations should be entered as c(0.025, 0.975)}

\item{percentileColor}{Color for the centered distribution of outcomes.}

\item{percentileAlpha}{Transparency param (between 0 and 1) for the percentile color}

\item{lineColor}{Color of the median trend line.}

\item{doHist}{Should the historical time period be included in the plot?}

\item{dpi}{Resolution in dots per inch of the resulting saved chart.}

\item{imageName}{Character. A name for the resulting plot(s)}

\item{scales}{From ggplot2::facet_wrap Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?}

\item{outputDir}{Directory where the output plot is saved. If NULL, it is set to wd.}
}
\description{
Time series of changes in spawning stock biomass and catch weight (two separate plots produced) relative to the year prior to projection time period. Saves a chart to the wd. Examples can be found in data-raw/DAR_projection_example.R
}
