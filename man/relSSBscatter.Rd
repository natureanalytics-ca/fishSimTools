% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chartsTables.R
\name{relSSBscatter}
\alias{relSSBscatter}
\title{Relative change in biomass vs. relative catch scatter plot}
\usage{
relSSBscatter(
  wd,
  fileName,
  facetName,
  newLabel = NULL,
  chooseArea = 0,
  proYear,
  dpi = 300,
  imageName = "Relative_SSB_catch",
  outputDir = NULL,
  percentile = c(0.025, 0.975),
  doPlot = TRUE
)
}
\arguments{
\item{wd}{A working directory where the output of runProjection is saved}

\item{fileName}{List. List of file names in the working directory}

\item{facetName}{List. Plot uses ggplot2 facets. File names can be assigned to facets, thus producing separate plots for a given facet, such a low M scenario vs. a high M scenario.}

\item{newLabel}{List. Replacement label for each file.}

\item{chooseArea}{The area to summarized in the plot. Value of 0 sums quantities across all areas}

\item{proYear}{Numeric. The projection year used to calculate relative change in SSB and catch in weight.}

\item{dpi}{Resolution in dots per inch of the resulting saved chart.}

\item{imageName}{Character. A name for the resulting plot(s)}

\item{outputDir}{Directory where the output plot is saved. If NULL, it is set to wd.}

\item{percentile}{Vector of length two indicating centered percent of observations to represent uncertainty in the table. For example, 95\% centered observations should be entered as c(0.025, 0.975)}

\item{doPlot}{Logical, whether to create plot}
}
\description{
Plots the ratio of year prior to projection to a specified projection year. Iterations are summarized as the median ratio. Calculation are made for relative SSB (y-axis) and relative catch in weight (a-axis). Saves a chart to the wd. Examples can be found in data-raw/DAR_projection_example.R
}
