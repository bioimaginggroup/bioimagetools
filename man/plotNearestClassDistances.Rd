% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_nearest_class_distances.R
\name{plotNearestClassDistances}
\alias{plotNearestClassDistances}
\title{Title Plot nearest class distances}
\usage{
plotNearestClassDistances(
  distances,
  method,
  classes = length(distances),
  ylim = c(0, 1),
  qu = 0.01,
  mfrow = NULL
)
}
\arguments{
\item{distances}{list of list with distances as produced by nearestClassDistances()}

\item{method}{"boxplot", "min" or "quantile"}

\item{classes}{number of classes, default=7}

\item{ylim}{limits for distances, default=c(0,1)}

\item{qu}{quantile for method="quantile"; default 0.01}

\item{mfrow}{mfrow option forwarded to par; default NULL, computes some optimal values}
}
\value{
plots
}
\description{
Title Plot nearest class distances
}
