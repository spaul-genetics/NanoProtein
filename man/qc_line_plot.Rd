% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_line_plot.R
\name{qc_line_plot}
\alias{qc_line_plot}
\title{Line Plot of the counts}
\usage{
qc_line_plot(
  nano_obj,
  target,
  shadow = "Endogenous",
  target_group_col = "Target_Group",
  split_by = NULL,
  interactive = F,
  order_by = "GeoMean"
)
}
\arguments{
\item{nano_obj}{A nano object}

\item{target}{Lines to highlight such as Background and Housekeeping}

\item{shadow}{The lines to keep in grey color. By default Endogenous. It can be any category from CodeClass}

\item{target_group_col}{Where to find the targets. The function will `grep` the `target` at this column of row data. Default is Target_Group}

\item{split_by}{Split by any column of id data.}

\item{interactive}{If TRUE returns a plotly graph.}

\item{order_by}{How the ROIs are ordered. Default is geometric mean 'GeoMean' if target contains more than one elements. Possible value is a background IgG protein or a housekeeping protein}
}
\description{
Line Plot of the counts
}
\author{
Subrata Paul
}
