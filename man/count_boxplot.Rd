% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_boxplot.R
\name{count_boxplot}
\alias{count_boxplot}
\title{Boxplot of the counts}
\usage{
count_boxplot(
  nano_obj,
  by = "feature",
  slot_id = "count",
  array_name = NA,
  features = NA,
  filter_for = NA,
  color_by = NA
)
}
\arguments{
\item{nano_obj}{A nano object}

\item{by}{feature or sample.}

\item{slot_id}{Slot to used for the plot. For raw counts use 'count' (default) and for normalized count use 'count_norm'}

\item{array_name}{If normalize count is used then enter the name of the array which was given in the normalization command}

\item{features}{Instead of filter_for you can also give a vector of target names (RNA or protein). Both filter_for and features can't be supplied.}

\item{filter_for}{Character string. CodeClass name for the features you want to include in the count. For example Endogenous, Negative, Control etc.}

\item{color_by}{Column name of the id data to use for coloring the points}
}
\description{
If by = 'feature' then the independent variables are the targets (RNA/Protein) otherwise boxplots represents count on each ROI.
}
