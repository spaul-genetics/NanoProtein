% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_nano_obj.R
\name{subset_nano_obj}
\alias{subset_nano_obj}
\title{Subset a nano object}
\usage{
subset_nano_obj(
  nano_obj,
  filter_by = NA,
  filter_str = NA,
  keep_id = NULL,
  keep_feature = NA
)
}
\arguments{
\item{nano_obj}{A nano_object to subset}

\item{filter_by}{One of the three metadata slots: id, rows, or sample corresponding to id.data, row.data, and sample.data, respectively. A column of given data.frame will be used for filtering.}

\item{filter_str}{An evalutable condition that will be used for filtering such as "Status=='Normal'" will keep the normal tissue samples given that the Status variable on the id.data contains this information.}

\item{keep_id}{A vector of sample ids to keep if not provided all the sample are kept}

\item{keep_feature}{A vector of feature names to keep. If not provided all the features are kept}
}
\description{
If keep_id or keep_feature is given then the object is subsetted based on sample or feature. No other operation will be performed in that case.
If they are not given then the obsject will be subsetted based on filter_by slot and using filter_str expression.
}
\details{
Subsets a nano object based on conditions
}
