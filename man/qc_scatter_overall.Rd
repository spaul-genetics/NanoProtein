% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_scatter_overall.R
\name{qc_scatter_overall}
\alias{qc_scatter_overall}
\title{Scatter plot with area and neucli count}
\usage{
qc_scatter_overall(
  nano_obj,
  Area = "AOI surface area",
  Nuclei = "AOI nuclei count",
  color_by = "Organism",
  shape_by = NA
)
}
\arguments{
\item{nano_obj}{A nano object}

\item{Area}{Rowname of the sample data that contains Area of the ROI}

\item{Nuclei}{Row of the sample data for AOI nuclei count}

\item{color_by}{Column of id data for coloring the points}

\item{shape_by}{Optional. Column of id data for point shape}
}
\description{
Scatter plot with area and neucli count
}
