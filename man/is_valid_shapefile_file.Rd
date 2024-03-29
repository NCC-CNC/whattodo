% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_file_validation.R
\name{is_valid_shapefile_file}
\alias{is_valid_shapefile_file}
\title{Is valid shapefile file?}
\usage{
is_valid_shapefile_file(x)
}
\arguments{
\item{x}{\code{character} file path.}
}
\value{
A \code{logical} value indicating validity, or a \code{character}
explanation of why the file is not valid.
}
\description{
Verify if a file path corresponds to a valid ESRI Shapefile.
}
