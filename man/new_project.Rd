% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Project.R
\name{new_project}
\alias{new_project}
\title{New project}
\usage{
new_project(
  site_ids,
  site_descriptions,
  feature_ids,
  feature_descriptions,
  action_ids,
  action_descriptions,
  site_data,
  feature_data,
  feasibility_data,
  consequence_data,
  parameters,
  site_geometry = NULL,
  id = uuid::UUIDgenerate()
)
}
\arguments{
\item{site_ids}{\code{character} identifiers for sites.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{site_descriptions}{\code{character} descriptions of sites.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{feature_ids}{\code{character} identifiers for biodiversity features.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{feature_descriptions}{\code{character} descriptions of biodiversity
features.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{action_ids}{\code{character} identifiers for management actions.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{action_descriptions}{\code{character} descriptions of management actions.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{site_data}{\code{data.frame} containing site data.}

\item{feature_data}{\code{data.frame} containing feature data.}

\item{feasibility_data}{\code{data.frame}containing feasibility data.}

\item{consequence_data}{\code{list} of \code{data.frame} objects
containing consequence data.}

\item{parameters}{\code{list} object containing parameters to customize
appearance of worksheet.}

\item{site_geometry}{\code{\link[sf:sf]{sf::st_sf()}} object containing spatial boundaries
for the sites.
Defaults to \code{NULL} such that site locations are generated based
on the longitude/latitude of each site (per argument to \code{site_data}).}

\item{id}{\code{character} identifier for object.}
}
\value{
A \link{Project} object.
}
\description{
Create a new \link{Project} object.
}
\details{
This function creates an Excel Workbook with data and results.
}
\examples{
#TODO
}
