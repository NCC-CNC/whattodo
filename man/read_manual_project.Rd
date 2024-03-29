% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_read_manual_project.R
\name{read_manual_project}
\alias{read_manual_project}
\title{Read project}
\usage{
read_manual_project(
  spreadsheet_path,
  parameters,
  spatial_path = NULL,
  name = tools::file_path_sans_ext(basename(spreadsheet_path)),
  author_name = get_golem_config("default_project_name"),
  author_email = get_golem_config("default_project_email")
)
}
\arguments{
\item{spreadsheet_path}{\code{character} file path for the spreadsheet data.}

\item{parameters}{\code{list} object to interpret spreadsheet data.}

\item{spatial_path}{\code{character} file path for the spatial data.
Defaults to \code{NULL} such that no spatial data are imported.}

\item{name}{\code{character} name for the project.
Defaults to a value derived from the argument to \code{spreadsheet_path}.}

\item{author_name}{\code{character} name for the author of the  project.
Defaults to a parameter specified in the Golem configuration file.}

\item{author_email}{\code{character} contact email for the project.
Defaults to a parameter specified in the Golem configuration file.}
}
\value{
A \code{list} containing the following elements:
\describe{
\item{name}{A \code{character} value indicating the name.}
\item{author_name}{A \code{character} indicating the author name for the data.}
\item{author_email}{A \code{character} indicating the contact email for the data.}
\item{project}{A \link{Project} object.}
}
}
\description{
Read a project from disk.
}
\examples{
# specify file paths
f1 <- system.file(
  "extdata", "projects", "simulated-data", "simulated-data.xlsx",
  package = "whattodo"
)
f2 <- system.file(
  "extdata",  "projects", "simulated-data", "simulated-data.shp",
  package = "whattodo"
)

# read project
x <- read_manual_project(f1, whatdataio::read_data_configuration(), f2)

# print project
print(x)
}
