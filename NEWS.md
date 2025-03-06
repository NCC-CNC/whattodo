# whattodo 1.0.0

### Notice

- Official What To Do version 1.
- The `renv.lock` file only includes application dependencies from Imports, Depends and LinksTo found within the DESCRIPTION file. It does not include the Suggests packages needed for development. This reduces the bloat on deployment. The steps to contribute to package development include cloning whattodo, running `renv::restore()`, and then manually `renv::install` the Suggests packages referencing the correct version. Example `renv::install(testthat@3.3.2.1.1)`. 

### Major changes

- Requires R version 4.4.1
- Updated all CRAN package dependencies to latest version as of February, 2025.
- Updated REMOTE leaflet package dependency to latest version as of August, 2024.
- Removed `leaflet.extras2::addHistory` button. This feature is not compatible with Shiny >= 1.7.0 when they removed the bundled copy of fontawesome to the fontawesome package.
- Updated license agreement.
- Updated `renv.lock` file only records application dependencies from Imports, Depends and LinksTo.

### Minor changes and bug fixes

- Replaced pryr with lobstr for tracking memory usage.

### Infrastrucure changes

- Updated to rocker/shiny:4.4.1 AS base in Dockerfile.
- Updated Github action workflows

# whattodo 0.0.0.9000

- Initial package version.
