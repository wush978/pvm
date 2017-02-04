#' Package Version Manager for R
#'
#'\itemize{
#'  \item{Record the version of installed packages to an YAML(\url{https://en.wikipedia.org/wiki/YAML}) file via \code{\link{export.packages}}.
#'  }
#'  \item{Install the exported packages with given version from CRAN(latest or source) or MRAN(outdated binary, \url{https://mran.revolutionanalytics.com}) 
#'    according to the given YAML file via \code{\link{import.packages}}
#'  }
#'}
#'
#'@examples
#'\dontrun{
#'# At original workspace:
#'pvm::export.packages()
#'# Modify the content of `pvm.yml` if there are non-CRAN packages.
#'
#'# Copy the `pvm.yml` to the new workspace
#'# Run:
#'pvm::import.packages()
#'}
"_PACKAGE"