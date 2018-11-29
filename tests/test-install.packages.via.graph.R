Sys.setenv("R_TESTS"="")
library(pvm)
R.date <- R.release.dates[sprintf("%s.%s", R.version$major, R.version$minor)]
if (is.na(R.date)) {
  repos <- c(CRAN = "https://cloud.r-project.org/")
} else {
  repos <- c(CRAN = sprintf("https://cran.microsoft.com/snapshot/%s", R.date + 7))
}
options(repos = repos)
install.packages("Rcpp")
install.packages.via.graph("xml2")
packageVersion("xml2")
