
library(pvm)
options(repos = c(CRAN = "http://cloud.r-project.org/"))
lib.loc <- tempfile(fileext = ".lib")
dir.create(lib.loc, showWarnings = FALSE)
Sys.setenv("R_TESTS" = "")
install.packages("RcppCNPy", lib = lib.loc)
export.packages(lib.loc = c(lib.loc, .libPaths()))
obj <- yaml::yaml.load_file("pvm.yml")
packageVersion("RcppCNPy", lib.loc = lib.loc)
lapply(c("Rcpp", "RcppCNPy"), function(pkg) {
  stopifnot(obj[[pkg]]$version == packageVersion(pkg, lib.loc))
})
