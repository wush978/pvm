
library(pvm)
options(repos = c(CRAN = "http://cloud.r-project.org/"))
lib.loc <- tempfile(fileext = ".lib")
dir.create(lib.loc, showWarnings = FALSE)
Sys.setenv("R_TESTS" = "")
utils::install.packages("RcppCNPy", lib = lib.loc)
pvm <- export.packages(lib.loc = c(lib.loc, .libPaths()))
print(installed.packages(lib.loc = c(lib.loc, .libPaths()))["Rcpp",])
obj <- yaml::yaml.load_file("pvm.yml")
lapply(c("Rcpp", "RcppCNPy"), function(pkg) {
  stopifnot(v1 == v2)
  invisible(NULL)
})
