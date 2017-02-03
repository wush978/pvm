
if (Sys.getenv("TEST_RPVM") != "TRUE") quit("no")
library(rpvm)
options(repos = c(CRAN = "http://cloud.r-project.org/"))
lib.loc <- tempfile(fileext = ".lib")
lapply(dir("rpvm", pattern = "yml", full.names = TRUE), function(path) {
  dir.create(lib.loc, showWarnings = FALSE)
  R_TEST <- Sys.getenv("R_TESTS")
  Sys.setenv("R_TESTS" = "")
  on.exit({
    unlink(lib.loc, recursive = TRUE)
    Sys.setenv("R_TESTS" = R_TEST)
  })
  import.packages(path, lib.loc = lib.loc)
  pkgs <- installed.packages(lib.loc = lib.loc)
  obj <- yaml::yaml.load_file(path)
  for(name in names(obj)) {
    if (is.na(obj[[name]]$priority)) {
      stopifnot(obj[[name]]$version == pkgs[name,"Version"])
    }
  }
})
