
if (Sys.getenv("TEST_PVM") != "TRUE") quit("no")
library(pvm)
options(repos = c(CRAN = "http://cloud.r-project.org/"))
lib.loc <- tempfile(fileext = ".lib")
result <- lapply(dir("pvm", pattern = "yml", full.names = TRUE), function(path) {
  dir.create(lib.loc, showWarnings = FALSE)
  R_TEST <- Sys.getenv("R_TESTS")
  cat(sprintf("Importing from %s\n", path))
  Sys.setenv("R_TESTS" = "")
  on.exit({
    unlink(lib.loc, recursive = TRUE)
    Sys.setenv("R_TESTS" = R_TEST)
  })
  import.packages(path, lib.loc = lib.loc)
  pkgs <- installed.packages(lib.loc = lib.loc, fields = pvm:::.check.fields)
  obj <- yaml::yaml.load_file(path)
  for(name in names(obj)) {
    if (name == "__version__") next
    for(pkg in names(obj[[name]])) {
      version <- try(package_version(obj[[name]][[pkg]]), silent = TRUE)
      if (class(version)[1] == "try-error") {
        version <- NA
      }
      if (!is.na(version)) {
        # CRAN
        stopifnot(pkgs[pkg,"Version"] == version)
      } else {
        stop("TODO")
      }
    }
  }
  NULL
})
