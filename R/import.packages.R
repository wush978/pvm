.mran.url <- function(date) sprintf("https://mran.revolutionanalytics.com/snapshot/%s", date)

.get.archive.installer <- function(url, lib) {
  force(url)
  force(lib)
  function() {
    .pkg.path <- base::tempfile(fileext = ".tar.gz")
    utils::download.file(url, destfile = .pkg.path)
    utils::install.packages(.pkg.path, lib = lib, repos = NULL)
  }
}

.get.utils.installer <- function(pkgs, lib, repos) {
  force(pkgs)
  force(lib)
  force(repos)
  function() {
    utils::install.packages(pkgs = pkgs, lib = lib, repos = repos, dependencies = FALSE)
  }
}

.get.remotes.installer <- function(pkg, lib) {
  force(pkg)
  force(lib)
  repos <- strsplit(pkg$repository, split = "::", fixed = TRUE)[[1]]
  remotes.env <- asNamespace("remotes")
  function() {
    remotes.env[[sprintf("install_%s", repos[1])]](repos[2], lib = lib)
  }
}

#'Import packages
#'
#'Install the package with the specific version according to the YAML file which is created by \code{\link{export.packages}}.
#'
#'@param file the filename of the YAML file.
#'@param lib.loc character vector giving the library directories where to check and install the packages.
#'@param ... Further arguments passed to \code{\link[utils]{installed.packages}}.
#'@param repos character vector, the base URL(s) of the CRAN repositories to use.
#'@param import.recommended logical value. Whether to install those packages whose priority is \code{"recommended"}.
#'@param dryrun logical value. If \code{TRUE}, then \code{import.packages} only check the CRAN/MRAN without installing the packages.
#'@param verbose logical value. If \code{TRUE}, show more information.
#'@param strict.version logical value. If \code{TRUE}, install the package even if there is a later version of the package in the \code{lib.loc}.
#'@export
#'@examples
#'\dontrun{
#'# import packages
#'import.packages() # import from "pvm.yml"
#'
#'# Set a local library directory for importing
#'dir.create(".lib")
#'.libPaths(".lib")
#'import.packages()
#'}
import.packages <- function(file = "pvm.yml", lib.loc = NULL, ..., repos = getOption("repos"), import.recommended = FALSE, dryrun = FALSE, verbose = TRUE, strict.version = TRUE) {
  pvm <- yaml::yaml.load_file(file)
  pvm <- .pvmrize(pvm)
  pkg.list <- utils::installed.packages(lib.loc, ...)
  varg <- list(...)
  is.target <- logical(length(pvm))
  is.target[] <- TRUE
  names(is.target) <- names(pvm)
  # remove installed packages / base / recommended
  for(name in names(pvm)) {
    pkg <- pvm[[name]]
    if (!is.na(pkg$priority)) {
      if (pkg$priority == "recommended") {
        is.target[name] <- import.recommended
      } else is.target[name] <- FALSE
    }
    if (!is.target[name]) next
    if (name %in% rownames(pkg.list)) {
      version <- package_version(pvm[[name]]$version)
      installed.version <- package_version(pkg.list[name,"Version"])
      if (strict.version) {
        if (installed.version == version) {
          is.target[name] <- FALSE
          next
        }
      } else {
        if (installed.version >= version) {
          is.target[name] <- FALSE
          next
        }
      }
    }
  }
  pre.installed <- names(which(!is.target))
  schedule <- sort(pvm, pre.installed = pre.installed)
  # check CRAN
  contrib.urls <- utils::contrib.url(repos, "source")
  names(contrib.urls) <- names(repos)
  availables <- lapply(contrib.urls, function(contrib.url) {
    utils::available.packages(contriburl = contrib.url, fields = c("Version", "Published", "Built"))
  })
  names(availables) <- names(repos)
  archives <- lapply(contrib.urls, function(contrib.url) {
    tryCatch({
      con <- gzcon(url(sprintf("%s/Meta/archive.rds", contrib.url), "rb"))
      on.exit(close(con))
      readRDS(con)
    }, warning = function(e) list(), error = function(e) list())
  })
  names(archives) <- names(repos)
  # installation
  for(pkg.turn in schedule) {
    if (verbose) cat(sprintf("Installing %s ...\n", paste(pkg.turn, collapse = " ")))
    install.turn <- lapply(pkg.turn, function(pkg.name) {
      pkg <- pvm[[pkg.name]]
      if (verbose) cat(sprintf("Installing %s (%s)...\n", pkg$name, pkg$version))
      target.version <- package_version(pkg$version)
      if (pkg$repository == "CRAN") {
        if (target.version == package_version(availables[["CRAN"]][pkg$name,"Version"])) {
          if (verbose) cat("Install %s (%s) from CRAN\n", pkg$name, pkg$version)
          .retval <- .get.utils.installer(pkg$name, lib.loc, repos)
          return(.retval)
        } else if (target.version < package_version(availables[["CRAN"]][pkg$name,"Version"])) {
          # Search archives
          type <- .Platform$pkgType
          if (type %in% c("win.binary", "mac.binary", "mac.binary.mavericks")) {
            if (verbose) cat("Checking if there is a binary package in MRAN...\n")
            meta <- yaml::yaml.load_file(url(sprintf("https://wush978.github.io/metamran/%s/%s/info.yml", pkg$name, pkg$version)))
            Rversion <- sprintf("%s.%s", R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
            meta.match <- Filter(function(x) {
              x$type == type & x$Rversion == Rversion
            }, meta)
            if (length(meta.match) == 1) {
              if (verbose) cat(sprintf("Install %s (%s) from MRAN snapshot of date %s for type: %s\n", pkg$name, pkg$version, meta.match[[1]]$end, type))
              .retval <- .get.utils.installer(pkg$name, lib.loc, .mran.url(meta.match[[1]]$end))
              return(.retval)
            }
          }
          if (verbose) cat("Checking if there is a source package in CRAN...\n")
          if (!is.null(archives[["CRAN"]][[pkg$name]])) {
            .ar <- archives[["CRAN"]][[pkg$name]]
            .filename <- sprintf("%s/%s_%s.tar.gz", pkg$name, pkg$name, pkg$version)
            if (.filename %in% rownames(.ar)) {
              if (verbose) cat(sprintf("Install %s (%s) from archive of CRAN\n", pkg$name, pkg$version))
              # utils::download.file(sprintf("%s/Archive/%s", contrib.urls["CRAN"], .filename), destfile = .pkg.path <- tempfile(fileext = ".tar.gz"))
              # .retval <- .get.utils.installer(.pkg.path, lib.loc, NULL)
              .retval <- .get.archive.installer(sprintf("%s/Archive/%s", contrib.urls["CRAN"], .filename), lib.loc)
              return(.retval)
            }
          }
          stop(sprintf("Failed to find %s (%s) from CRAN", pkg$name, pkg$version))
        } else stop(sprintf("Failed to find %s (%s) from CRAN", pkg$name, pkg$version))
      } else {
        if (verbose) cat(sprintf("Install %s (%s) from %s\n", pkg$name, pkg$version, pkg$repository))
        .retval <- .get.remotes.installer(pkg, lib.loc)
        return(.retval)
      }
    })
    if (dryrun) {
      print(install.turn)
    } else {
      lapply(install.turn, function(f) {
        f()
      })
    }
  }
}
