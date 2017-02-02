.mran.url <- function(date) sprintf("https://mran.revolutionanalytics.com/snapshot/%s", date)
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

#'@export
import.packages <- function(file = "rpvm.yml", ..., repos = getOption("repos"), import.recommended = FALSE, dryrun = FALSE, verbose = TRUE, strict.version = TRUE) {
  rpvm <- yaml::yaml.load_file(file)
  rpvm <- .rpvmrize(rpvm)
  pkg.list <- installed.packages(...)
  varg <- list(...)
  lib.loc <- if (is.null(varg$lib.loc)) .libPaths()[1] else varg$lib.loc
  is.target <- logical(length(rpvm))
  is.target[] <- TRUE
  names(is.target) <- names(rpvm)
  # remove installed packages / base / recommended
  for(name in names(rpvm)) {
    pkg <- rpvm[[name]]
    if (!is.na(pkg$priority)) {
      if (pkg$priority == "recommended") {
        is.target[name] <- import.recommended
      } else is.target[name] <- FALSE
    }
    if (!is.target[name]) next
    if (name %in% rownames(pkg.list)) {
      version <- package_version(rpvm[[name]]$version)
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
  schedule <- sort(rpvm, pre.installed = pre.installed)
  # check CRAN
  contrib.urls <- contrib.url(repos, "source")
  names(contrib.urls) <- names(repos)
  availables <- lapply(contrib.urls, function(contrib.url) {
    available.packages(contriburl = contrib.url, fields = c("Version", "Published", "Built"))
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
      pkg <- rpvm[[pkg.name]]
      if (verbose) cat(sprintf("Installing %s (%s)...\n", pkg$name, pkg$version))
      target.version <- package_version(pkg$version)
      if (pkg$repository == "CRAN") {
        if (target.version == package_version(availables[["CRAN"]][pkg$name,"Version"])) {
          .retval <- .get.utils.installer(pkg$name, lib.loc, repos)
          return(.retval)
        } else if (target.version < package_version(availables[["CRAN"]][pkg$name,"Version"])) {
          # Search archives
          type <- .Platform$pkgType
          if (type %in% c("win.binary", "mac.binary.mavericks")) {
            if (verbose) cat("Checking if there is a binary package in MRAN...\n")
            meta <- yaml::yaml.load_file(url(sprintf("https://wush978.github.io/metamran/%s/%s/info.yml", pkg$name, pkg$version)))
            Rversion <- sprintf("%s.%s", R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
            meta.match <- Filter(function(x) {
              x$type == type & x$Rversion == Rversion
            }, meta)
            if (length(meta.match) == 1) {
              .retval <- .get.utils.installer(pkg$name, lib.loc, .mran.url(meta.match[[1]]$end))
              return(.retval)
            }
          }
          if (verbose) cat("Checking if there is a source package in CRAN...\n")
          if (!is.null(archives[["CRAN"]][[pkg$name]])) {
            .ar <- archives[["CRAN"]][[pkg$name]]
            .filename <- sprintf("%s/%s_%s.tar.gz", pkg$name, pkg$name, pkg$version)
            if (.filename %in% rownames(.ar)) {
              download.file(sprintf("%s/Archive/%s", contrib.urls["CRAN"], .filename), destfile = .pkg.path <- tempfile(fileext = ".tar.gz"))
              .retval <- .get.utils.installer(.pkg.path, lib.loc, NULL)
              return(.retval)
            }
          }
          stop(sprintf("Failed to find %s (%s) from CRAN", pkg$name, pkg$version))
        } else stop(sprintf("Failed to find %s (%s) from CRAN", pkg$name, pkg$version))
      } else {
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
