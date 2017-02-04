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

.get.utils.installer <- function(pkgs, lib, repos, type = "source") {
  force(pkgs)
  force(lib)
  force(repos)
  function() {
    # .origin.option <- getOption("install.packages.compile.from.source")
    # options(install.packages.compile.from.source = "never")
    # on.exit(options(install.packages.compile.from.source = .origin.option))
    utils::install.packages(pkgs = pkgs, lib = lib, repos = repos, dependencies = FALSE, type = type)
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

.meta <- new.env()
.check_metamran <- function() {
  metamran.path <- system.file("metamran.rda", package = "pvm")
  if (metamran.path != "") {
    load(metamran.path, envir = .meta)
  }
  if (is.null(.meta$metamran)) data(metamran, package = "pvm", envir = .meta)
}

#'Update the Index of packages on MRAN Daily Snapshots
#'
#'Download the latest index from github.
#'
#'@param verbose logical value. If \code{TRUE}, show more information.
#'
#'@details
#'The package \code{git2r} is required to update.
#'@export
metamran.update <- function(verbose = TRUE) {
  .branch <- readLines("https://api.github.com/repos/wush978/metamran/git/refs/heads/gh-pages", warn = FALSE)
  .branch <- strsplit(.branch, ",")[[1]]
  .commit.url <- regmatches(.branch, regexec('\\"url":"([^"]+)"', .branch))
  .commit.url <- tail(Filter(f = function(x) length(x) > 1, .commit.url), 1)[[1]][2]
  .commit <- readLines(.commit.url, warn = FALSE)
  .date <- regmatches(.commit, regexec('"message":"Update: (.{10})"', .commit))[[1]][2]
  .date <- as.Date(.date)
  if (verbose) cat(sprintf("The updating date of remote dataset: %s\n", .date))
  stopifnot(!is.na(.date))
  .check_metamran()
  if (verbose) cat(sprintf("The updating date of local dataset: %s\n", .meta$metamran$.date))
  if (.date > .meta$metamran$.date) {
    if (verbose) cat("Updating data...\n")
    git2r::clone(url = "https://github.com/wush978/metamran", branch = "gh-pages", local_path = repo.path <- tempfile())
    infos <- dir(repo.path, pattern = "info.yml", full.names = TRUE, recursive = TRUE)
    metamran <- new.env(parent = emptyenv())
    if (verbose) pb <- txtProgressBar(max = length(infos), style = 3)
    for(info in infos) {
      version <- basename(dirname(info))
      package <- basename(dirname(dirname(info)))
      obj <- yaml::yaml.load_file(info)
      key <- sprintf("%s_%s", package, version)
      assign(key, obj, envir = metamran)
      if (verbose) setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
    }
    if (verbose) close(pb)
    assign(".date", .date, envir = metamran)
    save(metamran, file = file.path(system.file(package = "pvm"), "metamran.rda"), compress = "bzip2")
    if (verbose) cat("Done\n")
  }
  invisible(NULL)
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
  if (repos["CRAN"] == "@CRAN@") {
    repos <- getOption("repos")
  }
  names(contrib.urls) <- names(repos)
  availables.src <- lapply(contrib.urls, function(contrib.url) {
    utils::available.packages(contriburl = contrib.url)
  })
  if (base::.Platform$pkgType == "source") {
    availables.binary <- availables.src
  } else {
    availables.binary <- lapply(utils::contrib.url(repos, base::.Platform$pkgType), function(contrib.url) {
      utils::available.packages(contriburl = contrib.url)
    })
  }
(repos)
  archives <- lapply(contrib.urls, function(contrib.url) {
    tryCatch({
      con <- gzcon(url(sprintf("%s/Meta/archive.rds", contrib.url), "rb"))
      on.exit(close(con))
      readRDS(con)
    }, warning = function(e) list(), error = function(e) list())
  })
  names(availables.src) <- 
    names(availables.binary) <- 
    names(archives) <- 
    names(repos)
  # installation
  for(pkg.turn in schedule) {
    if (verbose) cat(sprintf("Installing %s ...\n", paste(pkg.turn, collapse = " ")))
    install.turn <- lapply(pkg.turn, function(pkg.name) {
      pkg <- pvm[[pkg.name]]
      if (verbose) cat(sprintf("Installing %s (%s)...\n", pkg$name, pkg$version))
      target.version <- package_version(pkg$version)
      if (pkg$repository == "CRAN") {
        if (target.version == package_version(availables.binary[["CRAN"]][pkg$name,"Version"])) {
          if (verbose) cat(sprintf("Install %s (%s) from CRAN\n", pkg$name, pkg$version))
          .retval <- .get.utils.installer(pkg$name, lib.loc, repos, type = base::.Platform$pkgType)
          return(.retval)
        } else if (target.version < package_version(availables.binary[["CRAN"]][pkg$name,"Version"])) {
          # Search archives
          type <- .Platform$pkgType
          if (type %in% c("win.binary", "mac.binary", "mac.binary.mavericks")) {
            if (verbose) cat("Checking if there is a binary package in MRAN from local dataset\n")
            Rversion <- sprintf("%s.%s", R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
            .check_metamran()
            meta <- .meta$metamran[[sprintf("%s_%s", pkg$name, pkg$version)]]
            if (!is.null(meta)) {
              meta.match <- Filter(function(x) {
                x$type == type & x$Rversion == Rversion
              }, meta)
              if (length(meta.match) == 1) {
                if (verbose) cat(sprintf("Install %s (%s) from MRAN snapshot of date %s for type: %s\n", pkg$name, pkg$version, meta.match[[1]]$end, type))
                .retval <- .get.utils.installer(pkg$name, lib.loc, .mran.url(meta.match[[1]]$end), type = type)
                return(.retval)
              }
            }
            if (verbose) cat("Checking if there is a binary package in MRAN from the internet\n")
            meta <- try(yaml::yaml.load_file(url(sprintf("https://wush978.github.io/metamran/%s/%s/info.yml", pkg$name, pkg$version))), silent = TRUE)
            if (class(meta)[1] != "try-error") {
              meta.match <- Filter(function(x) {
                x$type == type & x$Rversion == Rversion
              }, meta)
              if (length(meta.match) == 1) {
                if (verbose) cat(sprintf("Install %s (%s) from MRAN snapshot of date %s for type: %s\n", pkg$name, pkg$version, meta.match[[1]]$end, type))
                .retval <- .get.utils.installer(pkg$name, lib.loc, .mran.url(meta.match[[1]]$end), type = type)
                return(.retval)
              }
            }
          }
          if (verbose) cat("Checking if there is a source package in CRAN...\n")
          if (!is.null(archives[["CRAN"]][[pkg$name]])) {
            .ar <- archives[["CRAN"]][[pkg$name]]
            .filename <- sprintf("%s/%s_%s.tar.gz", pkg$name, pkg$name, pkg$version)
            if (.filename %in% rownames(.ar)) {
              if (verbose) cat(sprintf("Install source package %s (%s) from archive of CRAN\n", pkg$name, pkg$version))
              .retval <- .get.archive.installer(sprintf("%s/Archive/%s", contrib.urls["CRAN"], .filename), lib.loc)
              return(.retval)
            }
          }
          stop(sprintf("Failed to find %s (%s) from CRAN", pkg$name, pkg$version))
        } else if (target.version == package_version(availables.src[["CRAN"]][pkg$name,"Version"])) {
          if (verbose) cat(sprintf("Install source package %s (%s) from CRAN\n", pkg$name, pkg$version))
          .retval <- .get.utils.installer(pkg$name, lib.loc, repos, type = "source")
          return(.retval)
        } else {
          stop(sprintf("Failed to find %s (%s) from CRAN", pkg$name, pkg$version))
        }
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

#'@name metamran
#'@title The Index of packages on MRAN Daily Snapshots
#'@description
#'The dataset contains the beginning dates and ending dates of all versioned packages on CRAN. 
#'@source \url{https://mran.revolutionanalytics.com} and \url{https://github.com/wush978/metamran}
NULL
