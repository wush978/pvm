.mran.url <- function(date) sprintf("https://mran.revolutionanalytics.com/snapshot/%s", date)

.check.installation <- function(lib, pkg) {
  if(is.null(lib)) {
    lib <- .libPaths()[1]
  }
  base::stopifnot(base::dir.exists(base::file.path(lib, pkg)))
}

.get.archive.installer <- function(url, lib, pkg) {
  force(url)
  force(lib)
  function() {
    .pkg.path <- base::tempfile(fileext = ".tar.gz")
    utils::download.file(url, destfile = .pkg.path)
    utils::install.packages(.pkg.path, lib = lib, repos = NULL)
    .check.installation(lib, pkg)
  }
}

.get.utils.installer <- function(pkgs, lib, repos, type = "source") {
  force(pkgs)
  force(lib)
  force(repos)
  function() {
    utils::install.packages(pkgs = pkgs, lib = lib, repos = repos, dependencies = FALSE, type = type)
    .check.installation(lib, pkgs)
  }
}

.get.remotes.install_version.installer <- function(pkg, version, repo, lib, type) {
  force(repo)
  force(lib)
  argv <- list(
    package = pkg,
    version = version,
    repos = repo,
    lib = lib,
    upgrade = "never",
    type = type
  )
  function() {
    base::do.call(remotes::install_version, argv)
    .check.installation(lib, pkg)
  }
}

.get.remotes.installer <- function(repo, lib, pkg) {
  force(repo)
  force(lib)
  argv <- strsplit(repo, split = "::", fixed = TRUE)[[1]]
  method <- argv[1]
  tokens <- strsplit(argv[2], split = "&", fixed = TRUE)
  retval <- list()
  for(token in tokens) {
    kv <- strsplit(token, split = "=", fixed = TRUE)[[1]]
    retval[[kv[1]]] <- kv[2]
  }
  retval[["lib"]] <- lib
  retval[["dependencies"]] <- FALSE
  remotes.env <- asNamespace("remotes")
  function() {
    base::do.call(remotes.env[[base::sprintf("install_%s", method)]], retval)
    .check.installation(lib, pkg)
  }
}

.get.url <- function() {
  # https://storage.googleapis.com/metamran-deploy/rda/bin-macosx-el-capitan-contrib-3.4.rda
  . <- utils::contrib.url("", base::.Platform$pkgType)
  . <- base::substring(., 2, base::nchar(.))
  if (substring(., 1, 3) == "bin") {
    .name <- base::gsub("/", "-", ., fixed = TRUE)
  } else {
    # source package
    .name <- "src-contrib"
  }
  base::sprintf("https://storage.googleapis.com/metamran-deploy/rda/%s.rda", .name)
}

.get.dst <- function() {
  .url <- .get.url()
  base::system.file(base::file.path("rda", base::basename(.url)), package = .packageName)
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
  .url <- .get.url()
  .dst <- .get.dst()
  if (verbose) base::cat(base::sprintf("Download metamran from %s to %s\n", .url, .dst))
  utils::download.file(.url, .dst, mode = "wb")
}

.metamran.find <- function(pkg, version) {
  .url <- .get.url()
  .dst <- .get.dst()
  if (base::file.exists(.dst)) {
    .env <- base::new.env()
    base::load(.dst, envir = .env)
    package.id <- base::local({
      . <- base::match(pkg, .env$pkg$package)
      .env$pkg$package_id[.]
    })
    version.id <- base::local({
      . <- base::match(version, .env$ver$version)
      .env$ver$version_id[.]
    })
    if (is.na(package.id) || is.na(version.id)) return(FALSE)
    . <- base::subset(.env$metamran, .env$metamran$package_id == package.id)
    . <- base::subset(., .$version_id == version.id)
    if (base::nrow(.) == 1) {
      return(base::as.Date(.$end))
    } else {
      return(FALSE)
    }
  } else {
    FALSE
  }
}

.compare.version <- function(v1, v2, operator) {
  if (is.na(v1)) FALSE else if (is.na(v2)) FALSE else operator(v1, v2)
}

package_version <- function(x) {
  retval <- try(base::package_version(x), silent = TRUE)
  if (class(retval)[1] == "try-error") {
    NA
  } else retval
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
#'@param Ncpus integer. The number of parallel processes to use for a parallel install of more than one packages.
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
import.packages <- function(file = "pvm.yml", lib.loc = NULL, ..., repos = getOption("repos"), import.recommended = FALSE, dryrun = FALSE, verbose = TRUE, strict.version = TRUE, Ncpus = getOption("Ncpus", 1L)) {
  pvm <- .upgrade.yaml(file)
  pkg.list <- utils::installed.packages(lib.loc, ...)
  varg <- list(...)
  is.target <- logical(length(pvm))
  is.target[] <- FALSE
  names(is.target) <- names(pvm)
  # remove installed packages / base / recommended
  for(name in names(pvm)) {
    if (name %in% rownames(pkg.list)) {
      if (!is.na(pkg.list[name, "Priority"])) {
        if (pkg.list[name, "Priority"] == "base") {
          is.target[name] <- FALSE
          next
        }
        if (pkg.list[name, "Priority"] == "recommended") {
          is.target[name] <- import.recommended
          next
        }
      }
      target.version <- package_version(pvm[name])
      current.version <- package_version(pkg.list[name, "Version"])
      if (!is.na(target.version)) if (target.version == current.version) {
        is.target[name] <- FALSE
        next
      }
    } # name %in% rownames(pkg.list)
    is.target[name] <- TRUE
  }
  # check CRAN
  contrib.urls <- utils::contrib.url(repos, "source")
  if (repos["CRAN"] == "@CRAN@") {
    repos <- getOption("repos")
  }
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
  archives <- lapply(contrib.urls, function(contrib.url) {
    tryCatch({
      con <- gzcon(url(base::sprintf("%s/Meta/archive.rds", contrib.url), "rb"))
      on.exit(close(con))
      readRDS(con)
    }, warning = function(e) list(), error = function(e) list())
  })
  names(contrib.urls) <- 
    names(availables.binary) <- 
    names(availables.src) <- 
    names(archives) <- 
    names(repos)
  Rversion <- sprintf("%s.%s", R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
  # get installers
  installers <- lapply(names(pvm)[is.target], function(name) {
    force(name)
    if (name == "R") {
      target.Rversion <- paste(utils::head(strsplit(pvm[name], ".", TRUE)[[1]], 2), collapse = ".")
      if (Rversion != target.Rversion) {
        warning(sprintf("The version of R is not matched. Your version is %s and the exported version is %s", Rversion, target.Rversion))
      }
      return(NULL)
    }
    if (verbose) base::cat(base::sprintf("Installing %s (%s) ...\n", name, pvm[name]))
    version <- package_version(pvm[name])
    if (!is.na(version)) {
      # CRAN
      if (name %in% rownames(availables.binary[["CRAN"]])) {
        binary.version <- package_version(availables.binary[["CRAN"]][name,"Version"])
      } else {
        binary.version <- NA
      }
      if (name %in% rownames(availables.src[["CRAN"]])) {
        src.version <- package_version(availables.src[["CRAN"]][name,"Version"])
      } else {
        src.version <- NA
      }
      if (.compare.version(version, binary.version, `==`)) {
        # Found binaries on CRAN
        if (verbose) base::cat(base::sprintf("Install %s (%s) from CRAN\n", name, pvm[name]))
        .retval <- .get.utils.installer(name, lib.loc, repos["CRAN"], type = base::.Platform$pkgType)
        return(.retval)
      } else {
        # There is no proper binary version in CRAN, check MRAN
        .type <- .Platform$pkgType
        if (.type == "source") {
          if (verbose) base::cat(base::sprintf("Install %s (%s) from CRAN for type: %s\n", name, pvm[name], .type))
          .retval <- .get.remotes.install_version.installer(name, version = pvm[name], lib = lib.loc, repo = repos["CRAN"], type = .type)
          return(.retval)
        }
        .date <- .metamran.find(name, pvm[name])
        if (class(.date) == "Date") {
          if (verbose) base::cat(base::sprintf("Install %s (%s) from MRAN snapshot of date %s for type: %s\n", name, pvm[name], .date, .type))
          .retval <- .get.utils.installer(name, lib.loc, .mran.url(.date), type = .type)
          return(.retval)
        }
        if (verbose) base::cat("Checking if there is a source package in CRAN...\n")
        if (!is.null(archives[["CRAN"]][[name]])) {
          .ar <- archives[["CRAN"]][[name]]
          .filename <- sprintf("%s/%s_%s.tar.gz", name, name, pvm[name])
          if (.filename %in% rownames(.ar)) {
            if (verbose) base::cat(base::sprintf("Install source package %s (%s) from archive of CRAN\n", name, pvm[name]))
            .retval <- .get.archive.installer(base::sprintf("%s/Archive/%s", contrib.urls["CRAN"], .filename), lib.loc, name)
            return(.retval)
          }
        }
        stop(base::sprintf("Failed to find %s (%s) from CRAN", name, pvm[name]))
        # older version
      }
    } else {
      # Non-CRAN
      if (verbose) base::cat(base::sprintf("Install %s from %s\n", name, pvm[name]))
      .retval <- .get.remotes.installer(pvm[name], lib.loc, name)
      return(.retval)
    }
  })
  if (!dryrun) {
    if (Ncpus > 1L) {
      installers.order <- split(installers, paste(attr(pvm, "order")[is.target]))
      cl <- parallel::makeCluster(Ncpus, outfile = "")
      tryCatch({
        for(o in seq_len(max(attr(pvm, "order")))) {
          current.targets <- Filter(f = function(x) !is.null(x), installers.order[[paste(o)]])
          parallel::parLapplyLB(cl, current.targets, function(f) {
            f()
          })
        }
      }, finally = {
        parallel::stopCluster(cl)
      })
    } else {
      for(f in installers) {
        if (is.null(f)) next
        f()
      }
    }
  }
}
