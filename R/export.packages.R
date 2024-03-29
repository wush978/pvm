.trimws <- if (exists("trimws", envir = baseenv())) base::trimws else function(x) {
  mysub <- function(re, x) sub(re, "", x, perl = TRUE)
  mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
}

.upgrade.yaml <- function(file) {
  # check version
  src <- readLines(file)
  if (substring(src[1], 1, 13) == "__version__: ") {
    version <- base::package_version(substring(src[1], 14, nchar(src[1])))
  } else {
    version <- base::package_version("0.1")
  }
  if (version < base::package_version("0.2")) {
    pvm.src <- yaml::yaml.load_file(file)
    pvm.dst <- sapply(pvm.src, function(x) {
      if (x$repository == "CRAN") x$version else x$repository
    })
    names(pvm.dst) <- sapply(pvm.src, "[[", "name")
    attr(pvm.dst, "order") <- seq_along(pvm.dst)
    pvm.dst
  } else {
    src <- utils::tail(src, -1)
    if (length(src) == 0) return(structure(character(0), order = integer(0)))
    current.order <- NA
    is.order <- grepl("^(\\d+):$", src)
    first.colon <- sapply(gregexpr("^[^:]+:", src), attr, "match.length")
    header <- substring(src, 1, first.colon - 1)
    header <- .trimws(header)
    tailer<- substring(src, first.colon + 1, nchar(src))
    tailer <- .trimws(tailer)
    pvm.dst <- tailer[!is.order]
    names(pvm.dst) <- header[!is.order]
    pvm.order <- numeric(length(pvm.dst))
    j <- 1
    current.order <- NA
    for(i in seq_along(src)) {
      if (is.order[i]) {
        current.order <- as.integer(header[i])
        next
      }
      pvm.order[j] <- current.order
      j <- j + 1
    }
    stopifnot(!any(is.na(pvm.order)))
    attr(pvm.dst, "order") <- pvm.order
    pvm.dst
  }
}

.to.yaml <- function(pvm) {
  stopifnot(class(pvm)[1] == "pvm")
  pvm.order <- sapply(pvm, attr, "order")
  if (!all(!is.null(pvm.order))) {
    schedule <- sort(pvm)
    pvm <- pvm[unlist(schedule)]
    pvm <- .pvmrize(pvm, schedule)
  }
  if (any(diff(sapply(pvm, attr, "order")) < 0)) {
    schedule <- split(pvm, sapply(pvm, attr, "order"))
    schedule <- schedule[order(as.integer(names(schedule)))]
    schedule <- lapply(schedule, names)
    names(schedule) <- NULL
    pvm <- pvm[unlist(schedule)]
    pvm <- .pvmrize(pvm, schedule)
  }
  current.order <- 0
  retval <- lapply(pvm, function(pkg) {
    if (attr(pkg, "order") != current.order) {
      current.order <<- attr(pkg, "order")
      header <- sprintf("%d:\n", current.order)
    } else {
      header <- ""
    }
    if (pkg$repository == "CRAN") {
      sprintf("%s  %s: %s", header, pkg$name, pkg$version)
    } else {
      sprintf("%s  %s: %s", header, pkg$name, pkg$repository)
    }
  })
  paste(c("__version__: 0.2", retval), collapse = "\n")
}

.remote.fields <- unique(
  paste0("Remote", 
         c("Type", "Host", "Repo", "Subdir", "Username", "Ref", "Sha", "Url", "SvnSubdir", "Revision", "Args", "Branch", "Config",
           "Mirror", "Release", "Password", "Repos", "PkgType")
         )
  )
.check.fields <-  c("Package", "Version", "Priority", "Depends", "Imports", "LinkingTo", "Suggests", "Enhances", .remote.fields)

#'Export Packages
#'
#'Write the package and its version, dependency, priority and repository to a YAML file.
#'The user can invoke \code{\link{import.packages}} re-install the specific versions of 
#'the packages from the YAML file.
#'
#'@param file \code{NULL}, or a character string naming the file to write.
#'If \code{NULL}, return a S3 object \code{pvm}. The default is \code{"pvm.yml"}.
#'@param pvm a S3 object \code{pvm} created by \code{export.packages}.
#'@param ... Further arguments passed to \code{\link[utils]{installed.packages}}.
#'@details
#'Export a list of packages found via \code{utils::installed.packages(...)}.
#'
#'The repository of all packages will be set as \code{"CRAN"} as default. 
#'If there are some packages not on CRAN, the user should modify the 
#'generated YAML file and edit the repository to these packages to
#'appropriate repository. For some non-CRAN examples:
#'
#'\describe{
#'  \item{github}{\code{github::wush978/pvm}}
#'  \item{bitbucket}{\code{bitbucket::wush978/pvm}}
#'  \item{url}{\code{url::https://github.com/wush978/pvm/archive/master.zip}}
#'  \item{svn}{\code{svn::svn://github.com/wush978/pvm/trunk}}
#'  \item{git}{\code{git::git://github.com/wush978/pvm.git}}
#'}
#'
#'Note that the package \code{remotes} is required to import these non-CRAN packages.
#'@examples
#'\dontrun{
#'library(pvm)
#'export.packages()
#'cat(readLines("pvm.yml"), sep = "\n")
#'}
#'@seealso Please check \code{\link[remotes]{install_github}}, \code{\link[remotes]{install_bitbucket}}, 
#'\code{\link[remotes]{install_url}}, \code{\link[remotes]{install_svn}}, and \code{\link[remotes]{install_git}}
#'for more details of the non-CRAN repository.
#'@export
export.packages <- function(file = "pvm.yml", pvm = NULL, ...) {
  if (is.null(pvm)) {
    .ip <- utils::installed.packages(..., fields = .check.fields)
    dict <- build.pkg.graph(.ip, .ip)
    pvm <- .pvmrize(dict, NULL)
    schedule <- sort(pvm)
    pvm <- pvm[unlist(schedule)]
    pvm <- .pvmrize(pvm, schedule)
    # remove base packages
    pvm <- .pvmrize(Filter(function(x) {
      if (is.na(x$priority)) TRUE else !(x$priority %in% c("base", "recommended"))
    }, pvm))
    if (is.null(file)) return(invisible(pvm))
  }
  stopifnot(class(pvm) == "pvm")
  yaml <- .to.yaml(pvm)
  if (is.character(file)) {
    if (file != "" & (substring(file, 1L, 1L) != "|")) {
      # target is a file
      tmp.path <- tempfile(tmpdir = dirname(file), fileext = ".yml")
      tryCatch({
        write(yaml, file = tmp.path)
        file.rename(tmp.path, file)
      }, finally = {
        if (file.exists(tmp.path)) unlink(tmp.path, recursive = TRUE, force = TRUE)
      })
      return(invisible(pvm))
    }
  }
  write(yaml, file = file)
  return(invisible(pvm))
}

#'Install Necessary via Analyzing Package Dependency Graph
#'
#'This function will analysis the package graph before installation.
#'Only those packages whose version are lower than required version are going to be installed.
#'
#'@param name character vector. The package that the user wants to install.
#'@param pkg.list.raw the output of \code{utils::available.packages}.
#'@param ... arguments that will be passed to \code{utils::installed.packages}
#'@examples
#'\dontrun{
#'  install.packages.via.graph("dplyr")
#'}
#'@export
install.packages.via.graph <- function(name, pkg.list.raw = utils::available.packages(fields = .check.fields), ...) {
  if (.Platform$pkgType == "source") {
    utils::install.packages(name)
  } else {
    pg <- get.pkg.installation.graph(name, pkg.list.raw, ...)
    tmp.yaml <- tempfile(fileext = ".yml")
    write(.to.yaml(pg), tmp.yaml)
    import.packages(tmp.yaml)
  }
}

get.pkg.installation.graph <- function(name, pkg.list.raw = utils::available.packages(fields = .check.fields), ...) {
  dp <- tools::package_dependencies(name, db = pkg.list.raw, recursive = TRUE)
  targets <- lapply(name, function(.) {
    setdiff(dp[[.]], get.base.pkg.list())
  })
  targets <- unique(unlist(targets))
  targets <- c(name, targets)
  dict <- build.pkg.graph(pkg.list.raw[targets,,drop=FALSE])
  result <- .pvmrize(dict, NULL)
  schedule <- sort(result)
  result <- .pvmrize(result, schedule)
  # remove base packages
  ip <- utils::installed.packages(...)
  check.result <- names(result) %in% rownames(ip)
  names(check.result) <- names(result)
  for(obj in rev(result)) {
    if (!is.null(obj$deps)) {
      for(dep in obj$deps) {
        if (check.result[dep$name]) {
          if (dep$name %in% rownames(ip)) {
            if (!is.null(dep$op)) check.result[dep$name] <- get(dep$op, envir = base::baseenv())(utils::packageVersion(dep$name), package_version(dep$version))
          } else {
            check.result[dep$name] <- FALSE
          }
        }
      }
    }
  }
  result <- result[!check.result]
  result <- .pvmrize(result, NULL)
  result <- .pvmrize(Filter(function(x) {
    if (is.na(x$priority)) TRUE else x$priority != "base"
  }, result))
  result
}

get.base.pkg.list <- function() {
  pkg.list.installed <- utils::installed.packages(fields = .check.fields)
  . <- pkg.list.installed[which(pkg.list.installed[,"Priority"] == "base"),,drop=FALSE]
  rownames(.)
}

build.pkg.graph <- function(pkg.list.raw, pkg.list.installed = utils::installed.packages(fields = .check.fields)) {
  # check duplication of the same package with different version
  # it will happen if length(lib.loc) > 1
  pkg.list.raw <- local({
    pkgs <- rownames(pkg.list.raw)
    .i <- which(duplicated(pkgs))
    dpkgs <- unique(pkgs[.i])
    if (length(.i) == 0) pkg.list.raw else {
      cat(sprintf("The following packages are duplicated: %s\n", paste(dpkgs, collapse = ",")))
      cat("Pick the latest version\n")
      .x1 <- pkg.list.raw[-.i,]
      .x2 <- do.call(what = rbind, lapply(dpkgs, function(pkg) {
        .x <- pkg.list.raw[which(pkg == pkgs),]
        .v <- package_version(.x[,"Version"])
        .x[which(max(.v) == .v)[1],,drop = FALSE]
      }))
      rbind(.x1, .x2)
    }
  })
  
  pkg.list.priority <- pkg.list.raw[,"Priority"]
  pkg.list.base <- pkg.list.installed[which(pkg.list.installed[,"Priority"] == "base"),,drop=FALSE]
  pkg.list.recommended <- pkg.list.raw[which(pkg.list.priority == "recommended"),, drop = FALSE]
  pkg.list.target <- pkg.list.raw[which(is.na(pkg.list.priority)),, drop = FALSE]
  # Constructing package graph
  dict <- new.env()
  # init nodes for "base" and "R"
  .create.node(new.env(parent = emptyenv()), "R", sprintf("%s.%s", R.version$major, R.version$minor), "base", dict)
  .insert.node <- function(pkg.list) {
    if (nrow(pkg.list) == 0) return(invisible(NULL))
    .check.last <- NULL
    while(!all(.check <- apply(pkg.list, 1, .create.node.character, dict))) {
      if (isTRUE(all.equal(.check, .check.last))) {
        failed.index <- which(!.check.last)
        for(missing.index in failed.index) {
          required.pkgs <- .create.node.character(pkg.list[missing.index,], dict, TRUE)
          existed.but.not.fulfilled.pkgs <- required.pkgs[!required.pkgs %in% pkg.list[failed.index,"Package"]]
          if (length(existed.but.not.fulfilled.pkgs) > 0) {
            stop(sprintf("Requirements of %s are not matched. (Please check the following packages: %s)", pkg.list[missing.index,"Package"], paste(existed.but.not.fulfilled.pkgs, collapse = ",")))
          }
        }
        stop(sprintf("Requirements of %s are not matched.", paste(pkg.list[failed.index, "Package"], collapse = ",")))
      }
      .check.last <- .check
    }
  }
  .insert.node(pkg.list.base)
  .insert.node(pkg.list.recommended)
  .insert.node(pkg.list.target)
  .truncate(dict)
  .clean.base.pkg.from.deps(dict)
  dict
}

.pvmrize <- function(x, schedule = NULL) {
  x <- lapply(x, function(x) {
    x <- as.list(x)
    class(x) <- "pvm.package"
    x
  })
  class(x) <- "pvm"
  if (!is.null(schedule)) {
    for(i in seq_along(schedule)) {
      . <- which(names(x) %in% schedule[[i]])
      for(j in .) {
        attr(x[[j]], "order") <- i
      }
    }
  }
  x
}

.na2empty <- function(x) {
  x[is.na(x)] <- ""
  x
}

.join.dependency <- function(x, y) paste(x, y, sep = ",")

.create.node <- function(x, ...) {
  UseMethod(".create.node")
}

.create.node.environment <- function(x, name, version, priority, dict) {
  retval <- x
  if (!is.null(dict[[name]])) return(TRUE)
  retval$name <- name
  package_version(version)
  retval$version <- version
  retval$priority <- priority
  retval$parent <- list()
  retval$repository <- "CRAN"
  dict[[name]] <- retval
  TRUE
}

.create.node.character <- function(x, dict, error.out = FALSE) {
  if (!is.null(dict[[x["Package"]]])) return(TRUE)
  dep <- .na2empty(x[c("Depends", "Imports", "LinkingTo")])
  dep <- Reduce(.join.dependency, dep)
  deps <- .parse.dep(dep)
  if (length(deps) > 0) {
    if (!all(sapply(deps, .check.dep, dict = dict))) {
      required.pkgs <- sapply(deps, "[[", "name")[!sapply(deps, .check.dep, dict = dict)]
      # if (error.out) stop(sprintf("The package %s requires following missing packages: %s", x["Package"], paste(required.pkgs, collapse = ",")))
      if (error.out) return(required.pkgs)
      return(FALSE)
    }
    parent <- sapply(deps, "[[", "name")
  } else {
    parent <- character(0)
  }
  retval <- new.env(parent = emptyenv())
  retval$name <- as.vector(x["Package"])
  package_version(x["Version"])
  retval$version <- as.vector(x["Version"])
  retval$priority <- if (is.na(x["Priority"])) NA else as.vector(x["Priority"])
  retval$parent <- parent
  retval$deps <- deps
  if (!is.na(x["RemoteType"])) {
    retval$repository <- switch(x["RemoteType"], 
                          "cran" = x["Version"],
                          "github" = local({
                            if (is.na(x["RemoteSubdir"])) {
                              sprintf("github::repo=%s/%s@%s", x["RemoteUsername"], x["RemoteRepo"], x["RemoteSha"])
                            } else {
                              sprintf("github::repo=%s/%s/%s@%s", x["RemoteUsername"], x["RemoteRepo"], x["RemoteSubdir"], x["RemoteSha"])
                            }}),
                          "git" = local({
                            args <- list()
                            args[["url"]] <- x["RemoteUrl"]
                            if (!is.na(x["RemoteRef"])) args[["branch"]] <- x["RemoteRef"]
                            if (!is.na(x["RemoteSubdir"])) args[["subdir"]] <- x["RemoteSubdir"]
                            args <- paste(sapply(names(args), function(name) {
                              sprintf("%s=%s", name, args[[name]])
                            }), collapse = "&")
                            sprintf("git::%s", args)
                          }),
                          "bitbucket" = local({
                            if (is.na(x["RemoteSubdir"])) {
                              sprintf("bitbucket::repo=%s/%s@%s", x["RemoteUsername"], x["RemoteRepo"], x["RemoteSha"])
                            } else {
                              sprintf("bitbucket::repo=%s/%s/%s@%s", x["RemoteUsername"], x["RemoteRepo"], x["RemoteSubdir"], x["RemoteSha"])
                              }
                            }),
                          "svn" = local({
                            args <- list()
                            args[["url"]] <- x["RemoteUrl"]
                            if (!is.na(x["RemoteSubdir"])) args[["subdir"]] <- x["RemoteSubdir"]
                            if (!is.na(x["RemoteBranch"])) args[["revision"]] <- x["RemoteBranch"]
                            args <- paste(sapply(names(args), function(name) {
                              sprintf("%s=%s", name, args[[name]])
                            }), collapse = "&")
                            sprintf("svn::%s", args)
                          }),
                          "local" = stop("TODO:local"),
                          "url" = local({
                            args <- list(url = x["RemoteUrl"], subdir = x["RemoteSubdir"])
                            args <- Filter(function(x) !is.na(x), args)
                            args <- paste(sapply(names(args), function(name) {
                              sprintf("%s=%s", name, args[[name]])
                            }), collapse = "&")
                            sprintf("url::%s", args)
                          }),
                          "bioc" = stop("TODO:bioc"),
                          stop(sprintf("Unknown remote type: %s", x["RemoteType"]))
    )
  } else {
    retval$repository <- "CRAN"
  }
  # retval$repository <- "CRAN"
  dict[[retval$name]] <- retval
  TRUE
}

.parse.dep <- function(dep) {
  deps <- strsplit(dep, split = ",", fixed = TRUE)[[1]]
  deps <- deps[nzchar(deps)]
  deps <- sub("[[:space:]]+$", "", deps)
  deps <- unique(sub("^[[:space:]]*(.*)", "\\1", deps))
  lapply(deps, .split_op_version)
}

.check.dep <- function(dep, dict) {
  stopifnot(names(dep) %in% c("name", "op", "version"))
  if (! dep$name %in% ls(dict)) {
    return(FALSE)
  }
  if (!is.null(dep$version)) {
    baseenv()[[dep$op]](package_version(dict[[dep$name]]$version), package_version(dep$version))
  } else TRUE
}

.startsWith <- function(x, prefix) {
  if (nchar(x) < nchar(prefix)) return(FALSE)
  substring(x, 1, nchar(prefix)) == prefix
}

.split_op_version <- function (x) {
  pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
  x1 <- sub(pat, "\\1", x)
  x2 <- sub(pat, "\\2", x)
  if (x2 != x1) {
    pat <- "[[:space:]]*([[<>=!]+)[[:space:]]+(.*)"
    version <- sub(pat, "\\2", x2)
    if (!.startsWith(version, "r")) {
      version <- version
    }
    list(name = x1, op = sub(pat, "\\1", x2), version = version)
  }
  else list(name = x1)
}

.truncate <- function(dict) {
  for(name in ls(dict)) {
    for(property in ls(dict[[name]])) {
      if (length(dict[[name]][[property]]) == 0) rm(list = property, envir = dict[[name]])
    }
  }
}

.clean.base.pkg.from.deps <- function(dict) {
  base.pkgs <- c("R", get.base.pkg.list())
  rm(list = base.pkgs[base.pkgs %in% ls(dict)], envir = dict)
  for(name in ls(dict)) {
    if (!is.null(dict[[name]]$deps)) {
      deps <- dict[[name]]$deps
      for(i in rev(seq_along(deps))) {
        if (! deps[[i]]$name %in% ls(dict)) {
          deps[[i]] <- NULL
        }
      }
      dict[[name]]$deps <- deps
    }
    if (!is.null(dict[[name]]$parent)) {
      parent <- dict[[name]]$parent
      parent <- parent[parent %in% ls(dict)]
      dict[[name]]$parent <-parent
    }
  }
}
