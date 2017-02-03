#'Export package list with version
#'
#'Write the package and its version to a JSON file
#'
#'@param file \code{NULL}, or a character string naming the file to write.
#'If \code{NULL}, return a S3 object \code{pvm}.
#'@param ... Further arguments passed to \code{\link[utils]{installed.packages}}.
#'@details
#'TODO
#'@export
export.packages <- function(file = "pvm.yml", pvm = NULL, ...) {
  if (is.null(pvm)) {
    pkg.list.raw <- utils::installed.packages(...)
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
    pkg.list.base <- pkg.list.raw[which(pkg.list.priority == "base"),, drop = FALSE]
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
          stop(sprintf("Requirements of %s are not matched", paste(names(which(!.check.last)), collapse = ",")))
        }
        .check.last <- .check
      }
      invisible(pvm)
    }
    .insert.node(pkg.list.base)
    .insert.node(pkg.list.recommended)
    .insert.node(pkg.list.target)
    .truncate(dict)
    pvm <- .pvmrize(dict)
    schedule <- sort(pvm)
    pvm <- pvm[unlist(schedule)]
    pvm <- .pvmrize(pvm)
    if (is.null(file)) return(invisible(pvm))
  }
  stopifnot(class(pvm) == "pvm")
  yaml <- yaml::as.yaml(pvm)
  if (is.character(file)) {
    if (file != "" & (substring(file, 1L, 1L) != "|")) {
      # target is a file
      tmp.path <- tempfile(tmpdir = dirname(file), fileext = ".yml")
      tryCatch({
        write(yaml, file = tmp.path)
        pvm2 <- yaml::yaml.load_file(tmp.path)
        pvm2 <- .pvmrize(pvm2)
        stopifnot(isTRUE(all.equal(pvm2, pvm)))
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

.pvmrize <- function(x) {
  x <- lapply(x, function(x) {
    x <- as.list(x)
    class(x) <- "pvm.package"
    x
  })
  class(x) <- "pvm"
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
  if (!is.null(dict[[name]])) TRUE
  retval$name <- name
  package_version(version)
  retval$version <- version
  retval$priority <- priority
  retval$parent <- list()
  retval$repository <- "CRAN"
  dict[[name]] <- retval
  TRUE
}

.create.node.character <- function(x, dict) {
  if (!is.null(dict[[x["Package"]]])) TRUE
  dep <- .na2empty(x[c("Depends", "Imports", "LinkingTo")])
  dep <- Reduce(.join.dependency, dep)
  deps <- .parse.dep(dep)
  if (length(deps) > 0) {
    if (!all(sapply(deps, .check.dep, dict = dict))) {
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
  retval$repository <- "CRAN"
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

.split_op_version <- function (x) {
  pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
  x1 <- sub(pat, "\\1", x)
  x2 <- sub(pat, "\\2", x)
  if (x2 != x1) {
    pat <- "[[:space:]]*([[<>=!]+)[[:space:]]+(.*)"
    version <- sub(pat, "\\2", x2)
    if (!base::startsWith(version, "r"))
      version <- version
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

