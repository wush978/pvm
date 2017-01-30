#'Export package list with version
#'
#'Write the package and its version to a JSON file
#'
#'@param file An argument which will be passed to the argument \code{file} of \code{\link[base]{write}}.
#'@param ... Further arguments passed to \code{\link[utils]{installed.packages}}.
#'@details
#'TODO
#'@export
export.packages <- function(file = "rpvm.yml", ...) {
  pkg.list.raw <- installed.packages(...)
  pkg.list.priority <- pkg.list.raw[,"Priority"]
  pkg.list.base <- pkg.list.raw[which(pkg.list.priority == "base"),]
  pkg.list.recommended <- pkg.list.raw[which(pkg.list.priority == "recommended"),]
  pkg.list.target <- pkg.list.raw[which(is.na(pkg.list.priority)),]
  # Constructing package graph
  dict <- new.env()
  # init nodes for "base" and "R"
  .create.node(new.env(parent = emptyenv()), "R", sprintf("%s.%s", R.version$major, R.version$minor), "base", dict)
  .insert.node <- function(pkg.list) {
    .check.last <- NULL
    while(!all(.check <- apply(pkg.list, 1, .create.node.character, dict))) {
      if (isTRUE(all.equal(.check, .check.last))) {
        stop(sprintf("Requirements of %s are not matched", paste(names(which(!.check.last)), collapse = ",")))
      }
      .check.last <- .check
    }
    invisible(NULL)
  }
  .insert.node(pkg.list.base)
  .insert.node(pkg.list.recommended)
  .insert.node(pkg.list.target)
  .truncate(dict)
  dict.out <- lapply(dict, as.list)
  yaml <- yaml::as.yaml(dict.out)
  if (is.character(file)) {
    if (file != "" & (substring(file, 1L, 1L) != "|")) {
      # target is a file
      tmp.path <- tempfile(fileext = ".yml")
      write(yaml, file = tmp.path)
      dict.out2 <- yaml::yaml.load_file(tmp.path)
      stopifnot(isTRUE(all.equal(dict.out2, dict.out)))
      file.rename(tmp.path, file)
      return(invisible(NULL))
    }
  }
  write(json, file = file)
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
  dict[[retval$name]] <- retval
  TRUE
}

# .split_dependencies <- function(x) {
#     .split2 <- function(x) {
#         x <- sub("[[:space:]]+$", "", x)
#         x <- unique(sub("^[[:space:]]*(.*)", "\\1", x))
#         names(x) <- sub("^([[:alnum:].]+).*$", "\\1", x)
#         x <- x[names(x) != "R"]
#         x <- x[nzchar(x)]
#         x <- x[!duplicated(names(x))]
#         lapply(x, tools:::.split_op_version)
#     }
#     if (!any(nzchar(x)))
#         return(list())
#     unlist(lapply(strsplit(x, ","), .split2), FALSE, FALSE)
# }
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
    if (!startsWith(version, "r"))
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

