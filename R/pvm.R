#'@export
print.pvm.package <- function(x, ...) {
  mark <- switch(as.character(x$priority), "base" = "*", "recommended" = "+", `NA` = "")
  cat(sprintf("%s (%s)%s repository: %s\n", x$name, x$version, mark, x$repository))
}

#'@export
print.pvm <- function(x, ...) {
  lapply(x, print)
}

#'@export
sort.pvm <- function(x, decreasing, pre.installed = character(0), ...) {
  target <- setdiff(names(x), pre.installed)
  schedule <- list()
  installed <- c(pre.installed, unlist(schedule))
  while(!all(target %in% installed)) {
    pkg.turn <- c()
    for(name in target) {
      if (name %in% installed) next
      if (all(x[[name]]$parent %in% installed)) {
        pkg.turn <- append(pkg.turn, name)
      }
    }
    schedule <- append(schedule, list(pkg.turn))
    installed <- c(pre.installed, unlist(schedule))
  }
  schedule
}

#'@export
plot.pvm <- function(x, ...) {

}
