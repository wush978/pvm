library(pvm)
.env <- new.env()
load(pvm:::.get.dst(), envir = .env)
.package.id <- .env$pkg$package_id[.env$pkg$package == "Rcpp"]
.df <- .env$metamran[.env$metamran$package_id == .package.id,]
.ver <- .env$ver$version[match(.df$version_id, .env$ver$version_id)]
.ver <- package_version(.ver)
.ver <- min(.ver)
.date <- pvm:::.metamran.find("Rcpp", as.character(.ver))
stopifnot(class(.date) == "Date")
.av <- available.packages(contrib.url(repos = pvm:::.mran.url(.date), .Platform$pkgType))
stopifnot(.av["Rcpp","Version"] == as.character(.ver))
