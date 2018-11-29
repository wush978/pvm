library(pvm)
print(R.version.string)
R.date <- local({
  m <- regexec("\\((.*)\\)$", R.version.string)
  . <- regmatches(R.version.string, m)
  . <- Filter(., f = function(.) length(.) == 2)
  .[[1]][2]
})
R.date <- as.Date(R.date)
repos <- c(CRAN = sprintf("https://cran.microsoft.com/snapshot/%s", R.date + 7))
options(repos = repos)
install.packages("Rcpp")
install.packages.via.graph("xml2")
packageVersion("xml2")
