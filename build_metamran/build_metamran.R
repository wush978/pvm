.libPaths(".lib")
import.packages(lib.loc = ".lib")
library(git2r, ".lib")
library(yaml, ".lib")
git2r::clone(url = "https://github.com/wush978/metamran", branch = "gh-pages", local_path = repo.path <- tempfile())
infos <- dir(repo.path, pattern = "info.yml", full.names = TRUE, recursive = TRUE)
metamran <- new.env(parent = emptyenv())
pb <- txtProgressBar(max = length(infos), style = 3)
for(info in infos) {
  version <- basename(dirname(info))
  package <- basename(dirname(dirname(info)))
  obj <- yaml::yaml.load_file(info)
  if (!exists(package, where = metamran)) {
    assign(package, value = new.env(parent = emptyenv()), envir = metamran)
  }
  package.env <- metamran[[package]]
  if (!exists(version, where = package.env)) {
    assign(version, value = new.env(parent = emptyenv()), envir = package.env)
  }
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
}
save(metamran, file = "../data/metamran.rda")
