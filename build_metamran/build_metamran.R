.libPaths(".lib")
library(git2r, ".lib")
library(yaml, ".lib")
if (!file.exists(repo.path <- "gh-pages")) {
  dir.create(repo.path, showWarnings = FALSE)
  git2r::clone(url = "https://github.com/wush978/metamran", branch = "gh-pages", local_path = repos.path)
}
repo <- git2r::repository(repo.path)
git2r::pull(repo = repo)
infos <- dir(repo.path, pattern = "info.yml", full.names = TRUE, recursive = TRUE)
metamran <- new.env(parent = emptyenv())
pb <- txtProgressBar(max = length(infos), style = 3)
for(info in infos) {
  version <- basename(dirname(info))
  package <- basename(dirname(dirname(info)))
  obj <- yaml::yaml.load_file(info)
  key <- sprintf("%s_%s", package, version)
  assign(key, obj, envir = metamran)
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
}
close(pb)
save(metamran, file = "../data/metamran.rda", compress = "bzip2")
