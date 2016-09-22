##########################################
# Almost Entire NeuroC pipeline
##########################################
rm( list = ls())
library(git2r)
library(devtools)
source("worker_functions.R")


##########################################
# Clone stuff
##########################################
root_path = "/dcl01/smart/data/structural/neuroc"
base_path = file.path(root_path, "packages")
tab_path = file.path(root_path, "tables")
pop_path = file.path(root_path, "population")

tabs = list.files(path = tab_path,
  full.names = TRUE)

tab = tabs[1]
N = length(tabs)
repo_infos = vector(mode = "list",
  length = N)
for (itab in seq(N)){
    tab = tabs[itab]
    print(tab)
    load(tab)
    repo_infos[[itab]] = repo_info
}
neuro_pkgs = sapply(repo_infos, `[[`, 
  "package")
names(repo_infos) = neuro_pkgs

outfile = file.path(pop_path,
    "all_repos.Rda")
save(repo_infos,
    file = outfile 
    )