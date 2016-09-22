##########################################
# Almost Entire NeuroC pipeline
##########################################
rm( list = ls())
library(git2r)
library(devtools)
source("worker_functions.R")
source("all_repos.R")

##########################################
# Clone stuff
##########################################
root_path = "/dcl01/smart/data/structural/neuroc"
base_path = file.path(root_path, "packages")
tab_path = file.path(root_path, "tables")

# stub = "jfortin1/RAVEL"
# url = paste0(base_url, stub)
stub = all_repos[1]
for (stub in all_repos){
    print(stub)
    repo_info = repo_puller(
        base_path = base_path, 
        stub = stub)

    pkg = repo_info$package
    ver = repo_info$version
    local_path = repo_info$local_path
    repo = repo_info$repo

    dep_pack = get_all_deps(local_path)
    repo_info$dependencies = dep_pack
    outfile = file.path(tab_path,
        paste0(pkg, ".rda"))
    save(repo_info,
        file = outfile)
}