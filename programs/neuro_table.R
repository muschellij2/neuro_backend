##########################################
# Almost Entire NeuroC pipeline
##########################################
rm( list = ls())
library(git2r)
library(igraph)
library(devtools)
source("worker_functions.R")

##########################################
# Clone stuff
##########################################
root_path = "/dcl01/smart/data/structural/neuroc"
base_path = file.path(root_path, "packages")
tab_path = file.path(root_path, "tables")
pop_path = file.path(root_path, "population")

outfile = file.path(pop_path,
    "all_repos.Rda")
load(outfile)
neuro_deps = names(repo_infos)

neuroc_table = data.frame(
  package = neuro_deps,
  commit = "",
  stringsAsFactors = FALSE)

outfile = file.path(pop_path,
    "commit_table_original.Rda")
save(neuroc_table, 
    file = outfile)

neuro_deps = names(repo_infos)

n = length(neuro_deps)

dep_mat = sapply(repo_infos, function(x){
        neuro_deps %in% x$dependencies
    })
rownames(dep_mat) = neuro_deps
# dep_mat = t(dep_mat)

install_order = list()
i = 1
while(length(neuro_deps) > 0) {
    graph = graph_from_adjacency_matrix(
        dep_mat,
        mode = "directed")

    outs = degree(graph, mode = "in")
    installer = names(outs)[outs == 0]
    install_order = c(install_order, 
        list(installer))
    # no_dep = names(deg)[deg == 0]

    keep = !(neuro_deps %in% installer)
    dep_mat = dep_mat[keep, keep, drop = FALSE]
    neuro_deps = neuro_deps[keep]
    i = i + 1
    if (i > 200) {
        stop("something is wrong")
    }
}

install_order = unlist(install_order)
outfile = file.path(pop_path,
    "install_order.Rda")
save(install_order,
    file = outfile)
