##########################################
# Graph of all Networks
##########################################
rm( list = ls())
library(git2r)
library(devtools)
library(gh) # from install_github("gaborcsardi/gh")
library(igraph)
library(networkD3)

read_dcf <- function(path, rewrite = TRUE) {
  file = file(path)
  on.exit({
    close(file)
  })
  fields <- colnames(read.dcf(file))
  dcf = as.list(read.dcf(file, keep.white = fields, all = TRUE)[1, ])
  return(list(fields = fields,
              dcf = dcf))
}

split_remotes <- function(x) {
  trimws(unlist(strsplit(x, ",[[:space:]]*")))
}

get_all_repos = function() {
  my_repos <- gh("GET /users/:username/repos", 
                 username = "neuroconductor")
}

get_repo_names = function() {
  repos = get_all_repos()
  sapply(repos, `[[`, "name")
}

repos = get_repo_names()
repos = paste0("neuroconductor/", repos)


remote_package_info = function(remote,
                               url = "https://github.com", ...) {
  
  tmp <- tempfile()
  path <- paste(c(remote$username, 
                  remote$repo, "raw", remote$ref, 
                  remote$subdir, "DESCRIPTION"), 
                collapse = "/")
  req <- httr::GET(url, path = path, 
                   httr::write_disk(path = tmp))
  if (httr::status_code(req) >= 400) {
    L = list(Package = NA,
             Version = NA)
    return(L)
  }
  x = read_dcf(tmp)$dcf
  return(x)  
}

get_pkg_info = function(stub){
  remote = devtools:::github_remote(stub)
  pkg = remote_package_info(remote)  
  return(pkg)
}



info = sapply(repos, get_pkg_info)

pkgs = sapply(info, function(x) {
  x$Package
})
dep_grab = c("Depends", "Imports", 
             "LinkingTo", "Suggests",
             "Enhances")
tdeps = lapply(info, function(xx) {
  run_pack = xx$Package
  print(run_pack)
  grab = names(xx) %in% dep_grab 
  if (any(grab)) {
    res = xx[grab]
    res = lapply(res, function(x) {
      if (length(x) > 0) {
      return(split_remotes(x))
      } else {
        return("")
      }
    })
    tab = mapply(function(pack, dep){
      cbind(type = pack, dep = dep)
    }, names(res), res, SIMPLIFY = FALSE)
    tab = do.call("rbind", tab)
    tab = data.frame(tab, stringsAsFactors = FALSE)
    tab$package = run_pack
    return(tab)
  } else {
    tab = data.frame(package = run_pack, type = "", dep = "")
  }
})
tdeps = do.call("rbind", tdeps)
rownames(tdeps) = NULL
tdeps$dep = sapply(strsplit(tdeps$dep, " "), `[`, 1)
tdeps = tdeps[ !tdeps$dep %in% c("R"), ]

edges = tdeps[, c("dep", "package", "type")]
vert = sort(unique(c(edges$dep, edges$package)))
edges = edges[ !is.na(edges$dep), ]
edges = edges[ !edges$type %in% "", ]

ret <- igraph::graph.data.frame(d = edges, 
                                directed = TRUE, 
                                vertices = vert)
class(ret) <- c("pkgDepGraph", "igraph")
# attr(ret, "pkgs") <- "Neuroconductor"
attr(ret, "pkgs") <- pkgs

# png("network_depgraph.png", 
#     height = 5, width = 20,
#     units = "in", res = 300)
pdfname = "network_depgraph.pdf"
pdf(pdfname, 
    height = 10, width = 20)
starting.par.settings <- par(no.readonly = TRUE)
par(
  oma = c(0, 0, 0, 0), 
  mar = rep(0, 4))
plot(ret, vertex.size = 10, cex = 0.7, main = "Neuroconductor")
mfg.settings <- par()$mfg
par(starting.par.settings)
par(mfg = mfg.settings, new = FALSE)
dev.off()
knitr::plot_crop(pdfname)



deps = sapply(info, function(xx) {
  print(xx$Package)
  res = xx[dep_grab]
  res = unlist(res)
  if (length(res) > 0) {
    res = split_remotes(res)
    res = sapply(strsplit(res, " "), `[`, 1)
    res = setdiff(res, "R")
    res = setdiff(res, "")
    res
  } else {
    ""
  }
})
names(deps) = pkgs




neuro_deps = lapply(deps, function(x) {
  x[ x %in% pkgs ]
})

dep_mat = sapply(neuro_deps, function(x){
  pkgs %in% x
})
rownames(dep_mat) = pkgs

ograph = graph_from_adjacency_matrix(
  dep_mat,
  mode = "directed")
png("network_graph.png", height = 7, width = 7,
    units = "in", res = 300)
plot(ograph, vertex.size = 20)
dev.off()



tab = mapply(function(pack, dep){
  cbind(pack, dep)
}, pkgs, deps)
tab = do.call("rbind", tab)
colnames(tab) = c("target", "source")
tab = data.frame(tab, stringsAsFactors = FALSE)
tab = tab[, c("source", "target")]


neuro_tab = tab[ tab$source %in% pkgs, ]
simpleNetwork(neuro_tab, linkDistance = 200, fontSize = 20, charge = -300) %>% 
  saveNetwork(file = 'neuro_graph.html', selfcontained = FALSE)
