##########################################
# Almost Entire NeuroC pipeline
##########################################
rm( list = ls())
library(git2r)
library(devtools)
library(travisci)
library(gh)
source("worker_functions.R")

##########################################
# Clone stuff
##########################################
root_path = "/dcl01/smart/data/structural/neuroc"
base_path = file.path(root_path, "packages")
tab_path = file.path(root_path, "tables")
pop_path = file.path(root_path, "population")

outfile = file.path(pop_path,
    "install_order.Rda")

load(outfile)
pkg = install_order[1]

outfile = file.path(pop_path,
    "commit_table.Rda")
if (!file.exists(outfile)) {
  outfile = file.path(pop_path,
      "commit_table_original.Rda")
}
load(outfile)



tab = file.path(tab_path, 
  paste0(pkg, ".rda"))
load(tab)

has_gh_repo = pkg %in% get_repo_names()
if (!has_gh_repo) {
  create_repo(pkg)
}


local_path = repo_info$local_path
repo = repo_info$repo
dep_pack = repo_info$dependencies


if (!"neuroc" %in% remotes(repo)) {
  gh_url = "https://github.com/"
  # gh_url = "git@github.com:"
  neuroc_url = paste0(gh_url, 
    "neuroconductor", "/", 
    pkg, ".git")
  remote_add(repo = repo, 
    "neuroc", 
    neuroc_url)
}

###################################################
# See if ANY packages are neuroc ones
# If not remote exists, add it
# If one exists, change user to neuroconductor and commit to that
# from the table
################################################
neuro_deps = neuroc_table[ 
  neuroc_table$package %in% dep_pack, , drop = FALSE
  ]


# read in description file
dcf = file.path(local_path, "DESCRIPTION")
orig_dcf = tempfile()
file.copy(dcf, orig_dcf)
rres = read_dcf(dcf)
fields = rres$fields
res = rres$dcf
nres = names(res)
##############################
# Adding in biocViews just in case
# may want to move this to 
# the if statement
##############################
if (!("biocViews" %in% nres)) {
  res$biocViews = ""
}

if (nrow(neuro_deps) > 0) {

  remotes = get_remotes(res)
  remotes = split_remotes(remotes)
  if (length(remotes) == 0) {
    remotes = ""
  }  
  # no remotes
  if (length(remotes) == 1 &
      all(remotes == "")) {
    fixed = paste0("neuroconductor/",
                     neuro_deps$package,
                     "@", neuro_deps$commit
    )
  } else {
    parsed = lapply(remotes, parse_one_remote)
    pack_with_remote = sapply(parsed, function(x) {
      x$repo
    })
    names(remotes) = names(parsed) = pack_with_remote
    keep_these = setdiff(pack_with_remote, neuro_deps$package)
    remotes = remotes[keep_these]
    
    # need_to_add = setdiff(neuro_deps$package, pack_with_remote)
    # if (length(need_to_add) > 0) {
      adders = paste0("neuroconductor/",
                      neuro_deps$package,
                     "@", neuro_deps$commit)      
      names(adders) = neuro_deps$package
      remotes = c(remotes, adders)
    # }
    parsed = lapply(remotes, parse_one_remote)
    
    remote_repos = lapply(remotes, devtools:::parse_git_repo)
    
    fixed = sapply(parsed, function(x) {
      xx = paste(x$username, x$repo, x$subdir, sep = "/")
      xx = gsub("/$", "", xx)
      xx = gsub("//", "/", xx)
      xx = paste0(xx, "@", x$ref)
      xx = gsub("/$", "", xx)
      return(xx)
    })
  }
  fixed_remotes = paste(fixed, collapse = ", ")
  res$Remotes = fixed_remotes
} 
res = as.data.frame(res, stringsAsFactors = FALSE)

write.dcf(x = res, file = dcf, keep.white = fields)

add_travis = !have_travis(local_path)
if (add_travis) {
  use_travis(pkg = local_path)
}
# save(repo, dcf)
##############################
# R COMMAND CHECK
##############################

add(repo, path = dcf)
if (add_travis) {
  add(repo, 
  path = file.path(local_path, 
    ".travis.yml"))
}
stat = status(repo)
staged = stat$staged$modified
if (length(staged) > 0) { 
  cid = commit(repo, 
    message = "neuroc_ready")
}
cred = cred_token()
push_id = push(repo, name = "neuroc", 
  refspec = "refs/heads/master",
  credentials = cred)
sync_users()  
enable_all_travis()



### add biocViews

# 
# check_stat = devtools::check(pkg = local_path)
# errors = check_stat$errors
# warns = check_stat$warnings
# # checker = errors
# checker = c(errors, warns)
# if (length(checker) > 0) {
#   file.copy(from = orig_dcf, to = dcf, overwrite = TRUE)
# }
