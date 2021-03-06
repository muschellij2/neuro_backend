##########################################
# Almost Entire NeuroC pipeline
##########################################
rm( list = ls())
library(git2r)
library(devtools)
library(travisci)
library(gh)
library(dplyr)
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
pkg = install_order[5]

outfile = file.path(pop_path,
    "commit_table.Rda")
if (!file.exists(outfile)) {
  outfile = file.path(pop_path,
      "commit_table_original.Rda")
}
load(outfile)

####################################
# Load Repo
####################################
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

# run if have neuroconductor dependencies
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

res = as.data.frame(res, 
  stringsAsFactors = FALSE)

write.dcf(x = res, file = dcf, 
  keep.white = fields)

###################################
# Adding to TRAVIS yaml
###################################
add_travis = !have_travis(local_path)
if (add_travis) {
  # use_travis(pkg = local_path)
  make_travis(local_path)
}

############################
# Adding use_bioc and bioc_required
# to the YAML so it installs bioc packages
############################
yaml_file = file.path(local_path, 
    ".travis.yml")
fix_yaml(yaml_file)

# save(repo, dcf)
##############################
# R COMMAND CHECK
##############################
add(repo, path = dcf)
if (add_travis) {
  add(repo, path = yaml_file)
}
stat = status(repo)
staged = stat$staged$modified
if (length(staged) > 0) { 
  this_commit = commit(repo, 
    message = "neuroc_test")
  this_cid = this_commit@sha
}
current_commit_id = commits(repo)[[1]]@sha

cred = cred_token()
push_id = push(repo, name = "neuroc", 
  refspec = "refs/heads/master",
  credentials = cred)
# get neuroc commit_id from GH
gh_curr_id = get_current_commit(pkg)

if (!is_enabled_travis(pkg)){
  sync_users()
  Sys.sleep(10)
  enable_all_travis()
  travis_enable_status()
  # re-trigger
  push_id = push(repo, name = "neuroc", 
    refspec = "refs/heads/master",
    credentials = cred)
}
  ######################################
  # Getting build information
  ######################################
  res = get_builds(pkg)
  cids = lapply(res$commits, `[[`, "sha")
  starts = lapply(res$builds, `[[`, 
    "started_at")
  nums = lapply(res$builds, `[[`, 
    "number")  
  states = lapply(res$builds, `[[`, 
    "state")
  df = mapply(function(x, y, z) {
    nonull = function(x) {
      if (is.null(x)) {
        x = NA
      }
      x
    }
    data.frame(num = nonull(x),
      cid = nonull(y),
      state = nonull(z)
      )
  }, nums, cids, states,
    SIMPLIFY = FALSE)
  df = do.call("rbind", df)
  df$num = as.numeric(df$num)
  df = df %>% filter(cid %in% this_cid)
  # df = df %>% arrange(# FIND lAST ONE)
  # res$builds[[2]]$state
  # df = df[ df$cid %in% cid,]

# }
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
