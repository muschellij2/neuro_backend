library(httr)
read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  dcf = as.list(read.dcf(path, keep.white = fields, all = TRUE)[1, ])
  return(list(fields = fields,
              dcf = dcf))
}

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

get_pkg_name = function(stub){
  remote = devtools:::github_remote(stub)
  pkg = devtools:::remote_package_name(remote)  
  return(pkg)
}

get_remotes = function(x){
  remotes = x$Remotes[[1]]
  remotes = trimws(remotes)
  if (is.null(remotes)) {
    remotes = ""
  }
  return(remotes)
}

split_remotes <- function(x) {
  trimws(unlist(strsplit(x, ",[[:space:]]*")))
}



parse_one_remote <- function(x) {
  pieces <- strsplit(x, "::", fixed = TRUE)[[1]]
  
  if (length(pieces) == 1) {
    type <- "github"
    repo <- pieces
  } else if (length(pieces) == 2) {
    type <- pieces[1]
    repo <- pieces[2]
  } else {
    stop("Malformed remote specification '", x, "'", call. = FALSE)
  }
  fun <- tryCatch(get(paste0(tolower(type), "_remote"),
                      envir = asNamespace("devtools"), mode = "function", inherits = FALSE),
                  error = function(e) stop("Unknown remote type: ", type, call. = FALSE))
  
  fun(repo)
}


repo_puller = function(base_path, stub) {
  url = repo_url_maker(stub)
  # pkg = get_pkg_name(stub)
  sha = get_sha(stub)
  info = get_pkg_info(stub)
  pkg = info$Package
  ver = info$Version
  local_path = file.path(base_path, pkg)
  
  if (!dir.exists(local_path)) {
    repo = clone(url, local_path = local_path)
  } else {
    repo <- init(local_path)
    # upstream = 
    res = pull(repo)
    conf = res@conflicts
    if (length(conf) > 0) {
      if (conf) {
        cmd = paste0(paste0("cd ", local_path, "; "),
                     "git checkout --theirs DESCRIPTION")
        system(cmd)
      }
    }
    res = pull(repo)
    conf = res@conflicts
    if (length(conf) > 0) {
      if (conf) {
        stop("Conflicts are not merged correctly")
      }
    }
  }
  return(list(repo = repo,
              local_path = local_path,
              version = ver,
              package = pkg,
              sha = sha)
  )
}


repo_url_maker = function(stub){
  gh_url = base_url = "http://github.com/"
  url = paste0(base_url, stub)
  return(url) 
}

get_all_deps = function(local_path) {
  pack = as.package(local_path)
  dep_grab = c("Depends", "Imports", 
    "LinkingTo", "Suggests")
  dep_grab = tolower(dep_grab)
  dependencies = pack[dep_grab]
  parsed <- lapply(dependencies, 
      parse_deps)
  deps <- unlist(lapply(parsed, `[[`, "name"))
  return(deps)
}

create_repo = function(pkg) {
  new_repo <- gh("POST /user/repos", 
    name = pkg)  
  return(new_repo)
}

delete_repo = function(pkg) {  
  del = gh("DELETE /repos/:owner/:repo", 
    owner = "neuroconductor",
   repo = pkg)
  return(del)
}

get_all_repos = function() {
  my_repos <- gh("GET /users/:username/repos", 
    username = "neuroconductor")
}

get_repo_names = function() {
  repos = get_all_repos()
  sapply(repos, `[[`, "name")
}


enable_all_travis = function(){
  h <- get_hooks()
  pkgs = sapply(h$hooks, `[[`, "name")
  active = sapply(h$hooks, `[[`, "active")
  active = sapply(active, function(x){
    if (is.null(x)){
      return(FALSE)
    } 
    return(x)
  })
  if (all(active)) {
    return(TRUE)
  } else {
    non_active = pkgs[!active]
    ids = h$hooks[!active]
    ids = sapply(ids, `[[`, "id")
    res = sapply(ids, my_enable_hook)
    return(res)
  }
}

my_enable_hook = function(id){
  token = Sys.getenv("TRAVIS_CI_TOKEN")
  base = "https://api.travis-ci.org"
  path = "/hooks"
  body = list(hook = list(
    id = id, 
    active = TRUE))
  url <- paste0(base, path)
  
  htoken = add_headers(
    Authorization = paste0("token ", token))

  req <- httr::PUT(url, 
    body = body, 
    encode = "json", 
    htoken)
  stop_for_status(req)
  content(req)$result 
}



get_sha = function(stub, 
  url = "https://github.com",
  ...){
  remote = devtools:::github_remote(stub)

    if (!is.null(remote$ref) && 
      !is.null(remote$sha) && 
      grepl(paste0("^", 
        remote$ref), remote$sha)) {
        return(remote$sha)
    }
    tryCatch({
        res <- git2r::remote_ls(
          paste0(url, "/", remote$username, 
            "/", remote$repo, ".git"), ...)
        found <- grep(pattern = 
          paste0("/", remote$ref), 
          x = names(res))
        if (length(found) == 0) {
            return(NA_character_)
        }
        unname(res[found[1]])
    }, error = function(e) NA_character_)  
}
have_travis = function(local_path){
  f = file.path(local_path, ".travis.yml")
  file.exists(f)
}