#' Clone github classroom repositories
#' @inheritParams authenticate_github
#' @inheritParams find_classroom_repositories
#' @inheritParams clone_repositories
#' @return repositories data frame, see \link{find_classroom_repositories} for details
#' @export
clone_classroom_repositories <- function(token, org, repo_prefix, folder = ".", pull = TRUE) {
  
  # security checks
  if (missing(token)) stop("token required", call. = FALSE)
  if (missing(org)) stop("organization required", call. = FALSE)
  if (missing(repo_prefix)) stop("repository prefix required", call. = FALSE)
  
  # fetch repos info
  repos <- authenticate_github(token) %>% 
    find_classroom_repositories(org = org, repo_prefix = repo_prefix)
  
  # clone repos
  clone_repositories(repos, folder = folder, token = token, pull = pull)
  # return repos data frame
  return(repos)
}

# helper functions =====

#' Authenticate GitHub account
#' @param token authentication token for github API (https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/), requires scope "repo"
#' @return GraphqlClient object
#' @export
authenticate_github <- function(token) {
  GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = add_headers(Authorization = paste0("Bearer ", token))
  )
}


#' Find GitHub Classroom repositories
#' @param gqlc authenticated graphql client
#' @param org organization name (i.e. organization github user)
#' @param repo_prefix repository prefix (i.e. prefix set of classroom repositories)
#' @param max maximum number of repositories to retrieve
#' @return data frame with repository names, urls, last commits, last pull requests
#' @export
find_classroom_repositories <- function (gqlc, org, repo_prefix, max = 100, quiet = FALSE) {
  qry <- Query$new()
  qry$query("classroom_repos",
    str_interp(
"query{
  search(first: $[d]{max_n}, query: \"user:${user} ${prefix}\", type: REPOSITORY){
      repositoryCount
    	nodes{
        ... on Repository {
          name
          url
          createdAt
          pushedAt
          isPrivate
          pullRequests (last: 1) {
            nodes{
              publishedAt
              author {
                login
              }
            }
          }
        }
      }
  }
}", list(max_n = max, user = org, prefix = repo_prefix)))
  
  # query output
  results <- gqlc$exec(qry$queries$classroom_repos)
  if (is.null(results$data)) {
    message("There were errors:")
    print(results$errors)
    stop("cannot process further", call. = FALSE)
  }
  
  # pull requests
  prs <- results$data$search$nodes$pullRequests$nodes
  
  # rest of data
  df <- 
    select(results$data$search$nodes, -pullRequests) %>% 
    as_data_frame() %>% 
    # add last pull request to data frame
    mutate(
      last_pr_login = prs %>% sapply(function(i) i$author$login %>% { if(is.null(.)) NA else . }) ,
      last_pr_created = prs %>% sapply(function(i) i$publishedAt %>% { if(is.null(.)) NA else . })
    ) %>% 
    # format date columns
    mutate_at(c("createdAt", "pushedAt", "last_pr_created"), as.POSIXct) %>% 
    # names
    rename(repository = name,
           created = createdAt,
           last_pushed = pushedAt,
           private = isPrivate) %>% 
    # make sure only repositories that start with the prefix
    filter(grepl(str_c("^", repo_prefix), repository))
  
  # info 
  if (!quiet) {
    message(sprintf("Found %d respositories with the prefix '%s' for the organization %s.",
                    nrow(df), repo_prefix, org))
  }
  
  # return data frame
  return(df)
}

#' Clone repositories
#' @inheritParams clone_repository
#' @param repos data frame with, at minimum, columns 'repository' (name) and 'url'
#' @param folder the target directory where to clone all the repositories to
#' @export 
clone_repositories <- function(repos, folder = ".", token = NULL, pull = TRUE) {
  mapply(clone_repository, repos$url, file.path(folder, repos$repository), token = token, pull = pull)
  invisible(repos)
}

#' Clone a repository to the target directory
#' @param url the https://... repository 
#' @param path the directory to clone to, if not provided, clones into the current working directory with the repository name
#' @param token the authentication token (only required for private repositories)
#' @param pull if the repository already exists, whether to update it from the remote
#' @export
clone_repository <- function(url, path = NULL, token = NULL, pull = TRUE) {
  
  # token
  if (!is.null(token)) url <- str_c("https://", token, "@", str_replace(url, "^https://", "")) 
  if (is.null(path)) path <- basename(url)
  
  # parent directory
  if (!dir.exists(dirname(path)))  {
    message("Creating parent directory: ", dirname(path))
    dir.create(dirname(path), recursive = TRUE)
  } 
  
  # github call (always use pull instead of clone to avoid writing tokens to disk)
  github_call <- "cd \"${path}\" &&${ if(new) ' git init &&' else '' } git pull \"${url}\""
  
  if (!dir.exists(path)) {
    message("Cloning repository for the first time: ", path)
    dir.create(path)
    new <- TRUE
  } else if (dir.exists(path) && pull) {
    message("Repository already exists -> pulling changes from remote: ", path)
    new <- FALSE
  } else {
    message("Repository already exists but NOT pulling changes from remote -> no action: ", path)
    return(FALSE)
  } 
  
  # run cloning
  github_call <- str_interp(github_call, list(path = path, new = new, url = url))
  invisible(system(github_call))
}
