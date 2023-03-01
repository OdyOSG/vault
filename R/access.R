#' Create a vault object to use
#' @param repo_string a string specified as <org>/<repo>
#' @param savePath a path to use for saving items from vault, defaults to project directory
#' @export
vault <- function(repo_string,
                  savePath = here::here()) {

  st <- stringr::str_split_1(repo_string, "/")
  structure(
    list(
      org = st[1],
      repo = st[2],
      savePath = savePath
    )
  )
}

#' Check if have access to vault
#' @param vault a vault object to check access
#' @export
checkAccess <- function(vault) {
  safe_gh <- purrr::safely(gh::gh)
  check <- safe_gh("GET /repos/{owner}/{repo}",
                   owner = vault$org,
                   repo = vault$repo)
  if (is.null(check$error)) {
    TRUE
  } else {
    FALSE
  }
}


