filesToIgnore <- function() {
  #TODO update to match all ignorable files
  c(".gitattributes", ".gitignore", "README.md")
}

`%notin%` <- Negate("%in%")

#' Look up contents of vault
#' @param repo a string set up as <org>/<repo>
#' @export
vaultContents <- function(repo) {
  vv <- vault(repo)

  gh_contents <- gh::gh("GET /repos/{owner}/{repo}/contents",
                     owner = vv$owner,
                     repo = vv$repo)

  nm <- purrr::map_chr(gh_contents, ~.x$name)
  url <- purrr::map_chr(gh_contents, ~.x$url)

  contents <- tibble::tibble(
    name = nm,
    url = url
  ) %>%
    dplyr::filter(name %notin% filesToIgnore())

  return(contents)

}
