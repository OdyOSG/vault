filesToIgnore <- function() {
  #TODO update to match all ignorable files
  c(".gitattributes", ".gitignore", "README.md")
}

`%notin%` <- Negate("%in%")

#' Look up contents of vault
#' @param owner name of organization
#' @param repo name of repository
#' @export
vaultContents <- function(owner, repo) {

  gh_contents <- gh::gh("GET /repos/{owner}/{repo}/contents",
                        owner = owner,
                        repo = repo) %>%
    purrr::discard(~.x$name %in% filesToIgnore())

  tb <- purrr::map_chr(gh_contents, ~.x$name) %>%
    purrr::map_dfr(~getReadmeMeta(owner = owner,
                                  repo = repo,
                                  dir = .x))
  return(tb)

}


getReadmeMeta <- function(owner, repo, dir) {

  dd <- gh::gh("GET /repos/{owner}/{repo}/readme/{dir}",
               owner = owner,
               repo = repo,
               dir = dir)

  tmp <- tempfile()
  download.file(url = dd$download_url,
                destfile = tmp,
                quiet = TRUE)
  txt <- readr::read_lines(tmp)

  readmeTibble(txt)

}


readmeTibble <- function(txt) {
  # find where meta starts and stops
  start <- which(txt == "## Meta") + 2
  stop <- which(txt == "## Dependencies") - 2
  #extract lines with meta info
  dd <- txt[start:stop]

  #extract values of meta
  value <- stringr::str_remove(dd, '^.*: ')
  # get variable names
  names(value) <- sub(":.*", "", dd) %>%
    snakecase::to_snake_case()
  #turn into tibble
  tb <- value %>% tibble::as_tibble_row()
  return(tb)
}

previewVault <- function(repo, directory) {
  vv <- vault(repo)
}
