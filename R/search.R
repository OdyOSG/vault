# Dependent functions ----------------

listRepos <- function(vault) {
  res <- gh_getContents(vault) %>%
    purrr::map_chr(~.x$name)

  return(res)
}

findMetaItem <- function(txt, item) {
  metaItem <- paste0("-  ", item)
  ii <- txt[grepl(metaItem, txt)]
  value <- stringr::str_remove(ii, '^.*: ')
  return(value)
}

listMeta <- function(vault, dir) {
  dd <- gh_getDirReadMe(vault, dir)$download_url
  txt <- downloadVault(dd)
  res <- purrr::map(metaItems(), ~findMetaItem(txt, item =.x)) %>%
    purrr::set_names(nm = metaItems()) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      org = vault$org,
      repo = vault$repo,
      .before = 1
    )

  return(res)

}

#' List contents of vault
#' @param vault a vault object
#' @export
listContents <- function(vault) {
  repos <- listRepos(vault)
  purrr::map_dfr(repos, ~listMeta(vault, dir = .x))
}

