# Dependent functions ----------------

listRepos <- function(vault) {
  res <- gh_getContents(vault) %>%
    purrr::map_chr(~.x$name)

  return(res)
}

findMetaItem <- function(txt, item) {
  metaItem <- paste0(item, ":")
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

# Contents ------------------

#' List details of vault
#' @param vault a vault object
#' @return a detailed tibble with information about the vault. This takes longer to load
#' @export
listDetails <- function(vault) {
  repos <- listRepos(vault)
  purrr::map_dfr(repos, ~listMeta(vault, dir = .x))
}

#' List details of vault
#' @param vault a vault object
#' @return a character string with the names of the directories in the vault
#' @export
listContents <- function(vault) {
  gh_getContents(vault) %>%
    purrr::map_chr(~.x$name)
}


getMdFoViewer <- function(type = c("news", "preview"), vault, item) {

  type <- checkmate::assertChoice(x = type, choices = c("news", "preview")) %>%
    switch (
      news = "NEWS.md",
      preview = "README.md"
    )
  path <- paste(item, type, sep = "/")
  txt <- gh_downloadFile(vault = vault, path = path)
  #replace first header with % to fit pandoc
  txt[1] <- gsub("#", "%", txt[1])
  #create a temp dir for md file
  tempDir <- tempfile()
  dir.create(tempDir)
  tmpRmd <- file.path(tempDir, type)
  #write file to tmp
  readr::write_lines(txt, file = tmpRmd)
  tmpHtml <- rmarkdown::render(tmpRmd, params = "ask", quiet = TRUE)
  rstudioapi::viewer(tmpHtml)
  invisible(tmpHtml)

}


# Preview --------------------
#' Preview file in Rstudio viewer
#' @param vault a vault object
#' @param item the item in the vault to preview
#' @return opens r studio viewer to preview vault item readme
#' @export
preview <- function(vault, item) {
  getMdFoViewer(type = "preview", vault = vault, item = item)
}

# News -----------------------
#' Get software news for the vault item in Rstudio viewer
#' @param vault a vault object
#' @param item the item in the vault to preview
#' @return opens r studio viewer to preview vault item news
#' @export
news <- function(vault, item) {
  getMdFoViewer(type = "news", vault = vault, item = item)
}
