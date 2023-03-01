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

# Contents ------------------

#' List contents of vault
#' @param vault a vault object
#' @export
listContents <- function(vault) {
  repos <- listRepos(vault)
  purrr::map_dfr(repos, ~listMeta(vault, dir = .x))
}

# Preview --------------------
#' Preview file in Rstudio viewer
#' @param vault a vault object
#' @param item the item in the vault to preview
#' @export
preview <- function(vault, item) {
  dd <- gh_getDirReadMe(vault, item)$download_url
  txt <- downloadVault(dd)
  #replace first header with % to fit pandoc
  txt[1] <- gsub("#", "%", txt[1])
  #create a temp dir for readme file
  tempDir <- tempfile()
  dir.create(tempDir)
  tmpRmd <- file.path(tempDir, "README.Rmd")
  #write file to tmp
  readr::write_lines(txt, file = tmpRmd)
  tmpHtml <- rmarkdown::render(tmpRmd, params = "ask", quiet = TRUE)
  rstudioapi::viewer(tmpHtml)
  invisible(tmpHtml)
}
