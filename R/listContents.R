listContents <- function(vv) {
  if (is.null(vv$repo)) {
    res <- gh_getOrgRepos(vv) %>%
      purrr::map_chr(~.x$name)
  }

  if (!is.null(vv$repo) & is.null(vv$dir)) {
    res <- gh_getContents(vv) %>%
      purrr::map_chr(~.x$name)
  }

  if (!is.null(vv$repo) & !is.null(vv$dir)) {
    res <- gh_getFolder(vv) %>%
      purrr::map_chr(~.x$name)
  }


  return(res)
}



