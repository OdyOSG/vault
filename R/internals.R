# Presets ------------------------

filesToIgnore <- function() {
  #TODO update to match all ignorable files
  c(".gitattributes", ".gitignore", "README.md", "LICENSE")
}



metaItems <- function() {
  #TODO update to match all READMES
  #TODO consider adding a tag
  c("name", "type", "version", "maintainer", "description")
}

# GH Functions -------------------------

gh_getContents <- function(vault) {
  checkmate::assertCharacter(vault$org)
  checkmate::assertCharacter(vault$repo)
  check <- checkAccess(vault)
  if (check) {
    gh::gh("GET /repos/{owner}/{repo}/contents",
           owner = vault$org,
           repo = vault$repo) %>%
      purrr::discard(~.x$name %in% filesToIgnore())
  } else{
    stop("User does not have access to this vault")
  }


}

gh_getDirReadMe <- function(vault, dir) {
  checkmate::assertCharacter(vault$org)
  checkmate::assertCharacter(vault$repo)
  checkmate::assertCharacter(dir)
  check <- checkAccess(vault)
  if (check) {
    gh::gh("GET /repos/{owner}/{repo}/readme/{dir}",
           owner = vault$org,
           repo = vault$repo,
           dir = dir)
  } else{
    stop("User does not have access to this vault")
  }
}

gh_getFiles <- function(vault, item) {
  check <- checkAccess(vault)
  if (check) {
    gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
           owner = vault$org,
           repo = vault$repo,
           path = item) %>%
      purrr::discard(~.x$name %in% filesToIgnore())
  } else{
    stop("User does not have access to this vault")
  }
}

gh_downloadFile <- function(vault, path) {
  check <- checkAccess(vault)
  if (check) {
    gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
           owner = vault$org,
           repo = vault$repo,
           path = path)$download_url %>%
      downloadVault()
  } else{
    stop("User does not have access to this vault")
  }
}

# Checkers ---------------------------

howManyFiles <- function(vault, item) {
  rr <- gh_getFiles(vault, item)
  length(rr)
}

# Download ---------------
downloadVault <- function(downloadUrl) {
  tmp <- tempfile()
  utils::download.file(url = downloadUrl,
                destfile = tmp,
                quiet = TRUE)
  txt <- readr::read_lines(tmp)
  return(txt)
}
