
vault <- function(org, repo = NULL, dir = NULL, file = NULL) {
  structure(
    list(
      org = org,
      repo = repo,
      dir = dir,
      file = file
    ),
    class = "vaultMeta"
  )
}

addRepo <- function(vault, repo){
  vault$repo <- repo
  return(vault)
}

addDir <- function(vault, dir) {
  vault$dir <- dir
  return(vault)
}

addFile <- function(vault, file) {
  vault$file <- file
  return(vault)
}

gh_getRepo <- function(vault) {
  checkmate::assertCharacter(vault$org)
  checkmate::assertCharacter(vault$repo)
  gh::gh("GET /repos/{owner}/{repo}",
         owner = vault$org,
         repo = vault$repo)
}

filesToIgnore <- function() {
  #TODO update to match all ignorable files
  c(".gitattributes", ".gitignore", "README.md")
}

gh_getContents <- function(vault) {
  checkmate::assertCharacter(vault$org)
  checkmate::assertCharacter(vault$repo)
  gh::gh("GET /repos/{owner}/{repo}/contents",
         owner = vault$org,
         repo = vault$repo) %>%
    purrr::discard(~.x$name %in% filesToIgnore())
}

gh_getDirReadMe <- function(vault) {
  checkmate::assertCharacter(vault$org)
  checkmate::assertCharacter(vault$repo)
  checkmate::assertCharacter(vault$dir)
  gh::gh("GET /repos/{owner}/{repo}/readme/{dir}",
         owner = vault$org,
         repo = vault$repo,
         dir = vault$dir)
}

gh_getFolder <- function(vault) {
  checkmate::assertCharacter(vault$org)
  checkmate::assertCharacter(vault$repo)
  checkmate::assertCharacter(vault$dir)
  gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
         owner = vault$org,
         repo = vault$repo,
         path = vault$dir)
}


gh_getFile <- function(vault) {
  checkmate::assertCharacter(vault$org)
  checkmate::assertCharacter(vault$repo)
  checkmate::assertCharacter(vault$dir)
  checkmate::assertCharacter(vault$file)
  path <- fs::path(vault$dir, vault$file)

  gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
         owner = vault$org,
         repo = vault$repo,
         path = path)
}

vaultRepos <- function(vault) {
  org <- vault$org
  switch(org,
         OdyOSG = c("picardScripts", "conceptSetLibrary", "caprTemplates"))
}


gh_getOrgRepos <- function(vault) {

  tst <- gh::gh("GET /orgs/{org}/repos",
                org = vault$org,
                per_page = 50) %>%
    purrr::keep(~.x$name %in% vaultRepos(vault))
}
