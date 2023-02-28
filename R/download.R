checkoutVault <- function(owner, repo, path) {
  item <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
         owner = owner,
         repo = repo,
         path = path)

  tb <- tibble::tibble(
    org = owner,
    repo = repo,
    file = purrr::map_chr(item, ~.x$name),
    path = purrr::map_chr(item, ~.x$path)
  )
  return(tb)
}


getPicardScript <- function(repo, item, path) {
  scriptFile <- fs::path(item, item, ext = "R")
  vv <- vault(repo)
  download_link <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
                        owner = vv$owner,
                        repo = vv$repo,
                        path = scriptFile)$download_url
  savePath <- fs::path(path, item, ext = "R")
  download.file(download_link, destfile = savePath)
}


getPicardInternals <- function(repo, item, path) {
  internalFile <- fs::path(item, paste0("_", item), ext = "R")
  vv <- vault(repo)
  download_link <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
                          owner = vv$owner,
                          repo = vv$repo,
                          path = internalFile)$download_url
  savePath <- fs::path(path, paste0("_", item), ext = "R")
  download.file(download_link, destfile = savePath)
}


extractPicard <- function(repo, item, picard_project) {

  #get paths for scripts and internals
  script_path <- fs::path(picard_project, "R")
  internals_path <- fs::path(picard_project, "R/internals")

  getPicardScript(repo = repo, item = item, path = script_path)
  cli::cat_bullet("Downloaded ", crayon::green(fs::path(item, ext = "R")), " to ", crayon::cyan(script_path),
                  bullet = "tick", bullet_col = "green")

  getPicardInternals(repo = repo, item = item, path = internals_path)
  ff <- paste0("_", item)
  cli::cat_bullet("Downloaded ", crayon::green(fs::path(ff, ext = "R")), " to ", crayon::cyan(internals_path),
                  bullet = "tick", bullet_col = "green")

}
