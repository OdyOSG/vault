#' Function to checkout file from vault
#' @param vault a vault object
#' @param item the item to checkout as a character string
#' @param openFile a toggle specifying whether to open the file after download in R studio
#' @export
checkout <- function(vault, item, openFile = TRUE) {
  numFiles <- howManyFiles(vault, item)
  if (numFiles == 1) {
    dd <- gh_getFiles(vault = vv, item = item)
    fileName <- dd[[1]]$name
    theFile <- downloadVault(dd[[1]]$download_url)
    filePath <- fs::path(vault$savePath, fileName)
    readr::write_lines(theFile, file = filePath)
    cli::cat_bullet("Downloaded file ", crayon::green(fileName), " to ", crayon::cyan(vault$savePath),
                    bullet = "tick", bullet_col = "green")
  } else {
    #TODO add situation where there are mulitple files
  }

  if (openFile) {
    rstudioapi::navigateToFile(filePath)
  }

  invisible(item)
}

#' Function to checkout multiple files from vault
#' @param vault a vault object
#' @param items the items to checkout as a character vector
#' @export
mapCheckout <- function(vault, items) {
  purrr::walk(items, ~checkout(vault = vault, item = .x, openFile = FALSE))
}

#
# checkoutVault <- function(owner, repo, path) {
#   item <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
#          owner = owner,
#          repo = repo,
#          path = path)
#
#   tb <- tibble::tibble(
#     org = owner,
#     repo = repo,
#     file = purrr::map_chr(item, ~.x$name),
#     path = purrr::map_chr(item, ~.x$path)
#   )
#   return(tb)
# }
#
#
# getPicardScript <- function(repo, item, path) {
#   scriptFile <- fs::path(item, item, ext = "R")
#   vv <- vault(repo)
#   download_link <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
#                         owner = vv$owner,
#                         repo = vv$repo,
#                         path = scriptFile)$download_url
#   savePath <- fs::path(path, item, ext = "R")
#   download.file(download_link, destfile = savePath)
# }
#
#
# getPicardInternals <- function(repo, item, path) {
#   internalFile <- fs::path(item, paste0("_", item), ext = "R")
#   vv <- vault(repo)
#   download_link <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
#                           owner = vv$owner,
#                           repo = vv$repo,
#                           path = internalFile)$download_url
#   savePath <- fs::path(path, paste0("_", item), ext = "R")
#   download.file(download_link, destfile = savePath)
# }
#
#
# extractPicard <- function(repo, item, picard_project) {
#
#   #get paths for scripts and internals
#   script_path <- fs::path(picard_project, "R")
#   internals_path <- fs::path(picard_project, "R/internals")
#
#   getPicardScript(repo = repo, item = item, path = script_path)
#   cli::cat_bullet("Downloaded ", crayon::green(fs::path(item, ext = "R")), " to ", crayon::cyan(script_path),
#                   bullet = "tick", bullet_col = "green")
#
#   getPicardInternals(repo = repo, item = item, path = internals_path)
#   ff <- paste0("_", item)
#   cli::cat_bullet("Downloaded ", crayon::green(fs::path(ff, ext = "R")), " to ", crayon::cyan(internals_path),
#                   bullet = "tick", bullet_col = "green")
#
# }
