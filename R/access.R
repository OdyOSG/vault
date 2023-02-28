vault <- function(repo) {
  sp <- stringr::str_split_1(string = repo, pattern = "/")
  vault <- structure(
    list(
      owner = sp[1],
      repo = sp[2]
    ),
    class = "vaultMeta"
  )
  return(vault)
}

vault2 <- function(...) {
  dots <- rlang::list2(...)
  structure(
    dots,
    class = "vaultManifest"
  )
}

#' Check if have access to vault
#' @param repo a string set up as <org>/<repo>
#' @export
checkVaultAccess <- function(repo) {
  vault <- vault(repo)
  checkAccess <- purrr::safely(gh::gh)


  tst <- checkAccess("GET /repos/{owner}/{repo}",
                     owner = vault$owner,
                     repo = vault$repo)

  if (is.null(tst$error)) {
    access <- glue::glue("Vault access to ", crayon::blue(repo), " granted!")
  } else{
    access <- glue::glue("Vault access to ", crayon::blue(repo),
                         "denied.\n Check if you have set PAT or if you have permissions to access this repository.")
  }
  access
}

