test_that("create vault object", {
  tempDir <- tempfile()
  dir.create(tempDir)
  vv <- vault(repo_string = "OdyOSG/testRepo",
              savePath = tempDir)
  expect_equal(vv$org, "OdyOSG")
  expect_equal(vv$repo, "testRepo")
  expect_equal(vv$savePath, tempDir)
  expect_s3_class(vv, "vault")
})


test_that("Can check access", {
  tempDir <- tempfile()
  dir.create(tempDir)
  vv <- vault(repo_string = "OdyOSG/testRepo",
              savePath = tempDir)
  expect_true(checkAccess(vv))
})


test_that("List Contents", {
  tempDir <- tempfile()
  dir.create(tempDir)
  vv <- vault(repo_string = "OdyOSG/testRepo",
              savePath = tempDir)

  # Test listRepos works
  repo_contents <- listRepos(vv)
  expect_equal(repo_contents[1], c("simpleCondition"))

  # test listMeta works
  meta_contents <- listMeta(vv, "simpleCondition")
  tst_tb <- tibble::tibble(
    org = "OdyOSG",
    repo = "testRepo",
    name = "simpleCondition",
    type = "capr script",
    version = "0.0.1",
    maintainer = "Martin Lavallee",
    description = "create a simple condition cohort using Capr"
  )
  expect_equal(ncol(meta_contents), 7)
  expect_equal(nrow(meta_contents), 1)
  expect_equal(meta_contents, tst_tb)

  #test listContents works
  contents <- listContents(vv)
  expect_equal(ncol(contents), 7)
  expect_equal(nrow(contents), 3)
  expect_equal(contents[1,], tst_tb)

})


#TODO error in checkout within test environment. Likely an issue with the gitcred
#probably need to supply a dummy token in the test
#
test_that("Single Checkout works", {
  tempDir <- tempfile()
  dir.create(tempDir)
  vv <- vault(repo_string = "OdyOSG/testRepo",
              savePath = tempDir)

  tt <- purrr::quietly(checkout)(vv, item = "simpleCondition", openFile = FALSE)$result
  tstFile <- fs::path(tempDir, tt, ext = "R")
  expect_true(fs::file_exists(tstFile))
})
#
#
test_that("Multiple Checkout works", {
  tempDir <- tempfile()
  dir.create(tempDir)
  vv <- vault(repo_string = "OdyOSG/testRepo",
              savePath = tempDir)
  cart <- c("simpleCondition", "simpleDrug")
  tt <- purrr::quietly(mapCheckout)(vv, items = cart)$result
  tstFile <- fs::path(tempDir, tt, ext = "R")
  expect_true(all(fs::file_exists(tstFile)))
})

test_that("Multifile Checkout works", {
  tempDir <- tempfile()
  dir.create(tempDir)
  vv <- vault(repo_string = "OdyOSG/testRepo",
              savePath = tempDir)
  tt <- purrr::quietly(checkout)(vv, item = "testPipeline", openFile = FALSE)$result
  tstFile <- fs::path(tempDir, "testPipeline/helloWorld", ext = "R")
  tstFile2 <- fs::path(tempDir, "testPipeline/_helloWorld", ext = "R")
  expect_true(fs::file_exists(tstFile))
  expect_true(fs::file_exists(tstFile2))
})
