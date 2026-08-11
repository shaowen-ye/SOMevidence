test_that("package citation identifies the sole package author", {
  package_root <- system.file(package = "SOMevidence")
  root_candidates <- unique(c(package_root, dirname(package_root)))
  package_root <- root_candidates[
    file.exists(file.path(root_candidates, "DESCRIPTION"))
  ][[1L]]
  description_path <- file.path(package_root, "DESCRIPTION")
  citation_path <- file.path(package_root, "CITATION")
  if (!file.exists(citation_path)) {
    citation_path <- file.path(package_root, "inst", "CITATION")
  }
  metadata <- as.list(read.dcf(description_path)[1L, ])
  citation <- utils::readCitationFile(citation_path, meta = metadata)
  text <- format(citation, style = "text")
  authors <- format(citation[[1L]]$author)

  expect_length(citation, 1L)
  expect_identical(authors, "Shaowen Ye")
  expect_match(text, metadata$Version, fixed = TRUE)
  expect_identical(length(citation[[1L]]$author), 1L)
})

test_that("CITATION.cff agrees with package metadata", {
  skip_if_not_installed("yaml")
  cff_path <- testthat::test_path("..", "..", "CITATION.cff")
  if (!file.exists(cff_path)) {
    skip("Repository-level CITATION.cff is validated by the lint workflow")
  }

  cff <- yaml::read_yaml(cff_path)
  metadata <- as.list(read.dcf(
    testthat::test_path("..", "..", "DESCRIPTION")
  )[1L, ])
  released <- as.Date(as.character(cff[["date-released"]]))
  shanghai_today <- as.Date(Sys.time(), tz = "Asia/Shanghai")

  expect_identical(as.character(cff$version), metadata$Version)
  expect_identical(cff$authors[[1L]][["given-names"]], "Shaowen")
  expect_identical(cff$authors[[1L]][["family-names"]], "Ye")
  expect_identical(length(cff$authors), 1L)
  expect_false(is.na(released))
  expect_lte(released, shanghai_today)
})
