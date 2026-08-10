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
