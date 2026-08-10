test_that("open-data registry has explicit provenance and roles", {
  registry <- open_data_registry()
  required <- c(
    "id", "domain", "role", "title", "doi", "version", "license",
    "landing_url", "redistribution", "planned_use", "status"
  )

  expect_true(all(required %in% names(registry)))
  expect_equal(anyDuplicated(registry$id), 0L)
  expect_false(anyNA(registry[, required]))
  expect_setequal(unique(registry$domain), c("ecology", "environment", "evolution"))
  expect_equal(nrow(open_data_registry(role = "quickstart")), 1)
})
