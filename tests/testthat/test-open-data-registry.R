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

test_that("open-data registry filters preserve exact matching semantics", {
  registry <- open_data_registry()
  expect_identical(names(formals(open_data_registry)), c("role", "domain"))

  selected_roles <- open_data_registry(role = c("quickstart", "benchmark"))
  expect_setequal(unique(selected_roles$role), c("quickstart", "benchmark"))
  expect_true(all(open_data_registry(domain = "ecology")$domain == "ecology"))
  expect_identical(
    open_data_registry(role = c("quickstart", "quickstart")),
    open_data_registry(role = "quickstart")
  )
  expect_identical(
    open_data_registry(domain = c("ecology", "ecology")),
    open_data_registry(domain = "ecology")
  )

  expect_identical(
    open_data_registry(role = "not_a_role"),
    registry[FALSE, , drop = FALSE]
  )
  expect_identical(
    open_data_registry(domain = "not_a_domain"),
    registry[FALSE, , drop = FALSE]
  )
  expect_identical(
    open_data_registry(role = c("quickstart", "not_a_role")),
    open_data_registry(role = "quickstart")
  )
  expect_equal(nrow(open_data_registry(role = "Quickstart")), 0L)
  expect_equal(nrow(open_data_registry(domain = "Ecology")), 0L)
  expect_identical(
    open_data_registry(role = factor("quickstart")),
    open_data_registry(role = "quickstart")
  )
})
