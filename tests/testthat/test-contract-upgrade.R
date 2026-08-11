.strip_contract_attribute <- function(x) {
  attr(x, "som_contract_version") <- NULL
  x
}

.strip_workflow_contracts <- function(workflow) {
  workflow <- .strip_contract_attribute(workflow)
  workflow$ensemble <- .strip_contract_attribute(workflow$ensemble)
  workflow$ensemble$data <- .strip_contract_attribute(workflow$ensemble$data)
  workflow$ensemble$spec <- .strip_contract_attribute(workflow$ensemble$spec)
  workflow$ensemble$resamples <-
    .strip_contract_attribute(workflow$ensemble$resamples)
  workflow$audit <- .strip_contract_attribute(workflow$audit)
  workflow$audit$ensemble <- .strip_contract_attribute(workflow$audit$ensemble)
  workflow$partitions <- .strip_contract_attribute(workflow$partitions)
  workflow$partitions$ensemble <-
    .strip_contract_attribute(workflow$partitions$ensemble)
  workflow$consensus <- lapply(workflow$consensus, function(consensus) {
    consensus <- .strip_contract_attribute(consensus)
    consensus$ensemble <- .strip_contract_attribute(consensus$ensemble)
    consensus
  })
  if (!is.null(workflow$cross_models)) {
    workflow$cross_models <- .strip_contract_attribute(workflow$cross_models)
    workflow$cross_models$ensemble <-
      .strip_contract_attribute(workflow$cross_models$ensemble)
  }
  if (!is.null(workflow$cross_comparison)) {
    workflow$cross_comparison <-
      .strip_contract_attribute(workflow$cross_comparison)
    workflow$cross_comparison$ensemble <-
      .strip_contract_attribute(workflow$cross_comparison$ensemble)
  }
  workflow
}

test_that("new public objects record the structural contract version", {
  data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 4101)
  preprocessing <- som_preprocess()
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(list(
      id = "calibration",
      analysis = seq_len(35L),
      assessment = 36:45
    ))
  )
  specification <- som_spec(
    c(3, 2), seeds = c(4102, 4103), rlen = 10, k = 2
  )
  workflow <- run_som_workflow(
    data,
    specification,
    resamples,
    cross_models = c("kmeans", "ward"),
    cross_model_control = list(kmeans_iter_max = 50L),
    keep_models = TRUE
  )
  consensus <- workflow$consensus$k2
  external <- evaluate_external_labels(consensus)
  gate <- som_gate(min_success_rate = 0, min_median_ari = 0)
  defensibility <- assess_defensibility(
    workflow$audit,
    workflow$partitions,
    2,
    gate,
    consensus,
    workflow$cross_comparison
  )
  mapping <- map_som_ensemble(
    workflow$ensemble,
    som_data(
      layers = lapply(
        data$layers, function(layer) layer[seq_len(5L), , drop = FALSE]
      ),
      id = paste0("new_", seq_len(5L))
    )
  )
  sensitivity <- run_som_sensitivity(
    list(default = list(
      data = data,
      spec = som_spec(c(3, 2), seeds = 4104:4105, rlen = 5, k = 2)
    )),
    cross_models = "ward"
  )
  representation <- audit_som_representation(
    workflow$ensemble,
    pairs = data.frame(
      fit_a = workflow$ensemble$fits[[1L]]$id,
      fit_b = workflow$ensemble$fits[[2L]]$id
    )
  )
  objects <- list(
    data,
    preprocessing,
    resamples,
    specification,
    workflow$ensemble,
    workflow$audit,
    workflow$partitions,
    consensus,
    workflow$cross_models,
    workflow$cross_comparison,
    workflow,
    gate,
    defensibility,
    external,
    audit_transfer(workflow$ensemble),
    mapping,
    sensitivity,
    representation
  )

  expect_length(objects, length(SOMevidence:::.som_required_components))
  for (object in objects) {
    expect_identical(attr(object, "som_contract_version"), "1.2.0")
    expect_invisible(SOMevidence:::.validate_som_object(object))
  }
})

test_that("legacy leaf objects upgrade only when evidence is recoverable", {
  labelled <- som_data(
    matrix(seq_len(12L), nrow = 4L),
    external_label = c("a", "a", "b", "b")
  )
  legacy_data <- .strip_contract_attribute(labelled)
  legacy_data$external_label_source <- NULL
  original <- serialize(legacy_data, NULL)
  upgraded <- upgrade_som_object(legacy_data)

  expect_identical(serialize(legacy_data, NULL), original)
  expect_identical(upgraded$external_label_source, "legacy")
  expect_identical(attr(upgraded, "som_contract_version"), "1.2.0")
  expect_identical(upgrade_som_object(upgraded), upgraded)

  legacy_resamples <- som_resamples(labelled)
  legacy_resamples <- .strip_contract_attribute(legacy_resamples)
  legacy_resamples$sample_ids <- NULL
  expect_error(
    upgrade_som_object(legacy_resamples),
    "cannot be upgraded safely on their own",
    fixed = TRUE
  )

  legacy_external <- structure(
    list(
      n_total = 4L, n_used = 4L, n_omitted = 0L,
      source = "supplied", excluded_values = NULL,
      ari = 1, ami = 1, contingency = table(1:4, 1:4),
      composition = data.frame()
    ),
    class = "som_external_assessment"
  )
  expect_error(
    upgrade_som_object(legacy_external),
    "record-level matching and omission states",
    fixed = TRUE
  )
})

test_that("legacy workflows upgrade recursively with common provenance", {
  data <- simulate_som_scenario("clusters", n = 45, p = 3, seed = 4111)
  workflow <- run_som_workflow(
    data,
    som_spec(c(3, 2), seeds = 4112:4113, rlen = 8, k = 2),
    cross_models = c("kmeans", "ward")
  )
  legacy <- .strip_workflow_contracts(workflow)
  legacy$ensemble$resamples$sample_ids <- NULL
  legacy$audit$ensemble <- legacy$ensemble
  legacy$partitions$ensemble <- legacy$ensemble
  legacy$consensus <- lapply(legacy$consensus, function(consensus) {
    consensus$ensemble <- legacy$ensemble
    consensus
  })
  legacy$cross_models$ensemble <- legacy$ensemble
  legacy$cross_comparison$ensemble <- legacy$ensemble
  original <- serialize(legacy, NULL)

  upgraded <- upgrade_som_object(legacy)
  expect_identical(serialize(legacy, NULL), original)
  expect_identical(attr(upgraded, "som_contract_version"), "1.2.0")
  expect_identical(
    upgraded$ensemble$resamples$sample_ids,
    upgraded$ensemble$data$metadata$id
  )
  expect_identical(upgraded$audit$ensemble, upgraded$ensemble)
  expect_identical(upgraded$partitions$ensemble, upgraded$ensemble)
  expect_identical(upgraded$consensus$k2$ensemble, upgraded$ensemble)
  expect_identical(upgraded$cross_models$ensemble, upgraded$ensemble)
  expect_identical(upgraded$cross_comparison$ensemble, upgraded$ensemble)
  expect_identical(upgrade_som_object(upgraded), upgraded)
})

test_that("workflow migration preserves NULL cross-model fields", {
  data <- simulate_som_scenario("gradient", n = 35, p = 3, seed = 4114)
  workflow <- run_som_workflow(
    data,
    som_spec(c(3, 2), seeds = 4115:4116, rlen = 5, k = 2),
    cross_models = character()
  )

  expect_true(all(c("cross_models", "cross_comparison") %in% names(workflow)))
  expect_null(workflow$cross_models)
  expect_null(workflow$cross_comparison)
  expect_identical(upgrade_som_object(workflow), workflow)
})

test_that("workflow migration rejects conflicting nested provenance", {
  first_data <- simulate_som_scenario(
    "clusters", n = 35, p = 3, seed = 4117
  )
  second_data <- simulate_som_scenario(
    "gradient", n = 35, p = 3, seed = 4118
  )
  first <- run_som_workflow(
    first_data,
    som_spec(c(3, 2), seeds = 4119:4120, rlen = 5, k = 2),
    cross_models = character()
  )
  second <- run_som_workflow(
    second_data,
    som_spec(c(3, 2), seeds = 4121:4122, rlen = 5, k = 2),
    cross_models = character()
  )
  first$audit <- second$audit

  expect_error(
    upgrade_som_object(first),
    "different originating ensemble",
    fixed = TRUE
  )
})

test_that("resample migration accepts only verified sample identities", {
  data <- simulate_som_scenario("gradient", n = 35, p = 3, seed = 4123)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4124, rlen = 5, k = 2),
    keep_models = FALSE
  )
  legacy <- .strip_contract_attribute(ensemble)
  legacy$data <- .strip_contract_attribute(legacy$data)
  legacy$spec <- .strip_contract_attribute(legacy$spec)
  legacy$resamples <- .strip_contract_attribute(legacy$resamples)
  legacy$resamples$sample_ids <- NULL

  duplicated <- legacy
  duplicated$data$metadata$id[[2L]] <- duplicated$data$metadata$id[[1L]]
  expect_error(
    upgrade_som_object(duplicated),
    "unique, non-empty sample IDs",
    fixed = TRUE
  )

  missing <- legacy
  missing$data$metadata$id[[2L]] <- NA_character_
  expect_error(
    upgrade_som_object(missing),
    "one valid sample ID",
    fixed = TRUE
  )
})

test_that("contract migration rejects unknown, damaged and future objects", {
  expect_error(
    upgrade_som_object(structure(list(), class = "other_result")),
    "exactly one supported",
    fixed = TRUE
  )

  damaged <- som_data(matrix(seq_len(12L), nrow = 4L))
  damaged$metadata <- NULL
  expect_error(
    upgrade_som_object(damaged),
    "lacks required components: metadata",
    fixed = TRUE
  )

  future <- som_data(matrix(seq_len(12L), nrow = 4L))
  attr(future, "som_contract_version") <- "9.0.0"
  expect_error(
    upgrade_som_object(future),
    "newer contract version",
    fixed = TRUE
  )

  malformed <- som_data(matrix(seq_len(12L), nrow = 4L))
  attr(malformed, "som_contract_version") <- "not-a-version"
  expect_error(
    upgrade_som_object(malformed),
    "not a valid version string",
    fixed = TRUE
  )
})

test_that("contract attributes survive RDS serialization", {
  object <- som_data(matrix(seq_len(12L), nrow = 4L))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path)
  restored <- readRDS(path)

  expect_identical(restored, object)
  expect_identical(attr(restored, "som_contract_version"), "1.2.0")
})
