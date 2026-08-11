read_contract <- function(filename) {
  path <- system.file("extdata", filename, package = "SOMevidence")
  if (!nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", "extdata", filename)
  }
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("version 1.0.0 function and argument contracts remain compatible", {
  api <- read_contract("api-contract-v1.0.0.csv")
  expect_true(all(api[["function"]] %in% getNamespaceExports("SOMevidence")))

  for (i in seq_len(nrow(api))) {
    function_name <- api[["function"]][[i]]
    expected <- api$arguments[[i]]
    expected <- if (is.na(expected) || !nzchar(expected)) {
      character()
    } else {
      strsplit(expected, "|", fixed = TRUE)[[1L]]
    }
    observed <- names(formals(getExportedValue("SOMevidence", function_name)))
    if (is.null(observed)) observed <- character()
    if (api$lifecycle[[i]] == "stable") {
      expect_true(
        length(observed) >= length(expected),
        info = function_name
      )
      expect_identical(
        observed[seq_along(expected)], expected,
        info = function_name
      )
    } else {
      positions <- match(expected, observed)
      expect_false(anyNA(positions), info = function_name)
      expect_identical(positions, sort(positions), info = function_name)
    }
  }

  experimental <- api[["function"]][api$lifecycle == "experimental"]
  expect_setequal(
    experimental,
    c("launch_som_app", "run_som_sensitivity", "simulate_som_scenario")
  )
})

test_that("version 1.1.0 exports and arguments remain compatible", {
  api <- read_contract("api-contract-v1.1.0.csv")
  expect_true(all(api[["function"]] %in% getNamespaceExports("SOMevidence")))

  for (i in seq_len(nrow(api))) {
    function_name <- api[["function"]][[i]]
    expected <- api$arguments[[i]]
    expected <- if (is.na(expected) || !nzchar(expected)) {
      character()
    } else {
      strsplit(expected, "|", fixed = TRUE)[[1L]]
    }
    observed <- names(formals(getExportedValue("SOMevidence", function_name)))
    if (is.null(observed)) observed <- character()
    if (api$lifecycle[[i]] == "stable") {
      expect_true(
        length(observed) >= length(expected),
        info = function_name
      )
      expect_identical(
        observed[seq_along(expected)], expected,
        info = function_name
      )
    } else {
      positions <- match(expected, observed)
      expect_false(anyNA(positions), info = function_name)
      expect_identical(positions, sort(positions), info = function_name)
    }
  }

  expect_identical(
    formals(run_som_workflow)$cross_models,
    quote(c("kmeans", "ward"))
  )
  expect_identical(formals(fit_cross_models)$gmm_seed, 1L)
  expect_identical(
    api$return_class[api[["function"]] == "launch_som_app"],
    "shiny.appobj"
  )
})

test_that("version 1.2.0 exports formals and defaults match exactly", {
  api <- read_contract("api-contract-v1.2.0.csv")
  defaults <- read_contract("api-formals-v1.2.0.csv")
  exports <- getNamespaceExports("SOMevidence")
  expect_setequal(exports, api[["function"]])
  expect_identical(defaults[["function"]], api[["function"]])

  normalise_default <- function(value) {
    if (identical(value, quote(expr = ))) return("<required>")
    paste(deparse(value, width.cutoff = 500L), collapse = " ")
  }
  for (i in seq_len(nrow(api))) {
    function_name <- api[["function"]][[i]]
    expected_arguments <- api$arguments[[i]]
    expected_arguments <- if (
      is.na(expected_arguments) || !nzchar(expected_arguments)
    ) {
      character()
    } else {
      strsplit(expected_arguments, "|", fixed = TRUE)[[1L]]
    }
    function_formals <- formals(
      getExportedValue("SOMevidence", function_name)
    )
    observed_arguments <- names(function_formals)
    if (is.null(observed_arguments)) observed_arguments <- character()
    expect_identical(
      observed_arguments, expected_arguments, info = function_name
    )

    expected_defaults <- defaults$defaults[[i]]
    expected_defaults <- if (
      is.na(expected_defaults) || !nzchar(expected_defaults)
    ) {
      character()
    } else {
      strsplit(expected_defaults, "|", fixed = TRUE)[[1L]]
    }
    observed_defaults <- vapply(
      function_formals, normalise_default, character(1)
    )
    expect_identical(
      unname(observed_defaults), expected_defaults, info = function_name
    )
  }

  expect_identical(
    api$lifecycle[api[["function"]] == "audit_som_representation"],
    "experimental"
  )
})

test_that("version 1.0.0 objects satisfy their structural contract", {
  object_contract <- read_contract("object-contract-v1.0.0.csv")
  data <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 2101)
  preprocessing <- som_preprocess()
  resamples <- som_resamples(
    data, method = "subsample", repeats = 2, prop = 0.8, seed = 2102
  )
  specification <- som_spec(
    c(3, 2), seeds = c(2103, 2104), rlen = 10, k = 2
  )
  workflow <- run_som_workflow(
    data, specification, resamples,
    preprocess = preprocessing,
    cross_models = c("kmeans", "ward"),
    cross_model_control = list(kmeans_iter_max = 50L),
    keep_models = TRUE
  )
  consensus <- workflow$consensus$k2
  gate <- som_gate(min_success_rate = 0)
  defensibility <- assess_defensibility(
    workflow$audit, workflow$partitions, 2, gate,
    consensus, workflow$cross_comparison
  )
  external <- evaluate_external_labels(consensus)
  transfer <- audit_transfer(workflow$ensemble)
  new_data <- som_data(
    layers = lapply(data$layers, function(x) x[seq_len(8), , drop = FALSE]),
    id = paste0("new_", seq_len(8))
  )
  mapping <- map_som_ensemble(workflow$ensemble, new_data)
  sensitivity <- run_som_sensitivity(
    list(default = list(
      data = data,
      spec = som_spec(c(3, 2), seeds = c(2105, 2106), rlen = 10, k = 2)
    )),
    cross_models = "ward"
  )

  objects <- list(
    som_data = data,
    som_preprocess = preprocessing,
    som_resamples = resamples,
    som_spec = specification,
    som_ensemble = workflow$ensemble,
    som_audit = workflow$audit,
    som_partitions = workflow$partitions,
    som_consensus = consensus,
    som_cross_models = workflow$cross_models,
    som_cross_comparison = workflow$cross_comparison,
    som_workflow = workflow,
    som_gate = gate,
    som_defensibility = defensibility,
    som_external_assessment = external,
    som_transfer_audit = transfer,
    som_newdata_mapping = mapping,
    som_sensitivity = sensitivity
  )

  expect_setequal(names(objects), object_contract$class)
  for (i in seq_len(nrow(object_contract))) {
    class_name <- object_contract$class[[i]]
    object <- objects[[class_name]]
    required <- strsplit(
      object_contract$required_components[[i]], "|", fixed = TRUE
    )[[1L]]
    expect_s3_class(object, class_name)
    expect_true(
      all(required %in% names(object)),
      info = paste(class_name, "missing", paste(setdiff(required, names(object)), collapse = ", "))
    )
  }

  current_contract <- read_contract("object-contract-v1.1.0.csv")
  expect_setequal(names(objects), current_contract$class)
  for (i in seq_len(nrow(current_contract))) {
    class_name <- current_contract$class[[i]]
    required <- strsplit(
      current_contract$required_components[[i]], "|", fixed = TRUE
    )[[1L]]
    expect_true(
      all(required %in% names(objects[[class_name]])),
      info = paste(
        class_name, "missing",
        paste(setdiff(required, names(objects[[class_name]])), collapse = ", ")
      )
    )
  }

  expect_true(all(c(
    "grid", "median_quantization_error", "median_topographic_error"
  ) %in% names(workflow$audit$grid_summary)))
  expect_true(all(c(
    "n_partitions", "n_complete_partitions", "min_observed_clusters"
  ) %in% names(workflow$partitions$stability)))
  expect_true(all(c(
    "n_analysis_mapped", "n_assessment_mapped",
    "analysis_mapping_coverage", "assessment_mapping_coverage"
  ) %in% names(transfer$metrics)))
  expect_true("mapping_coverage" %in% names(mapping$summary))

  representation <- audit_som_representation(
    workflow$ensemble,
    pairs = data.frame(
      fit_a = workflow$ensemble$fits[[1L]]$id,
      fit_b = workflow$ensemble$fits[[2L]]$id
    )
  )
  current_objects <- c(
    objects,
    list(som_representation_audit = representation)
  )
  version_12_contract <- read_contract("object-contract-v1.2.0.csv")
  expect_setequal(names(current_objects), version_12_contract$class)
  expect_true(all(version_12_contract$contract_version == "1.2.0"))
  expect_true(all(
    version_12_contract$required_attributes ==
      "som_contract_version=1.2.0"
  ))
  for (i in seq_len(nrow(version_12_contract))) {
    class_name <- version_12_contract$class[[i]]
    object <- current_objects[[class_name]]
    required <- strsplit(
      version_12_contract$required_components[[i]], "|", fixed = TRUE
    )[[1L]]
    expect_s3_class(object, class_name)
    expect_true(
      all(required %in% names(object)),
      info = paste(
        class_name, "missing",
        paste(setdiff(required, names(object)), collapse = ", ")
      )
    )
    expect_identical(attr(object, "som_contract_version"), "1.2.0")
  }
})
