read_contract <- function(filename) {
  path <- system.file("extdata", filename, package = "SOMevidence")
  if (!nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", "extdata", filename)
  }
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("version 1.0.0 exports and arguments match the API contract", {
  api <- read_contract("api-contract-v1.0.0.csv")
  expect_setequal(getNamespaceExports("SOMevidence"), api[["function"]])
  expect_equal(length(getNamespaceExports("SOMevidence")), nrow(api))

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
    expect_identical(observed, expected, info = function_name)
  }

  experimental <- api[["function"]][api$lifecycle == "experimental"]
  expect_setequal(
    experimental,
    c("launch_som_app", "run_som_sensitivity", "simulate_som_scenario")
  )
  expect_identical(
    api$return_class[api[["function"]] == "launch_som_app"],
    "interactive_side_effect"
  )
})

test_that("API validation distinguishes tested returns from GUI exceptions", {
  path <- testthat::test_path(
    "..", "..", "benchmarks", "results", "api_contract_validation.csv"
  )
  skip_if_not(file.exists(path), "API validation result is not built yet")
  validation <- utils::read.csv(path, stringsAsFactors = FALSE)

  expect_false(any(validation$check == "runtime_return:launch_som_app"))
  expect_true(any(
    validation$check == "runtime_exception_documented:launch_som_app" &
      validation$passed
  ))
  expect_true(any(
    validation$check == "runtime_return_coverage" & validation$passed
  ))
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
})
