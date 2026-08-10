test_that("data contracts assign names and reject ambiguous metadata", {
  unnamed <- som_data(matrix(1:6, nrow = 2))
  expect_identical(
    colnames(unnamed$layers$data),
    c("data_01", "data_02", "data_03")
  )
  expect_output(print(unnamed), "samples: 2")
  expect_output(print(unnamed), "sample id only")

  duplicated <- matrix(1:8, nrow = 4)
  colnames(duplicated) <- c("x", "x")
  expect_error(som_data(duplicated), "unique, non-empty")
  expect_error(
    som_data(layers = stats::setNames(list(matrix(1:4, nrow = 2)), "")),
    "unique, non-empty name"
  )
  expect_error(som_data(matrix(1:6, nrow = 3), id = c("a", "a", "b")), "unique")
  expect_error(som_data(matrix(1:6, nrow = 3), group = 1:2), "length 3")
  expect_error(som_data(matrix(1:6, nrow = 3), weight = c(1, -1, 1)), "non-negative")
  expect_error(som_data(matrix(c(1, Inf, 2, 3), nrow = 2)), "infinite")
})

test_that("preprocessing rejects undefined ecological transformations", {
  expect_error(som_preprocess("unsupported"), "unsupported")
  expect_error(som_preprocess(center = NA), "TRUE or FALSE")
  expect_error(som_preprocess(zero_replacement = 0), "one number")
  expect_error(
    SOMevidence:::.transform_matrix(
      matrix(c(-1, 1, 2, 3), nrow = 2),
      som_preprocess("sqrt")
    ),
    "non-negative"
  )
  expect_error(
    SOMevidence:::.transform_matrix(
      matrix(c(1, NA, 2, 3), nrow = 2),
      som_preprocess("hellinger")
    ),
    "missing components"
  )
  expect_error(
    SOMevidence:::.transform_matrix(
      matrix(c(0, 0, 1, 2), nrow = 2, byrow = TRUE),
      som_preprocess("hellinger")
    ),
    "positive total"
  )
  expect_error(
    SOMevidence:::.fit_preprocessor(
      cbind(a = c(1, 2), b = c(3, 4)),
      som_preprocess(c(a = "identity", wrong = "sqrt"))
    ),
    "column names"
  )
})

test_that("resampling designs validate units, domains and custom indices", {
  data <- som_data(matrix(seq_len(40), nrow = 10), group = rep(1:5, each = 2))
  full <- som_resamples(data, method = "full")
  expect_output(print(full), "method    : full")
  expect_length(full$splits, 1L)

  blocks <- som_resamples(
    data, method = "block_subsample", unit = "group",
    repeats = 3, prop = 0.6, seed = 11
  )
  expect_length(blocks$splits, 3L)
  expect_error(
    som_resamples(data, method = "group_subsample", unit = "missing"),
    "was not found"
  )
  expect_error(
    som_resamples(data, method = "leave_domain_out", domain = rep("one", 10)),
    "at least two domains"
  )
  expect_error(
    som_resamples(data, method = "custom", splits = list(list(assessment = 1:2))),
    "contain `analysis`"
  )
  expect_error(
    som_resamples(
      data, method = "custom",
      splits = list(list(analysis = 1:5, assessment = 5:10))
    ),
    "must not overlap"
  )
})

test_that("SOM specifications reject unsafe model budgets", {
  expect_error(som_spec(c(1, 2)), "at least two")
  expect_error(som_spec(c(2.5, 2)), "integer")
  expect_error(som_spec(c(2, 2), seeds = numeric()), "non-missing integers")
  expect_error(som_spec(c(2, 2), alpha = c(0, 1)), "positive")
  expect_error(som_spec(c(2, 2), k = 1), "at least two")
  expect_error(som_spec(c(2, 2), k = 5), "smallest grid")
  expect_error(
    som_spec(c(2, 2), k = 2, layer_weights = c(0, 0)),
    "positive sum"
  )
  specification <- som_spec(c(2, 2), seeds = 1:2, rlen = 5, k = 2)
  expect_output(print(specification), "models: 2 per resample")
  expect_equal(nrow(expand_som_spec(specification)), 2L)
})

test_that("print methods expose evidence boundaries", {
  data <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 1201)
  ensemble <- fit_som_ensemble(
    data, som_spec(c(3, 2), seeds = c(1202, 1203), rlen = 10, k = 2)
  )
  audit <- audit_som(ensemble)
  partitions <- partition_som(ensemble)
  consensus <- consensus_som(partitions, k = 2)
  references <- fit_cross_models(ensemble, methods = c("kmeans", "ward"), k = 2)
  comparison <- compare_cross_models(partitions, references)
  external <- evaluate_external_labels(consensus)
  gate <- som_gate(min_success_rate = 1)
  decision <- assess_defensibility(audit, partitions, 2, gate, consensus, comparison)

  expect_output(print(ensemble), "failed")
  expect_output(print(audit), "success rate")
  expect_output(print(partitions), "candidate k")
  expect_output(print(consensus), "median support")
  expect_output(print(references), "methods")
  expect_output(print(comparison), "agreement")
  expect_output(print(external), "agreement, not classification accuracy")
  expect_output(print(gate), "analyst-specified")
  expect_output(print(decision), "supported")
})

test_that("GUI parsers reject invalid sets and render built-in sources", {
  expect_identical(SOMevidence:::.parse_integer_set("1, 2,2", "seeds"), 1:2)
  expect_error(SOMevidence:::.parse_integer_set("1,x", "seeds"), "comma-separated")
  config <- list(
    data_source = "built_in", input_file = NULL,
    predictors = c("environment_01", "environment_02"),
    id_column = "sample_id", group_column = NULL,
    domain_column = NULL, external_column = "external_label",
    transform = "identity", center = TRUE, scale = TRUE,
    zero_replacement = NULL, resample_method = "full", repeats = 1L,
    prop = 1, resample_seed = 1L, xdim = 3L, ydim = 2L,
    seeds = 1:2, rlen = 20L, k = 2:3,
    cross_models = character()
  )
  script <- SOMevidence:::.render_gui_script(config)
  expect_error(parse(text = script), NA)
  expect_true(any(grepl("simulate_som_scenario", script, fixed = TRUE)))
})
