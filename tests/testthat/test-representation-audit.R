test_that("analysis representation metrics reproduce established diagnostics", {
  data <- simulate_som_scenario("gradient", n = 60, p = 4, seed = 4201)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(
      list(c(3, 2), c(4, 2)), seeds = 4202:4203, rlen = 10, k = 2
    ),
    keep_models = FALSE
  )
  established <- audit_som(ensemble)$fit_metrics
  representation <- audit_som_representation(ensemble)
  observed <- representation$fit_metrics[
    match(established$id, representation$fit_metrics$fit_id),
    ,
    drop = FALSE
  ]

  expect_s3_class(representation, "som_representation_audit")
  expect_false(inherits(representation, "som_audit"))
  expect_identical(
    attr(representation, "som_contract_version"),
    "1.2.0"
  )
  expect_equal(
    observed$quantization_error,
    established$quantization_error,
    tolerance = 0
  )
  expect_equal(
    observed$topographic_error,
    established$topographic_error,
    tolerance = 0
  )
  expect_equal(
    observed$empty_unit_rate,
    established$empty_unit_rate,
    tolerance = 0
  )
  expect_true(all(observed$mapping_coverage == 1))
  expect_true(representation$comparison_budget$exact)
  expect_identical(
    representation$provenance$graph_distance,
    "shortest_hop"
  )
  forbidden <- c("score", "rank", "winner", "selected", "supported")
  expect_length(intersect(forbidden, names(representation)), 0L)
  expect_length(intersect(forbidden, names(representation$pairwise)), 0L)
  expect_error(
    assess_defensibility(
      representation,
      partition_som(ensemble),
      2,
      som_gate(min_median_ari = 0)
    ),
    "must come from `audit_som()`",
    fixed = TRUE
  )
})

test_that("assessment metrics use training-derived preprocessing", {
  data <- simulate_som_scenario("clusters", n = 60, p = 4, seed = 4211)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(list(
      id = "held",
      analysis = seq_len(40L),
      assessment = 41:60
    ))
  )
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4212:4213, rlen = 10, k = 2),
    resamples,
    keep_models = FALSE
  )
  representation <- audit_som_representation(
    ensemble,
    scope = "assessment",
    neighbourhood_size = 3
  )

  for (i in seq_along(ensemble$fits)) {
    fit <- ensemble$fits[[i]]
    metric <- representation$fit_metrics[
      representation$fit_metrics$fit_id == fit$id,
      ,
      drop = FALSE
    ]
    mapped <- fit$assessment[
      !is.na(fit$bmu[fit$assessment]) &
        is.finite(fit$distances[fit$assessment])
    ]
    restored <- .v113_restore_processed_layers(fit, ensemble$data)
    expected_te <- .v112_topographic_error(restored, mapped)
    expect_equal(metric$quantization_error, mean(fit$distances[mapped]))
    expect_equal(metric$topographic_error, expected_te, tolerance = 0)
    expect_identical(metric$n_requested, 20L)
    expect_identical(metric$n_mapped, length(mapped))
  }
  expect_true(all(representation$pairwise$scope == "assessment"))
  expect_true(all(representation$neighbourhood_records$scope == "assessment"))
  expect_true(all(
    representation$neighbourhood_records$n_neighbours_a >= 3L
  ))
  expect_true(all(
    representation$neighbourhood_records$n_neighbours_b >= 3L
  ))
})

test_that("identical mappings give unit topology reproducibility", {
  data <- simulate_som_scenario("gradient", n = 36, p = 3, seed = 4221)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4222, rlen = 8, k = 2),
    keep_models = FALSE
  )
  duplicate <- ensemble$fits[[1L]]
  duplicate$id <- paste0(duplicate$id, "__duplicate")
  duplicate$seed <- duplicate$seed + 1L
  ensemble$fits <- c(ensemble$fits, list(duplicate))
  ensemble$expected_models <- 2L

  representation <- audit_som_representation(
    ensemble,
    neighbourhood_size = 4
  )
  expect_identical(
    representation$pairwise$distance_rank_correlation,
    1
  )
  expect_identical(
    representation$pairwise$median_neighbourhood_jaccard,
    1
  )
  expect_true(all(representation$neighbourhood_records$jaccard == 1))
})

test_that("two common samples support an exact one-neighbour comparison", {
  data <- simulate_som_scenario("gradient", n = 30, p = 3, seed = 4223)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(list(
      id = "two_held",
      analysis = 3:30,
      assessment = 1:2
    ))
  )
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4224:4225, rlen = 8, k = 2),
    resamples,
    keep_models = FALSE
  )
  representation <- audit_som_representation(
    ensemble,
    scope = "assessment",
    neighbourhood_size = 1
  )

  expect_identical(representation$pairwise$n_common_mapped, 2L)
  expect_identical(representation$pairwise$correlation_status,
                   "too_few_jointly_mapped")
  expect_identical(representation$pairwise$neighbourhood_status, "computed")
  expect_identical(representation$pairwise$median_neighbourhood_jaccard, 1)
  expect_equal(nrow(representation$neighbourhood_records), 2L)
  expect_true(all(representation$neighbourhood_records$jaccard == 1))
})

test_that("topology disruption is detected when quantization is unchanged", {
  data <- simulate_som_scenario("gradient", n = 80, p = 3, seed = 4225)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4226, rlen = 10, k = 2),
    keep_models = FALSE
  )
  original <- ensemble$fits[[1L]]
  disrupted <- original
  permutation <- c(1L, 4L, 2L, 6L, 3L, 5L)
  disrupted$codes <- lapply(original$codes, function(codebook) {
    out <- codebook
    out[permutation, ] <- codebook
    out
  })
  disrupted$bmu <- permutation[original$bmu]
  disrupted$id <- paste0(original$id, "__topology_disrupted")
  disrupted$seed <- original$seed + 1L
  disrupted$training_topographic_error <- NA_real_
  ensemble$fits <- list(original, disrupted)
  ensemble$expected_models <- 2L

  representation <- audit_som_representation(ensemble)
  fit_metrics <- representation$fit_metrics

  expect_equal(
    fit_metrics$quantization_error[[1L]],
    fit_metrics$quantization_error[[2L]],
    tolerance = 0
  )
  expect_identical(fit_metrics$topographic_error[[1L]], 0)
  expect_gt(fit_metrics$topographic_error[[2L]], 0.5)
  expect_lt(representation$pairwise$distance_rank_correlation, 0.5)
})

test_that("representation comparison budgets stop before dense calculations", {
  data <- simulate_som_scenario("gradient", n = 30, p = 3, seed = 4231)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4232:4234, rlen = 5, k = 2),
    keep_models = FALSE
  )
  expect_error(
    audit_som_representation(
      ensemble,
      max_pairwise_comparisons = 2L
    ),
    "requests 3 fit pairs",
    fixed = TRUE
  )
  expect_error(
    audit_som_representation(
      ensemble,
      pairs = data.frame(
        fit_a = ensemble$fits[[1L]]$id,
        fit_b = ensemble$fits[[2L]]$id
      ),
      max_sample_pairs = 100L
    ),
    "435 jointly mapped sample-pair comparisons",
    fixed = TRUE
  )
})

test_that("prespecified fit pairs are validated without implicit weighting", {
  data <- simulate_som_scenario("clusters", n = 30, p = 3, seed = 4241)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4242:4244, rlen = 5, k = 2),
    keep_models = FALSE
  )
  ids <- vapply(ensemble$fits, `[[`, character(1), "id")
  requested <- data.frame(fit_a = ids[[1L]], fit_b = ids[[3L]])
  representation <- audit_som_representation(ensemble, pairs = requested)
  expect_identical(representation$pairwise$fit_a, ids[[1L]])
  expect_identical(representation$pairwise$fit_b, ids[[3L]])

  expect_error(
    audit_som_representation(
      ensemble,
      pairs = data.frame(fit_a = ids[[1L]], fit_b = ids[[1L]])
    ),
    "cannot compare a fit with itself",
    fixed = TRUE
  )
  expect_error(
    audit_som_representation(
      ensemble,
      pairs = data.frame(fit_a = ids[[1L]], fit_b = "unknown")
    ),
    "unknown or failed fits",
    fixed = TRUE
  )
  expect_error(
    audit_som_representation(
      ensemble,
      pairs = rbind(
        requested,
        data.frame(fit_a = requested$fit_b, fit_b = requested$fit_a)
      )
    ),
    "must not repeat an unordered fit comparison",
    fixed = TRUE
  )
})

test_that("empty assessment overlap remains explicitly unevaluable", {
  data <- simulate_som_scenario("clusters", n = 40, p = 3, seed = 4251)
  resamples <- som_resamples(
    data,
    method = "custom",
    splits = list(
      list(id = "first", analysis = 11:40, assessment = 1:10),
      list(id = "second", analysis = c(1:10, 21:40), assessment = 11:20)
    )
  )
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4252, rlen = 5, k = 2),
    resamples,
    keep_models = FALSE
  )
  representation <- audit_som_representation(
    ensemble,
    scope = "assessment",
    pairs = data.frame(
      fit_a = ensemble$fits[[1L]]$id,
      fit_b = ensemble$fits[[2L]]$id
    )
  )

  expect_identical(representation$pairwise$n_common_design, 0L)
  expect_identical(
    representation$pairwise$correlation_status,
    "no_common_design_rows"
  )
  expect_true(is.na(representation$pairwise$distance_rank_correlation))
})

test_that("representation audits reject malformed fit identity and mappings", {
  data <- simulate_som_scenario("clusters", n = 30, p = 3, seed = 4261)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4262:4263, rlen = 5, k = 2),
    keep_models = FALSE
  )
  malformed <- ensemble
  malformed$fits[[1L]]$bmu <- malformed$fits[[1L]]$bmu[-1L]
  expect_error(
    audit_som_representation(malformed),
    "does not map every ensemble sample",
    fixed = TRUE
  )

  wrong_ids <- ensemble
  wrong_ids$data$metadata$id[[2L]] <- wrong_ids$data$metadata$id[[1L]]
  expect_error(
    audit_som_representation(wrong_ids),
    "unique, non-empty IDs",
    fixed = TRUE
  )

  reordered_ids <- ensemble
  reordered_ids$data$metadata$id <- rev(reordered_ids$data$metadata$id)
  expect_error(
    audit_som_representation(reordered_ids),
    "do not match the ordered resampling sample IDs",
    fixed = TRUE
  )

  wrong_split <- ensemble
  wrong_split$fits[[1L]]$analysis <- rev(wrong_split$fits[[1L]]$analysis)
  expect_error(
    audit_som_representation(wrong_split),
    "do not match its originating resampling split",
    fixed = TRUE
  )

  bad_codebook <- ensemble
  bad_codebook$fits[[1L]]$codes <- bad_codebook$fits[[1L]]$codes[[1L]]
  expect_error(
    audit_som_representation(bad_codebook),
    "incompatible codebook preprocessing fields",
    fixed = TRUE
  )

  bad_grid <- ensemble
  bad_grid$fits[[1L]]$grid$xdim <-
    bad_grid$fits[[1L]]$grid$xdim + 1L
  expect_error(
    audit_som_representation(bad_grid),
    "inconsistent codebook and grid dimensions",
    fixed = TRUE
  )

  nonfinite_grid <- ensemble
  nonfinite_grid$fits[[1L]]$grid$pts[1L, 1L] <- NA_real_
  expect_error(
    audit_som_representation(nonfinite_grid),
    "inconsistent codebook and grid dimensions",
    fixed = TRUE
  )

  extra_coordinate <- ensemble
  extra_coordinate$fits[[1L]]$grid$pts <- cbind(
    extra_coordinate$fits[[1L]]$grid$pts,
    z = 0
  )
  expect_error(
    audit_som_representation(extra_coordinate),
    "inconsistent codebook and grid dimensions",
    fixed = TRUE
  )

  unknown_topology <- ensemble
  unknown_topology$fits[[1L]]$grid$topo <- "unknown"
  expect_error(
    audit_som_representation(unknown_topology),
    "must be rectangular or hexagonal",
    fixed = TRUE
  )

  missing_toroidal <- ensemble
  missing_toroidal$fits[[1L]]$grid$toroidal <- NA
  expect_error(
    audit_som_representation(missing_toroidal),
    "must be TRUE or FALSE",
    fixed = TRUE
  )

  overflowing_weights <- ensemble
  overflowing_weights$fits[[1L]]$user_weights[] <- .Machine$double.xmax
  overflowing_weights$fits[[1L]]$distance_weights[] <-
    .Machine$double.xmax
  expect_error(
    audit_som_representation(overflowing_weights),
    "incompatible layer weights",
    fixed = TRUE
  )
})

test_that("shortest-hop distances respect rectangular and toroidal grids", {
  rectangular <- kohonen::somgrid(3, 2, topo = "rectangular")
  rectangular_hops <- SOMevidence:::.som_grid_hop_distances(rectangular)
  expect_identical(diag(rectangular_hops), rep(0L, 6L))
  expect_identical(rectangular_hops[[1L, 2L]], 1L)
  expect_identical(rectangular_hops[[1L, 6L]], 2L)
  expect_identical(rectangular_hops, t(rectangular_hops))

  toroidal <- kohonen::somgrid(
    4, 2, topo = "rectangular", toroidal = TRUE
  )
  toroidal_hops <- SOMevidence:::.som_grid_hop_distances(toroidal)
  expect_identical(toroidal_hops[[1L, 4L]], 1L)
  expect_true(all(is.finite(toroidal_hops)))

  hexagonal <- kohonen::somgrid(3, 2, topo = "hexagonal")
  hexagonal_hops <- SOMevidence:::.som_grid_hop_distances(hexagonal)
  expect_identical(hexagonal_hops[[1L, 6L]], 2L)

  for (ydim in c(3L, 4L)) {
    hexagonal_torus <- kohonen::somgrid(
      4, ydim, topo = "hexagonal", toroidal = TRUE
    )
    torus_adjacency <- SOMevidence:::.som_grid_adjacency(hexagonal_torus)
    torus_hops <- SOMevidence:::.som_grid_hop_distances(hexagonal_torus)
    expect_true(torus_adjacency[[1L, 2L]])
    expect_true(torus_adjacency[[5L, 6L]])
    expect_true(all(is.finite(torus_hops)))
  }
  odd_hexagonal_torus <- kohonen::somgrid(
    4, 3, topo = "hexagonal", toroidal = TRUE
  )
  odd_adjacency <- SOMevidence:::.som_grid_adjacency(odd_hexagonal_torus)
  expect_true(odd_adjacency[[1L, 9L]])

  codebook <- matrix(10, nrow = 12L, ncol = 1L)
  codebook[5:6, 1L] <- c(0, 1)
  te_fit <- list(
    success = TRUE,
    processed_all = list(environment = matrix(0.2, nrow = 1L)),
    codes = list(environment = codebook),
    user_weights = 1,
    distance_weights = 1,
    grid = odd_hexagonal_torus
  )
  expect_identical(SOMevidence:::.topographic_error(te_fit, 1L), 0)
})

test_that("pairwise topology failures are recorded or raised as requested", {
  data <- simulate_som_scenario("gradient", n = 30, p = 3, seed = 4271)
  ensemble <- fit_som_ensemble(
    data,
    som_spec(c(3, 2), seeds = 4272:4273, rlen = 5, k = 2),
    keep_models = FALSE
  )
  ensemble$fits[[2L]]$grid$pts[6L, ] <- c(100, 100)

  recorded <- audit_som_representation(ensemble, fail_fast = FALSE)
  expect_identical(recorded$pairwise$correlation_status, "metric_error")
  expect_true(any(recorded$failures$stage == "pairwise_topology"))
  expect_match(recorded$failures$error, "disconnected", fixed = TRUE)

  expect_error(
    audit_som_representation(ensemble, fail_fast = TRUE),
    "disconnected",
    fixed = TRUE
  )
})
