test_that("som_data separates training layers from design metadata", {
  x <- matrix(seq_len(24), nrow = 6)
  d <- som_data(
    x,
    id = paste0("sample_", seq_len(6)),
    group = rep(c("site_a", "site_b"), each = 3),
    external_label = rep(c("reference_a", "reference_b"), each = 3)
  )

  expect_s3_class(d, "som_data")
  expect_named(d$layers, "data")
  expect_identical(d$id_source, "provided")
  expect_false("external_label" %in% colnames(d$layers[[1]]))
  expect_identical(d$metadata$external_label, rep(
    c("reference_a", "reference_b"),
    each = 3
  ))
})

test_that("som_data records whether identifiers are stable", {
  generated <- som_data(matrix(seq_len(12), nrow = 4))
  expect_identical(generated$id_source, "generated")

  round_trip <- som_data(layers = generated$layers)
  expect_identical(round_trip$id_source, "generated")
  subset_layers <- lapply(
    generated$layers,
    function(x) x[c(2, 4), , drop = FALSE]
  )
  subset_round_trip <- som_data(layers = subset_layers)
  expect_identical(subset_round_trip$id_source, "generated")
  mixed_layers <- generated$layers
  rownames(mixed_layers[[1L]])[[1L]] <- "custom_1"
  mixed_round_trip <- som_data(layers = mixed_layers)
  expect_identical(mixed_round_trip$id_source, "generated")

  automatic_rows <- som_data(data.frame(a = 1:4, b = 5:8))
  expect_identical(automatic_rows$id_source, "generated")

  positional <- matrix(seq_len(12), nrow = 4)
  rownames(positional) <- as.character(seq_len(4))
  positional_rows <- som_data(positional)
  expect_identical(positional_rows$id_source, "generated")

  named <- matrix(seq_len(12), nrow = 4)
  rownames(named) <- letters[1:4]
  row_names <- som_data(named)
  expect_identical(row_names$id_source, "rownames")
  expect_identical(row_names$metadata$id, letters[1:4])
})

test_that("som_data rejects ambiguous or non-numeric inputs", {
  expect_error(som_data(), "exactly one")
  expect_error(
    som_data(x = matrix(1:6, 3), layers = list(a = matrix(1:6, 3))),
    "exactly one"
  )
  expect_error(
    som_data(data.frame(x = 1:3, label = letters[1:3])),
    "non-numeric"
  )
  expect_error(som_data(1:3), "numeric matrix or data frame")
})

test_that("multiple layers must align by row", {
  expect_error(
    som_data(layers = list(a = matrix(1:8, 4), b = matrix(1:6, 3))),
    "same samples"
  )
})
