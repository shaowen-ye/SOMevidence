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

test_that("named external labels are aligned by sample identity", {
  ids <- paste0("sample_", seq_len(4L))
  labels <- stats::setNames(c("a", "b", "c", "d"), ids)
  reordered <- labels[rev(ids)]
  data <- som_data(
    matrix(seq_len(8L), nrow = 4L),
    id = ids,
    external_label = reordered
  )

  expect_identical(data$metadata$external_label, unname(labels))
  expect_identical(data$external_label_source, "named_id")
  positional <- som_data(
    matrix(seq_len(8L), nrow = 4L),
    id = ids,
    external_label = unname(labels)
  )
  expect_identical(positional$external_label_source, "position")
  expect_error(
    som_data(
      matrix(seq_len(8L), nrow = 4L),
      id = ids,
      external_label = stats::setNames(unname(labels), c(ids[-1L], ""))
    ),
    "unique, non-empty",
    fixed = TRUE
  )
  expect_error(
    som_data(
      matrix(seq_len(8L), nrow = 4L),
      id = ids,
      external_label = stats::setNames(
        unname(labels), c(ids[-1L], "outside")
      )
    ),
    "must match `id` exactly",
    fixed = TRUE
  )
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

  mixed_real_names <- matrix(seq_len(12), nrow = 4)
  rownames(mixed_real_names) <- c("sample_1", "field_a", "field_b", "field_c")
  mixed_real_rows <- som_data(mixed_real_names)
  expect_identical(mixed_real_rows$id_source, "generated")

  confirmed_real_rows <- som_data(
    mixed_real_names,
    id = rownames(mixed_real_names)
  )
  expect_identical(confirmed_real_rows$id_source, "provided")
  expect_identical(
    confirmed_real_rows$metadata$id,
    rownames(mixed_real_names)
  )

  generic_names <- matrix(seq_len(12), nrow = 4)
  rownames(generic_names) <- paste0("sample_", seq_len(4))
  generic_rows <- som_data(generic_names)
  expect_identical(generic_rows$id_source, "generated")

  generic_reordered <- generic_names[4:1, , drop = FALSE]
  generic_layers <- som_data(layers = list(
    first = generic_names,
    second = generic_reordered
  ))
  expect_identical(generic_layers$id_source, "generated")
  expect_identical(
    rownames(generic_layers$layers$second),
    generic_layers$metadata$id
  )
  expect_equal(
    unname(generic_layers$layers$second),
    unname(generic_names)
  )

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
