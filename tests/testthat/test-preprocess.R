test_that("preprocessing parameters are learned from analysis rows only", {
  x <- cbind(a = c(0, 1, 2, 3, 4, 100), b = c(2, 3, 4, 5, 6, 50))
  d <- som_data(x)
  r <- som_resamples(
    d,
    method = "custom",
    splits = list(list(id = "held_outlier", analysis = 1:5, assessment = 6))
  )
  s <- som_spec(c(2, 2), seeds = 7, rlen = 15, k = 2)
  e <- fit_som_ensemble(d, s, r)

  fitted <- e$fits[[1]]$fitted_preprocess$data
  expect_equal(unname(fitted$means), unname(colMeans(x[1:5, , drop = FALSE])))
  expect_false(isTRUE(all.equal(unname(fitted$means), colMeans(x))))
  expect_true(all(abs(colMeans(
    e$fits[[1]]$processed_all$data[1:5, , drop = FALSE]
  )) < 1e-12))
})

test_that("compositional transforms require explicit valid inputs", {
  d <- som_data(matrix(c(0, 1:11), nrow = 6))
  r <- som_resamples(d, method = "full")
  s <- som_spec(c(2, 2), seeds = 1, rlen = 10, k = 2)

  failed <- fit_som_ensemble(d, s, r, preprocess = som_preprocess("clr"))
  expect_equal(nrow(failed$failures), 1)
  expect_match(failed$failures$error, "zero_replacement")

  ok <- fit_som_ensemble(
    d, s, r,
    preprocess = som_preprocess("clr", zero_replacement = 0.5)
  )
  expect_equal(nrow(ok$failures), 0)
})

test_that("columnwise transformations are name-safe", {
  x <- cbind(temperature = c(10, 12, 14), nutrient = c(0, 1, 9))
  specification <- som_preprocess(c(
    temperature = "identity",
    nutrient = "log1p"
  ))
  fitted <- SOMevidence:::.fit_preprocessor(x, specification)

  expect_equal(
    fitted$fitted$means[["nutrient"]],
    mean(log1p(x[, "nutrient"]))
  )
  expect_error(
    SOMevidence:::.fit_preprocessor(x[, 2:1], specification),
    NA
  )
  expect_error(
    som_preprocess(c("identity", "hellinger")),
    "whole-matrix"
  )
  expect_equal(
    SOMevidence:::.transform_matrix(x + 1, som_preprocess("log")),
    log(x + 1),
    tolerance = 1e-12
  )
  expect_error(
    SOMevidence:::.transform_matrix(
      cbind(a = c(0, 1), b = c(1, 2)),
      som_preprocess("log")
    ),
    "positive"
  )
})
