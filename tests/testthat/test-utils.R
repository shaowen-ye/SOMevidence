test_that("count labels use singular and plural forms", {
  expect_identical(SOMevidence:::.count_noun(0L, "warning"), "0 warnings")
  expect_identical(SOMevidence:::.count_noun(1L, "warning"), "1 warning")
  expect_identical(SOMevidence:::.count_noun(2L, "warning"), "2 warnings")
  expect_identical(
    SOMevidence:::.count_noun(2L, "analysis", "analyses"),
    "2 analyses"
  )
})
