verifier <- normalizePath(
  ".github/scripts/verify-check-log.R",
  winslash = "/",
  mustWork = TRUE
)

make_fixture <- function(lines, manual = FALSE) {
  root <- tempfile("check-log-fixture-")
  check <- file.path(root, "SOMevidence.Rcheck")
  dir.create(check, recursive = TRUE)
  writeLines(lines, file.path(check, "00check.log"), useBytes = TRUE)
  if (manual) {
    writeBin(
      charToRaw("%PDF-1.4\n% minimal test fixture\n"),
      file.path(check, "SOMevidence-manual.pdf")
    )
  }
  root
}

run_verifier <- function(fixture, options = character(), should_pass = TRUE) {
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(verifier), shQuote(fixture), options),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status") %||% 0L
  if (isTRUE(status == 0L) != should_pass) {
    stop(
      paste(c(
        "Unexpected check-log verifier result.",
        paste0("status=", status),
        output
      ), collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(output)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

routine_ok <- make_fixture(c(
  "* checking package namespace information ... OK",
  "* DONE",
  "Status: OK"
))
run_verifier(routine_ok)

incomplete <- make_fixture(c(
  "* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers",
  "Maintainer: 'Shaowen Ye <yeshaowen2119@gmail.com>'"
))
run_verifier(incomplete, should_pass = FALSE)

remote_ok <- make_fixture(c(
  "* checking CRAN incoming feasibility ... [2s/3s] OK",
  "* checking PDF version of manual ... [1s/1s] OK",
  "* DONE",
  "Status: OK"
), manual = TRUE)
run_verifier(
  remote_ok,
  c("--require-remote-incoming", "--require-pdf-manual")
)

new_submission <- make_fixture(c(
  "* checking CRAN incoming feasibility ... [2s/3s] NOTE",
  "Maintainer: 'Shaowen Ye <yeshaowen2119@gmail.com>'",
  "",
  "New submission",
  "* checking PDF version of manual ... OK",
  "* DONE",
  "Status: 1 NOTE"
), manual = TRUE)
run_verifier(
  new_submission,
  c("--require-remote-incoming", "--require-pdf-manual")
)

other_note <- make_fixture(c(
  "* checking CRAN incoming feasibility ... NOTE",
  "Maintainer: 'Shaowen Ye <yeshaowen2119@gmail.com>'",
  "New submission",
  "Possibly misspelled words in DESCRIPTION:",
  "* DONE",
  "Status: 1 NOTE"
))
run_verifier(
  other_note,
  "--require-remote-incoming",
  should_pass = FALSE
)

missing_manual <- make_fixture(c(
  "* checking PDF version of manual ... OK",
  "* DONE",
  "Status: OK"
))
run_verifier(
  missing_manual,
  "--require-pdf-manual",
  should_pass = FALSE
)

message("Check-log verifier regression fixtures passed.")
