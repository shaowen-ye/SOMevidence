args <- commandArgs(trailingOnly = TRUE)
check_dir <- if (length(args)) args[[1L]] else "check"
require_remote <- "--require-remote-incoming" %in% args[-1L]
require_manual <- "--require-pdf-manual" %in% args[-1L]

logs <- list.files(
  check_dir,
  pattern = "00check[.]log$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(logs) != 1L) {
  stop(
    sprintf("Expected one 00check.log under `%s`; found %d.", check_dir, length(logs)),
    call. = FALSE
  )
}

lines <- readLines(logs[[1L]], warn = FALSE)
completed <- any(grepl("^\\* DONE$", lines))
halted <- any(grepl("Execution halted", lines, fixed = TRUE))
status_lines <- lines[grepl("^Status:", lines)]
if (!completed || length(status_lines) != 1L || halted) {
  stop(paste0(
    "R CMD check did not finish cleanly: DONE=", completed,
    ", status lines=", length(status_lines),
    ", Execution halted=", halted,
    ". See ", logs[[1L]], "."
  ), call. = FALSE)
}

if (require_remote) {
  remote_pattern <- paste0(
    "^\\* checking CRAN incoming feasibility \\.\\.\\. ",
    "(?:\\[[^]]+\\] )?(OK|NOTE|Note_to_CRAN_maintainers)$"
  )
  remote_index <- grep(remote_pattern, lines, perl = TRUE)
  remote_unavailable <- any(grepl(
    "unable to access index|need Internet access to use CRAN incoming checks",
    lines,
    ignore.case = TRUE
  ))
  if (length(remote_index) != 1L || remote_unavailable) {
    stop(
      "The remote CRAN incoming check did not produce one usable result.",
      call. = FALSE
    )
  }
  remote_result <- sub(remote_pattern, "\\1", lines[[remote_index]], perl = TRUE)
  if (identical(remote_result, "OK")) {
    if (!identical(status_lines, "Status: OK")) {
      stop("Remote incoming passed, but another check issue remains.", call. = FALSE)
    }
  } else if (identical(remote_result, "NOTE")) {
    next_check <- which(
      seq_along(lines) > remote_index & grepl("^\\* checking ", lines)
    )
    end <- if (length(next_check)) next_check[[1L]] - 1L else length(lines)
    details <- trimws(lines[seq.int(remote_index + 1L, end)])
    details <- details[nzchar(details)]
    note_checks <- grep(
      "^\\* checking .* \\.\\.\\. (?:\\[[^]]+\\] )?NOTE$",
      lines,
      perl = TRUE
    )
    allowed_details <- grepl("^Maintainer: .+", details) |
      details == "New submission"
    new_submission_only <- length(details) && all(allowed_details) &&
      any(details == "New submission")
    if (!identical(status_lines, "Status: 1 NOTE") ||
          length(note_checks) != 1L ||
          note_checks[[1L]] != remote_index ||
          !new_submission_only) {
      stop(paste0(
        "Remote incoming returned a NOTE other than the sole permitted ",
        "new-submission NOTE."
      ), call. = FALSE)
    }
  } else {
    stop(
      "The remote incoming check was disabled or not evaluated remotely.",
      call. = FALSE
    )
  }
} else if (!identical(status_lines, "Status: OK")) {
  stop("Routine R CMD check did not finish with `Status: OK`.", call. = FALSE)
}

if (require_manual) {
  manual_ok <- any(grepl(
    paste0(
      "^\\* checking PDF version of manual \\.\\.\\. ",
      "(?:\\[[^]]+\\] )?OK$"
    ),
    lines,
    perl = TRUE
  ))
  manuals <- list.files(
    check_dir,
    pattern = "-manual[.]pdf$",
    recursive = TRUE,
    full.names = TRUE
  )
  nonempty_manual <- length(manuals) == 1L &&
    is.finite(file.info(manuals)$size) && file.info(manuals)$size > 0
  if (!manual_ok || !nonempty_manual) {
    stop(paste0(
      "PDF manual verification failed: log OK=", manual_ok,
      ", one non-empty manual=", nonempty_manual, "."
    ), call. = FALSE)
  }
}

message("Verified complete R CMD check log: ", logs[[1L]])
