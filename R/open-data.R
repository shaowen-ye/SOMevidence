#' List governed tutorial and candidate open-data sources
#'
#' The registry records provenance, version, licence, implementation status and
#' the intended role of each external dataset. Only records marked
#' `implemented` are used by a current package tutorial. Candidate records are
#' governed prospects, not claims of completed package validation. The
#' function returns metadata only; it never downloads data or changes the
#' user's files.
#'
#' @param role Optional role used to filter the registry. Matching is exact and
#'   case-sensitive; unmatched values return no matching rows.
#' @param domain Optional scientific domain used to filter the registry.
#'   Matching is exact and case-sensitive; unmatched values return no matching
#'   rows.
#'
#' @return A data frame with one row per governed external dataset.
#' @export
#'
#' @examples
#' open_data_registry(role = "quickstart")
open_data_registry <- function(role = NULL, domain = NULL) {
  path <- system.file(
    "extdata", "open-data-registry.csv",
    package = "SOMevidence"
  )
  if (!nzchar(path)) .abort("The installed open-data registry was not found.")
  registry <- utils::read.csv(
    path,
    stringsAsFactors = FALSE, na.strings = c("", "NA")
  )

  if (!is.null(role)) registry <- registry[registry$role %in% role, , drop = FALSE]
  if (!is.null(domain)) {
    registry <- registry[registry$domain %in% domain, , drop = FALSE]
  }
  rownames(registry) <- NULL
  registry
}
