#' List governed open-data sources used in examples and validation
#'
#' The registry records provenance, version, licence and the intended role of
#' each external dataset. The function returns metadata only; it never
#' downloads data or changes the user's files.
#'
#' @param role Optional role used to filter the registry.
#' @param domain Optional scientific domain used to filter the registry.
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
