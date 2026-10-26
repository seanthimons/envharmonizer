#' Explicit offline ECOTOX lifestage mapping
#'
#' Experimental mappings; not independently reviewed. Users must assess
#' suitability for their analysis. Matching uses the source description,
#' not the taxon. No provider request or database change is made.
#' September 2026 applicability was checked against identical native source
#' code-description pairs. Its tables identify June 2026 as the mapping evidence
#' release. June tables remain unchanged. Only explicitly listed releases work.
#'
#' @param ecotox_release Exact ECOTOX source release identifier. Required.
#' @return `lifestage_dictionary()` returns mapping evidence and derived fields.
#'   `lifestage_review()` returns unresolved decisions and evidence.
#'   `harmonize_lifestage()` returns the input rows, in order, with derived
#'   fields and portable mapping provenance. Unknown descriptions have missing
#'   derived fields and an explicit status.
#' @export
lifestage_dictionary <- function(ecotox_release) {
  metadata <- check_lifestage_release(ecotox_release)
  dictionary <- read_release_table(
    metadata$dictionary_table %||% 'lifestage_dictionary'
  )
  if (
    anyNA(dictionary$org_lifestage) || anyDuplicated(dictionary$org_lifestage)
  ) {
    stop(
      'Lifestage dictionary descriptions must be unique and non-missing.',
      call. = FALSE
    )
  }
  dictionary
}

check_lifestage_release <- function(ecotox_release) {
  metadata <- release_metadata()$ecotox
  expected <- c(metadata$ecotox_release, names(metadata$additional_releases))
  if (
    !is.character(ecotox_release) ||
      length(ecotox_release) != 1L ||
      is.na(ecotox_release) ||
      !ecotox_release %in% expected
  ) {
    stop(
      sprintf(
        'ECOTOX release mismatch: select the embedded release %s.',
        paste(expected, collapse = ', ')
      ),
      call. = FALSE
    )
  }
  if (identical(ecotox_release, metadata$ecotox_release)) {
    metadata
  } else {
    metadata$additional_releases[[ecotox_release]]
  }
}

#' @rdname lifestage_dictionary
#' @export
lifestage_review <- function(ecotox_release) {
  metadata <- check_lifestage_release(ecotox_release)
  read_release_table(metadata$review_table %||% 'lifestage_review')
}

#' @rdname lifestage_dictionary
#' @param x Data frame with `org_lifestage`. Source columns are preserved.
#'   Existing derived or mapping provenance columns are rejected.
#' @export
harmonize_lifestage <- function(x, ecotox_release) {
  dictionary <- lifestage_dictionary(ecotox_release)
  output <- c(
    'harmonized_life_stage',
    'reproductive_stage',
    'lifestage_match_status',
    'lifestage_artifact_version',
    'lifestage_review_status'
  )
  if (
    !is.data.frame(x) ||
      !'org_lifestage' %in% names(x) ||
      !(is.character(x$org_lifestage) || is.factor(x$org_lifestage)) ||
      anyDuplicated(names(x))
  ) {
    stop(
      'x must be a data frame with unique column names and a character org_lifestage column.',
      call. = FALSE
    )
  }
  if (any(output %in% names(x))) {
    stop(
      'Remove existing derived or lifestage provenance columns before harmonization.',
      call. = FALSE
    )
  }
  keys <- unique(as.character(x$org_lifestage))
  positions <- match(keys, dictionary$org_lifestage)
  row_positions <- positions[match(as.character(x$org_lifestage), keys)]
  x$harmonized_life_stage <- dictionary$harmonized_life_stage[row_positions]
  x$reproductive_stage <- dictionary$reproductive_stage[row_positions]
  review <- lifestage_review(ecotox_release)
  x$lifestage_match_status <- ifelse(
    !is.na(row_positions),
    'matched',
    ifelse(
      is.na(x$org_lifestage),
      'missing',
      ifelse(
        x$org_lifestage %in% review$org_lifestage,
        'unresolved',
        'unmatched'
      )
    )
  )
  x$lifestage_match_status <- as.character(x$lifestage_match_status)
  metadata <- check_lifestage_release(ecotox_release)
  x$lifestage_artifact_version <- rep(metadata$artifact_version, nrow(x))
  x$lifestage_review_status <- rep(metadata$review_status, nrow(x))
  x
}
