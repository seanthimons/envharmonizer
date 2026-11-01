#' Explicit offline ECOTOX lifestage mapping
#'
#' Experimental mappings; not independently reviewed. Users must assess
#' suitability for their analysis. Matching uses the source description and
#' the ECOTOX taxon group (`eco_group`), because some descriptions mean
#' different stages in different taxa. No provider request or database change
#' is made. Requires lifestage tables with schema 2.x; older tables are refused.
#' September 2026 applicability was checked against identical native source
#' code-description pairs. Its tables identify June 2026 as the mapping evidence
#' release. Only explicitly listed releases work.
#'
#' @param ecotox_release Exact ECOTOX source release identifier. Required.
#' @return `lifestage_dictionary()` returns mapping evidence and derived fields,
#'   one row per `org_lifestage` and `eco_group`.
#'   `lifestage_species_groups()` returns one `eco_group` per ECOTOX
#'   `species_number`.
#'   `lifestage_review()` returns unresolved decisions and evidence.
#'   `harmonize_lifestage()` returns the input rows, in order, with derived
#'   fields and portable mapping provenance. `lifestage_eco_group` records the
#'   group used for the join. Unknown descriptions have missing derived fields
#'   and an explicit status.
#' @export
lifestage_dictionary <- function(ecotox_release) {
  metadata <- check_lifestage_schema(ecotox_release)
  dictionary <- read_release_table(
    metadata$dictionary_table %||% 'lifestage_dictionary'
  )
  if (
    !'eco_group' %in% names(dictionary) ||
      anyNA(dictionary[c('org_lifestage', 'eco_group')]) ||
      !all(dictionary$eco_group %in% ECO_GROUPS) ||
      anyDuplicated(dictionary[c('org_lifestage', 'eco_group')]) ||
      nrow(dictionary) !=
        length(ECO_GROUPS) * length(unique(dictionary$org_lifestage))
  ) {
    stop(
      paste(
        'Lifestage dictionary must have exactly one non-missing row per',
        'org_lifestage for each known eco_group.'
      ),
      call. = FALSE
    )
  }
  dictionary
}

# ECOTOX taxon groups of lifestage schema 2 (ComptoxR ecotox.duckdb
# species.eco_group). Missing or other values join as 'Unknown'.
ECO_GROUPS <- c(
  'Insects/Spiders', 'Flowers/Trees/Shrubs/Ferns', 'Fungi', 'Algae', 'Fish',
  'Crustaceans', 'Invertebrates', 'Worms', 'Molluscs', 'Birds', 'Mammals',
  'Amphibians', 'Reptiles', 'Moss/Hornworts', 'Bees', 'Miscellaneous',
  'Unknown'
)

check_lifestage_schema <- function(ecotox_release) {
  metadata <- check_lifestage_release(ecotox_release)
  schema <- metadata$schema_version %||% '1.0.0'
  if (numeric_version(schema)[[1, 1]] != 2L) {
    stop(
      sprintf(
        paste(
          'Lifestage tables for %s use schema %s; this envharmonizer needs',
          'schema 2.x (one row per org_lifestage and eco_group). Joining them',
          'would mis-assign taxon-specific stages. Update the embedded data.'
        ),
        ecotox_release, schema
      ),
      call. = FALSE
    )
  }
  metadata
}

#' @rdname lifestage_dictionary
#' @export
lifestage_species_groups <- function(ecotox_release) {
  check_lifestage_schema(ecotox_release)
  metadata <- release_metadata()$ecotox$species_groups
  if (!ecotox_release %in% unlist(metadata$applies_to)) {
    stop(
      sprintf('No species groups are embedded for %s.', ecotox_release),
      call. = FALSE
    )
  }
  # ponytail: the content hash is checked by the build and by the tests, not
  # here; checking it at run time would add digest as a dependency.
  groups <- read_release_table(metadata$table %||% 'lifestage_species_groups')
  if (
    !is.character(groups$species_number) ||
      anyNA(groups$species_number) ||
      anyDuplicated(groups$species_number) ||
      !all(groups$eco_group %in% ECO_GROUPS)
  ) {
    stop(
      paste(
        'Lifestage species groups must have one known eco_group per unique,',
        'non-missing species_number.'
      ),
      call. = FALSE
    )
  }
  groups
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
#' @param x Data frame with `org_lifestage` and either `eco_group`, the ECOTOX
#'   species group of each row, or `species_number`, which is looked up in
#'   `lifestage_species_groups()`. `eco_group` is used when both are present.
#'   Missing, unrecognized or unlisted groups use the `Unknown` mapping. Source
#'   columns are preserved. Existing derived or mapping provenance columns are
#'   rejected.
#' @export
harmonize_lifestage <- function(x, ecotox_release) {
  dictionary <- lifestage_dictionary(ecotox_release)
  output <- c(
    'harmonized_life_stage',
    'reproductive_stage',
    'lifestage_match_status',
    'lifestage_eco_group',
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
      paste(
        'x must be a data frame with unique column names and a character',
        'org_lifestage column.'
      ),
      call. = FALSE
    )
  }
  if ('eco_group' %in% names(x)) {
    group <- x$eco_group
  } else if ('species_number' %in% names(x)) {
    species <- x$species_number
    if (is.numeric(species)) {
      if (any(species != trunc(species), na.rm = TRUE)) {
        stop('species_number must be whole numbers.', call. = FALSE)
      }
      species <- ifelse(is.na(species), NA, sprintf('%.0f', species))
    }
    groups <- lifestage_species_groups(ecotox_release)
    group <- groups$eco_group[match(as.character(species), groups$species_number)]
  } else {
    group <- NULL
  }
  if (
    is.null(group) ||
      !(is.character(group) || is.factor(group) || all(is.na(group)))
  ) {
    stop(
      paste(
        'x needs a character eco_group column or a species_number column:',
        'mappings are taxon-specific.'
      ),
      call. = FALSE
    )
  }
  if (any(output %in% names(x))) {
    stop(
      'Remove existing derived or lifestage provenance columns before harmonization.',
      call. = FALSE
    )
  }
  group <- as.character(group)
  group[!group %in% ECO_GROUPS] <- 'Unknown'
  key <- paste(x$org_lifestage, group, sep = '\u001f')
  key[is.na(x$org_lifestage)] <- NA
  row_positions <- match(
    key,
    paste(dictionary$org_lifestage, dictionary$eco_group, sep = '\u001f')
  )
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
  x$lifestage_eco_group <- group
  metadata <- check_lifestage_release(ecotox_release)
  x$lifestage_artifact_version <- rep(metadata$artifact_version, nrow(x))
  x$lifestage_review_status <- rep(metadata$review_status, nrow(x))
  x
}
