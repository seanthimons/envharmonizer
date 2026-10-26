build_matrix_lookup_index <- function(synonyms) {
  split(seq_len(nrow(synonyms)), synonyms$normalized_label)
}

matrix_lookup_rows <- function(phrase, terms, synonyms, lookup_index = NULL) {
  key <- normalize_text(phrase)
  if (is.na(key)) {
    return(tibble::tibble())
  }
  if (is.null(lookup_index)) lookup_index <- build_matrix_lookup_index(synonyms)
  positions <- lookup_index[[key]] %||% integer()
  syn <- synonyms[positions, , drop = FALSE]
  if ("active" %in% names(syn)) {
    syn <- syn[is.na(syn$active) | syn$active, , drop = FALSE]
  }
  curated <- syn$source == "amosharmonizer"
  curated[is.na(curated)] <- FALSE
  if (any(curated)) syn <- syn[curated, , drop = FALSE]

  identity_ids <- terms$term_id[normalize_text(terms$preferred_label) == key]
  if (nrow(syn) == 0L && length(identity_ids)) {
    syn <- tibble::tibble(
      term_id = identity_ids,
      label = phrase,
      normalized_label = key,
      synonym_type = "preferred_label",
      source = terms$source_ontology[match(identity_ids, terms$term_id)],
      active = TRUE
    )
  }
  if (nrow(syn) == 0L) return(syn)
  syn <- dplyr::distinct(syn, term_id, .keep_all = TRUE)
  term_match <- match(syn$term_id, terms$term_id)
  for (column in setdiff(names(terms), "term_id")) {
    syn[[column]] <- terms[[column]][term_match]
  }
  # A matrix term cannot become an assertion target until its dominant phase
  # has been reviewed. Exact ENVO labels without that annotation stay queued.
  if ("dominant_physical_phase" %in% names(syn)) {
    syn <- syn[!is.na(syn$dominant_physical_phase) & nzchar(syn$dominant_physical_phase), , drop = FALSE]
  }
  syn
}

empty_matrix_result <- function() {
  tibble::tibble(
    input_id = integer(), raw_value = character(), term_id = character(),
    preferred_label = character(), physical_phase = character(),
    is_water_based = logical(), concert_unit_route = character(),
    match_type = character(), assertion_mode = character(),
    confidence_tier = character(), status = character()
  )
}

#' Harmonize analytical matrix strings
#'
#' Returns one row per matrix assertion. Compound coverage such as `soil and
#' water` therefore returns two rows; an unresolved phrase returns one row with
#' a missing `term_id`. Automatic matches are exact and may only target existing
#' ontology nodes.
#'
#' @param x Character vector of matrix strings.
#' @return A long-form tibble.
#' @export
harmonize_matrix <- function(x) {
  if (length(x) == 0L) return(empty_matrix_result())
  terms <- read_release_table("matrix_terms")
  synonyms <- read_release_table("matrix_synonyms")
  lookup_index <- build_matrix_lookup_index(synonyms)
  rows <- vector("list", length(x))

  for (i in seq_along(x)) {
    parsed <- parse_matrix_phrases(x[[i]])
    if (nrow(parsed) == 0L) {
      rows[[i]] <- tibble::tibble(
        input_id = as.integer(i), raw_value = as.character(x[[i]]), term_id = NA_character_,
        preferred_label = NA_character_, physical_phase = NA_character_,
        is_water_based = NA, concert_unit_route = NA_character_, match_type = NA_character_,
        assertion_mode = NA_character_, confidence_tier = "unresolved", status = "unresolved"
      )
      next
    }

    phrase_rows <- lapply(parsed$phrase, function(phrase) {
      matched <- matrix_lookup_rows(phrase, terms, synonyms, lookup_index)
      if (nrow(matched) == 0L) {
        return(tibble::tibble(
          term_id = NA_character_, preferred_label = NA_character_,
          dominant_physical_phase = NA_character_, is_water_based = NA,
          concert_unit_route = NA_character_, synonym_type = NA_character_,
          source = NA_character_, status = "unresolved"
        ))
      }
      matched$status <- if (nrow(matched) == 1L) "matched" else "ambiguous"
      matched
    })
    phrase_rows <- dplyr::bind_rows(phrase_rows, .id = "phrase_index")
    phrase_rows$phrase_index <- as.integer(phrase_rows$phrase_index)
    parsed_match <- parsed[phrase_rows$phrase_index, , drop = FALSE]
    reviewed <- phrase_rows$source == "amosharmonizer" |
      phrase_rows$synonym_type %in% c("reviewed_alias", "reviewed_decomposition") |
      startsWith(phrase_rows$term_id %||% "", "AMH:")
    reviewed[is.na(reviewed)] <- FALSE
    rows[[i]] <- tibble::tibble(
      input_id = rep(as.integer(i), nrow(phrase_rows)),
      raw_value = rep(as.character(x[[i]]), nrow(phrase_rows)),
      term_id = phrase_rows$term_id,
      preferred_label = phrase_rows$preferred_label,
      physical_phase = phrase_rows$dominant_physical_phase,
      is_water_based = as.logical(phrase_rows$is_water_based),
      concert_unit_route = phrase_rows$concert_unit_route,
      match_type = phrase_rows$synonym_type,
      assertion_mode = ifelse(is.na(phrase_rows$term_id), NA_character_, ifelse(reviewed, "reviewed", "auto")),
      confidence_tier = ifelse(phrase_rows$status == "matched", "high", phrase_rows$status),
      status = phrase_rows$status
    )
  }
  dplyr::bind_rows(rows)
}
