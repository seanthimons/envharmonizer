empty_analytical_result <- function() {
  tibble::tibble(
    input_id = integer(), raw_value = character(), term_id = character(),
    preferred_label = character(), analytical_role = character(),
    match_type = character(), assertion_mode = character(),
    confidence_tier = character(), status = character()
  )
}

build_analytical_lookup_index <- function(synonyms) {
  split(seq_len(nrow(synonyms)), synonyms$normalized_label)
}

analytical_lookup_rows <- function(value, terms, synonyms, lookup_index = NULL) {
  key <- normalize_text(value)
  if (is.na(key)) return(tibble::tibble())
  if (is.null(lookup_index)) lookup_index <- build_analytical_lookup_index(synonyms)
  positions <- lookup_index[[key]] %||% integer()
  rows <- synonyms[positions, , drop = FALSE]
  if ("active" %in% names(rows)) rows <- rows[is.na(rows$active) | rows$active, , drop = FALSE]
  curated <- rows$source == "amosharmonizer"
  curated[is.na(curated)] <- FALSE
  if (any(curated)) rows <- rows[curated, , drop = FALSE]
  if (nrow(rows) == 0L) {
    ids <- terms$term_id[normalize_text(terms$preferred_label) == key]
    rows <- tibble::tibble(
      term_id = ids, label = value, normalized_label = key,
      synonym_type = "preferred_label", source = "CHMO", active = TRUE,
      analytical_role = terms$analytical_role[match(ids, terms$term_id)],
      component_order = 1L, assertion_mode = "auto", confidence_tier = "high"
    )
  }
  rows <- dplyr::distinct(rows, term_id, analytical_role, .keep_all = TRUE)
  term_match <- match(rows$term_id, terms$term_id)
  rows$preferred_label <- terms$preferred_label[term_match]
  rows$analytical_role.term <- terms$analytical_role[term_match]
  rows
}

#' Harmonize AMOS methodology strings
#'
#' Exact CHMO composite matches are preferred. Decomposition occurs only for
#' labels with committed rules; arbitrary slash and hyphen strings are never
#' split.
#'
#' @param x Character vector of methodology strings.
#' @return A long-form tibble with analytical roles.
#' @export
harmonize_methodology <- function(x) {
  if (length(x) == 0L) return(empty_analytical_result())
  terms <- read_release_table("analytical_terms")
  synonyms <- read_release_table("analytical_synonyms")
  lookup_index <- build_analytical_lookup_index(synonyms)
  output <- vector("list", length(x))
  for (i in seq_along(x)) {
    matched <- analytical_lookup_rows(x[[i]], terms, synonyms, lookup_index)
    if (nrow(matched) == 0L) {
      output[[i]] <- tibble::tibble(
        input_id = as.integer(i), raw_value = as.character(x[[i]]), term_id = NA_character_,
        preferred_label = NA_character_, analytical_role = NA_character_, match_type = NA_character_,
        assertion_mode = NA_character_, confidence_tier = "unresolved", status = "queued"
      )
      next
    }
    status <- if (any(duplicated(paste(matched$term_id, matched$analytical_role)))) "ambiguous" else "matched"
    role <- matched$analytical_role
    if ("analytical_role.term" %in% names(matched)) {
      role[is.na(role) | !nzchar(role)] <- matched$analytical_role.term[is.na(role) | !nzchar(role)]
    }
    output[[i]] <- tibble::tibble(
      input_id = rep(as.integer(i), nrow(matched)), raw_value = rep(as.character(x[[i]]), nrow(matched)),
      term_id = matched$term_id, preferred_label = matched$preferred_label,
      analytical_role = role, match_type = matched$synonym_type,
      assertion_mode = matched$assertion_mode %||% ifelse(matched$source == "amosharmonizer", "reviewed", "auto"),
      confidence_tier = matched$confidence_tier %||% "high", status = status
    )
    if ("component_order" %in% names(matched)) {
      output[[i]] <- output[[i]][order(suppressWarnings(as.integer(matched$component_order)), na.last = TRUE), , drop = FALSE]
    }
  }
  dplyr::bind_rows(output)
}
