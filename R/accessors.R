find_release_dir <- function() {
  installed <- system.file("extdata", "release", package = "envharmonizer")
  if (nzchar(installed) && dir.exists(installed)) {
    return(installed)
  }

  current <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    candidate <- file.path(current, "inst", "extdata", "release")
    if (dir.exists(candidate)) {
      return(candidate)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  ""
}

read_release_table <- function(name) {
  release_dir <- find_release_dir()
  if (!nzchar(release_dir)) {
    stop("Embedded release directory not found. Reinstall envharmonizer.", call. = FALSE)
  }

  path <- file.path(release_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop(sprintf("Release table '%s' is not embedded.", name), call. = FALSE)
  }
  tibble::as_tibble(readRDS(path))
}

#' AMOS method manifest
#'
#' @return A tibble containing normalized AMOS method metadata.
#' @export
amos_methods <- function() {
  read_release_table("methods")
}

#' AMOS matrix assertions
#'
#' @return A long-form tibble linking AMOS methods to matrix terms.
#' @export
amos_matrix_assertions <- function() {
  read_release_table("method_matrices")
}

#' Matrix ontology terms
#'
#' @return A tibble of ENVO and reviewed local matrix terms.
#' @export
matrix_terms <- function() {
  read_release_table("matrix_terms")
}

#' Matrix ontology hierarchy
#'
#' `matrix_edges()` returns the published parent-child DAG.
#' `matrix_descendants()` resolves one exact preferred label or term identifier
#' and returns every path from that term to its descendants.
#'
#' @param x One matrix term identifier or exact preferred label.
#' @param observed_only Keep only terms used by an AMOS matrix assertion.
#' @param include_methods Join AMOS method and assertion provenance.
#' @return A tibble of edges or descendant paths and term metadata.
#' @export
matrix_edges <- function() {
  read_release_table("matrix_edges")
}

#' @rdname matrix_edges
#' @param observed_only Keep only roots or descendants represented by AMOS assertions.
#' @export
matrix_roots <- function(observed_only = TRUE) {
  hierarchy_roots(matrix_terms(), matrix_edges(), amos_matrix_assertions(), observed_only)
}

#' @rdname matrix_edges
#' @export
matrix_children <- function(x, observed_only = TRUE) {
  hierarchy_children(x, matrix_terms(), matrix_edges(), amos_matrix_assertions(), observed_only)
}

#' @rdname matrix_edges
#' @export
matrix_descendants <- function(x, observed_only = FALSE, include_methods = FALSE) {
  hierarchy_descendants(
    x, matrix_terms(), matrix_edges(), amos_matrix_assertions(),
    observed_only, include_methods
  )
}

resolve_hierarchy_term <- function(x, terms) {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`x` must be one non-empty term ID or preferred label.", call. = FALSE)
  }
  root <- terms$term_id[terms$term_id == x]
  if (length(root) == 0L) {
    root <- terms$term_id[normalize_text(terms$preferred_label) == normalize_text(x)]
  }
  if (length(root) == 0L) {
    stop(sprintf("Unknown term: %s", x), call. = FALSE)
  }
  if (length(root) > 1L) {
    stop(sprintf("Ambiguous label '%s': %s", x, paste(root, collapse = ", ")), call. = FALSE)
  }
  root
}

hierarchy_descendants <- function(x, terms, edges, assertions, observed_only, include_methods) {
  root <- resolve_hierarchy_term(x, terms)
  paths <- list(c(root))
  i <- 1L
  while (i <= length(paths)) {
    children <- edges$child_id[edges$parent_id == paths[[i]][[length(paths[[i]])]]]
    paths <- c(paths, lapply(children, function(child) c(paths[[i]], child)))
    i <- i + 1L
  }
  path_table <- tibble::tibble(
    term_id = vapply(paths, function(path) path[[length(path)]], character(1)),
    distance = lengths(paths) - 1L,
    path = vapply(paths, paste, character(1), collapse = " > "),
    path_labels = vapply(
      paths,
      function(path) paste(terms$preferred_label[match(path, terms$term_id)], collapse = " > "),
      character(1)
    )
  )
  result <- dplyr::left_join(path_table, terms, by = "term_id")
  if (observed_only) {
    result <- result[result$term_id %in% assertions$term_id, , drop = FALSE]
  }
  if (include_methods) {
    methods <- amos_methods()[, c("amos_method_id", "method_number", "method_title", "source_link")]
    result <- dplyr::left_join(
      result, assertions, by = "term_id", suffix = c("_term", "_assertion")
    )
    result <- dplyr::left_join(result, methods, by = "amos_method_id")
  }
  result$ancestor_id <- root
  result$ancestor_label <- terms$preferred_label[match(root, terms$term_id)]
  result[, c("ancestor_id", "ancestor_label", setdiff(names(result), c("ancestor_id", "ancestor_label")))]
}

hierarchy_roots <- function(terms, edges, assertions, observed_only) {
  roots <- setdiff(terms$term_id, edges$child_id)
  result <- hierarchy_summary(roots, terms, edges, assertions)
  covered <- attr(result, "covered")
  dominated <- vapply(seq_along(roots), function(i) {
    any(vapply(seq_along(roots), function(j) {
      i != j && length(covered[[j]]) > length(covered[[i]]) &&
        all(covered[[i]] %in% covered[[j]])
    }, logical(1)))
  }, logical(1))
  result <- result[!dominated, , drop = FALSE]
  attr(result, "covered") <- NULL
  if (observed_only) result <- result[result$observed_terms > 0L, , drop = FALSE]
  result[order(-result$method_count, result$preferred_label), , drop = FALSE]
}

hierarchy_children <- function(x, terms, edges, assertions, observed_only) {
  parent <- resolve_hierarchy_term(x, terms)
  children <- unique(edges$child_id[edges$parent_id == parent])
  result <- hierarchy_summary(children, terms, edges, assertions)
  attr(result, "covered") <- NULL
  if (observed_only) result <- result[result$observed_terms > 0L, , drop = FALSE]
  result$parent_id <- parent
  result$parent_label <- terms$preferred_label[match(parent, terms$term_id)]
  result <- result[, c("parent_id", "parent_label", setdiff(names(result), c("parent_id", "parent_label")))]
  result[order(-result$method_count, result$preferred_label), , drop = FALSE]
}

hierarchy_summary <- function(ids, terms, edges, assertions) {
  descendants <- lapply(ids, descendant_ids, edges = edges)
  used <- unique(assertions$term_id)
  covered <- lapply(descendants, intersect, y = used)
  observed_terms <- lengths(covered)
  method_count <- vapply(descendants, function(ids) {
    length(unique(assertions$amos_method_id[assertions$term_id %in% ids]))
  }, integer(1))
  result <- terms[match(ids, terms$term_id), , drop = FALSE]
  result$observed_terms <- observed_terms
  result$method_count <- method_count
  attr(result, "covered") <- covered
  result
}

#' AMOS analytical assertions
#'
#' @return A long-form tibble linking AMOS methods to CHMO terms.
#' @export
amos_analytical_assertions <- function() {
  read_release_table("method_analytical_assertions")
}

#' Analytical ontology terms
#'
#' @return A tibble of CHMO analytical terms and roles.
#' @export
analytical_terms <- function() {
  read_release_table("analytical_terms")
}

#' Analytical ontology hierarchy
#'
#' @param observed_only Keep only roots or descendants represented by AMOS assertions.
#' @param x One analytical term identifier or exact preferred label.
#' @param include_methods Join AMOS method and assertion provenance.
#' @return A tibble of roots, edges, or descendant paths and term metadata.
#' @export
analytical_edges <- function() {
  read_release_table("analytical_edges")
}

#' @rdname analytical_edges
#' @export
analytical_roots <- function(observed_only = TRUE) {
  hierarchy_roots(
    analytical_terms(), analytical_edges(), amos_analytical_assertions(), observed_only
  )
}

#' @rdname analytical_edges
#' @export
analytical_children <- function(x, observed_only = TRUE) {
  hierarchy_children(
    x, analytical_terms(), analytical_edges(), amos_analytical_assertions(), observed_only
  )
}

#' @rdname analytical_edges
#' @export
analytical_descendants <- function(x, observed_only = FALSE, include_methods = FALSE) {
  hierarchy_descendants(
    x, analytical_terms(), analytical_edges(), amos_analytical_assertions(),
    observed_only, include_methods
  )
}

#' Evidence-only instrument mentions
#'
#' @return A tibble of manufacturer or model text retained as evidence.
#' @export
instrument_mentions <- function() {
  read_release_table("instrument_mentions")
}

#' CONCERT compatibility map
#'
#' @return A reviewed compatibility tibble for pinned CONCERT imports.
#' @export
concert_media_map <- function() {
  read_release_table("concert_media_map")
}

# Highest release-manifest schema this code can read. Bump together with any
# column, type, or table change to the released tables.
SUPPORTED_SCHEMA_VERSION <- 1L

check_release_schema <- function(manifest, supported = SUPPORTED_SCHEMA_VERSION) {
  schema <- manifest$schema_version
  # Data built before `schema_version` existed is schema 1. A JSON null reads
  # as absent and is treated the same way.
  if (is.null(schema)) return(invisible(manifest))
  if (!is.numeric(schema) || length(schema) != 1L || is.na(schema) ||
        schema != trunc(schema) || schema < 1) {
    stop("Release manifest has an invalid `schema_version`.", call. = FALSE)
  }
  if (schema > supported) {
    stop(
      sprintf(
        "Release data uses schema version %d but this envharmonizer reads up to %d. Install a newer envharmonizer.",
        as.integer(schema), as.integer(supported)
      ),
      call. = FALSE
    )
  }
  invisible(manifest)
}

#' Release metadata
#'
#' Returns the manifest embedded with the release tables. Besides the build
#' provenance it records `schema_version` (the layout of the tables),
#' `data_version` (the tables release), `package_version` (the package that
#' embeds them) and `min_package_version` (the oldest package that can read
#' them). Data whose `schema_version` is newer than this package supports is
#' refused with an error asking for a newer package.
#'
#' @return The parsed release manifest as a named list.
#' @export
release_metadata <- function() {
  release_dir <- find_release_dir()
  path <- file.path(release_dir, "release-manifest.json")
  if (!file.exists(path)) {
    stop("Embedded release manifest not found. Reinstall envharmonizer.", call. = FALSE)
  }
  check_release_schema(jsonlite::read_json(path, simplifyVector = TRUE))
}
