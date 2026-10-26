# Helpers shared by the consumer-facing lookup code and the build pipeline.
# Keep this file free of build-only dependencies (see ROADMAP-SPLIT.md, section 3).

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

normalize_text <- function(x) {
  out <- trimws(tolower(as.character(x)))
  dash_pattern <- paste0("[_", intToUtf8(c(0x2013, 0x2014)), "]+")
  out <- gsub(dash_pattern, "-", out)
  out <- gsub("\\s+", " ", out)
  out[is.na(x) | !nzchar(out)] <- NA_character_
  out
}

split_top_level <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(trimws(x))) return(character())
  chars <- strsplit(as.character(x), "", fixed = TRUE)[[1]]
  depth <- 0L
  current <- character()
  output <- character()
  flush <- function() {
    value <- trimws(paste(current, collapse = ""))
    if (nzchar(value)) output <<- c(output, value)
    current <<- character()
  }

  i <- 1L
  while (i <= length(chars)) {
    char <- chars[[i]]
    if (char == "(") depth <- depth + 1L
    if (char == ")") depth <- max(0L, depth - 1L)
    if (depth == 0L && char %in% c(",", ";", "/")) {
      flush()
      i <- i + 1L
      next
    }
    if (depth == 0L && i + 4L <= length(chars)) {
      lookahead <- tolower(paste(chars[i:min(length(chars), i + 4L)], collapse = ""))
      if (startsWith(lookahead, " and ")) {
        flush()
        i <- i + 5L
        next
      }
      if (startsWith(lookahead, " or ")) {
        flush()
        i <- i + 4L
        next
      }
    }
    current <- c(current, char)
    i <- i + 1L
  }
  flush()
  output
}

expand_parenthetical_matrix <- function(segment) {
  match <- regexec("^(.+?)\\s*\\(([^()]*)\\)\\s*$", segment, perl = TRUE)
  parts <- regmatches(segment, match)[[1]]
  if (length(parts) == 0L) {
    return(tibble::tibble(phrase = trimws(segment), qualifier = NA_character_))
  }
  base <- normalize_text(parts[[2]])
  contents <- trimws(strsplit(parts[[3]], ",", fixed = TRUE)[[1]])
  normalized_contents <- normalize_text(contents)

  water_values <- c(
    drinking = "drinking water", ground = "groundwater", groundwater = "groundwater",
    surface = "surface water", waste = "wastewater", wastewater = "wastewater",
    fresh = "freshwater", freshwater = "freshwater", marine = "marine water",
    sea = "marine water", salt = "marine water", tap = "tap water",
    distilled = "distilled water", deionized = "deionized water", deionised = "deionized water"
  )
  sediment_values <- c(fresh = "freshwater sediment", freshwater = "freshwater sediment", marine = "marine sediment")
  expansion <- if (identical(base, "water") && all(normalized_contents %in% names(water_values))) {
    unname(water_values[normalized_contents])
  } else if (identical(base, "sediment") && all(normalized_contents %in% names(sediment_values))) {
    unname(sediment_values[normalized_contents])
  } else {
    character()
  }
  if (length(expansion)) {
    return(tibble::tibble(phrase = expansion, qualifier = NA_character_))
  }
  tibble::tibble(phrase = parts[[2]], qualifier = paste(contents, collapse = "; "))
}

parse_matrix_phrases <- function(x) {
  segments <- split_top_level(x)
  if (length(segments) == 0L) {
    return(tibble::tibble(phrase = character(), qualifier = character(), segment_index = integer()))
  }
  rows <- lapply(seq_along(segments), function(i) {
    row <- expand_parenthetical_matrix(segments[[i]])
    row$segment_index <- i
    row
  })
  result <- dplyr::bind_rows(rows)
  result$phrase <- normalize_text(result$phrase)
  result <- result[!is.na(result$phrase), , drop = FALSE]
  dplyr::distinct(result, phrase, .keep_all = TRUE)
}

descendant_ids <- function(root_id, edges) {
  selected <- root_id
  repeat {
    children <- unique(edges$child_id[edges$parent_id %in% selected])
    expanded <- unique(c(selected, children))
    if (length(expanded) == length(selected)) break
    selected <- expanded
  }
  selected
}
