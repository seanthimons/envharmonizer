# Copy a published R bundle's tables into inst/extdata/release/.
#
#   Rscript --vanilla scripts/sync-tables.R <extracted-rds-bundle-dir> [expected-data-version]
#
# The sync workflow has already verified the archive hash and vetted the
# archive (one top-level directory, no traversal or links). This script checks
# the extracted directory before copying anything: SHA256SUMS matches, the file
# set equals the tables named in release-manifest.json, the schema is one this
# code reads, and this package is new enough for the data. Content hashes
# (table_content_sha256) are verified in the build repo before publishing.

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args) %in% 1:2) stop('Usage: sync-tables.R <extracted-rds-bundle-dir> [expected-data-version]', call. = FALSE)
  bundle <- args[[1L]]
  target <- file.path('inst', 'extdata', 'release')
  if (!file.exists('DESCRIPTION') || !dir.exists(target)) {
    stop('Run from the package root.', call. = FALSE)
  }
  if (!is.function(tools::sha256sum)) stop('Needs R >= 4.5 for tools::sha256sum.', call. = FALSE)

  manifest_path <- file.path(bundle, 'release-manifest.json')
  if (!file.exists(manifest_path)) stop('No release-manifest.json in ', bundle, call. = FALSE)
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)

  tables <- c(
    names(manifest$table_content_sha256),
    names(manifest$ecotox$table_content_sha256),
    unlist(lapply(manifest$ecotox$additional_releases, function(r) names(r$table_content_sha256)))
  )
  if (anyDuplicated(tables)) stop('A table is listed twice in the manifest.', call. = FALSE)
  expected <- c(paste0(tables, '.rds'), 'release-manifest.json')

  sums_path <- file.path(bundle, 'SHA256SUMS')
  if (!file.exists(sums_path)) stop('No SHA256SUMS in ', bundle, call. = FALSE)
  lines <- readLines(sums_path)
  listed <- sub('^[0-9a-f]{64}  ', '', lines)
  hashes <- sub('^([0-9a-f]{64})  .*$', '\\1', lines)
  if (!all(grepl('^[0-9a-f]{64}  [^/]+$', lines))) stop('Malformed SHA256SUMS.', call. = FALSE)

  present <- setdiff(list.files(bundle, all.files = TRUE, no.. = TRUE), 'SHA256SUMS')
  problems <- c(
    if (length(setdiff(expected, present))) paste('missing:', paste(setdiff(expected, present), collapse = ', ')),
    if (length(setdiff(present, expected))) paste('not in manifest:', paste(setdiff(present, expected), collapse = ', ')),
    if (!setequal(listed, expected)) 'SHA256SUMS does not list exactly the bundle files'
  )
  if (!length(problems)) {
    actual <- unname(tools::sha256sum(file.path(bundle, listed)))
    bad <- listed[actual != hashes]
    if (length(bad)) problems <- paste('checksum mismatch:', paste(bad, collapse = ', '))
  }
  if (length(problems)) stop('Refusing to sync:\n  ', paste(problems, collapse = '\n  '), call. = FALSE)

  if (!is.character(manifest$data_version) || (length(args) == 2L && manifest$data_version != args[[2L]])) {
    stop('Manifest data_version ', format(manifest$data_version), ' does not match the release ', args[2L], '.', call. = FALSE)
  }
  source('R/accessors.R', local = TRUE)
  check_release_schema(manifest)
  min_version <- manifest$min_package_version
  version <- read.dcf('DESCRIPTION', fields = 'Version')[[1L]]
  if (!is.null(min_version) &&
        package_version(version) < package_version(min_version)) {
    stop('Data needs envharmonizer >= ', min_version, '; this is ', version, '.', call. = FALSE)
  }

  stale <- setdiff(list.files(target, pattern = '[.]rds$'), expected)
  unlink(file.path(target, stale))
  if (!all(file.copy(file.path(bundle, expected), target, overwrite = TRUE))) {
    stop('Failed to copy into ', target, call. = FALSE)
  }
  writeLines(manifest$data_version, 'data-version.txt')
  message(sprintf('Synced %d tables for data version %s.', length(tables), manifest$data_version))
}

main()
