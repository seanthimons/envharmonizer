# Read once at load time: find_release_dir() is mocked inside the tests, and
# re-reading it there would return the previous temporary manifest.
original_manifest <- jsonlite::read_json(
  file.path(find_release_dir(), 'release-manifest.json'),
  simplifyVector = FALSE
)

manifest_dir_with <- function(change, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  manifest <- change(original_manifest)
  jsonlite::write_json(
    manifest,
    file.path(dir, 'release-manifest.json'),
    auto_unbox = TRUE
  )
  dir
}

test_that('the embedded manifest records independent version fields', {
  metadata <- release_metadata()
  expect_identical(metadata$schema_version, 1L)
  semver <- '^[0-9]+\\.[0-9]+\\.[0-9]+$'
  expect_match(metadata$data_version, semver)
  expect_match(metadata$package_version, semver)
  expect_match(metadata$min_package_version, semver)
  expect_true(
    numeric_version(metadata$min_package_version) <=
      numeric_version(metadata$package_version)
  )
})

test_that('data with a newer schema is refused', {
  dir <- manifest_dir_with(function(m) {
    m$schema_version <- 2L
    m
  })
  testthat::local_mocked_bindings(find_release_dir = function() dir)
  expect_error(release_metadata(), 'schema version 2.*reads up to 1.*newer')
})

test_that('data without schema_version is read as the original schema', {
  dir <- manifest_dir_with(function(m) {
    m$schema_version <- NULL
    m
  })
  testthat::local_mocked_bindings(find_release_dir = function() dir)
  expect_no_error(release_metadata())
})

test_that('an invalid schema_version is rejected', {
  for (bad in list('one', 0L, 1.5, c(1L, 2L))) {
    dir <- manifest_dir_with(function(m) {
      m$schema_version <- bad
      m
    })
    testthat::local_mocked_bindings(find_release_dir = function() dir)
    expect_error(release_metadata(), 'invalid `schema_version`')
  }
})
