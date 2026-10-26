test_that('September applicability preserves June and strict release matching', {
  testthat::skip_if_not_installed('httr2')
  june <- 'ecotox_ascii_06_11_2026.zip'
  september <- 'ecotox_ascii_09_15_2026.zip'
  baseline <- readRDS(testthat::test_path('fixtures', 'lifestage-baseline.rds'))
  expect_null(release_metadata()$ecotox$reviewer)
  expect_null(release_metadata()$ecotox$review_date)
  june_metadata <- jsonlite::read_json(
    file.path(find_release_dir(), 'release-manifest.json'),
    simplifyVector = FALSE
  )$ecotox
  june_metadata$additional_releases <- NULL
  expect_identical(
    june_metadata,
    readRDS(testthat::test_path('fixtures', 'june-release-metadata.rds'))
  )
  expect_identical(lifestage_dictionary(june), baseline$dictionary)
  expect_identical(lifestage_review(june), baseline$review)
  dictionary <- lifestage_dictionary(september)
  expect_true(all(dictionary$ecotox_release == september))
  expect_true(all(dictionary$mapping_evidence_release == june))
  expect_identical(
    dictionary$harmonized_life_stage,
    baseline$dictionary$harmonized_life_stage
  )
  expect_identical(
    dictionary$reproductive_stage,
    baseline$dictionary$reproductive_stage
  )
  input <- baseline$input
  input$organism_lifestage <- rep(
    c('AD', NA_character_),
    length.out = nrow(input)
  )
  testthat::local_mocked_bindings(
    req_perform = function(...) stop('offline only'),
    .package = 'httr2'
  )
  actual <- harmonize_lifestage(input, september)
  expect_identical(actual[names(input)], input)
  expect_identical(
    actual$harmonized_life_stage,
    baseline$expected$harmonized_life_stage
  )
  expect_identical(
    actual$reproductive_stage,
    baseline$expected$reproductive_stage
  )
  expect_true(all(actual$lifestage_artifact_version == 'ecotox-v0.1.1'))
  expect_true(all(
    harmonize_lifestage(input, june)$lifestage_artifact_version ==
      'ecotox-v0.1.0'
  ))
  expect_error(
    harmonize_lifestage(input, 'ecotox_ascii_12_15_2026.zip'),
    'mismatch'
  )
  expect_error(lifestage_review('latest'), 'mismatch')
  empty <- harmonize_lifestage(input[FALSE, ], september)
  expect_type(empty$lifestage_match_status, 'character')
  expect_type(empty$reproductive_stage, 'logical')
})

test_that('native September code-description rows remain unchanged', {
  testthat::skip_if_not_installed('readr')
  codes <- readr::read_csv(
    testthat::test_path('fixtures', 'september-codes.csv'),
    col_types = 'cc',
    show_col_types = FALSE
  )
  input <- data.frame(
    organism_lifestage = codes$code,
    org_lifestage = codes$description
  )
  output <- harmonize_lifestage(input, 'ecotox_ascii_09_15_2026.zip')
  expect_identical(output[names(input)], input)
  june <- harmonize_lifestage(input, 'ecotox_ascii_06_11_2026.zip')
  expect_identical(output$harmonized_life_stage, june$harmonized_life_stage)
  expect_identical(output$reproductive_stage, june$reproductive_stage)
})
