test_that('stored June metadata is unchanged by the September release', {
  skip_if(embedded_lifestage_v2())
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
})

test_that('September applicability preserves June and strict release matching', {
  testthat::skip_if_not_installed('httr2')
  local_lifestage_v2()
  baseline <- readRDS(testthat::test_path('fixtures', 'lifestage-baseline.rds'))
  key <- c('org_lifestage', 'eco_group')
  june_dictionary <- lifestage_dictionary(june)
  dictionary <- lifestage_dictionary(september)
  expect_true(all(dictionary$ecotox_release == september))
  expect_true(all(dictionary$mapping_evidence_release == june))
  expect_identical(dictionary[key], june_dictionary[key])
  expect_identical(dictionary$harmonized_life_stage, june_dictionary$harmonized_life_stage)
  expect_identical(dictionary$reproductive_stage, june_dictionary$reproductive_stage)
  input <- baseline$input
  input$organism_lifestage <- rep(c('AD', NA_character_), length.out = nrow(input))
  input$eco_group <- 'Fish'
  testthat::local_mocked_bindings(
    req_perform = function(...) stop('offline only'),
    .package = 'httr2'
  )
  actual <- harmonize_lifestage(input, september)
  expect_identical(actual[names(input)], input)
  # Schema 1 values; schema 2 data is compared to its own baseline below.
  if (!embedded_lifestage_v2()) {
    expect_identical(actual$harmonized_life_stage, baseline$expected$harmonized_life_stage)
    expect_identical(actual$reproductive_stage, baseline$expected$reproductive_stage)
  }
  ecotox <- release_metadata()$ecotox
  expect_true(all(
    actual$lifestage_artifact_version ==
      ecotox$additional_releases[[september]]$artifact_version
  ))
  expect_true(all(
    harmonize_lifestage(input, june)$lifestage_artifact_version ==
      ecotox$artifact_version
  ))
  expect_error(harmonize_lifestage(input, 'ecotox_ascii_12_15_2026.zip'), 'mismatch')
  expect_error(lifestage_review('latest'), 'mismatch')
  empty <- harmonize_lifestage(input[FALSE, ], september)
  expect_type(empty$lifestage_match_status, 'character')
  expect_type(empty$reproductive_stage, 'logical')
})

test_that('native September code-description rows match June in every group', {
  testthat::skip_if_not_installed('readr')
  local_lifestage_v2()
  codes <- readr::read_csv(
    testthat::test_path('fixtures', 'september-codes.csv'),
    col_types = 'cc',
    show_col_types = FALSE
  )
  input <- data.frame(
    organism_lifestage = rep(codes$code, each = length(ECO_GROUPS)),
    org_lifestage = rep(codes$description, each = length(ECO_GROUPS)),
    eco_group = ECO_GROUPS
  )
  output <- harmonize_lifestage(input, september)
  expect_identical(output[names(input)], input)
  june_output <- harmonize_lifestage(input, june)
  expect_identical(output$harmonized_life_stage, june_output$harmonized_life_stage)
  expect_identical(output$reproductive_stage, june_output$reproductive_stage)
})
