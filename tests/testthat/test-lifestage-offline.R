test_that('offline dictionary, review, and observable values preserve the baseline', {
  testthat::skip_if_not_installed('httr2')
  baseline <- readRDS(testthat::test_path('fixtures', 'lifestage-baseline.rds'))
  expect_identical(lifestage_dictionary(baseline$release), baseline$dictionary)
  expect_identical(lifestage_review(baseline$release), baseline$review)
  testthat::local_mocked_bindings(
    req_perform = function(...) stop('offline request'),
    .package = 'httr2'
  )
  actual <- harmonize_lifestage(baseline$input, baseline$release)
  expect_identical(actual$row_id, baseline$input$row_id)
  expect_identical(actual$org_lifestage, baseline$input$org_lifestage)
  expect_identical(
    actual$harmonized_life_stage,
    baseline$expected$harmonized_life_stage
  )
  expect_identical(
    actual$reproductive_stage,
    baseline$expected$reproductive_stage
  )
  expect_true(all(
    c('matched', 'unresolved', 'unmatched', 'missing') %in%
      actual$lifestage_match_status
  ))
  expect_true(all(
    actual$lifestage_review_status == 'not_independently_reviewed'
  ))
  expect_error(lifestage_dictionary('wrong-release'), 'mismatch')
  expect_error(
    harmonize_lifestage(actual, baseline$release),
    'existing derived'
  )
  expect_error(
    harmonize_lifestage(data.frame(org_lifestage = 1), baseline$release),
    'character'
  )
  empty <- harmonize_lifestage(baseline$input[FALSE, ], baseline$release)
  expect_equal(nrow(empty), 0L)
  expect_type(empty$reproductive_stage, 'logical')
  expect_type(empty$lifestage_match_status, 'character')
  source <- baseline$input
  source$organism_lifestage <- seq_len(nrow(source))
  expect_identical(
    harmonize_lifestage(source, baseline$release)$organism_lifestage,
    source$organism_lifestage
  )
})

test_that('duplicate keys fail and missing reproductive flags remain missing', {
  release <- release_metadata()$ecotox$ecotox_release
  dictionary <- lifestage_dictionary(release)
  original <- read_release_table
  testthat::local_mocked_bindings(read_release_table = function(name) {
    if (name == 'lifestage_dictionary') {
      rbind(dictionary, dictionary[1, ])
    } else {
      original(name)
    }
  })
  expect_error(lifestage_dictionary(release), 'unique')
  dictionary$reproductive_stage[1] <- NA
  testthat::local_mocked_bindings(read_release_table = function(name) {
    if (name == 'lifestage_dictionary') dictionary else original(name)
  })
  result <- harmonize_lifestage(
    data.frame(org_lifestage = dictionary$org_lifestage[1]),
    release
  )
  expect_identical(result$reproductive_stage, NA)
})
