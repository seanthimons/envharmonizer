test_that('lifestage tables other than schema 2.x are refused', {
  for (schema in c('1.0.0', '1.1.0', '3.0.0')) {
    local_lifestage_schema(schema)
    expect_error(lifestage_dictionary(june), 'schema 2.x')
    expect_error(
      harmonize_lifestage(data.frame(org_lifestage = 'Adult', eco_group = 'Fish'), september),
      'schema 2.x'
    )
    # The review table did not change shape and stays readable.
    expect_no_error(lifestage_review(june))
  }
})

test_that('stored schema 1 tables preserve the baseline', {
  skip_if(embedded_lifestage_v2())
  baseline <- readRDS(testthat::test_path('fixtures', 'lifestage-baseline.rds'))
  expect_identical(read_release_table('lifestage_dictionary'), baseline$dictionary)
  expect_identical(lifestage_review(baseline$release), baseline$review)
  expect_error(lifestage_dictionary(baseline$release), 'schema 2.x')
})

test_that('harmonization joins on description and eco group, offline', {
  testthat::skip_if_not_installed('httr2')
  local_lifestage_v2()
  baseline <- readRDS(testthat::test_path('fixtures', 'lifestage-baseline.rds'))
  testthat::local_mocked_bindings(
    req_perform = function(...) stop('offline request'),
    .package = 'httr2'
  )
  input <- baseline$input
  input$eco_group <- 'Fish'
  actual <- harmonize_lifestage(input, baseline$release)
  expect_identical(actual[names(input)], input)
  # Schema 1 values; schema 2 data is compared to its own baseline below.
  if (!embedded_lifestage_v2()) {
    expect_identical(actual$harmonized_life_stage, baseline$expected$harmonized_life_stage)
    expect_identical(actual$reproductive_stage, baseline$expected$reproductive_stage)
  }
  expect_true(all(
    c('matched', 'unresolved', 'unmatched', 'missing') %in%
      actual$lifestage_match_status
  ))
  expect_true(all(actual$lifestage_review_status == 'not_independently_reviewed'))

  hatch <- data.frame(
    org_lifestage = 'New, newly or recent hatch',
    eco_group = c('Fish', 'Birds', NA, 'birds', 'Birds; Standard Test Species')
  )
  result <- harmonize_lifestage(hatch, baseline$release)
  expect_equal(nrow(result), nrow(hatch))
  expect_identical(result$harmonized_life_stage[1:2], c('Larva', 'Juvenile'))
  expect_identical(result$lifestage_eco_group, c('Fish', 'Birds', rep('Unknown', 3)))
  expect_identical(result$eco_group, hatch$eco_group)
  factors <- hatch
  factors[] <- lapply(factors, factor)
  expect_identical(
    harmonize_lifestage(factors, baseline$release)$harmonized_life_stage,
    result$harmonized_life_stage
  )

  expect_error(lifestage_dictionary('wrong-release'), 'mismatch')
  expect_error(harmonize_lifestage(actual, baseline$release), 'existing derived')
  expect_error(
    harmonize_lifestage(data.frame(org_lifestage = 1, eco_group = 'Fish'), baseline$release),
    'character'
  )
  expect_error(
    harmonize_lifestage(baseline$input, baseline$release),
    'eco_group column or a species_number'
  )
  empty <- harmonize_lifestage(input[FALSE, ], baseline$release)
  expect_equal(nrow(empty), 0L)
  expect_type(empty$reproductive_stage, 'logical')
  expect_type(empty$lifestage_match_status, 'character')
})

test_that('dictionaries without one row per description and group fail', {
  local_lifestage_v2()
  dictionary <- lifestage_dictionary(june)
  broken <- list(
    rbind(dictionary, dictionary[1, ]),
    dictionary[-1, ],
    within(dictionary, eco_group[1] <- NA),
    within(dictionary, eco_group[1] <- 'Plankton'),
    dictionary[names(dictionary) != 'eco_group']
  )
  for (table in broken) {
    testthat::local_mocked_bindings(read_release_table = function(name) table)
    expect_error(lifestage_dictionary(june), 'exactly one')
  }
})

test_that('missing reproductive flags remain missing', {
  local_lifestage_v2()
  dictionary <- lifestage_dictionary(june)
  dictionary$reproductive_stage[1] <- NA
  original <- read_release_table
  testthat::local_mocked_bindings(read_release_table = function(name) {
    if (name == 'lifestage_dictionary') dictionary else original(name)
  })
  result <- harmonize_lifestage(
    data.frame(org_lifestage = dictionary$org_lifestage[1], eco_group = dictionary$eco_group[1]),
    june
  )
  expect_identical(result$reproductive_stage, NA)
})

test_that('embedded schema 2 data resolves taxon-specific stages', {
  skip_if_not(embedded_lifestage_v2())
  input <- data.frame(
    org_lifestage = c(rep('New, newly or recent hatch', 2), rep('Neonate', 2)),
    eco_group = c('Fish', 'Birds', 'Crustaceans', 'Insects/Spiders')
  )
  for (release in c(june, september)) {
    expect_identical(
      harmonize_lifestage(input, release)$harmonized_life_stage,
      c('Larva', 'Juvenile', 'Juvenile', 'Larva')
    )
  }
})

test_that('species_number looks up eco_group when eco_group is absent', {
  local_lifestage_v2()
  groups <- lifestage_species_groups(june)
  fish <- groups$species_number[groups$eco_group == 'Fish'][1]
  birds <- groups$species_number[groups$eco_group == 'Birds'][1]
  hatch <- 'New, newly or recent hatch'
  input <- data.frame(org_lifestage = hatch, species_number = c(fish, birds, NA, '999999999'))
  result <- harmonize_lifestage(input, september)
  expect_identical(result$lifestage_eco_group, c('Fish', 'Birds', 'Unknown', 'Unknown'))
  expect_identical(result$harmonized_life_stage[1:2], c('Larva', 'Juvenile'))
  expect_identical(result[names(input)], input)
  numeric <- data.frame(org_lifestage = hatch, species_number = as.numeric(c(fish, birds)))
  expect_identical(
    harmonize_lifestage(numeric, june)$lifestage_eco_group,
    c('Fish', 'Birds')
  )
  expect_error(
    harmonize_lifestage(data.frame(org_lifestage = hatch, species_number = 1.5), june),
    'whole numbers'
  )
  # eco_group wins when both are given.
  both <- data.frame(org_lifestage = hatch, eco_group = 'Fish', species_number = birds)
  expect_identical(harmonize_lifestage(both, june)$lifestage_eco_group, 'Fish')
  expect_error(
    harmonize_lifestage(data.frame(org_lifestage = hatch), june),
    'eco_group column or a species_number'
  )
})

test_that('species groups are validated and limited to listed releases', {
  local_lifestage_v2()
  groups <- lifestage_species_groups(june)
  broken <- list(
    rbind(groups, groups[1, ]),
    within(groups, species_number[1] <- NA),
    within(groups, eco_group[1] <- 'Plankton'),
    within(groups, species_number <- as.integer(species_number))
  )
  for (table in broken) {
    testthat::local_mocked_bindings(read_release_table = function(name) table)
    expect_error(lifestage_species_groups(june), 'one known eco_group')
  }
  metadata <- release_metadata()
  metadata$ecotox$species_groups$applies_to <- list(june)
  testthat::local_mocked_bindings(release_metadata = function() metadata)
  expect_error(lifestage_species_groups(september), 'No species groups')
})

test_that('embedded species groups match the manifest content hash', {
  skip_if_not(embedded_lifestage_v2())
  testthat::skip_if_not_installed('digest')
  metadata <- release_metadata()$ecotox$species_groups
  groups <- read_release_table('lifestage_species_groups')
  # The build hashes the columns after sorting by species_number.
  groups <- groups[order(groups$species_number, method = 'radix'), ]
  expect_identical(
    digest::digest(as.list(groups), algo = 'sha256'),
    metadata$table_content_sha256[['lifestage_species_groups']]
  )
})

test_that('embedded schema 2 data matches the build baseline', {
  skip_if_not(embedded_lifestage_v2())
  # Copied from envharmonizer-build reports/baseline.rds (data 0.2.0).
  baseline <- readRDS(testthat::test_path('fixtures', 'lifestage-baseline-v2.rds'))
  for (release in c(june, september)) {
    actual <- harmonize_lifestage(baseline$input, release)
    expect_identical(actual$harmonized_life_stage, baseline$expected$harmonized_life_stage)
    expect_identical(actual$reproductive_stage, baseline$expected$reproductive_stage)
  }
})
