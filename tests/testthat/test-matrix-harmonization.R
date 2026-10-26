test_that("compound matrix coverage produces separate phrases", {
  expect_equal(
    envharmonizer:::parse_matrix_phrases("soil and water")$phrase,
    c("soil", "water")
  )
  expect_equal(
    envharmonizer:::parse_matrix_phrases("fish, sludge, soil and water (waste)")$phrase,
    c("fish", "sludge", "soil", "wastewater")
  )
})

test_that("parenthetical enumerations and qualifiers remain distinct", {
  water <- envharmonizer:::parse_matrix_phrases("water (drinking, ground, surface)")
  expect_equal(water$phrase, c("drinking water", "groundwater", "surface water"))
  expect_true(all(is.na(water$qualifier)))

  soil <- envharmonizer:::parse_matrix_phrases("soil (sand, sandy clay loam)")
  expect_equal(nrow(soil), 1L)
  expect_equal(soil$phrase, "soil")
  expect_equal(soil$qualifier, "sand; sandy clay loam")
})

test_that("real mixtures remain one matrix term", {
  expect_equal(nrow(envharmonizer:::parse_matrix_phrases("mud")), 1L)
  expect_equal(nrow(envharmonizer:::parse_matrix_phrases("emulsion")), 1L)
  expect_equal(harmonize_matrix(c("mud", "emulsion"))$status, c("matched", "matched"))
})

test_that("harmonize_matrix is long-form and never creates unknown nodes", {
  result <- harmonize_matrix(c("soil and water", "unobtainium slurry"))
  expect_equal(result$term_id[result$input_id == 1L], c("ENVO:00001998", "ENVO:00002006"))
  expect_equal(result$status[result$input_id == 2L], "unresolved")
  expect_true(is.na(result$term_id[result$input_id == 2L]))
})
