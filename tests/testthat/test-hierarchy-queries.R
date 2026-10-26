test_that("matrix descendants resolve labels and retain method provenance", {
  by_label <- matrix_descendants("food", observed_only = TRUE)
  by_id <- matrix_descendants("AMH:0000032", observed_only = TRUE)
  expect_setequal(by_label$term_id, by_id$term_id)
  expect_true(all(by_label$term_id %in% amos_matrix_assertions()$term_id))
  expect_true(all(startsWith(by_label$path, "AMH:0000032")))
  expect_true(all(startsWith(by_label$path_labels, "food")))

  with_methods <- matrix_descendants("tissue", observed_only = TRUE, include_methods = TRUE)
  expect_true(all(!is.na(with_methods$amos_method_id)))
  expect_true(all(c("matrix_raw", "evidence_source", "method_title") %in% names(with_methods)))
  expect_error(matrix_descendants("not a published matrix"), "Unknown term")
})

test_that("published roots expose screenable matrix and analytical branches", {
  matrix_root <- matrix_roots()
  expect_equal(matrix_root$preferred_label, "analytical matrix")
  expect_equal(matrix_root$observed_terms, length(unique(amos_matrix_assertions()$term_id)))

  analytical_root <- analytical_roots()
  expect_true(all(c("spectroscopy", "detection method", "separation method") %in%
    analytical_root$preferred_label))
  expect_true(all(analytical_root$observed_terms > 0L))

  matrix_branches <- matrix_children("analytical matrix")
  expect_true(all(c("environmental material", "body-derived material") %in%
    matrix_branches$preferred_label))
  expect_true(all(matrix_branches$observed_terms > 0L))

  spectroscopy <- analytical_children("spectroscopy")
  expect_true(all(spectroscopy$observed_terms > 0L))

  separation <- analytical_descendants("separation method", observed_only = TRUE)
  expect_true(all(separation$term_id %in% amos_analytical_assertions()$term_id))
  with_methods <- analytical_descendants(
    "detection method", observed_only = TRUE, include_methods = TRUE
  )
  expect_true(all(!is.na(with_methods$amos_method_id)))
  expect_true("raw_value" %in% names(with_methods))
})

test_that("published query roots span all matrices without misclassifying plant material as food", {
  assertion_targets <- unique(amos_matrix_assertions()$term_id)
  all_media <- matrix_descendants("analytical matrix", observed_only = TRUE)
  expect_setequal(all_media$term_id, assertion_targets)

  food <- matrix_descendants("food")$term_id
  expect_true(all(c("FOODON:02020152", "FOODON:03411237") %in% food))
  expect_false("FOODON:00004331" %in% food)

  body <- matrix_descendants("body-derived material")$term_id
  expect_true(all(c("AMH:0000031", "ENVO:02000019", "AMH:0000041") %in% body))

  edges <- matrix_edges()
  egg_bridge <- edges[
    edges$child_id == "FOODON:02020152" & edges$parent_id == "AMH:0000032",
  ]
  expect_equal(egg_bridge$assertion_mode, "reviewed")
  expect_true(nzchar(egg_bridge$notes))
})
