test_that("capitalization variants resolve identically", {
  lower <- harmonize_methodology("gc/ms")
  upper <- harmonize_methodology("GC/MS")
  expect_equal(lower$term_id, upper$term_id)
  expect_equal(lower$analytical_role, upper$analytical_role)
})

test_that("GC/MS and LC/MS prefer whole CHMO methods", {
  result <- harmonize_methodology(c("GC/MS", "LC/MS"))
  expect_equal(result$term_id, c("CHMO:0000497", "CHMO:0000524"))
  expect_equal(result$analytical_role, c("overall_method", "overall_method"))
})

test_that("reviewed detector combinations expose components", {
  fid <- harmonize_methodology("GC/FID")
  expect_equal(fid$term_id, c("CHMO:0001736", "CHMO:0001002", "CHMO:0001719"))
  expect_equal(fid$analytical_role, c("overall_method", "separation", "detection"))

  ecd <- harmonize_methodology("GC/ECD")
  expect_equal(ecd$term_id, c("CHMO:0001002", "CHMO:0001717"))
  expect_equal(ecd$analytical_role, c("separation", "detection"))
})

test_that("arbitrary slash strings are queued rather than split", {
  result <- harmonize_methodology("NOVEL/THING")
  expect_equal(result$status, "queued")
  expect_true(is.na(result$term_id))
})

test_that("all observed AMOS methodology labels are mapped or queued", {
  raw <- envharmonizer:::read_release_table("method_methodologies_raw")
  labels <- sort(unique(raw$raw_value))
  result <- harmonize_methodology(labels)
  expect_true(length(labels) >= 167L)
  expect_true(all(result$status %in% c("matched", "ambiguous", "queued")))
})
