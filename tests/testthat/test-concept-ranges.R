test_that("plausibility ranges follow the observed data, not ricu copies", {
  cfg <- as.data.frame(readr::read_csv(
    system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"),
    show_col_types = FALSE))
  icp <- cfg[cfg$concept_name == "icp", ]
  expect_true(nrow(icp) > 0)
  expect_true(all(as.numeric(icp$min_value) <= -10))
  expect_true(all(as.numeric(icp$max_value) >= 100))
  cl <- cfg[cfg$concept_name == "chloride", ]
  expect_true(nrow(cl) > 0)
  expect_true(all(as.numeric(cl$min_value) <= 60))
  expect_true(all(as.numeric(cl$max_value) >= 160))
})
