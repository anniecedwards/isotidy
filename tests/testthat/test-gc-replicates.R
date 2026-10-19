test_that("replicate summaries reduce repeated rows within each analysis", {
  replicate_fixture <- data.frame(
    run_id = "fixture_run",
    method_name = "fixture_method",
    sample_id = c("sample_A", "sample_A", "sample_A", "F8"),
    injection_id = c("1", "1", "1", "1"),
    analysis_no = c("analysis_1", "analysis_1", "analysis_2", "analysis_3"),
    element = "C",
    isotope = "C13",
    compound_id = "FA_1",
    is_external_standard = c(FALSE, FALSE, FALSE, TRUE),
    delta_value_final = c(8, 12, 14, 999)
  )
  gc_replicate_schema <- list(replicates = list(
    identify_by = "analysis_no",
    group_column = "sample_id"
  ))

  replicate_summary <- summarize_replicates(
    data = replicate_fixture,
    peripheral = "GC",
    schema = gc_replicate_schema,
    verbose = FALSE
  )

  expect_equal(nrow(replicate_summary), 1L)
  expect_equal(replicate_summary$sample_id, "sample_A")
  expect_equal(replicate_summary$n_replicates, 2L)
  expect_equal(replicate_summary$delta_value_final_mean, 12)
  expect_equal(replicate_summary$delta_value_final_sd, sqrt(8))
})
