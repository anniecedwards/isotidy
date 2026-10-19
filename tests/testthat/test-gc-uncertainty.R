.gc_uncertainty_fixture <- function(nacme_applied = FALSE) {
  dates <- as.Date(c("2026-05-12", "2026-05-13", "2026-05-14"))
  sample_ids <- c("Sample_A", "Sample_B", "Sample_C")
  measured_centers <- c(-25, -24, -23)
  data <- data.frame(
    injection_id = paste0(rep(sample_ids, each = 2), "_", rep(1:2, 3)),
    sample_id = rep(sample_ids, each = 2),
    compound_id = "Ala",
    start_time = rep(dates, each = 2),
    delta_value = as.vector(rbind(
      measured_centers - 0.1,
      measured_centers + 0.1
    )),
    delta_value_final = as.vector(rbind(
      measured_centers - 0.1,
      measured_centers + 0.1
    )) + rep(c(0, 0.5, 1), each = 2),
    is_external_standard = FALSE,
    is_derivatization_standard = FALSE
  )

  daily_offsets <- data.frame(
    correction_date = dates,
    delta_offset = c(0, 0.5, 1),
    standard_uncertainty = c(0.1, 0.1, 0.1),
    reference_uncertainty = c(0.05, 0.05, 0.05)
  )
  daily_reference_weights <- data.frame(
    correction_date = dates,
    compound_id = "Ala",
    reference_weight = 1,
    reference_delta = -30,
    reference_uncertainty = 0.05
  )

  derivatization_estimates <- if (nacme_applied) {
    data.frame(
      correction_date = dates,
      compound_id = "Ala",
      delta_derivatized_mean = -29,
      sigma_derivatized = 0.3,
      delta_known = -19.5,
      sigma_known = 0.15,
      AA_C_count = 3,
      NACME_C_count = 3
    )
  } else {
    data.frame()
  }

  list(
    data = data,
    daily_offsets = daily_offsets,
    daily_reference_weights = daily_reference_weights,
    derivatization_estimates = derivatization_estimates,
    nacme_correction_applied = nacme_applied
  )
}

test_that("GC slope uncertainty propagates calibration and replicate error", {
  result <- .gc_uncertainty_fixture()
  points <- data.frame(
    sample_id = c("Sample_A", "Sample_B", "Sample_C"),
    compound_id = "Ala",
    timepoint = 0:2
  )

  set.seed(104)
  propagated <- propagate_gc_slope_uncertainty(
    result = result,
    points = points,
    x_column = "timepoint",
    nsim = 500
  )

  expect_false(propagated$nacme_applied)
  expect_equal(nrow(propagated$point_estimates), 3L)
  expect_true(all(is.finite(propagated$point_estimates$standard_uncertainty)))
  expect_true(all(is.finite(propagated$y_draws)))
  expect_true(is.finite(propagated$slope$estimate))
  expect_gt(propagated$slope$standard_uncertainty, 0)
})

test_that("GC slope uncertainty includes NACME correction only when applied", {
  result <- .gc_uncertainty_fixture(nacme_applied = TRUE)
  points <- data.frame(
    sample_id = c("Sample_A", "Sample_B", "Sample_C"),
    compound_id = "Ala",
    timepoint = 0:2
  )

  set.seed(105)
  propagated <- propagate_gc_slope_uncertainty(
    result = result,
    points = points,
    x_column = "timepoint",
    nsim = 500
  )

  expect_true(propagated$nacme_applied)
  expect_true(all(is.finite(propagated$point_estimates$estimate)))
  expect_true(all(is.finite(propagated$point_estimates$standard_uncertainty)))
  expect_true(all(is.finite(propagated$y_draws)))
  expect_gt(propagated$slope$standard_uncertainty, 0)
})
