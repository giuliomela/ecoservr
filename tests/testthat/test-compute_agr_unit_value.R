test_that("compute_agr_unit_value returns the right output", {

  nuts <- c("Italia", "Umbria")

  last_yr = 2019

  all_corine_codes <- unique(master_table_agr$corine3_code)

  res <- compute_agr_unit_values(nuts, last_yr = 2019, corine_code = all_corine_codes)

  expect_false(all(is.na(res$unit_value))) # al unit values are numbers, no NAs

  expect_true(all(is.element(res$corine3_code, all_corine_codes)))


})
