test_that("landscape_value works", {

  nuts <- c("Italia", "Umbria")

  last_yr = 2019

  all_corine_codes <- sort(unique(master_table_agr$corine3_code))

  res <- landscape_value(nuts, corine_code = all_corine_codes, policy_yr = last_yr,
                         ref_yr = last_yr)

  expect_true(is.data.frame(res))

  expect_true(is.numeric(res$trans_value))

  expect_false(all(is.na(res$trans_value)))

  expect_true(all(is.element(res$corine3_code, all_corine_codes)))

})
