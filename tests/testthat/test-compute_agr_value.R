test_that("compute_agr_value works", {

  nuts <- c("Italia", "Umbria")

  last_yr = 2019

  all_corine_codes <- sort(unique(master_table_agr$corine3_code))

  res <- compute_agr_value(nuts, last_yr = last_yr, corine_code = all_corine_codes)

  expect_true(is.numeric(res$value)) # no NAs in result

  expect_true(is.data.frame(res)) # result is a data frame

  expect_true(all(is.element(unique(res$code), nuts2_codes$code))) # all returned NUTS codes are correct

  expect_true(all(is.element(unique(res$corine3_code), all_corine_codes)))


})
