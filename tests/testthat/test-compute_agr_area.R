test_that("compute_agr_area works properly", {

  nuts <- c("Italia", "Umbria")

  last_yr = 2019

  all_corine_codes <- unique(master_table_agr$corine3_code)

  res <- compute_agr_area(nuts = nuts, h = 3, last_yr, all_corine_codes)

  expect_true(is.data.frame(res)) # result is a dataframe

  expect_true(is.numeric(res$area)) # expect no NAs in the area column

  expect_true(all(is.element(res$corine3_code, all_corine_codes)))

})
