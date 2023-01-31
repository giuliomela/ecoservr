test_that("provisioning_value works", {

  nuts <- c("Italia", "Umbria")

  all_corine_codes <- unique(maes_corine[maes_corine$maes %in% c("Cropland", "Woodland and forest"), ]$corine3_code)

  all_corine_codes_no_212 <- all_corine_codes[-2]

  res <- provisioning_value(nuts, last_yr = 2019, corine_code = all_corine_codes_no_212)

  expect_true(is.data.frame(res))

  expect_true(all(res$label %in% nuts2_codes$label))

  expect_true(all(res$corine3_code %in% unique(maes_corine$corine3_code)))

  expect_true(all(is.numeric(res$eco_contribution)))

})
