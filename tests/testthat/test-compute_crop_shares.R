test_that("compute_crop_shares works", {

  nuts <- c("Italia", "Umbria")

  last_yr = 2019

  res <- compute_crop_shares(nuts)

  all_rse_classes <- unique(crop_area_istat$rse_class)

  expect_true(is.numeric(res$area_share)) # no NAs in result

  expect_true(is.data.frame(res)) # result is a data frame

  expect_true(all(is.element(unique(res$code), nuts2_codes$code))) # all returned NUTS codes are correct

  expect_true(all(is.element(unique(res$rse_class), all_rse_classes)))

  })
