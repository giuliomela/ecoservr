test_that("compute_agr_value_pollination works", {

  nuts <- c("Italia", "Umbria")

  last_yr = 2019

  all_rse_classes <- sort(unique(crop_area_istat$rse_class))

  res <- compute_agr_value_pollination(nuts)

  rse_classes <- sort(unique(res$rse_class))

  expect_identical(rse_classes, all_rse_classes)

  expect_true(is.data.frame(res))

  expect_true(is.numeric(res$unit_value)) # expect no NAs in the area column

  })
