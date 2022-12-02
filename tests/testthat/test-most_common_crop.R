test_that("most_common_crop works", {

  nuts <- c("Italia", "Umbria")

  res <- most_common_crop(nuts, last_yr = 2019)

  expect_true(all(res$code %in% nuts2_codes$code))

  expect_false(all(is.na(res$value)))

  expect_false(all(is.na(res$value_label)))

})
