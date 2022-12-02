test_that("compute_eco_con_forest works", {

  nuts <- c("Italia", "Umbria")

  last_yr = 2019

  res <- compute_eco_con_forest(nuts, ref_yr = last_yr)

  expect_true(is.data.frame(res))

  expect_true(all(is.element(res$label, nuts2_codes$label)))

  expect_true(is.numeric(res$eco_contribution))


})
