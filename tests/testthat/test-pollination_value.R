test_that("pollination_value works", {

  nuts <- c("Italia", "Umbria")

  res <- pollination_value(nuts)

  expect_true(is.data.frame(res))

  expect_true(all(res$label %in% nuts2_codes$label))

  expect_false(all(is.na(res$pollination_contribution)))


})
