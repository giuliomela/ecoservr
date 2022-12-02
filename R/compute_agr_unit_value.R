#' Computing average unit values values of a given agricultural crop (according to the Corine land classification)
#'
#' This function computes the average unit values of a given (agricultural and grassland) Corine class
#' for each Italian level 2 NUTS regions. Values are expressed in euro/ha.
#'
#' @inheritParams compute_agr_area
#' @param ref_yr A numeric value. The year at which price levels monetary values must be expressed.
#' @return A tibble with NUTS2 codes, corine3 codes and average production value in the time frame specified
#'     (expressed in constat euro/ha, at the prices of `last_yr`).
#' @export
#' @examples
#'
#' compute_agr_unit_values(
#' nuts = c("Italia", "Umbria", "Puglia"),
#' h = 3,
#' last_yr = 2019,
#' corine_code = c(211, 222, 242)
#' )
#'
compute_agr_unit_values <- function (nuts = "Italia", h = 3, last_yr, ref_yr = 2019, corine_code) {

  area <- value <- NULL

  areas <- compute_agr_area(nuts = nuts,
                        h = h,
                        last_yr = last_yr,
                        corine_code = corine_code)

  values <- compute_agr_value(nuts = nuts,
                          h = h,
                          last_yr = last_yr,
                          ref_yr = ref_yr,
                          corine_code = corine_code)

  unit_values <- areas %>%
    dplyr::left_join(values) %>%
    dplyr::mutate(unit_value = ifelse(area == 0 | value == 0,
                                      0,
                                      value / area * 1000)
    )

  unit_values

}
