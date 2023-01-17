#' Computing average unit values values of a given agricultural crop (according to the Corine land classification)
#'
#' This function computes the average unit values of a given (agricultural and grassland) Corine class
#' for each Italian level 2 NUTS regions. Values are expressed in euro/ha.
#'
#' @inheritParams compute_agr_area
#' @param corine_code A numeric vector. Vector of the Corine classes (level 3) for which the average
#'     area must be computed. Default is set to `211` (Non-irrigated arable land).
#' @param maes A character string. If can assume three values: `none`, `Cropland` and `Grasslands`. If it is set
#'     to `none` (the default), unit values are computed for each of the corine codes specified via the `corine_code`
#'     parameter. If either `Cropland` or `Grasslands` are specified, the unit value is computed aggregating all corine
#'     codes belonging to the MAES ecosystem specified.
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
compute_agr_unit_values <- function (nuts = "Italia", h = 3, last_yr, ref_yr = 2019, corine_code = 211,
                                     maes = "none", lang = "it") {

  area <- value <- code <- NULL

  if (!is.element(maes, c("none", "Grasslands", "Cropland"))) stop ("Please provide a valid MAES ecosystem name. For provisioning services either 'none', 'Cropland' or 'Grasslands' can be specified")

  if (maes != "none") {

    codes_to_download <- unique(ecoservr::master_table_agr[ecoservr::master_table_agr$maes == maes, ]$corine3_code)

  } else {

    codes_to_download <- corine_code

  }

  areas <- compute_agr_area(nuts = nuts,
                        h = h,
                        last_yr = last_yr,
                        corine_code = codes_to_download)

  values <- compute_agr_value(nuts = nuts,
                          h = h,
                          last_yr = last_yr,
                          ref_yr = ref_yr,
                          corine_code = codes_to_download)

  unit_values <- areas %>%
    dplyr::left_join(values)

  if (maes != "none") {

    unit_values <- unit_values %>%
      dplyr::group_by(code) %>%
      dplyr::summarise(dplyr::across(c(area, value), sum, na.rm = FALSE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(unit_value = ifelse(area == 0 | value == 0,
                                        0,
                                        value / area * 1000),
                    maes = maes
      )

  } else {

    unit_values <- unit_values %>%
      dplyr::mutate(unit_value = ifelse(area == 0 | value == 0,
                                        0,
                                        value / area * 1000)
      )

  }

  unit_values

}
