#' Computing the ecosystems' contribution to the provision of food and wood
#'
#' This function computes - using so-called eco-con-coefficients provided by the JRC of the European
#' Commission - the contribution of ecosystems in the provision of food and wood. Values are expressed in euro/ha (at the prices of `ref_yr`)
#' for each of the Italian NUTS 2 regions and each of the Corine 3 classes selected.
#'
#' @inheritParams compute_agr_area
#' @param corine_code A numeric vector. Vector of the Corine classes (level 3) for which the average
#'     area must be computed. Default is set to `211` (Non-irrigated arable land).
#' @param maes A character string. If can assume three values: `none`, `Cropland` and `Grasslands`. If it is set
#'     to `none` (the default), unit values are computed for each of the corine codes specified via the `corine_code`
#'     parameter. If either `Cropland` or `Grasslands` are specified, the unit value is computed aggregating all corine
#'     codes belonging to the MAES ecosystem specified.
#' @param ref_yr A numeric value. The year at which price levels monetary values must be expressed.
#' @return a Tibble with Corine 3 codes and labels, total unit value of agricultural production (three-year average
#' expressed in euro/ha), the `eco_con_coeff` used and the contribution of ecosystems to food provision computed
#' as the product between `unit_value` and `eco_con_coeff`.
#' @export
#' @examples
#'
#' provisioning_value(nuts = c("Umbria", "Puglia"),
#' last_yr = 2019, ref_yr = 2019, corine_code = c(211, 222, 231))
provisioning_value <- function(nuts = "Italia", h = 3, last_yr, ref_yr = 2019, corine_code = 211,
                               maes = "none",
                               lang = "it") {


  corine3_code <- value_label <- arable_land_codes <- maes_corine <- area_corine <-
    unit_value <- label <- corine3_label_en <- corine3_label_it <- NULL

  if (maes == "none") {

  cropland_codes <- intersect(corine_code, ecoservr::maes_corine[ecoservr::maes_corine$maes %in% c("Cropland", "Grasslands"), ]$corine3_code) # subvector of cropland codes

  forest_codes <- intersect(corine_code, ecoservr::maes_corine[ecoservr::maes_corine$maes == "Woodland and forest", ]$corine3_code) # subvector of forest codes
     # corine codes belonging to MAES cropland ecosystem

  other_codes <- setdiff(corine_code, ecoservr::maes_corine[is.element(ecoservr::maes_corine$maes, c("Cropland", "Grasslands", "Woodland and forest")), ]$corine3_code) # codes of corine classes not providing neither food nor wood

  } else { # ecosystem specific means

  cropland_codes <- unlist(lapply(maes,
                           function(x) ecoservr::maes_corine[ecoservr::maes_corine$maes == x, ]$corine3_code))

  forest_codes <- NULL

  other_codes <- NULL

  }

  if (length(cropland_codes) > 0) {
  # CROPLAND

  unit_values <- compute_agr_unit_values(nuts = nuts,
                                     h = h,
                                     last_yr = last_yr,
                                     ref_yr = ref_yr,
                                     corine_code = cropland_codes)

  if (lang == "it") {

    eco_con_coeff <- unique(ecoservr::master_table_agr[, c("value_label_it", "corine3_code", "eco_con_coeff")])

    eco_con_coeff$value_label <- eco_con_coeff$value_label_it

    eco_con_coeff$value_label_it <- NULL

  } else {

    eco_con_coeff <- unique(ecoservr::master_table_agr[, c("value_label_en", "corine3_code", "eco_con_coeff")])

    eco_con_coeff$value_label <- eco_con_coeff$value_label_en

    eco_con_coeff$value_label_en <- NULL

  }


  # creating a unique label to assign the correct eco_con coefficient

  arable_land_codes <- c(211, 212)

  eco_con_coeff$eco_con_label <- ifelse(
    eco_con_coeff$corine3_code %in% arable_land_codes,
    paste(eco_con_coeff$corine3_code, eco_con_coeff$value_label, sep = "_"),
    paste(eco_con_coeff$corine3_code, "NA", sep = "_")
  )

  eco_con_coeff <- transform(eco_con_coeff,
                             value_label = NULL,
                             corine3_code = NULL)

  eco_con_coeff <- unique(eco_con_coeff)

  eco_contribution <- unit_values %>%
    dplyr::mutate(eco_con_label = paste(corine3_code, value_label, sep = "_")) %>%
    dplyr::left_join(eco_con_coeff) %>%
    dplyr::mutate(eco_contribution = unit_value * eco_con_coeff)

  # Preparing final output

  eco_contr_cropland <- eco_contribution %>%
    dplyr::left_join(ecoservr::nuts2_codes) %>%
    dplyr::left_join(ecoservr::maes_corine) %>%
    dplyr::left_join(corine_area[, c("label", "corine3_code", "area_corine")]) %>%
    dplyr::select(label, corine3_code, corine3_label_en, area_corine,
                  corine3_label_it, maes, unit_value, eco_con_coeff, eco_contribution) %>%
    dplyr::mutate(eco_contribution = ifelse(is.na(area_corine),
                                            0,
                                            eco_contribution)) # if corine area is NA, no ecosystem service is provided

  if (maes != "none") {

    eco_contr_cropland <- eco_contr_cropland %>%
      dplyr::filter(!is.na(area_corine)) %>% # removing NAs (corine classes not present in the regin considered)
      dplyr::group_by(label, maes) %>%
      dplyr::summarise(eco_contribution = stats::weighted.mean(eco_contribution, area_corine)) %>%
      dplyr::ungroup()

  }

  } else {

    eco_contr_cropland <- NULL

  }


  if (length(forest_codes) > 0) {

  ### FOREST

  eco_contr_forest <- compute_eco_con_forest(ref_yr = ref_yr)

  eco_contr_forest <- eco_contr_forest %>%
    dplyr::slice(rep(1:dplyr::n(), each = length(forest_codes)))

  eco_contr_forest$corine3_code <- forest_codes

  eco_contr_forest <- eco_contr_forest %>%
    dplyr::left_join(ecoservr::maes_corine) %>%
    dplyr::select(label, corine3_code, corine3_label_en, corine3_label_it,
                  maes, unit_value, eco_con_coeff, eco_contribution)

  } else {

    eco_contr_forest <- NULL

  }

  if (length(other_codes) > 0) {

    eco_contr_other <- dplyr::tibble(label = nuts,
                                     unit_value = 0,
                                     eco_con_coeff = 0,
                                     eco_contribution = 0)

    eco_contr_other <- eco_contr_other %>%
      dplyr::slice(rep(1:dplyr::n(), each = length(other_codes)))

    eco_contr_other$corine3_code <- other_codes

    eco_contr_other <- eco_contr_other %>%
      dplyr::left_join(ecoservr::maes_corine) %>%
      dplyr::select(label, corine3_code, corine3_label_en, corine3_label_it, maes, unit_value, eco_con_coeff, eco_contribution)

  } else {

    eco_contr_other <- NULL

  }

  if (maes == "none") {

    dplyr::bind_rows(eco_contr_cropland, eco_contr_forest, eco_contr_other)

  } else {

    eco_contr_cropland

  }


}
