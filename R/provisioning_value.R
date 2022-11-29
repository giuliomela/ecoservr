#' Computing the ecosystems' contribution to the provision of food and wood
#'
#' This function computes - using so-called eco-con-coefficients provided by the JRC of the European
#' Commission - the contribution of ecosystems in the provision of food and wood. Values are expressed in euro/ha (at the prices of `ref_yr`)
#' for each of the Italian NUTS 2 regions and each of the Corine 3 classes selected.
#'
#' @inheritParams compute_agr_area
#' @param ref_yr A numeric value. The year at which price levels monetary values must be expressed.
#' @return a Tibble with Corine 3 codes and labels, total unit value of agricultural production (three-year average
#' expressed in euro/ha), the `eco_con_coeff` used and the contribution of ecosystems to food provision computed
#' as the product between `unit_value` and `eco_con_coeff`.
#' @export
#' @examples
#'
#' compute_eco_con(nuts = c("Umbria", "Puglia"), last_yr = 2019, ref_yr = 2019, corine_code = c("2.1.1", "2.2.2", "2.3.1"))
provisioning_value <- function(nuts = "Italia", h = 3, last_yr, ref_yr = 2019, corine_code) {

  cropland_codes <- intersect(corine_code, maes_corine[maes_corine$maes %in% c("Cropland", "Grasslands"), ]$corine3_code) # subvector of cropland codes

  forest_codes <- intersect(corine_code, maes_corine[maes_corine$maes == "Woodland and forest", ]$corine3_code) # subvector of forest codes
     # corine codes belonging to MAES cropland ecosystem

  other_codes <- setdiff(corine_code, maes_corine[is.element(maes_corine$maes, c("Cropland", "Grasslands", "Woodland and forest")), ]$corine3_code) # codes of corine classes not providing neither food nor wood

  if (length(cropland_codes) > 0) {
  # CROPLAND

  unit_values <- compute_agr_unit_values(nuts = nuts,
                                     h = h,
                                     last_yr = last_yr,
                                     ref_yr = ref_yr,
                                     corine_code = cropland_codes)

  eco_con_coeff <- unique(master_table_agr[, c("value_label", "corine3_code", "eco_con_coeff")])

  # creating a unique label to assign the correct eco_con coefficient

  eco_con_coeff$eco_con_label <- ifelse(
    eco_con_coeff$corine3_code == 211,
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
    dplyr::mutate(nuts = nuts) %>%
    dplyr::left_join(maes_corine) %>%
    dplyr::select(nuts, corine3_code, corine3_label, maes, unit_value, eco_con_coeff, eco_contribution)

  } else {

    eco_contr_cropland <- NULL

  }


  if (length(forest_codes) > 0) {

  ### FOREST

  eco_contr_forest <- compute_eco_con_forest(ref_yr = ref_yr)

  eco_contr_forest <- eco_contr_forest %>%
    dplyr::slice(rep(1:n(), each = length(forest_codes)))

  eco_contr_forest$corine3_code <- forest_codes

  eco_contr_forest <- eco_contr_forest %>%
    dplyr::left_join(maes_corine) %>%
    dplyr::select(nuts, corine3_code, corine3_label, maes, unit_value, eco_con_coeff, eco_contribution)

  } else {

    eco_contr_forest <- NULL

  }

  if (length(other_codes) > 0) {

    eco_contr_other <- dplyr::tibble(nuts = nuts,
                                     unit_value = 0,
                                     eco_con_coeff = 0,
                                     eco_contribution = 0)

    eco_contr_other <- eco_contr_other %>%
      dplyr::slice(rep(1:n(), each = length(other_codes)))

    eco_contr_other$corine3_code <- other_codes

    eco_contr_other <- eco_contr_other %>%
      dplyr::left_join(maes_corine) %>%
      dplyr::select(nuts, corine3_code, corine3_label, maes, unit_value, eco_con_coeff, eco_contribution)

  } else {

    eco_contr_other <- NULL

  }

    dplyr::bind_rows(eco_contr_cropland, eco_contr_forest, eco_contr_other)

}
