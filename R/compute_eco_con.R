#' Computing the ecosystems' contribution to food provisioning
#'
#' This function computes - using so-called eco-con-coefficients provided by the JRC of the European
#' Commission - the contribution of ecosystems in the provision of food, as a share of the total
#' value of agricultural production. Values are expressed in euro/ha (at the prices of `last_yr`) for each
#' of the Italian NUTS 2 regions and each of the Corine 3 classes selected.
#'
#' @inheritParams compute_area
#' @return a Tibble with Corine 3 codes and labels, total unit value of agricultural production (three-year average
#' expressed in euro/ha), the `eco_con_coeff` used and the contribution of ecosystems to food provision computed
#' as the product between `unit_value` and `eco_con_coeff`.
#' @export
#' @examples
#'
#' compute_eco_con(nuts = c("Umbria", "Puglia"), last_yr = 2019, corine_code = c("2.1.1", "2.2.2", "2.3.1"))
compute_eco_con <- function(nuts = "Italia", h = 3, last_yr, corine_code) {

  unit_values <- compute_unit_values(nuts = nuts,
                                     h = h,
                                     last_yr = last_yr,
                                     corine_code = corine_code)

  eco_con_coeff <- unique(master_table[, c("value_label", "corine3_code", "eco_con_coeff")])

  # creating a unique label to assign the correct eco_con coefficient

  eco_con_coeff$eco_con_label <- ifelse(
    eco_con_coeff$corine3_code == "2.1.1",
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

  corine_labels <- unique(master_table[, c("corine3_code", "corine3_label")])

  eco_contribution %>%
    dplyr::left_join(corine_labels) %>%
    dplyr::select(corine3_code, corine3_label, unit_value, eco_con_coeff, eco_contribution)


}
