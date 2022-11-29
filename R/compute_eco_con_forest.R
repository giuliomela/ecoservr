#' Computes the economic value of provisioning serivces from forest ecosystems
#'
#' This function computes the economic value of provisoning services (wood supply) provided by forest ecosystems
#' in Italian NUTS2 regions.
#'
#' @param nuts A character vector. The names of the the Italian NUTS2 regions of interest. Names
#'     must be in Italian and chosen among the folloqing list: `r knitr::combine_words(nuts2_codes$label)`.
#'     `Bolzano` and `Trento` are treated as separate regions. Data on `Italia` as a whole can be retrieved
#'     as well.
#' @param eco_con_timber A numeric value. The ecosystems contribution to wood suppl. It can range between 0 (no ecosystem
#'     contribution) and 1. The default value is `0.97`, the value calculated by the JRC see Vallecillo et al. (2018).
#' @param nai_value A numeric value. A factor (a number between 0 and 1) total forestry output must be multiplied by to
#'     obtain the economic value of the net annual increment of wood in Italian forests.Default value is `0.402` fo
#'     all Italian NUTS2 (no estimates at regional level are available).
#' @param ref_yr A numeric value. The year at which price levels monetary values must be expressed.
#' @param forestry_data_yr A numeric value. The year to which  latest data on Italian forests are available.
#'     The default is `2015`, the latest available year. This parameter is important because, since the latest
#'     available forest data are about that year, also economic values refer to the same year.
#' @seealso \url{https://www.inventarioforestale.org/it/}
compute_eco_con_forest <- function(nuts = "Italia", eco_con_timber = 0.97, nai_value = 0.402,
                                   ref_yr, forestry_data_yr = 2015) {

  if (any(!is.element(nuts, nuts2_codes$label)) == TRUE)
    stop(paste0("Please provide a valid NUTS2 name. Pick a name among: ",
                knitr::combine_words(nuts2_codes$label, and = "or ")))

  # downloading GDP deflator (ITALY)

  gdp_defl <- rdbnomics::rdb("Eurostat/nama_10_gdp/A.PD15_NAC.B1GQ.IT")[, c("original_period", "value")]

  names(gdp_defl) <- c("original_period", "defl")

  ref_yr_dfl <- gdp_defl[gdp_defl$original_period == as.character(ref_yr), ]$defl

  # Computing NAI value (using nai_share exogenous parameter)

  nai_value_ita <- val_prod_forest[val_prod_forest$label == "Italia" &
                                     val_prod_forest$year == forestry_data_yr, ]$value * nai_value

  # computing the economic value of one m3 of net annual increment on forest available for wood supply

  nai_ita_m3 <- nai_value_ita / forestry_data[forestry_data$label == "Italia", ]$nai_m3_faws * 10^6

  results <- forestry_data %>%
    dplyr::filter(label == nuts) %>%
    dplyr::mutate(nuts = nuts,
                  nai_euro = nai_m3_faws * nai_ita_m3,
                  unit_value = nai_euro / faws,
                  eco_contribution = unit_value * eco_con_timber,
                  eco_con_coeff = eco_con_timber) %>%
    dplyr::select(nuts, unit_value, eco_con_coeff, eco_contribution)

  results


}
