#' Computes the economic value of pollination services for Italian NUTS2 regions.
#'
#' This function computes the economic value of the contribution of the pollination service to
#' agricultural production. Starting from the average value of the insect pollinated crops in a
#' given time horizon (user-defined) it computes the average value of the ecosystem service
#' "pollination" on the basis of the share of each insect pollinated crop on total regional
#' utilized agricultural area and crop-specific pollination contribution coeffcients (how much
#' of a crop's total output is due to pollinaton services) taking into account the fact
#' that not all of pollination demand is actually met (see table 4.5 in Vallecillo, S et al. 2018).
#'
#' @inheritParams compute_agr_area
#' @param pol_dependence A string. It allows the user to choose between crop-specific coefficients
#'     provided by either the JRS (`jrc`) or ISPRA (`ispra`) or a nation-wide average (still based on
#'     Ispra data: `ita_avg` of 10.4%).
#' @return A tibble with the economic value of pollination services in each of the NUTS2 selected.
#'     Values are expressed in euro/ha at constant prices (as specified through `ref_yr`).
#' @examples
#'
#' pollination_value(nuts = c("Italia", "Umbria"))
pollination_value <- function(nuts = "Italia", last_yr = 2019, h = 3, ref_yr = 2019,
                              pol_dependence = "ispra") {

  if(!pol_dependence %in% c("ispra", "jrc", "ita_avg"))
    stop("Please provide a valid value for 'pol_dependence': 'ipsra', 'jrc' or 'ita_avg'")

    # computing area shares
    area_shares <- compute_crop_shares(nuts = nuts, last_yr = last_yr)

    # computing unit values

    unit_values <- compute_agr_value_pollination(nuts = nuts,
                                                 ref_yr = ref_yr,
                                                 last_yr = last_yr,
                                                 h = h)

    all_data <- unit_values %>%
      dplyr::left_join(area_shares)

    if (pol_dependence == "ispra") {

      pollination_contribution <- all_data %>%
        dplyr::left_join(pollination_dependence) %>%
        dplyr::mutate(pollination_contribution = unit_value * area_share * dependence_ispra)


    } else if (pol_dependence == "jrc") {

      pollination_contribution <- all_data %>%
        dplyr::left_join(pollination_dependence) %>%
        dplyr::mutate(pollination_contribution = unit_value * area_share * dependence_jrc)

    } else {

      avg_ita <- pollination_dependence[pollination_dependence$rse_class == "all_crops", ]$dependence_ispra

      pollination_contribution <- all_data %>%
        dplyr::left_join(pollination_dependence) %>%
        dplyr::mutate(pollination_contribution = unit_value * area_share * avg_ita)

    }

    pollination_contribution <- pollination_contribution %>%
      dplyr::group_by(label) %>%
      dplyr::summarise(pollination_contribution = sum(pollination_contribution, na.rm = TRUE)) %>%
      dplyr::ungroup()

    pollination_contribution

}
