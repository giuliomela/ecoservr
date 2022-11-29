#' Compute landscape values for a specific corine land cover and maes ecosystem
#'
#' This function computes region-specific landscape values startin from literature values for
#' corine land cover classes and MAES ecosystems for which literature values are available.
#'
#' @param nuts A string vector. Italian NUTS to which the literature values must be transferred
#' @param include_forest A logical value. If `FALSE` (the default) the landscape value of urban areas is set to
#'     zero. This is because, due to the trees, the view is obstructed.
#' @param corine_code A vector of numeric values. Corine classes for which landscape value must be computed
#' @param policy_yr A Numeric value. The year to which the value transfer must be performed to
#' @param ref_yr A Numeric value. The year at which levels prices must be expressed
#' @param epsilon A numeric value. The income elasticity of willingness to pay for environmental goods.
#' @return A tibble contaning information on corine class, maes eocystem, original and transferred values and
#'     study and policy areas
#' @export
#' @examples
#' landscape_value(nuts = c("Italia", "Umbria"), corine_code = c(222, 321), policy_yr = 2019, ref_yr = 2019)
landscape_value <- function (nuts = "Italia", include_forest = FALSE, corine_code,
                             policy_yr, ref_yr, epsilon = 0.2){

  data_raw <- landscape_values_raw %>%
    dplyr::filter(corine3_code %in% corine_code) %>%
    dplyr::select(corine3_code, maes, value, study_area, study_year) # subsetting the original values dataset

  n_rows_raw <- nrow(data_raw)

  land_values <- NULL

  for (i in nuts) {

    land_values[[i]] <- transform(data_raw,
                                  policy_area = i)

  }

  land_values <- dplyr::bind_rows(land_values)

  # performing value transfer

  land_values$trans_fct <- mapply(function(x, y, z) {

      btransfer::btransfer_nuts2(study_site = x,
                                 policy_site = z,
                                 study_yr = y,
                                 policy_yr = policy_yr,
                                 ref_yr = ref_yr,
                                 epsilon = epsilon)

    },
    land_values$study_area,
    land_values$study_year,
    land_values$policy_area)

  land_values$trans_value <- land_values$value * land_values$trans_fct

  if (isFALSE(include_forest)) {

    land_values$trans_value <- replace(land_values$trans_value,
                                       land_values$maes == "Woodland and forest",
                                       0)
  }

  land_values

}

