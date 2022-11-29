#' COmputing unit values of agricultural production in Italian NUTS2
#'
#' This function computes unit values of agricultural production to be used for the
#' calculation of the economic value of pollination services. Values are computed for crops that
#' benefit from pollination from insects. Crop categories are a special aggregation of crops
#' as defined by ISTAT and Eurostat, in order to reflect the classification used by Ispra to
#' quantify the ecosystems' contribution to crop pollination.
#'
#' @inheritParams compute_agr_area
#' @return A tibble with unit values, expressed at `ref_yr` prices, of the main crops
#'     that benefit from insect pollination for Italy and all Italian NUTS2 regions.
#'
compute_agr_value_pollination <- function (nuts = "Italia", ref_yr = 2019,
                                           last_yr = 2019, h = 3) {

  nuts_code <- nuts2_codes[nuts2_codes$label %in% nuts, ]$code

  period <- c((last_yr - h + 1):last_yr)

  # computing average areas in the chosen period

  areas <- crop_area_istat %>%
    dplyr::filter(year %in% period & label %in% nuts & rse_class != "tessili") %>%
    dplyr::group_by(label, rse_class) %>%
    dplyr::summarise(area = mean(area, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(nuts2_codes)

  # Downloading data from Eurostat

  estat_codes <- lapply(nuts_code, function (x) {
    paste0(c("Eurostat/agr_r_accts/A.PROD_BP.02110.MIO_EUR.", # rape
             "Eurostat/agr_r_accts/A.PROD_BP.02120.MIO_EUR.", # sunflower
             "Eurostat/agr_r_accts/A.PROD_BP.02130.MIO_EUR.", # soya
             "Eurostat/agr_r_accts/A.PROD_BP.02100.MIO_EUR.", # all oleaginous plants
             "Eurostat/agr_r_accts/A.PROD_BP.02200.MIO_EUR."), # pretein crops
           x)

  }) %>% unlist()

  estat_data <- rdbnomics::rdb(ids = estat_codes) %>%
    dplyr::select(geo, original_period, series_code, value)

  # computing values

  # downloading Italy's GDP deflator

  # downloading GDP deflator (ITALY)

  gdp_defl <- rdbnomics::rdb("Eurostat/nama_10_gdp/A.PD15_NAC.B1GQ.IT")[, c("original_period", "value")]

  names(gdp_defl) <- c("year", "defl")

  ref_yr_dfl <- gdp_defl[gdp_defl$year == as.character(ref_yr), ]$defl

  crop_value_istat_adj <- crop_value_istat %>%
    dplyr::left_join(gdp_defl) %>%
    dplyr::mutate(value = value / defl *ref_yr_dfl,
                  .keep = "unused") %>% # constant values at ref_yr prices
    dplyr::filter(year %in% period &
                    label %in% nuts)

  # crops which value is already in the ISTAT database

  crops_already_istat <- c("agrumi", "girasole", "mele", "pere", "pesche", "pomodoro", "soia")

  crop_value_1 <- crop_value_istat_adj %>%
    dplyr::filter(crop_label %in% crops_already_istat) %>%
    dplyr::group_by(label, crop_label) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() # production value of crops already in the Istat database

  ### Computing the value of other crop aggregations

  crop_value_istat_adj_agg <- crop_value_istat_adj %>%
    dplyr::mutate(value = ifelse(
      crop_label %in% c("pomodoro", "patate", "mele", "pere", "pesche", "colza", "girasole", "soia"),
      -value,
      value
    ))


  # Computing "altre ortive" total value of production

  crop_value_2 <- crop_value_istat_adj_agg %>%
    dplyr::filter(crop_label %in% c("patate", "pomodoro", "patate e ortaggi")) %>%
    dplyr::group_by(label, year) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(crop_label = "altre ortive")

  # Computing "altri frutti" total value of production

  crop_value_3 <- crop_value_istat_adj_agg %>%
    dplyr::filter(crop_label %in% c("mele", "pere", "pesche", "fruttiferi")) %>%
    dplyr::group_by(label, year) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(crop_label = "altri frutti")

  # Computing "altri semi da olio" total value of production

  # rapeseed and protein crops from Eurostat

  crop_value_4 <- estat_data %>%
    dplyr::filter(stringr::str_detect(series_code, "02110|02200") &
                    geo %in% nuts_code &
                    original_period %in% as.character(period)) %>%
    dplyr::left_join(gdp_defl, by = c("original_period" = "year")) %>%
    dplyr::mutate(value = value / defl * ref_yr_dfl, .keep = "unused") %>%
    dplyr::group_by(geo, series_code) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE) * 1000) %>% # value in 000 euro
    dplyr::ungroup() %>%
    dplyr::mutate(crop_label = ifelse(
      stringr::str_detect(series_code, "02110"),
      "colza",
      "legumi"
    ), series_code = NULL
    ) %>%
    dplyr::left_join(nuts2_codes, by = c("geo" = "code")) %>%
    dplyr::select(-geo)

  # other oilseed crops

  crop_value_5 <- estat_data %>%
    dplyr::filter(stringr::str_detect(series_code, "02110|02120|02130|02100") &
                    geo %in% nuts_code &
                    original_period %in% as.character(period)) %>%
    dplyr::left_join(gdp_defl, by = c("original_period" = "year")) %>%
    dplyr::mutate(value = value / defl * ref_yr_dfl, .keep = "unused") %>%
    dplyr::group_by(geo, series_code) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE) * 1000) %>% # value in 000 euro
    dplyr::ungroup() %>%
    dplyr::mutate(value = ifelse(
      stringr::str_detect(series_code, "02110|02120|02130"),
      - value,
      value
    )
    ) %>%
    dplyr::group_by(geo) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(crop_label = "altri semi da olio") %>%
    dplyr::left_join(nuts2_codes, by = c("geo" = "code")) %>%
    dplyr::select(-geo)

  # fiber crops

  # downloading total value of fiber producton (Italy) and cropped area

  fiber_unit_val_it <- rdbnomics::rdb(ids = c("Eurostat/aact_eaa01/A.02910.PROD_BP.MIO_EUR.IT", # prod value
                                              "Eurostat/apro_cpsh1/A.I2000.AR.IT")) %>% # areas
    dplyr::select("original_period", "value", "series_code") %>%
    tidyr::pivot_wider(names_from = "series_code", values_from = "value") %>%
    dplyr::left_join(gdp_defl, by = c("original_period" = "year")) %>%
    dplyr::mutate(value = A.02910.PROD_BP.MIO_EUR.IT / defl * ref_yr_dfl, .keep = "unused") %>% # taking inflation into account
    dplyr::filter(original_period %in% period) %>%
    dplyr::summarise(unit_value = mean(value, na.rm = TRUE) /
                                  mean(A.I2000.AR.IT, na.rm = TRUE) * 1000) %>%
    dplyr::pull(unit_value) # Unit value of fiber crops (euro/ha)

  fiber_unit_value <- dplyr::tibble(label = nuts) %>%
    dplyr::mutate(rse_class = "tessili",
                  unit_value = fiber_unit_val_it)

  #### Computing unit values for all crops and the selected NUTS to be used for pollination services valuation

  unit_values_pollination <- dplyr::bind_rows(crop_value_1, crop_value_2, crop_value_3, crop_value_4, crop_value_5) %>%
    dplyr::rename("rse_class" = "crop_label") %>%
    dplyr::left_join(areas) %>%
    dplyr::mutate(unit_value = value / area * 1000,
                  code = NULL,
                  .keep = "unused")  %>% # unit value in euro/ha, constant prices
   dplyr::bind_rows(fiber_unit_value)

  unit_values_pollination

}
