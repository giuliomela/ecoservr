#' Computing average production values of a given agricultural crop (Corine classification)
#'
#' This function computes the average value of production (at base prices) of a given (agricultural and grassland) Corine class
#' for each Italian level 2 NUTS regions.
#'
#' @inheritParams compute_agr_area
#' @param ref_yr A numeric value. The year at which price levels monetary values must be expressed.
#' @return A tibble with NUTS2 codes, corine3 codes and average production value in the time frame specified
#'     (expressed in constat million euro, at the prices of `last_yr`).
#'
compute_agr_value <- function(nuts = "Italia", h = 3, last_yr, ref_yr = 2019, corine_code) {

  code <- original_period <- corine3_code <- defl <- value_label <- value_code <- unit <- fct <- NULL

  if (212 %in% corine_code)
    stop("The Corine 3 class '212' (Permanently irrigated land) cannot be selected. Please use '211' (non-irrigated arable land) instead.")

  geo <- nuts2_codes[nuts2_codes$label %in% nuts, ]$code

  # downloading GDP deflator (ITALY)

  gdp_defl <- rdbnomics::rdb("Eurostat/nama_10_gdp/A.PD15_NAC.B1GQ.IT")[, c("original_period", "value")]

  names(gdp_defl) <- c("original_period", "defl")

  ref_yr_dfl <- gdp_defl[gdp_defl$original_period == as.character(ref_yr), ]$defl


  if (!is.element(211, corine_code) | 211 %in% corine_code & length(corine_code) > 1) {

  metadata <- master_table_agr[master_table_agr$corine3_code %in% corine_code[corine_code != 211], # exluding arable land
                           c("value_label", "value_code", "unit", "corine3_code")]

  metadata <- lapply(geo, function(x){

    output <- transform(metadata,
                        nuts = x)

    output

  }
  ) %>%
    dplyr::bind_rows() %>%
    unique()

  metadata$series_code <- paste0(
    "A.PROD_BP.",
    metadata$value_code, ".",
    metadata$unit, ".",
    metadata$nuts)

  metadata$full_code <- paste0(
    "Eurostat/agr_r_accts/",
    metadata$series_code) # generating mask codes to download

  # dowloading area data

  data_raw <- rdbnomics::rdb(unique(metadata$full_code))

  data_raw <- data_raw %>%
    dplyr::rename(code = geo)

  # summarizing data

  time_period <- as.character(c((last_yr - h + 1):last_yr))

  corine_codes <- metadata[, c("corine3_code", "series_code")]

  data_raw <- data_raw %>%
    dplyr::left_join(corine_codes) %>%
    dplyr::group_by(code, original_period, corine3_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if(!is.element(as.character(last_yr), data_raw$original_period))
    stop(paste0("Please provide one of the following years as 'last year': ",
                knitr::combine_words(data_raw$original_period)))

  data_raw <- data_raw[data_raw$original_period %in% time_period, ]

  avg_values <- data_raw %>%
    dplyr::left_join(gdp_defl) %>%
    dplyr::mutate(value = value / defl * ref_yr_dfl) %>%
    dplyr::group_by(code, corine3_code) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup()

  }

  if (is.element(211, corine_code)) {

    # If the chosen Corine class is "arable land" the most common crop grown in the region is identified

    main_crop <- most_common_crop(nuts, h, last_yr)[, c("code", "value_label")]

    metadata_arable <- master_table_agr %>%
      dplyr::filter(value_label %in% main_crop$value_label) %>%
      dplyr::left_join(main_crop) %>%
      dplyr::mutate(full_code = paste0("Eurostat/agr_r_accts/A.PROD_BP.",
                                       value_code,
                                       ".",
                                       unit,
                                       ".",
                                       code))

    data_raw_arable <- rdbnomics::rdb(metadata_arable$full_code)

    data_raw_arable <- data_raw_arable %>%
      dplyr::rename(code = geo)

    data_raw_arable <- data_raw_arable %>%
      dplyr::group_by(code, original_period) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup()

    if(!is.element(as.character(last_yr), data_raw_arable$original_period))
      stop(paste0("Please provide one of the following years as 'last year': ",
                  knitr::combine_words(data_raw_arable$original_period)))

    data_raw_arable <- data_raw_arable[data_raw_arable$original_period %in% time_period, ]

    avg_values_arable <- data_raw_arable %>%
      dplyr::left_join(gdp_defl) %>%
      dplyr::mutate(value = value / defl * ref_yr_dfl) %>%
      dplyr::group_by(code) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(corine3_code = 211)

  }

  if (length(corine_code) == 1 & 211 %in% corine_code) {

    value <- avg_values_arable

  } else if (!is.element(211, corine_code)){

    value <- avg_values

  } else {

    value <- dplyr::bind_rows(avg_values, avg_values_arable)

  }

  # adjusting values for for mixed agricultural-natural land Corine classes (i.e. Complex cultivation patterns)
  # Adjusting factors can be found in the master_table_agr

  adj_fct <- master_table_agr[master_table_agr$corine3_code %in% value$corine3_code, c("corine3_code", "fct")] %>%
    unique()

  value %>%
    dplyr::left_join(adj_fct) %>%
    dplyr::mutate(value = value * fct, fct = NULL)

}
