#' Computing average areas covered by a given agricultural crop (Corine classification)
#'
#' This function computes average areas covered by a given (agricultural and grassland) Corine class
#' for each Italian level 2 NUTS regions.
#'
#' @inheritParams most_common_crop
#' @param corine_code A numeric vector. Vector of the Corine classes (level 3) for which the average
#'     area must be computed.
#' @return A tibble with NUTS2 codes, corine3 codes and average area in the time frame specified (expressed in 000 ha)
#'
compute_agr_area <- function(nuts = "Italia", h = 3, last_yr, corine_code) {

  if (any(!is.element(nuts, nuts2_codes$label)) == TRUE)
    stop(paste0("Please provide a valid NUTS2 name. Pick a name among: ",
                knitr::combine_words(nuts2_codes$label, and = "or ")))

  if (212 %in% corine_code)
    stop("The Corine 3 class '212' (Permanently irrigated land) cannot be selected. Please use '211' (non-irrigated arable land) instead.")

  geo <- nuts2_codes[nuts2_codes$label %in% nuts, ]$code

  if (!is.element(211, corine_code) | 211 %in% corine_code & length(corine_code) > 1) {

    metadata <- master_table_agr[master_table_agr$corine3_code %in% corine_code[corine_code != 211], # exluding arable land
                             c("value_label", "area_code", "strucpro", "corine3_code")]

    metadata <- lapply(geo, function(x){

    output <- transform(metadata,
                        nuts = x)

    output

  }
  ) %>%
    dplyr::bind_rows()

  metadata$series_code <- paste0(
    "A.",
    metadata$area_code, ".",
    metadata$strucpro, ".",
    metadata$nuts)

  metadata$full_code <- paste0(
                          "Eurostat/apro_cpshr/",
                          metadata$series_code) # generating mask codes to download

  # dowloading area data

  data_raw <- rdbnomics::rdb(unique(metadata$full_code))

  # summarizing data

  time_period <- as.character(c((last_yr - h + 1):last_yr))

  # defining corine 3 codes conversion table (Estat with series code)

  corine_legend <- unique(metadata[, c("corine3_code", "series_code")])

  data_raw <- data_raw %>%
    dplyr::left_join(corine_legend) %>%
    dplyr::group_by(geo, original_period, corine3_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if(!is.element(as.character(last_yr), data_raw$original_period))
    stop(paste0("Please provide one of the following years as 'last year': ",
                knitr::combine_words(data_raw$original_period)))

  data_raw <- data_raw[data_raw$original_period %in% time_period, ]

  avg_areas <- data_raw %>%
    dplyr::group_by(geo, corine3_code) %>%
    dplyr::summarise(area = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value_label = NA_character_)

  }

  if (is.element(211, corine_code)) {

    # If the chosen Corine class is "arable land" the most common crop grown in the region is identified

    main_crop <- ecoservr::most_common_crop(nuts, h, last_yr)

    main_crop <- main_crop %>%
      dplyr::select(geo, value_label, area = value) %>%
      dplyr::mutate(corine3_code = 211)


  }

  if (length(corine_code) == 1 & 211 %in% corine_code) {

    main_crop

  } else if (!is.element(211, corine_code)){

    avg_areas

  } else {

    dplyr::bind_rows(main_crop, avg_areas)

  }

}

